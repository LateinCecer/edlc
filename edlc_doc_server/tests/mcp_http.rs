/*
 * EDLc, a compiler for the EDL programming language.
 * Copyright (C) 2026  Adrian Paskert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
//! Integration test for the MCP Streamable HTTP transport (TCP).
//!
//! Starts the real `serve_http` on an ephemeral loopback port against a small temporary
//! documentation database, then drives the Streamable HTTP handshake with raw HTTP requests:
//! `initialize` → `notifications/initialized` → `tools/list` → `tools/call search_docs`.

use edlc_core::prelude::{
    DocGenerator, EnvDoc, FuncDoc, FuncParamDoc, FuncParamsDoc, Item, Modifiers, ModuleDoc,
    PortableModuleSrc, SrcPos, TypeDoc,
};
use edlc_doc_db::{DocDb, DocDbWriter};

/// Extracts the JSON-RPC messages from a Streamable HTTP response body, which is either a
/// plain JSON value or an SSE stream whose `data:` lines each carry one JSON value.
fn extract_json(body: &str) -> Vec<serde_json::Value> {
    let trimmed = body.trim();
    if trimmed.starts_with('{') || trimmed.starts_with('[') {
        return vec![serde_json::from_str(trimmed).unwrap_or_else(|e| {
            panic!("response is not valid JSON: {e}\nbody: {body}")
        })];
    }
    trimmed
        .lines()
        .filter_map(|line| line.strip_prefix("data:"))
        .filter_map(|data| serde_json::from_str::<serde_json::Value>(data.trim()).ok())
        .collect()
}

/// Finds the response to the request with the given id among extracted JSON-RPC messages.
fn response_with_id<'a>(msgs: &'a [serde_json::Value], id: u64) -> &'a serde_json::Value {
    msgs.iter()
        .find(|m| m.get("id").and_then(|i| i.as_u64()) == Some(id))
        .unwrap_or_else(|| panic!("no response with id {id} in {msgs:?}"))
}

/// Calls an MCP tool over the established session and returns the JSON-RPC response value.
async fn call_tool(
    client: &reqwest::Client,
    url: &str,
    session_id: &str,
    id: u64,
    tool: &str,
    arguments: serde_json::Value,
) -> serde_json::Value {
    let body = serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": "tools/call",
        "params": { "name": tool, "arguments": arguments },
    });
    let text = client
        .post(url)
        .header("Content-Type", "application/json")
        .header("Accept", "application/json, text/event-stream")
        .header("mcp-session-id", session_id)
        .header("Mcp-Protocol-Version", "2025-06-18")
        .body(body.to_string())
        .send()
        .await
        .unwrap_or_else(|e| panic!("tools/call {tool} request failed: {e}"))
        .text()
        .await
        .unwrap_or_else(|e| panic!("tools/call {tool} body failed: {e}"));
    let msgs = extract_json(&text);
    response_with_id(&msgs, id).clone()
}

/// Parses the `content[0].text` of a tool result as JSON.
fn tool_result_json(call: &serde_json::Value) -> serde_json::Value {
    let text = call["result"]["content"][0]["text"]
        .as_str()
        .unwrap_or_else(|| panic!("tool result has no text content: {call}"));
    serde_json::from_str(text).unwrap_or_else(|e| panic!("tool result is not JSON: {e}"))
}

/// Builds a `FuncDoc` for `fn {qual}::add_overflow(a: usize, b: usize) -> usize`.
fn overflow_fn(qual: &[&str]) -> Item {
    let pos = SrcPos::new(1, 1, 10);
    Item::from(FuncDoc {
        name: qual.iter().map(|s| s.to_string()).collect::<Vec<_>>().into(),
        src: PortableModuleSrc::File("test.edl".to_string()),
        pos,
        doc: "Adds two usizes, wrapping on overflow.".to_string(),
        env: EnvDoc { params: vec![] },
        params: FuncParamsDoc::from(vec![
            FuncParamDoc {
                name: "a".to_string(),
                pos,
                ty: TypeDoc::Base("usize".to_string().into(), None),
                ms: Modifiers::default(),
            },
            FuncParamDoc {
                name: "b".to_string(),
                pos,
                ty: TypeDoc::Base("usize".to_string().into(), None),
                ms: Modifiers::default(),
            },
        ]),
        ret: TypeDoc::Base("usize".to_string().into(), None),
        ms: Modifiers::default(),
        async_return: false,
        associated_type: None,
    })
}

/// Builds a small documentation database at `path` containing the modules `example` and
/// `other`, each with a function named `add_overflow` — so the simple name is ambiguous.
fn build_db(path: &std::path::Path) {
    let mut writer = DocDbWriter::open(path).expect("open db");
    writer
        .insert_definition(&Item::from(ModuleDoc {
            name: vec!["example".to_string()].into(),
            doc: "The example module.".to_string(),
        }))
        .expect("insert example module");
    writer
        .insert_definition(&Item::from(ModuleDoc {
            name: vec!["other".to_string()].into(),
            doc: "The other module.".to_string(),
        }))
        .expect("insert other module");
    writer
        .insert_definition(&overflow_fn(&["example", "add_overflow"]))
        .expect("insert example::add_overflow");
    writer
        .insert_definition(&overflow_fn(&["other", "add_overflow"]))
        .expect("insert other::add_overflow");
    writer.finish().expect("finish db");
}

#[test]
fn mcp_http_end_to_end() {
    let dir = std::env::temp_dir().join(format!(
        "edlc_doc_server_mcp_http_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let db_path = dir.join("docs.db");
    let _ = std::fs::remove_file(&db_path);
    build_db(&db_path);
    let db = DocDb::open_readonly(&db_path).expect("open read-only db");

    let runtime = tokio::runtime::Runtime::new().expect("tokio runtime");
    runtime.block_on(async move {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
            .await
            .expect("bind ephemeral port");
        let addr = listener.local_addr().expect("local address");
        let bind = "127.0.0.1".to_string();
        tokio::spawn(async move {
            let _ = edlc_doc_server::mcp::serve_http(listener, db, &bind).await;
        });

        let client = reqwest::Client::new();
        let url = format!("http://{addr}/mcp");

        // 1) initialize (the protocol version goes in the body; the header is optional here).
        let resp = client
            .post(&url)
            .header("Content-Type", "application/json")
            .header("Accept", "application/json, text/event-stream")
            .body(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"mcp-http-test","version":"1.0.0"}}}"#)
            .send()
            .await
            .expect("initialize request");
        assert_eq!(resp.status(), 200, "initialize should be accepted");
        let session_id = resp
            .headers()
            .get("mcp-session-id")
            .and_then(|v| v.to_str().ok())
            .expect("mcp-session-id header")
            .to_string();
        let body = resp.text().await.expect("initialize body");
        let msgs = extract_json(&body);
        let init = response_with_id(&msgs, 1);
        assert!(
            init.get("result")
                .and_then(|r| r.get("serverInfo"))
                .is_some(),
            "initialize result missing: {init}"
        );

        // 2) initialized notification → 202 Accepted.
        let status = client
            .post(&url)
            .header("Content-Type", "application/json")
            .header("Accept", "application/json, text/event-stream")
            .header("mcp-session-id", &session_id)
            .header("Mcp-Protocol-Version", "2025-06-18")
            .body(r#"{"jsonrpc":"2.0","method":"notifications/initialized"}"#)
            .send()
            .await
            .expect("initialized notification")
            .status();
        assert_eq!(status, 202, "initialized notification should be accepted");

        // 3) tools/list exposes all five tools.
        let body = client
            .post(&url)
            .header("Content-Type", "application/json")
            .header("Accept", "application/json, text/event-stream")
            .header("mcp-session-id", &session_id)
            .header("Mcp-Protocol-Version", "2025-06-18")
            .body(r#"{"jsonrpc":"2.0","id":2,"method":"tools/list"}"#)
            .send()
            .await
            .expect("tools/list request")
            .text()
            .await
            .expect("tools/list body");
        let msgs = extract_json(&body);
        let list = response_with_id(&msgs, 2);
        let names: Vec<String> = list["result"]["tools"]
            .as_array()
            .expect("tools array")
            .iter()
            .map(|t| t["name"].as_str().expect("tool name").to_string())
            .collect();
        for tool in ["search_docs", "get_doc", "list_modules", "get_module", "list_items"] {
            assert!(
                names.contains(&tool.to_string()),
                "missing tool {tool} in {names:?}"
            );
        }

        // 4) tools/call search_docs with a prefix query.
        let body = client
            .post(&url)
            .header("Content-Type", "application/json")
            .header("Accept", "application/json, text/event-stream")
            .header("mcp-session-id", &session_id)
            .header("Mcp-Protocol-Version", "2025-06-18")
            .body(r#"{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"search_docs","arguments":{"query":"add ov","limit":5}}}"#)
            .send()
            .await
            .expect("search_docs request")
            .text()
            .await
            .expect("search_docs body");
        let msgs = extract_json(&body);
        let call = response_with_id(&msgs, 3);
        let results = tool_result_json(call);
        let hits = results.as_array().expect("search results array");
        assert!(
            hits.iter().any(|h| h["name"] == "add_overflow"),
            "search_docs should find add_overflow: {results}"
        );
        // By default the heavy blob is omitted from results.
        assert!(
            !results.to_string().contains("\"blob\""),
            "search_docs should omit blobs by default: {results}"
        );

        // 5) search_docs with a kind filter.
        let call = call_tool(
            &client,
            &url,
            &session_id,
            4,
            "search_docs",
            serde_json::json!({ "query": "add ov", "limit": 5, "kind": "fn" }),
        )
        .await;
        let fns = tool_result_json(&call);
        let fns = fns.as_array().expect("fn search results array");
        assert_eq!(fns.len(), 2, "both add_overflow fns should match: {fns:?}");
        assert!(
            fns.iter().all(|h| h["kind"] == "fn"),
            "kind filter should return only fns: {fns:?}"
        );

        // 6) list_items covers all items, including both modules.
        let call = call_tool(
            &client,
            &url,
            &session_id,
            5,
            "list_items",
            serde_json::json!({}),
        )
        .await;
        let all = tool_result_json(&call);
        let all = all.as_array().expect("list_items array");
        let quals: Vec<&str> = all
            .iter()
            .map(|i| i["qual_name"].as_str().expect("qual_name"))
            .collect();
        for expected in ["example", "other", "example::add_overflow", "other::add_overflow"] {
            assert!(quals.contains(&expected), "list_items missing {expected}: {quals:?}");
        }

        // 7) list_items with a kind filter.
        let call = call_tool(
            &client,
            &url,
            &session_id,
            6,
            "list_items",
            serde_json::json!({ "kind": "fn" }),
        )
        .await;
        let fns = tool_result_json(&call);
        let fns = fns.as_array().expect("list_items fn array");
        assert_eq!(fns.len(), 2, "only the two fns should remain: {fns:?}");

        // 8) get_doc with an ambiguous simple name reports the candidates.
        let call = call_tool(
            &client,
            &url,
            &session_id,
            7,
            "get_doc",
            serde_json::json!({ "name": "add_overflow" }),
        )
        .await;
        let ambig = tool_result_json(&call);
        assert_eq!(ambig["error"], "ambiguous", "expected ambiguous error: {ambig}");
        let candidates = ambig["candidates"].as_array().expect("candidates array");
        let cand_quals: Vec<&str> = candidates
            .iter()
            .map(|c| c["qual_name"].as_str().expect("candidate qual_name"))
            .collect();
        assert_eq!(cand_quals.len(), 2, "unexpected candidates: {cand_quals:?}");
        for expected in ["example::add_overflow", "other::add_overflow"] {
            assert!(
                cand_quals.contains(&expected),
                "candidates missing {expected}: {cand_quals:?}"
            );
        }

        // 9) get_doc with a qualified name succeeds and omits the blob by default.
        let call = call_tool(
            &client,
            &url,
            &session_id,
            8,
            "get_doc",
            serde_json::json!({ "name": "example::add_overflow" }),
        )
        .await;
        let doc = tool_result_json(&call);
        assert_eq!(doc["qual_name"], "example::add_overflow", "wrong item: {doc}");
        assert!(
            !doc.as_object().expect("doc object").contains_key("blob"),
            "get_doc should omit the blob by default: {doc}"
        );

        // 10) get_doc with details: true includes the blob.
        let call = call_tool(
            &client,
            &url,
            &session_id,
            9,
            "get_doc",
            serde_json::json!({ "name": "example::add_overflow", "details": true }),
        )
        .await;
        let doc = tool_result_json(&call);
        assert!(
            doc.get("blob").is_some_and(|b| !b.is_null()),
            "get_doc details should include the blob: {doc}"
        );

        // 11) get_doc with an unknown name is a not-found error.
        let call = call_tool(
            &client,
            &url,
            &session_id,
            10,
            "get_doc",
            serde_json::json!({ "name": "nope" }),
        )
        .await;
        let missing = tool_result_json(&call);
        assert_eq!(missing["error"], "not found", "expected not found: {missing}");
    });

    let _ = std::fs::remove_dir_all(&dir);
}
