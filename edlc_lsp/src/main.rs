/*
 *    Copyright 2025 Adrian Paskert
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

use std::{fs, mem};
use std::str::Chars;

use tower_lsp::{Client, LanguageServer, LspService, Server};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult>
    {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            range: Some(true),
                            full: None,
                            legend: SemanticTokensLegend {
                                token_modifiers: vec![
                                    SemanticTokenModifier::DECLARATION,
                                    SemanticTokenModifier::ASYNC,
                                ],
                                token_types: vec![
                                    SemanticTokenType::DECORATOR,
                                    SemanticTokenType::KEYWORD,
                                ],
                            },
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                        }
                    )
                ),
                .. Default::default()
            },
            .. Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await
    }


    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(
                MarkedString::String("You're hovering!".to_string())
            ),
            range: None,
        }))
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        let src = fs::read_to_string(params.text_document.uri.path())
            .expect("failed to read file");
        // find keywords
        let lexer = BasicLexer::new(&src);
        let tokens: Vec<_> = lexer.collect();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: Some("1".to_string()),
            data: tokens
        })))
    }
}

struct BasicLexer<'a> {
    chars: Chars<'a>,
    current_char: Option<char>,
    line: usize,
    col: usize,

    last_line: usize,
    last_col: usize,
}

impl<'a> BasicLexer<'a> {
    fn new(src: &'a str) -> Self {
        let mut lexer = BasicLexer {
            chars: src.chars(),
            current_char: None,
            line: 0,
            col: 0,
            last_line: 0,
            last_col: 0,
        };
        let _ = lexer.next_char();
        lexer
    }

    fn next_char(&mut self) -> Option<char> {
        let mut c = self.chars.next();
        mem::swap(&mut self.current_char, &mut c);

        match c {
            Some('\n') => {
                self.col = 0;
                self.line += 1;
            }
            Some(_) => {
                self.col += 1;
            }
            None => ()
        }
        c
    }

    fn next_token(&mut self) -> Option<SemanticToken> {
        match self.next_char()? {
            c if c.is_whitespace() => None,
            mut c => {
                // start collecting chars into string
                let delta_line = self.last_line - self.line;
                let col = self.col;
                let delta_col = self.last_col - col;

                let mut buf = String::new();
                buf.push(c);
                loop {
                    if let Some(next_c) = self.next_char() {
                        c = next_c;
                    } else {
                        break;
                    }

                    if c.is_whitespace() {
                        break;
                    }
                    buf.push(c);
                }
                // update last line & col
                self.last_line = self.line;
                self.last_col = self.col;

                // process string
                match buf {
                    s if s == "fn" => Some(SemanticToken {
                        delta_line: delta_line as u32,
                        delta_start: delta_col as u32,
                        length: (self.col - col) as u32,
                        token_type: 1,
                        token_modifiers_bitset: 1,
                    }),
                    _ => Some(SemanticToken {
                        delta_line: delta_line as u32,
                        delta_start: delta_col as u32,
                        length: (self.col - col) as u32,
                        token_type: 0,
                        token_modifiers_bitset: 0,
                    }),
                }
            }
        }
    }
}

impl<'a> Iterator for BasicLexer<'a> {
    type Item = SemanticToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}


#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}
