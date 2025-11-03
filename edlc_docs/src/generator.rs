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

use std::io::Write;
use edlc_core::prelude::*;
use build_html::{Html, HtmlChild, HtmlContainer, HtmlElement, HtmlPage, HtmlTag};
use serde::Serialize;

#[derive(Debug, Default, Serialize)]
struct DocsInfo {
    modules: Vec<ModuleDoc>,
    fns: Vec<FuncDoc>,
    lets: Vec<LetDoc>,
    consts: Vec<ConstDoc>,
}

pub struct HtmlGenerator<W> {
    writer: W,
    docs_info: DocsInfo,
}

impl<W: Write> HtmlGenerator<W> {
    pub fn new(writer: W) -> Self {
        HtmlGenerator {
            writer,
            docs_info: DocsInfo::default(),
        }
    }

    pub fn generate_style_sheet(&self) -> String {
        r#"
body {
    background-color: #272829;
}

h1 {
    background-color: #272829;
    font: 1.5em system-ui;
    font-style: bold;
    color: grey;
    line-height: 1.7em;
    padding: 10px;
    border: 2px solid #5b5c5e;
    border-radius: 5px;
}

.code {
    background-color: #302d33;
    font: 1em system-ui;
    color: grey;
    padding: 5px;
    border: 1px solid #5b5c5e;
    border-radius: 5px;
}

.doc-text {
    background-color: #272829;
    font: 1.0em system-ui;
    color: grey;
    line-height: 1.2em;
    margin-top: 5px;
    margin-bottom: 5px;
}

.search-bar {
    background-color: #272829;
    font: 1.0em system-ui;
    color: grey;
    line-height: 1.2em;
    margin-top: 0px;
    position: fixed;
    border: 2px solid #5b5c5e;
    border-radius: 5px;
    padding: 5px;
    right: 0;
    top: 0;
}

.marker {
    position: absolute;
    font: 1em system-ui;
    background-color: #0c8499;
    z-index: 16;
    opacity: 30%;
    rounded: 2px;
}

.keyword {
    font: 1em system-ui;
    color: #a81b72;
    font-style: italic;
}

.reference-item {
    border: none;
    text-decoration: none;
    transition: all 250ms ease-out;
}

.reference-item:hover {
    border: none;
    text-decoration: underline #990c7d;
    text-decoration-thickness: 2px;
    transition: all 250ms ease-in;
}

.type-name {
    font: 1em system-ui;
    color: #0c9935;
}

.fn-name {
    font: 1em system-ui;
    color: #5d88a1;
}

.path-segment {
    font: 1em system-ui;
    color: #abb0ab;
}

.var-name {
    font: 1em system-ui;
    color: #e8e8e6;
}

.const-name {
    font: 1em system-ui;
    color: #bd8113;
}

.operator {
    font: 1em system-ui;
    color: #de6b0d;
}

.decorator {
    font: 1em system-ui;
    color: grey;
}

.comment {
    font: 1em system-ui;
    color: grey;
}

.num-literal {
    font: 1em system-ui;
    color: #d69c13;
}

.str-literal {
    font: 1em system-ui;
    color: #d17f13;
}

.char-literal {
    font: 1em system-ui;
    color: #d17f13;
}

.bool-literal {
    font: 1em system-ui;
    color: #96033c;
}
        "#.to_string()
    }

    pub fn generate_script(&self) -> String {
        format!(r#"
const docs = JSON.parse(decodeURIComponent("{}"));
{}
        "#,
                urlencoding::encode(&serde_json::to_string(&self.docs_info).unwrap()),
                include_str!("dynamic_docs.js")).to_string()
    }

    pub fn finish(&mut self) -> Result<(), std::io::Error> {
        // let _docs = serde_json::to_string(&self.docs_info)?;
        let page = HtmlPage::new()
            .with_meta([("charset", "UTF-8")])
            .with_title("docs.eq")
            .with_meta([("name", "viewport"), ("content", "with=device-width, initial-scale=1")])
            .with_style(self.generate_style_sheet())
            .with_html(HtmlElement::new(HtmlTag::Div)
                .with_child(HtmlChild::Raw(r#"
                <div class="search-bar">
                    <label for="search">search:</label>
                    <input type="text" id="search-text" name="search" class="code"><br>
                </div>
                "#.to_string())))
            .with_html(HtmlElement::new(HtmlTag::Div)
                .with_attribute("id", "doc-content")
                .with_paragraph("Documentation")
                .with_child(HtmlChild::Raw(format!(r#"<script type="text/javascript">{}</script>"#, self.generate_script())))
            );
        write!(self.writer, "{}", page.to_html_string())
    }
}

impl<W: Write> DocGenerator for HtmlGenerator<W> {
    type Error = std::io::Error;

    fn insert_definition(&mut self, item: &Item) -> Result<(), Self::Error> {
        match item {
            Item::GlobalVar(val) => self.docs_info.lets.push(val.clone()),
            Item::GlobalConst(val) => self.docs_info.consts.push(val.clone()),
            Item::Func(val) =>  self.docs_info.fns.push(val.clone()),
            Item::Module(val) => self.docs_info.modules.push(val.clone()),
        }
        Ok(())
    }
}
