pub mod lsp {
    use std::collections::{BTreeMap, HashMap};
    use std::fmt::Debug;
    use std::ops::Bound::Included;

    use ebnf_parser::ast::SingleDefinition;
    use ebnf_parser::error::SyntaxError;
    use ebnf_parser::ParseResult;
    use nom::Offset;
    use rangemap::RangeMap;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Location {
        pub line: usize,
        pub col: usize,
    }

    #[derive(Debug)]
    pub struct LspContext {
        src: Option<String>,
        definitions: HashMap<String, Location>,
        alternative_definitions: HashMap<String, Vec<Location>>,
        hover: HashMap<String, String>,
        alternative_hover: HashMap<String, Vec<String>>,
        references: HashMap<String, Vec<Location>>,
        offset_to_line: BTreeMap<usize, usize>,
        line_to_offset: HashMap<usize, usize>,
        symbols: RangeMap<usize, String>,
        syntax_error: Option<SyntaxError>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum LspErrorType {
        SyntaxError,
        UnusedDefinition,
        UndefinedReference,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct LspError {
        pub message: String,
        pub start: Location,
        pub end: Location,
        pub error_type: LspErrorType,
    }

    impl LspContext {
        pub fn from_src(doc_content: String) -> Self {
            let mut offset_to_line = BTreeMap::new();
            let mut line_to_offset = HashMap::new();
            doc_content
                .lines()
                .enumerate()
                .map(|(i, l)| (i, doc_content.offset(l)))
                .for_each(|(line_number, offset)| {
                    offset_to_line.insert(offset, line_number);
                    line_to_offset.insert(line_number, offset);
                });

            let mut lsp_context = Self {
                src: None,
                definitions: HashMap::new(),
                alternative_definitions: HashMap::new(),
                hover: HashMap::new(),
                references: HashMap::new(),
                offset_to_line,
                line_to_offset,
                symbols: RangeMap::new(),
                alternative_hover: HashMap::new(),
                syntax_error: None,
            };

            let lexer = ebnf_parser::Lexer::new(&doc_content);
            let parser = ebnf_parser::Parser::new(lexer);
            let parse_results = parser.parse();

            match parse_results {
                Ok(x) => lsp_context.compute_lsp_context(&doc_content, &x),
                Err(e) => lsp_context.syntax_error = Some(e),
            }
            lsp_context.src = Some(doc_content);
            lsp_context
        }

        fn compute_lsp_context(&mut self, doc_content: &str, parse_results: &ParseResult) {
            self.compute_symbols(parse_results);
            for rule in &parse_results.syntax.rules {
                let loc = self.location_at_offset(rule.span.start);
                let old_def = self.definitions.insert(rule.name.to_string(), loc.clone());
                match old_def {
                    Some(old_loc) => self
                        .alternative_definitions
                        .entry(rule.name.to_string())
                        .or_insert_with(Vec::new)
                        .push(old_loc),
                    None => {}
                }
                let hover_span = rule.span.start..rule.span.end;
                let hover_text = doc_content[hover_span].to_string();
                let old_hover = self.hover.insert(rule.name.to_string(), hover_text);
                match old_hover {
                    Some(old_hover_text) => self
                        .alternative_hover
                        .entry(rule.name.to_string())
                        .or_insert_with(Vec::new)
                        .push(old_hover_text),
                    None => {}
                }
                self.refs_from_definitions(&rule.definitions);
            }
        }

        fn refs_from_definitions<'src>(&mut self, definitions: &Vec<SingleDefinition<'src>>) {
            for def in definitions {
                for term in &def.terms {
                    self.refs_from_syntatic_primary(&term.factor.primary);
                    if let Some(expcetion) = &term.exception {
                        self.refs_from_syntatic_primary(&expcetion.primary)
                    }
                }
            }
        }

        fn refs_from_syntatic_primary(&mut self, primary: &ebnf_parser::ast::SyntacticPrimary) {
            match &primary.kind {
                ebnf_parser::ast::SyntacticPrimaryKind::OptionalSequence(a) => {
                    self.refs_from_definitions(&a)
                }
                ebnf_parser::ast::SyntacticPrimaryKind::RepeatedSequence(r) => {
                    self.refs_from_definitions(&r)
                }
                ebnf_parser::ast::SyntacticPrimaryKind::GroupedSequence(g) => {
                    self.refs_from_definitions(&g)
                }
                ebnf_parser::ast::SyntacticPrimaryKind::MetaIdentifier(i) => {
                    let loc = self.location_at_offset(primary.span.start);
                    self.references
                        .entry(i.to_string())
                        .or_insert_with(Vec::new)
                        .push(loc)
                }
                _ => {} // Do nothing for special sequences, terminals and empty
            }
        }

        fn compute_symbols(&mut self, parse_results: &ParseResult) {
            parse_results
                .tokens
                .iter()
                .filter_map(|t| {
                    if let ebnf_parser::TokenKind::Identifier(x) = t.kind {
                        Some((x, t.span))
                    } else {
                        None
                    }
                })
                .for_each(|(x, span)| {
                    let range = span.start..span.end;
                    self.symbols.insert(range, x.to_string());
                })
        }

        fn location_at_offset(&self, offset: usize) -> Location {
            let line_offsets = self
                .offset_to_line
                .range((Included(0), Included(offset)))
                .into_iter()
                .rev()
                .next();

            match line_offsets {
                Some((line_offset, line_number)) => Location {
                    line: *line_number,
                    col: offset - line_offset,
                },
                None => Location {
                    line: 0,
                    col: offset,
                },
            }
        }

        pub fn syntax_error_to_lsp_error(&self) -> Option<LspError> {
            match &self.syntax_error {
                Some(x) => Some(LspError {
                    message: x.message.clone(),
                    start: self.location_at_offset(x.span.start),
                    end: self.location_at_offset(x.span.end),
                    error_type: LspErrorType::SyntaxError,
                }),
                None => None,
            }
        }

        fn offset_at_location(&self, location: &Location) -> usize {
            let line_number = location.line;
            let line_offset = self.line_to_offset.get(&line_number).unwrap_or(&0);
            line_offset + location.col
        }

        fn unused_defs(&self) -> Vec<LspError> {
            self.definitions
                .keys()
                .filter(|k| self.references.get(*k).is_none())
                .map(|k| {
                    let start = self.definitions.get(k).unwrap();
                    let end = Location {
                        line: start.line,
                        col: start.col + k.len(),
                    };
                    let other_defs: Vec<LspError> = self
                        .alternative_definitions
                        .get(k)
                        .unwrap_or(&Vec::new())
                        .iter()
                        .map(|loc| LspError {
                            message: format!("Unused definition: {}", k),
                            start: loc.clone(),
                            end: Location {
                                line: loc.line,
                                col: loc.col + k.len(),
                            },
                            error_type: LspErrorType::UnusedDefinition,
                        })
                        .collect();
                    let main_def = LspError {
                        message: format!("Unused definition: {}", k),
                        start: start.clone(),
                        end,
                        error_type: LspErrorType::UnusedDefinition,
                    };
                    let mut v = vec![main_def];
                    v.extend(other_defs);
                    v
                })
                .flatten()
                .collect()
        }

        pub fn root_rule(&self) -> Option<(Location, Location)> {
            let unused_defs = self.unused_defs();
            if unused_defs.len() == 1 {
                Some((unused_defs[0].start.clone(), unused_defs[0].end.clone()))
            } else {
                None
            }
        }

        pub fn format(&self) -> Option<String> {
            let gg = ebnf_fmt::format_code(
                self.src.as_ref()?,
                &ebnf_fmt::Configuration {
                    line_width: 4,
                    ..Default::default()
                },
            );
            match gg {
                Ok(x) => Some(x),
                Err(_) => None,
            }
        }

        pub fn diagnostics(&self) -> Vec<LspError> {
            let unused_defs = self.unused_defs();
            let undefined_refs: Vec<LspError> = self
                .references
                .keys()
                .filter(|k| self.definitions.get(*k).is_none())
                .map(|k| {
                    let gg: Vec<LspError> = self
                        .references
                        .get(k)
                        .unwrap()
                        .into_iter()
                        .map(|r| {
                            let end = Location {
                                line: r.line,
                                col: r.col + k.len(),
                            };
                            LspError {
                                message: format!("Undefined reference: {}", k),
                                start: r.clone(),
                                end,
                                error_type: LspErrorType::UndefinedReference,
                            }
                        })
                        .collect();
                    gg
                })
                .flatten()
                .collect();

            let mut diagnostics = Vec::new();
            if unused_defs.len() > 1 {
                diagnostics.extend(unused_defs);
            }
            diagnostics.extend(undefined_refs);
            diagnostics
        }

        pub fn hover_from_def(&self, str: &str) -> Option<String> {
            self.hover.get(str).map(|s| s.clone())
        }

        pub fn hover(&self, location: &Location) -> Option<&str> {
            let offset = self.offset_at_location(location);
            let symbol = self.symbols.get(&offset)?;
            self.hover.get(symbol.as_str()).map(|s| s.as_str())
        }

        pub fn hover_alternatives(&self, location: &Location) -> Vec<String> {
            let offset = self.offset_at_location(location);
            let symbol = self.symbols.get(&offset).ok_or("No symbol found");
            match symbol {
                Ok(symbol) => self
                    .alternative_hover
                    .get(symbol)
                    .cloned()
                    .unwrap_or(Vec::new()),
                Err(_) => Vec::new(),
            }
        }

        pub fn references(&self, location: &Location) -> Option<Vec<Location>> {
            let offset = self.offset_at_location(location);
            let symbol = self.symbols.get(&offset)?;
            self.references.get(symbol.as_str()).map(|v| v.clone())
        }

        pub fn symbol(&self, location: &Location) -> Option<&str> {
            let offset = self.offset_at_location(location);
            self.symbols.get(&offset).map(|s| s.as_str())
        }

        pub fn symbols(&self) -> Vec<String> {
            self.definitions.keys().cloned().collect()
        }

        pub fn definition(&self, location: &Location) -> Option<&Location> {
            let offset = self.offset_at_location(location);
            let symbol = self.symbols.get(&offset)?;
            self.definitions.get(symbol.as_str())
        }

        pub fn alternative_definitions(&self, location: &Location) -> Vec<Location> {
            let offset = self.offset_at_location(location);
            let symbol = self.symbols.get(&offset).ok_or("No symbol found");
            match symbol {
                Ok(symbol) => self
                    .alternative_definitions
                    .get(symbol)
                    .cloned()
                    .unwrap_or(Vec::new()),
                Err(_) => Vec::new(),
            }
        }
    }

    mod tests {

        #[test]
        fn test_hovers() {
            let ebnf = "very_nice_stuff = hello;\nhello = \"world\";\ncool = hello;";
            let lsp_context = super::LspContext::from_src(ebnf.to_string());
            let hover = lsp_context.hover.get("hello").unwrap();
            assert_eq!(hover, "hello = \"world\";");
            let hover = lsp_context.hover.get("cool").unwrap();
            assert_eq!(hover, "cool = hello;");

            let hover = lsp_context.hover(&super::Location { line: 2, col: 9 });
            assert_eq!(hover, Some("hello = \"world\";"));
        }

        #[test]
        fn test_refs() {
            let ebnf = "very_nice_stuff = hello;\nhello = \"world\";\ncool = hello;";

            let lsp_context = super::LspContext::from_src(ebnf.to_string());
            let refs = lsp_context.references.get("hello").unwrap();
            assert_eq!(refs.len(), 2);
            assert_eq!(refs[0], super::Location { line: 0, col: 18 });
            assert_eq!(refs[1], super::Location { line: 2, col: 7 });

            let refs = lsp_context
                .references(&super::Location { line: 2, col: 10 })
                .expect("Should have refs");
            assert_eq!(refs.len(), 2);
            assert_eq!(refs[0], super::Location { line: 0, col: 18 });
            assert_eq!(refs[1], super::Location { line: 2, col: 7 });
        }

        #[test]
        fn test_defs() {
            let ebnf = "very_nice_stuff = hello;\nhello = \"world\";\ncool = hello;";
            let lsp_context = super::LspContext::from_src(ebnf.to_string());
            let hello_def_loc = lsp_context.definitions.get("hello").unwrap();
            assert_eq!(hello_def_loc, &super::Location { line: 1, col: 0 });
            let hello_def_loc = lsp_context
                .definition(&super::Location { line: 2, col: 9 })
                .unwrap();
            assert_eq!(hello_def_loc, &super::Location { line: 1, col: 0 });
        }

        #[test]
        fn test_nodiagnostics() {
            let ebnf = "very_nice_stuff = hello;\nhello = \"world\";";
            let lsp_context = super::LspContext::from_src(ebnf.to_string());
            let diagnostics = lsp_context.diagnostics();
            assert_eq!(diagnostics.len(), 0);
        }

        #[test]
        fn test_unused() {
            let ebnf = "hello = \"hello\";\nworld = hello;\ntest = hello;";
            let lsp_context = super::LspContext::from_src(ebnf.to_string());
            let diagnostics = lsp_context.diagnostics();
            assert_eq!(diagnostics.len(), 2);
            assert_eq!(diagnostics[0].message, "Unused definition: world");
            assert_eq!(diagnostics[1].message, "Unused definition: test");
        }

        #[test]
        fn test_undefined() {
            let ebnf = "hello = world;";
            let lsp_context = super::LspContext::from_src(ebnf.to_string());
            let diagnostics = lsp_context.diagnostics();
            assert_eq!(diagnostics.len(), 1);
            assert_eq!(diagnostics[0].message, "Undefined reference: world");
        }

        #[test]
        fn test_root_rule() {
            let ebnf = "hello = world;";
            let lsp_context = super::LspContext::from_src(ebnf.to_string());
            let root_rule = lsp_context.root_rule().unwrap();
            assert_eq!(root_rule.0, super::Location { line: 0, col: 0 });
            assert_eq!(root_rule.1, super::Location { line: 0, col: 5 });
        }
    }
}
