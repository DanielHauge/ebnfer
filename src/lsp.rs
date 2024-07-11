pub mod lsp {
    use std::collections::{BTreeMap, HashMap};
    use std::fmt::Debug;
    use std::ops::Bound::Included;

    use ebnf_parser::ast::SingleDefinition;
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
        definitions: HashMap<String, Location>,
        hover: HashMap<String, String>,
        references: HashMap<String, Vec<Location>>,
        offset_to_line: BTreeMap<usize, usize>, // Offset -> Line number
        line_to_offset: HashMap<usize, usize>,  // Line Number -> Offset
        symbols: RangeMap<usize, String>,
    }

    impl LspContext {
        pub fn from_parse_results(doc_content: &str, parse_results: &ParseResult) -> Self {
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
                definitions: HashMap::new(),
                hover: HashMap::new(),
                references: HashMap::new(),
                offset_to_line,
                line_to_offset,
                symbols: RangeMap::new(),
            };
            lsp_context.compute_lsp_context(doc_content, parse_results);
            lsp_context
        }

        fn compute_lsp_context(&mut self, doc_content: &str, parse_results: &ParseResult) {
            self.compute_symbols(parse_results);
            for rule in &parse_results.syntax.rules {
                let loc = self.location_at_offset(rule.span.start);
                self.definitions.insert(rule.name.to_string(), loc.clone());
                self.references
                    .entry(rule.name.to_string())
                    .or_insert_with(Vec::new)
                    .push(loc);
                let hover_span = rule.span.start..rule.span.end;
                let hover_text = doc_content[hover_span].to_string();
                self.hover.insert(rule.name.to_string(), hover_text);
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

        fn offset_at_location(&self, location: &Location) -> usize {
            let line_number = location.line;
            let line_offset = self.line_to_offset.get(&line_number).unwrap_or(&0);
            line_offset + location.col
        }

        pub fn hover(&self, location: &Location) -> Option<&str> {
            let offset = self.offset_at_location(location);
            let symbol = self.symbols.get(&offset)?;
            self.hover.get(symbol.as_str()).map(|s| s.as_str())
        }

        pub fn references(&self, location: &Location) -> Option<Vec<Location>> {
            let offset = self.offset_at_location(location);
            let symbol = self.symbols.get(&offset)?;
            self.references.get(symbol.as_str()).map(|v| v.clone())
        }

        pub fn definition(&self, location: &Location) -> Option<&Location> {
            let offset = self.offset_at_location(location);
            let symbol = self.symbols.get(&offset)?;
            self.definitions.get(symbol.as_str())
        }
    }

    mod tests {
        use ebnf_parser::ParseResult;

        use crate::lsp::lsp::Location;

        fn parse_test_example<'src>() -> (&'src str, ParseResult<'src>) {
            let ebnf = "very_nice_stuff = hello;\nhello = \"world\";\ncool = hello;";
            let lexer = ebnf_parser::Lexer::new(&ebnf);
            let parser = ebnf_parser::Parser::new(lexer);
            let parse_result = parser.parse().expect("Should be able to parse");
            (ebnf, parse_result)
        }

        #[test]
        fn test_hovers() {
            let (ebnf, parse_result) = parse_test_example();
            let lsp_context = super::LspContext::from_parse_results(ebnf, &parse_result);
            let hover = lsp_context.hover.get("hello").unwrap();
            assert_eq!(hover, "hello = \"world\";");
            let hover = lsp_context.hover.get("cool").unwrap();
            assert_eq!(hover, "cool = hello;");

            let hover = lsp_context.hover(&Location { line: 2, col: 9 });
            assert_eq!(hover, Some("hello = \"world\";"));
        }

        #[test]
        fn test_refs() {
            let (ebnf, parse_result) = parse_test_example();
            let lsp_context = super::LspContext::from_parse_results(ebnf, &parse_result);
            let refs = lsp_context.references.get("hello").unwrap();
            assert_eq!(refs.len(), 3);
            assert_eq!(refs[0], Location { line: 0, col: 18 });
            assert_eq!(refs[1], Location { line: 1, col: 0 });
            assert_eq!(refs[2], Location { line: 2, col: 7 });

            let refs = lsp_context
                .references(&Location { line: 2, col: 10 })
                .expect("Should have refs");
            assert_eq!(refs.len(), 3);
            assert_eq!(refs[0], Location { line: 0, col: 18 });
            assert_eq!(refs[1], Location { line: 1, col: 0 });
            assert_eq!(refs[2], Location { line: 2, col: 7 });
        }

        #[test]
        fn test_defs() {
            let (ebnf, parse_result) = parse_test_example();
            let lsp_context = super::LspContext::from_parse_results(ebnf, &parse_result);
            let hello_def_loc = lsp_context.definitions.get("hello").unwrap();
            assert_eq!(hello_def_loc, &Location { line: 1, col: 0 });
            let hello_def_loc = lsp_context
                .definition(&Location { line: 2, col: 9 })
                .unwrap();
            assert_eq!(hello_def_loc, &Location { line: 1, col: 0 });
        }
    }
}
