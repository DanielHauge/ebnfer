pub mod lsp {
    use std::collections::{BTreeMap, HashMap};
    use std::ops::Bound::Included;

    use nom::Offset;
    use rangemap::RangeMap;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Location {
        pub line: usize,
        pub col: usize,
    }

    #[derive(Debug)]
    pub struct LspContext<'a> {
        doc_content: &'a str,
        definitions: HashMap<&'a str, Location>,
        hover: HashMap<&'a str, &'a str>,
        references: HashMap<&'a str, Vec<Location>>,
        offset_to_line: BTreeMap<usize, usize>, // Offset -> Line number
        line_to_offset: HashMap<usize, usize>,  // Line Number -> Offset
        symbols: RangeMap<usize, &'a str>,
        last_lhs: Option<&'a str>,
    }

    impl<'a> LspContext<'a> {
        pub fn new(doc_content: &'a str) -> Self {
            let mut lines_offset_tree = BTreeMap::new();
            let mut offset_lines = HashMap::new();
            doc_content
                .lines()
                .enumerate()
                .map(|(i, l)| (i, doc_content.offset(l)))
                .for_each(|(line_number, offset)| {
                    lines_offset_tree.insert(offset, line_number);
                    offset_lines.insert(line_number, offset);
                });
            Self {
                doc_content,
                definitions: HashMap::new(),
                hover: HashMap::new(),
                references: HashMap::new(),
                offset_to_line: lines_offset_tree,
                line_to_offset: offset_lines,
                symbols: RangeMap::new(),
                last_lhs: None,
            }
        }

        pub fn offset_from_ptr(&self, ptr: usize) -> usize {
            ptr - self.doc_content.as_ptr() as usize
        }

        pub fn references(&self, symbol: &'a str) -> Option<&Vec<Location>> {
            self.references.get(symbol)
        }

        pub fn location_at_offset(&self, offset: usize) -> Location {
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

        pub fn sylbol_from_location(&self, loc: Location) -> Option<&'a str> {
            let line_offset = self.line_to_offset.get(&loc.line);

            match line_offset {
                Some(x) => match self.symbols.get(&(x + loc.col)) {
                    Some(s) => Some(*s),
                    None => None,
                },
                None => None,
            }
        }

        pub fn add_definition(&mut self, rule: &'a str) {
            let offset = self.doc_content.offset(rule);
            self.definitions
                .insert(rule, self.location_at_offset(offset));
            self.symbols.insert(offset..offset + rule.len(), rule);
            let loc = self.location_at_offset(offset);
            self.references.entry(rule).or_insert(vec![]).push(loc);
            self.last_lhs = Some(rule)
        }

        pub fn complete_hover(&mut self, production: &'a str) {
            match self.last_lhs {
                Some(lhs) => {
                    self.hover.insert(lhs, production);
                }
                None => {}
            }
        }

        pub fn hover(&self, loc: Location) -> Option<&'a str> {
            let symbol = self.sylbol_from_location(loc);
            match symbol {
                Some(s) => match self.hover.get(s) {
                    Some(h) => Some(*h),
                    None => None,
                },
                None => None,
            }
        }

        pub fn add_reference(&mut self, rule: &'a str, ptr_loc: usize) {
            let offset = self.doc_content.offset(rule);

            let location = self.location_at_offset(ptr_loc);
            self.symbols.insert(offset..offset + rule.len(), rule);
            self.references.entry(rule).or_insert(vec![]).push(location);
        }
    }

    mod tests {

        #[test]
        fn test_new() {
            let lsp_context = crate::lsp::lsp::LspContext::new("Some content");
            assert_eq!(lsp_context.doc_content, "Some content");
            assert_eq!(lsp_context.definitions.len(), 0);
            assert_eq!(lsp_context.hover.len(), 0);
            assert_eq!(lsp_context.references.len(), 0);
        }

        #[test]
        fn test_find_symbol() {
            let ebnf =
                "some cool stuff = hello;\nslightly_cool_stuff = hello;\n   hello = 'a' | 'b';";
            let (_, gram) = crate::ebnf::ebnf::parse_ebnf(ebnf).expect("Should parse fine");
            let lsp_context = gram.lsp_context;

            for i in 0..14 {
                let actual = lsp_context
                    .sylbol_from_location(crate::lsp::lsp::Location { line: 0, col: i })
                    .expect("Symbol should be there");
                assert_eq!(actual, "some cool stuff");
            }
            let actual = lsp_context
                .sylbol_from_location(crate::lsp::lsp::Location { line: 0, col: 19 })
                .expect("To be there");
            assert_eq!(actual, "hello");

            let actual1 = lsp_context
                .sylbol_from_location(crate::lsp::lsp::Location { line: 1, col: 24 })
                .expect("To be there");
            assert_eq!(actual1, "hello");
        }

        #[test]
        fn test_hover() {
            let ebnf =
                "some cool stuff = hello;\nslightly_cool_stuff = hello;\n   hello = 'a' | 'b';";
            let (_, gram) = crate::ebnf::ebnf::parse_ebnf(ebnf).expect("Should parse fine");
            let lsp_context = gram.lsp_context;
            let hello_hover = lsp_context
                .hover(crate::lsp::lsp::Location { line: 1, col: 24 })
                .expect("Hello hover should be there");
            assert_eq!(hello_hover, "'a' | 'b';")
        }

        #[test]
        fn test_add_definition() {
            let content = "Hello, World!";
            let mut lsp_context = crate::lsp::lsp::LspContext::new(content);
            let world_identifier = &content[7..12];
            lsp_context.add_definition(world_identifier);
            assert_eq!(lsp_context.definitions.len(), 1);
            assert_eq!(
                lsp_context.definitions.get("World").unwrap(),
                &crate::lsp::lsp::Location { line: 0, col: 7 }
            );
        }

        #[test]
        pub fn test_references() {
            let ebnf = "some cool stuff = hello;\nslightly_cool_stuff = hello;\n   hello = 'a';";
            let (_str, _grammar) =
                crate::ebnf::ebnf::parse_ebnf(ebnf).expect("Ebnf is not parsable.");
            let actual = _grammar
                .lsp_context
                .references("hello")
                .expect("Should have references.");
            let expected = &vec![
                crate::lsp::lsp::Location { line: 0, col: 18 },
                crate::lsp::lsp::Location { line: 1, col: 22 },
                crate::lsp::lsp::Location { line: 2, col: 3 },
            ];
            assert_eq!(actual, expected)
        }

        #[test]
        fn test_add_reference() {
            let content = "Hello, World! dsad hjaskda hakhjdsah Hello, World!";
            let mut lsp_context = crate::lsp::lsp::LspContext::new(content);
            let world_identifier = &content[7..12];
            lsp_context.add_reference(world_identifier, 7);
            lsp_context.add_reference(world_identifier, 31);
            assert_eq!(
                lsp_context.references.get(world_identifier).unwrap(),
                &vec![
                    crate::lsp::lsp::Location { line: 0, col: 7 },
                    crate::lsp::lsp::Location { line: 0, col: 31 }
                ]
            );
        }

        #[test]
        fn test_lineoffset() {
            let content = "Hello, World!\n Greetings, World!";
            let mut lsp_context = crate::lsp::lsp::LspContext::new(content);
            let greetings_identifier = &content[15..24];
            lsp_context.add_definition(greetings_identifier);
            assert_eq!(
                lsp_context.definitions.get(greetings_identifier).unwrap(),
                &crate::lsp::lsp::Location { line: 1, col: 1 }
            );
        }

        #[test]
        fn test_symbol_at_offset() {
            let content = "Hello, World!";
            let mut lsp_context = crate::lsp::lsp::LspContext::new(content);
            let world_identifier = &content[7..12];
            lsp_context.add_definition(world_identifier);
            assert_eq!(lsp_context.symbols.len(), 1);
            assert_eq!(lsp_context.symbols.get(&7).unwrap(), &world_identifier);
            assert_eq!(lsp_context.symbols.get(&9).unwrap(), &world_identifier);
            assert_eq!(lsp_context.symbols.get(&11).unwrap(), &world_identifier);
            assert_eq!(lsp_context.symbols.get(&13).is_none(), true);
            assert_eq!(lsp_context.symbols.get(&6).is_none(), true);
        }

        #[test]
        fn test_add_hover() {
            let mut lsp_context = crate::lsp::lsp::LspContext::new("Hello, World!");
            lsp_context.add_definition("GG");
            lsp_context.complete_hover("wp");
            assert_eq!(lsp_context.hover.len(), 1);
            assert_eq!(lsp_context.hover.get("GG").unwrap(), &"wp");
        }
    }
}
