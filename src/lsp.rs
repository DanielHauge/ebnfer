pub mod lsp {
    use std::collections::{BTreeMap, HashMap};
    use std::ops::Bound::Included;

    use nom::Offset;
    use rangemap::RangeMap;

    #[derive(Debug, PartialEq)]
    pub struct Location {
        pub line: usize,
        pub col: usize,
    }

    #[derive(Debug)]
    pub struct Located<T> {
        pub location: Location,
        pub value: T,
    }

    #[derive(Debug)]
    pub struct LspContext<'a> {
        doc_content: &'a str,
        definitions: HashMap<&'a str, Location>,
        hover: HashMap<&'a str, &'a str>,
        references: HashMap<&'a str, Vec<Location>>,
        new_lines_map: BTreeMap<usize, usize>, // Offset -> Line number
        symbols: RangeMap<usize, &'a str>,
    }

    impl<'a> LspContext<'a> {
        pub fn new(doc_content: &'a str) -> Self {
            let mut lines_offset_tree = BTreeMap::new();
            doc_content
                .lines()
                .enumerate()
                .map(|(i, l)| (i, doc_content.offset(l)))
                .for_each(|(line_number, offset)| {
                    lines_offset_tree.insert(offset, line_number);
                });
            Self {
                doc_content,
                definitions: HashMap::new(),
                hover: HashMap::new(),
                references: HashMap::new(),
                new_lines_map: lines_offset_tree,
                symbols: RangeMap::new(),
            }
        }

        fn location_at_offset(&self, offset: usize) -> Location {
            let line_offsets = self
                .new_lines_map
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

        pub fn add_definition(&mut self, rule: &'a str) {
            let offset = self.doc_content.offset(rule);
            self.definitions
                .insert(rule, self.location_at_offset(offset));
            self.symbols.insert(offset..offset + rule.len(), rule)
        }

        pub fn add_hover(&mut self, rule: &'a str, production: &'a str) {
            self.hover.insert(rule, production);
        }

        pub fn add_reference(&mut self, rule: &'a str, location: Location) {
            self.references.entry(rule).or_insert(vec![]).push(location);
        }
    }

    mod tests {
        use crate::lsp::lsp::Location;

        #[test]
        fn test_new() {
            let lsp_context = crate::lsp::lsp::LspContext::new("Some content");
            assert_eq!(lsp_context.doc_content, "Some content");
            assert_eq!(lsp_context.definitions.len(), 0);
            assert_eq!(lsp_context.hover.len(), 0);
            assert_eq!(lsp_context.references.len(), 0);
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
                &Location { line: 0, col: 7 }
            );
        }

        #[test]
        fn test_add_reference() {
            let content = "Hello, World! dsad hjaskda hakhjdsah Hello, World!";
            let mut lsp_context = crate::lsp::lsp::LspContext::new(content);
            let world_identifier = &content[7..12];
            let loc = Location { line: 0, col: 7 };
            let loc2 = Location { line: 0, col: 31 };
            lsp_context.add_reference(world_identifier, loc);
            lsp_context.add_reference(world_identifier, loc2);
            assert_eq!(
                lsp_context.references.get(world_identifier).unwrap(),
                &vec![Location { line: 0, col: 7 }, Location { line: 0, col: 31 }]
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
                &Location { line: 1, col: 1 }
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
            lsp_context.add_hover("GG", "wp");
            assert_eq!(lsp_context.hover.len(), 1);
            assert_eq!(lsp_context.hover.get("GG").unwrap(), &"wp");
        }
    }
}
