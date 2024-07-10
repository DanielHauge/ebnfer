pub mod lsp {
    use std::collections::{BTreeMap, HashMap};
    use std::ops::Bound::Included;

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
            lsp_context.compute_lsp_context(parse_results);
            lsp_context
        }

        // #[allow(dead_code)]
        // pub fn references(&self, symbol: &str) -> Option<&Vec<Location>> {
        //     self.references.get(symbol)
        // }

        fn compute_lsp_context(&mut self, parse_results: &ParseResult) {}

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
    }

    mod tests {

        #[test]
        fn test_add_hover() {}
    }
}
