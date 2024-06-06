pub mod ebnf {
    use std::usize;

    use crate::lsp::lsp::LspContext;
    use nom::{
        branch::alt,
        bytes::complete::{escaped, tag},
        character::complete::{alpha1, alphanumeric1, char, multispace0, none_of, one_of},
        combinator::recognize,
        error::{VerboseError, VerboseErrorKind},
        multi::{many0_count, many1},
        sequence::{delimited, pair, preceded, terminated},
        Err, IResult, Offset,
    };
    use parse_hyperlinks::take_until_unbalanced;

    #[derive(Debug)]
    pub struct Rule {
        pub identifier: String,
        pub production: Expression,
    }

    #[derive(Debug, Clone)]
    pub enum SymbolKind {
        Concatenation,
        Alternation,
    }

    #[derive(Debug, Clone)]
    pub enum Expression {
        Optional(Box<Expression>),
        Term(String),
        NonTerm(String, usize),
        Regex(String),
        Repeated(Box<Expression>),
        Symbol(Box<Expression>, SymbolKind, Box<Expression>),
        RegexExt(Box<Expression>, RegexExtKind),
        // SpecialSequence(String),
        // Comment(String),
        Group(Box<Expression>),
        Multiple(Vec<Expression>),
    }

    #[derive(Debug, Clone)]
    pub enum RegexExtKind {
        ZeroOrMore,
        OneOrMore,
        ZeroOrOne,
    }

    #[derive(Debug)]
    pub struct Grammar<'a> {
        pub rules: Vec<Rule>,
        pub lsp_context: LspContext<'a>,
    }

    type Res<T, U> = IResult<T, U, VerboseError<T>>;

    fn parse_identifer(input: &str) -> Res<&str, &str> {
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, alt((tag("_"), tag(" ")))))),
        ))(input)
    }

    fn parse_lhs<'a>(lsp_context: &mut LspContext<'a>, input: &'a str) -> Res<&'a str, &'a str> {
        let (assignment_symbol_rest, lhs) = preceded(multispace0, parse_identifer)(input)?;
        let lhs = lhs.trim();
        lsp_context.add_definition(lhs);
        let (rest, _) = preceded(multispace0, alt((tag("="), tag("::="))))(assignment_symbol_rest)?;
        // let _loc = compute_location(input); // TODO: do somen with the location
        Ok((rest, lhs))
    }

    fn find_non_terminals(rhs: &Expression) -> Vec<usize> {
        match rhs {
            Expression::Optional(o) => find_non_terminals(o),
            Expression::NonTerm(_, ptr) => vec![*ptr],
            Expression::Repeated(x) => find_non_terminals(x),
            Expression::Symbol(a, _, b) => {
                let mut non_terminals = find_non_terminals(a);
                non_terminals.extend(find_non_terminals(b));
                non_terminals
            }
            Expression::Group(g) => find_non_terminals(g),
            Expression::Multiple(a) => a.iter().map(|e| find_non_terminals(e)).flatten().collect(),
            _ => vec![],
        }
    }

    fn parse_rhs<'a>(lsp_context: &mut LspContext<'a>, input: &'a str) -> Res<&'a str, Expression> {
        // Make closure to parse multiple expressions passing in the lsp_context
        let (input, _) = multispace0(input)?;
        let start_rhs = input;
        let (rest, rhs) = terminated(parse_multiple, preceded(multispace0, char(';')))(input)?;
        // Recursively go through the rhs and find all NonTerminals
        let rhs_str = &start_rhs[..start_rhs.offset(rest)];
        lsp_context.complete_hover(rhs_str);
        let non_terminals = find_non_terminals(&rhs);
        for nt in non_terminals {
            lsp_context.add_reference(
                &rhs_str[..rhs_str.len() - 1],
                lsp_context.offset_from_ptr(nt),
            );
        }
        Ok((rest, rhs))
    }

    fn parse_expression(input: &str) -> Res<&str, Expression> {
        let (mut input, mut left_node) = preceded(
            multispace0,
            alt((
                parse_group,
                parse_optional,
                parse_repeat,
                parse_terminal,
                parse_regex_string,
                parse_non_terminal,
            )),
        )(input)?;

        let optional_regex_ext: Res<&str, RegexExtKind> = parse_regex_ext(input);

        match optional_regex_ext {
            Ok((s, regex_ext_kind)) => {
                input = s;
                left_node = Expression::RegexExt(Box::new(left_node), regex_ext_kind);
            }
            Err(_) => {}
        }
        let optional_symbol: Res<&str, (SymbolKind, Expression)> = parse_symbol(input);

        match optional_symbol {
            Ok((input, (symbol, right_node))) => Ok((
                input,
                Expression::Symbol(Box::new(left_node), symbol, Box::new(right_node)),
            )),
            Err(_) => Ok((input, left_node)),
        }
    }

    fn parse_terminal(input: &str) -> Res<&str, Expression> {
        let (input, string) = alt((
            delimited(
                char('\''),
                escaped(none_of("\\\'"), '\\', one_of(r#"tbnrf/\'"#)),
                char('\''),
            ),
            delimited(
                char('"'),
                escaped(none_of("\\\""), '\\', one_of(r#"tbnrf/\""#)),
                char('"'),
            ),
        ))(input)?;
        Ok((input, Expression::Term(string.to_string())))
    }

    fn parse_regex_string(input: &str) -> Res<&str, Expression> {
        let (input, string) = alt((
            delimited(
                tag("#'"),
                escaped(none_of("\\\'"), '\\', one_of(r#"tbnrf/\'"#)),
                char('\''),
            ),
            delimited(
                tag("#\""),
                escaped(none_of("\\\""), '\\', one_of(r#"tbnrf/\""#)),
                char('"'),
            ),
        ))(input)?;

        Ok((input, Expression::Regex(string.to_string())))
    }

    fn parse_non_terminal(input: &str) -> Res<&str, Expression> {
        let (input, symbol) =
            preceded(multispace0, terminated(parse_identifer, multispace0))(input)?;
        let symbol = symbol.trim();
        let ref_loc = symbol.as_ptr() as usize;
        Ok((input, Expression::NonTerm(symbol.to_string(), ref_loc)))
    }

    fn parse_regex_ext(input: &str) -> Res<&str, RegexExtKind> {
        let (input, regex_ext) =
            preceded(multispace0, alt((char('*'), char('+'), char('?'))))(input)?;

        let regex_kind = match regex_ext {
            '*' => RegexExtKind::ZeroOrMore,
            '+' => RegexExtKind::OneOrMore,
            '?' => RegexExtKind::ZeroOrOne,
            _ => unreachable!("Unexpected regex extension symbol. this should not happen"),
        };

        Ok((input, regex_kind))
    }

    fn parse_symbol(input: &str) -> Res<&str, (SymbolKind, Expression)> {
        let (input, symbol_pair) =
            preceded(multispace0, alt((parse_concatenation, parse_alternation)))(input)?;

        Ok((input, symbol_pair))
    }

    fn parse_concatenation(input: &str) -> Res<&str, (SymbolKind, Expression)> {
        let (input, node) = preceded(char(','), parse_multiple)(input)?;

        Ok((input, (SymbolKind::Concatenation, node)))
    }

    fn parse_alternation(input: &str) -> Res<&str, (SymbolKind, Expression)> {
        let (input, node) = preceded(char('|'), parse_multiple)(input)?;

        Ok((input, (SymbolKind::Alternation, node)))
    }

    fn parse_delimited_node(
        input: &str,
        opening_bracket: char,
        closing_bracket: char,
    ) -> Res<&str, &str> {
        let result = delimited(
            tag(opening_bracket.to_string().as_str()),
            take_until_unbalanced(opening_bracket, closing_bracket),
            tag(closing_bracket.to_string().as_str()),
        )(input);

        match result {
            Ok((input, inner)) => Ok((input, inner)),
            Err(_) => Err(Err::Error(VerboseError {
                errors: vec![(
                    input,
                    VerboseErrorKind::Context("Incomplete delimited node"),
                )],
            })),
        }
    }

    fn parse_group(input: &str) -> Res<&str, Expression> {
        let (input, inner) = parse_delimited_node(input, '(', ')')?;

        let (_, node) = preceded(multispace0, parse_multiple)(inner)?;

        Ok((input, Expression::Group(Box::new(node))))
    }

    fn parse_optional(input: &str) -> Res<&str, Expression> {
        let (input, inner) = parse_delimited_node(input, '[', ']')?;

        let (_, node) = preceded(multispace0, parse_multiple)(inner)?;

        Ok((input, Expression::Optional(Box::new(node))))
    }

    fn parse_repeat(input: &str) -> Res<&str, Expression> {
        let (input, inner) = parse_delimited_node(input, '{', '}')?;
        let parse_multi_lsp_context_applied = move |input| parse_multiple(input);

        let (_, node) = preceded(multispace0, parse_multi_lsp_context_applied)(inner)?;

        Ok((input, Expression::Repeated(Box::new(node))))
    }

    fn parse_multiple(input: &str) -> Res<&str, Expression> {
        let (input, node) = preceded(multispace0, many1(parse_expression))(input)?;

        match node {
            _ if node.len() == 1 => Ok((input, node[0].clone())),
            _ => Ok((input, Expression::Multiple(node))),
        }
    }

    pub fn parse_ebnf(ebnf: &str) -> Res<&str, Grammar> {
        let mut parse_context = ebnf;
        let mut rules: Vec<Rule> = Vec::new();
        let mut lsp_context = LspContext::new(ebnf);

        while !parse_context.is_empty() {
            let (rest, lhs) = parse_lhs(&mut lsp_context, parse_context)?;
            let (rest, rhs) = parse_rhs(&mut lsp_context, rest)?;
            rules.push(Rule {
                identifier: lhs.to_string(),
                production: rhs,
            });
            parse_context = rest;
        }
        Ok((ebnf, Grammar { rules, lsp_context }))
    }

    mod tests {

        use super::super::super::lsp::lsp::Location;
        use super::parse_ebnf;

        #[test]
        pub fn test_ebnf_references() {
            let ebnf = "some cool stuff = hello;\nslightly_cool_stuff = hello;\n   hello = 'a';";
            let (_str, _grammar) = parse_ebnf(ebnf).expect("Ebnf is not parsable.");
            let actual = _grammar
                .lsp_context
                .references("hello")
                .expect("Should have references.");
            let expected = &vec![Location { line: 0, col: 18 }, Location { line: 1, col: 22 }];
            assert_eq!(actual, expected)

            // _grammar.lsp_context
        }
    }
}
