pub mod ebnf {
    use crate::lsp::lsp::LspContext;

    use nom::{
        branch::alt,
        bytes::complete::{escaped, tag},
        character::complete::{alpha1, alphanumeric1, char, multispace0, none_of, one_of},
        combinator::recognize,
        error::{ParseError, VerboseError, VerboseErrorKind},
        multi::{many0_count, many1},
        sequence::{delimited, pair, preceded, terminated},
        Err, IResult, Into, Offset,
    };
    use parse_hyperlinks::take_until_unbalanced;
    use std::usize;

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

    // Parsing Error, unexpected symbol found: '-'
    // Context: LHS -> Identifer:
    //    A correct identifer is expected to start with an alphabet or an underscore, and only contain alphanumeric characters, whitespacers and underscores.
    //
    // Errors:
    //  - 1. ["-\nhell....", TAG]
    //  - 2. [";\ncool = hello;", TAG]
    #[derive(Debug)]
    pub struct EErr<T> {
        input: T,
        parsing_context: Vec<EbnfParseContext>,
        error_message: String,
        parser_errors: Vec<VerboseErrorKind>,
    }

    trait ParseContextualError {
        fn with_context(self, context: EbnfParseContext) -> Self;
    }

    impl<T> ParseContextualError for EErr<T> {
        fn with_context(self, context: EbnfParseContext) -> EErr<T> {
            let new_error_message = format!(
                "Could not parse {:?}, because: {}",
                context, self.error_message
            );
            let mut new_parsing_context = self.parsing_context;
            new_parsing_context.push(context);
            EErr {
                input: self.input,
                parsing_context: new_parsing_context,
                error_message: new_error_message,
                parser_errors: self.parser_errors,
            }
        }
    }

    impl<T, U> ParseContextualError for ERes<T, U> {
        fn with_context(self, context: EbnfParseContext) -> ERes<T, U> {
            match self {
                Result::Err(e) => Result::Err(e.with_context(context)),
                a => a,
            }
        }
    }

    #[derive(Debug)]
    pub enum EbnfParseContext {
        Identifier,
        Lhs,
    }

    impl<T> ParseContextualError for nom::Err<T>
    where
        T: ParseContextualError,
    {
        fn with_context(self, context: EbnfParseContext) -> Self {
            match self {
                Err::Error(e) => Err::Error(e.with_context(context)),
                Err::Failure(f) => Err::Failure(f.with_context(context)),
                a => a,
            }
        }
    }

    impl<T> ParseError<T> for EErr<T>
    where
        T: Offset,
    {
        fn from_error_kind(input: T, kind: nom::error::ErrorKind) -> Self {
            match kind {
                nom::error::ErrorKind::Tag => todo!(),
                nom::error::ErrorKind::MapRes => todo!(),
                nom::error::ErrorKind::MapOpt => todo!(),
                nom::error::ErrorKind::Alt => todo!(),
                nom::error::ErrorKind::IsNot => todo!(),
                nom::error::ErrorKind::IsA => todo!(),
                nom::error::ErrorKind::SeparatedList => todo!(),
                nom::error::ErrorKind::SeparatedNonEmptyList => todo!(),
                nom::error::ErrorKind::Many0 => todo!(),
                nom::error::ErrorKind::Many1 => todo!(),
                nom::error::ErrorKind::ManyTill => todo!(),
                nom::error::ErrorKind::Count => todo!(),
                nom::error::ErrorKind::TakeUntil => todo!(),
                nom::error::ErrorKind::LengthValue => todo!(),
                nom::error::ErrorKind::TagClosure => todo!(),
                nom::error::ErrorKind::Alpha => todo!(),
                nom::error::ErrorKind::Digit => todo!(),
                nom::error::ErrorKind::HexDigit => todo!(),
                nom::error::ErrorKind::OctDigit => todo!(),
                nom::error::ErrorKind::AlphaNumeric => todo!(),
                nom::error::ErrorKind::Space => todo!(),
                nom::error::ErrorKind::MultiSpace => todo!(),
                nom::error::ErrorKind::LengthValueFn => todo!(),
                nom::error::ErrorKind::Eof => todo!(),
                nom::error::ErrorKind::Switch => todo!(),
                nom::error::ErrorKind::TagBits => todo!(),
                nom::error::ErrorKind::OneOf => todo!(),
                nom::error::ErrorKind::NoneOf => todo!(),
                nom::error::ErrorKind::Char => todo!(),
                nom::error::ErrorKind::CrLf => todo!(),
                nom::error::ErrorKind::RegexpMatch => todo!(),
                nom::error::ErrorKind::RegexpMatches => todo!(),
                nom::error::ErrorKind::RegexpFind => todo!(),
                nom::error::ErrorKind::RegexpCapture => todo!(),
                nom::error::ErrorKind::RegexpCaptures => todo!(),
                nom::error::ErrorKind::TakeWhile1 => todo!(),
                nom::error::ErrorKind::Complete => todo!(),
                nom::error::ErrorKind::Fix => todo!(),
                nom::error::ErrorKind::Escaped => todo!(),
                nom::error::ErrorKind::EscapedTransform => todo!(),
                nom::error::ErrorKind::NonEmpty => todo!(),
                nom::error::ErrorKind::ManyMN => todo!(),
                nom::error::ErrorKind::Not => todo!(),
                nom::error::ErrorKind::Permutation => todo!(),
                nom::error::ErrorKind::Verify => todo!(),
                nom::error::ErrorKind::TakeTill1 => todo!(),
                nom::error::ErrorKind::TakeWhileMN => todo!(),
                nom::error::ErrorKind::TooLarge => todo!(),
                nom::error::ErrorKind::Many0Count => todo!(),
                nom::error::ErrorKind::Many1Count => todo!(),
                nom::error::ErrorKind::Float => todo!(),
                nom::error::ErrorKind::Satisfy => todo!(),
                nom::error::ErrorKind::Fail => todo!(),
            }
        }

        fn append(input: T, kind: nom::error::ErrorKind, other: Self) -> Self {
            todo!()
        }
    }

    pub type Res<T, U> = IResult<T, U, VerboseError<T>>;
    pub type ERes<T, U> = IResult<T, U, EErr<T>>; // Extended IResult, with optional context

    fn parse_identifer(input: &str) -> ERes<&str, &str> {
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, alt((tag("_"), tag(" ")))))),
        ))(input)
        .with_context(EbnfParseContext::Identifier)
    }

    fn parse_lhs<'a>(lsp_context: &mut LspContext<'a>, input: &'a str) -> ERes<&'a str, &'a str> {
        let (assignment_symbol_rest, lhs) = preceded(multispace0, parse_identifer)(input)?;
        let lhs = lhs.trim();
        lsp_context.add_definition(lhs);
        let (rest, _) = preceded(multispace0, alt((tag("="), tag("::="))))(assignment_symbol_rest)
            .with_context(EbnfParseContext::Lhs)?;

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

    fn parse_rhs<'a>(
        lsp_context: &mut LspContext<'a>,
        input: &'a str,
    ) -> ERes<&'a str, Expression> {
        let (input, _) = multispace0(input)?;
        let start_rhs = input;
        let (rest, rhs) = terminated(parse_multiple, preceded(multispace0, char(';')))(input)?;
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

    fn parse_expression(input: &str) -> ERes<&str, Expression> {
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

        let optional_regex_ext: ERes<&str, RegexExtKind> = parse_regex_ext(input);

        match optional_regex_ext {
            Ok((s, regex_ext_kind)) => {
                input = s;
                left_node = Expression::RegexExt(Box::new(left_node), regex_ext_kind);
            }
            Err(_) => {}
        }
        let optional_symbol: ERes<&str, (SymbolKind, Expression)> = parse_symbol(input);

        match optional_symbol {
            Ok((input, (symbol, right_node))) => Ok((
                input,
                Expression::Symbol(Box::new(left_node), symbol, Box::new(right_node)),
            )),
            Err(_) => Ok((input, left_node)),
        }
    }

    fn parse_terminal(input: &str) -> ERes<&str, Expression> {
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

    fn parse_regex_string(input: &str) -> ERes<&str, Expression> {
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

    fn parse_non_terminal(input: &str) -> ERes<&str, Expression> {
        let (input, symbol) =
            preceded(multispace0, terminated(parse_identifer, multispace0))(input)?;
        let symbol = symbol.trim();
        let ref_loc = symbol.as_ptr() as usize;
        Ok((input, Expression::NonTerm(symbol.to_string(), ref_loc)))
    }

    fn parse_regex_ext(input: &str) -> ERes<&str, RegexExtKind> {
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

    fn parse_symbol(input: &str) -> ERes<&str, (SymbolKind, Expression)> {
        let (input, symbol_pair) =
            preceded(multispace0, alt((parse_concatenation, parse_alternation)))(input)?;

        Ok((input, symbol_pair))
    }

    fn parse_concatenation(input: &str) -> ERes<&str, (SymbolKind, Expression)> {
        let (input, node) = preceded(char(','), parse_multiple)(input)?;

        Ok((input, (SymbolKind::Concatenation, node)))
    }

    fn parse_alternation(input: &str) -> ERes<&str, (SymbolKind, Expression)> {
        let (input, node) = preceded(char('|'), parse_multiple)(input)?;

        Ok((input, (SymbolKind::Alternation, node)))
    }

    fn parse_delimited_node(
        input: &str,
        opening_bracket: char,
        closing_bracket: char,
    ) -> ERes<&str, &str> {
        let result = delimited(
            tag(opening_bracket.to_string().as_str()),
            take_until_unbalanced(opening_bracket, closing_bracket),
            tag(closing_bracket.to_string().as_str()),
        )(input);

        match result {
            Ok((input, inner)) => Ok((input, inner)),
            Err(e) => Err(Err::Error(EErr {
                parsing_context: todo!(),
                error_message: todo!(),
                input,
                parser_errors: todo!(),
            })),
        }
    }

    fn parse_group(input: &str) -> ERes<&str, Expression> {
        let (input, inner) = parse_delimited_node(input, '(', ')')?;

        let (_, node) = preceded(multispace0, parse_multiple)(inner)?;

        Ok((input, Expression::Group(Box::new(node))))
    }

    fn parse_optional(input: &str) -> ERes<&str, Expression> {
        let (input, inner) = parse_delimited_node(input, '[', ']')?;

        let (_, node) = preceded(multispace0, parse_multiple)(inner)?;

        Ok((input, Expression::Optional(Box::new(node))))
    }

    fn parse_repeat(input: &str) -> ERes<&str, Expression> {
        let (input, inner) = parse_delimited_node(input, '{', '}')?;
        let parse_multi_lsp_context_applied = move |input| parse_multiple(input);

        let (_, node) = preceded(multispace0, parse_multi_lsp_context_applied)(inner)?;

        Ok((input, Expression::Repeated(Box::new(node))))
    }

    fn parse_multiple(input: &str) -> ERes<&str, Expression> {
        let (input, node) = preceded(multispace0, many1(parse_expression))(input)?;

        match node {
            _ if node.len() == 1 => Ok((input, node[0].clone())),
            _ => Ok((input, Expression::Multiple(node))),
        }
    }

    pub fn parse_ebnf(ebnf: &str) -> ERes<&str, Grammar> {
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

    mod tests {}
}
