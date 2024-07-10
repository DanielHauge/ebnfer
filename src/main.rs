use ipc::ipc::start;

mod ipc;
mod lsp;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let ebnf = "very_nice_stuff = hello;\nhello = \"world\";\ncool = hello;";
    // // let ebnf =
    // //     "Hello There = Hello , ' ' , World;\nHello = \"Hello\";\n    World = \"World!\";\n   ";
    // let test = parse_ebnf(ebnf);
    // match test {
    //     Ok((_, grammer)) => Ok({
    //         println!("{:#?}", grammer);
    //     }),
    //     Err(e) => match e {
    //         nom::Err::Incomplete(b) => panic!("Incomplete: {:?}", b),
    //         nom::Err::Error(e) => panic!("Error: {:?}", e),
    //         nom::Err::Failure(f) => panic!("Failure: {:?}", f),
    //     },
    // }
    let lexer = ebnf_parser::Lexer::new(ebnf);
    let parser = ebnf_parser::Parser::new(lexer);
    let gg = parser.parse();
    match gg {
        Ok(x) => println!("{:#?}", x),
        Err(e) => eprintln!("{:?}", e),
    }
    start()
}
