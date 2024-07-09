use ebnf::ebnf::parse_ebnf;
use ipc::ipc::start;

mod ebnf;
mod ipc;
mod lsp;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let ebnf = "very nice stuff = hello;\nhello = \"world\";\ncool = hello;";
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
    start()
}
