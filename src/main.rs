use crate::ebnf::ebnf::parse_ebnf;

mod ebnf;
mod lsp;

fn main() {
    let test = parse_ebnf("very_nice_stuff = hello;\nhello = \"world\";\ncool = hello;");
    match test {
        Ok((_, grammer)) => {
            println!("{:#?}", grammer);
        }
        Err(e) => match e {
            nom::Err::Incomplete(b) => panic!("Incomplete: {:?}", b),
            nom::Err::Error(e) => panic!("Error: {:?}", e),
            nom::Err::Failure(f) => panic!("Failure: {:?}", f),
        },
    }
}
