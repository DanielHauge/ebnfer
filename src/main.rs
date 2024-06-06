use crate::ebnf::ebnf::parse_ebnf;

mod ebnf;
mod lsp;

fn main() {
    // let ebnf = "very nice stuff = hello;\nhello = \"world\";\ncool = hello;" ;
    let ebnf = "some cool stuff = hello;\nslightly_cool_stuff = hello;\n   hello = 'a' | 'b';";
    let test = parse_ebnf(ebnf);
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
