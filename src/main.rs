use crate::ebnf::ebnf::parse_ebnf;

mod ebnf;
mod lsp;

fn main() {
    let _ = parse_ebnf("lol");
    println!("Hello, world!");
}
