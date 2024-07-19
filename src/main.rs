use ipc::ipc::start;

mod ipc;
mod lsp;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let ebnf = "very_nice_stuff = hello;\nhello = \"world\";\ncool = hello;";
    // let _lol = lsp::lsp::LspContext::from_src(ebnf.to_string());
    // println!("{:#?}", _lol);
    start()
}
