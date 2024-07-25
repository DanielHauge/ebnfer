mod ipc;
mod lsp;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    ipc::start()
}
