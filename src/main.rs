mod ipc;
mod lsp;
mod workspace;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    ipc::start()
}
