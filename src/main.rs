use tracing::Level;
use tracing_subscriber::FmtSubscriber;

use nora::{build, get_args};

fn init_loggging(level: Level) -> Result<(), String> {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(level)
        .pretty()
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .map_err(|_| format!("Failed to initialize tracing subscriber"))
}

fn main() -> Result<(), String> {
    init_loggging(Level::DEBUG)?;
    build(get_args())
}
