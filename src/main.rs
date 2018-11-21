use clap::{clap_app, crate_authors, crate_version};
use dirs::home_dir;
use dotfiles::TargetOS;
use std::path::PathBuf;

#[cfg(target_os = "linux")]
const TARGET_OS: TargetOS = TargetOS::Linux;
#[cfg(target_os = "macos")]
const TARGET_OS: TargetOS = TargetOS::MacOS;

fn main() {
    let matches = clap_app!(dotfiles =>
        (version: crate_version!())
        (author: crate_authors!())
        (@arg TEMPLATES_DIR: +required "templates directory")
    )
    .get_matches();
    let templates_dir = matches.value_of("TEMPLATES_DIR").unwrap();

    dotfiles::deploy(
        &PathBuf::from(templates_dir),
        &home_dir().unwrap(),
        &TARGET_OS,
    );
}
