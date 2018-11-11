extern crate clap;
extern crate dirs;
extern crate dotfiles;

use clap::{App, Arg};
use std::path;

#[cfg(target_os = "linux")]
const TARGET_OS: dotfiles::TargetOS = dotfiles::TargetOS::Linux;
#[cfg(target_os = "macos")]
const TARGET_OS: dotfiles::TargetOS = dotfiles::TargetOS::MacOS;

fn main() {
    let matches = App::new("dotfiles")
        .version("0.0.1")
        .author("Kai Zhang <kaizhang91@qq.com>")
        .arg(
            Arg::with_name("TEMPLATES_DIR")
                .help("templates directory")
                .required(true),
        ).get_matches();
    let templates_dir = matches.value_of("TEMPLATES_DIR").unwrap();

    dotfiles::deploy(
        path::Path::new(templates_dir),
        dirs::home_dir().unwrap().as_path(),
        &TARGET_OS,
    );
}
