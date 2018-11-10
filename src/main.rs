extern crate clap;
extern crate dotfiles;

use clap::{App, Arg};
use std::env;
use std::path;

#[cfg(target_os = "linux")]
const target_os: dotfiles::TargetOS = dotfiles::TargetOS::Linux;
#[cfg(target_os = "macos")]
const target_os: dotfiles::TargetOS = dotfiles::TargetOS::MacOS;

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
        env::home_dir().unwrap().as_path(),
        &target_os,
    );
}
