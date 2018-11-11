#[macro_use]
extern crate clap;

extern crate dirs;
extern crate dotfiles;

use dotfiles::TargetOS;
use std::path;

#[cfg(target_os = "linux")]
const TARGET_OS: TargetOS = TargetOS::Linux;
#[cfg(target_os = "macos")]
const TARGET_OS: TargetOS = TargetOS::MacOS;

fn main() {
    let matches = clap_app!(dotfiles =>
        (version: "2.0.0")
        (author: "Kai Zhang <kaizhang91@qq.com>")
        (@arg TEMPLATES_DIR: +required "templates directory")
    ).get_matches();
    let templates_dir = matches.value_of("TEMPLATES_DIR").unwrap();

    dotfiles::deploy(
        path::Path::new(templates_dir),
        dirs::home_dir().unwrap().as_path(),
        &TARGET_OS,
    );
}
