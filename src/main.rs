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
        (version: &clap::crate_version!()[..])
        (author: "Kai Zhang <kaizhang91@qq.com>")
        (@arg TEMPLATES_DIR: +required "templates directory")
    ).get_matches();
    let templates_dir = matches.value_of("TEMPLATES_DIR").unwrap();

    dotfiles::deploy(
        &path::PathBuf::from(templates_dir),
        &dirs::home_dir().unwrap(),
        &TARGET_OS,
    );
}
