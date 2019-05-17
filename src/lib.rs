extern crate ansi_term;

use ansi_term::Colour;
use std::fmt;
use std::fs;
use std::path;

const BACKUP_EXTENSION: &str = "bak";

pub enum TargetOS {
    Common,
    Linux,
    MacOS,
}

impl fmt::Display for TargetOS {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TargetOS::Common => write!(f, "common"),
            TargetOS::Linux => write!(f, "linux"),
            TargetOS::MacOS => write!(f, "macos"),
        }
    }
}

pub fn deploy(templates_dir: &path::PathBuf, home_dir: &path::PathBuf, target_os: &TargetOS) {
    list_template_paths(templates_dir)
        .into_iter()
        .filter(|p| is_for_os(p, target_os))
        .for_each(|p| deploy_one_template(p, templates_dir, home_dir));
    println!("{}", Colour::Green.paint("Deploy succeed."))
}

fn list_template_paths(template_dir: &path::PathBuf) -> Vec<path::PathBuf> {
    let mut template_paths = Vec::new();
    template_dir
        .read_dir()
        .expect("read_dir() failed")
        .map(|dir_entry| dir_entry.unwrap().path())
        .skip_while(|p| p.file_name().unwrap().to_str().unwrap().starts_with("."))
        .skip_while(|p| match p.extension() {
            Some(extension) => extension.to_str().unwrap() == BACKUP_EXTENSION,
            None => false,
        })
        .for_each(|p| {
            if p.is_file() {
                template_paths.push(p);
            } else {
                template_paths.extend(list_template_paths(&p));
            }
        });
    template_paths
}

fn is_for_os(template_path: &path::PathBuf, target_os: &TargetOS) -> bool {
    let file_stem = template_path.file_stem().unwrap().to_str().unwrap();
    file_stem.ends_with(format!("-{}", target_os).as_str())
        || file_stem.ends_with(format!("-{}", TargetOS::Common).as_str())
}

fn deploy_one_template(
    template_path: path::PathBuf,
    templates_dir: &path::Path,
    home_dir: &path::Path,
) {
    let dest_path = get_dest_path(&template_path, templates_dir, home_dir);
    let template_path = template_path.canonicalize().unwrap();
    println!(
        "Deploying {} -> {} ...",
        template_path.to_str().unwrap(),
        dest_path.to_str().unwrap()
    );
    let _ = fs::remove_file(&dest_path);
    fs::create_dir_all(dest_path.parent().unwrap()).unwrap();
    fs::copy(&template_path, &dest_path).unwrap();
}

fn get_dest_path(
    template_path: &path::PathBuf,
    templates_dir: &path::Path,
    home_dir: &path::Path,
) -> path::PathBuf {
    let link_file_stem = template_path
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .trim_end_matches(format!("-{}", TargetOS::Common).as_str())
        .trim_end_matches(format!("-{}", TargetOS::Linux).as_str())
        .trim_end_matches(format!("-{}", TargetOS::MacOS).as_str());
    let link_file_name = match template_path.extension() {
        Some(extension) => format!("{}.{}", link_file_stem, extension.to_str().unwrap()),
        None => String::from(link_file_stem),
    };
    let mut dest_path = path::PathBuf::from(template_path);
    dest_path.set_file_name(link_file_name);
    let dest_path = dest_path.strip_prefix(templates_dir).unwrap();
    path::PathBuf::from(format!(
        "{}/.{}",
        home_dir.to_str().unwrap(),
        dest_path.to_str().unwrap()
    ))
}
