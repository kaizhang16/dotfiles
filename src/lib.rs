use std::fmt;
use std::fs;
use std::path;

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

pub fn deploy(templates_dir: &path::Path, home_dir: &path::Path, target_os: &TargetOS) {
    let template_paths = list_template_files(templates_dir, &target_os);
    template_paths
        .into_iter()
        .for_each(|p| link(p, templates_dir, home_dir));
    println!("Deploy succeed.")
}

fn list_template_files(template_dir: &path::Path, target_os: &TargetOS) -> Vec<path::PathBuf> {
    let mut template_files = Vec::new();
    for entry in template_dir.read_dir().expect("read_dir() failed") {
        if let Ok(entry) = entry {
            let p = entry.path();
            if p.is_file() {
                template_files.push(p);
            } else {
                let file_name = p.file_name().unwrap().to_str().unwrap();
                if !file_name.starts_with(".") {
                    template_files.extend(list_template_files(&entry.path(), target_os));
                }
            }
        }
    }
    template_files
        .into_iter()
        .filter(|f| is_for_os(f, target_os))
        .collect()
}

fn is_for_os(template_file: &path::PathBuf, target_os: &TargetOS) -> bool {
    let basename = template_file.file_stem().unwrap().to_str().unwrap();
    basename.ends_with(format!("-{}", target_os).as_str())
        || basename.ends_with(format!("-{}", TargetOS::Common).as_str())
}

fn link(template_path: path::PathBuf, templates_dir: &path::Path, home_dir: &path::Path) {
    let link_path = get_link_path(&template_path, templates_dir, home_dir);
    let template_path = template_path.canonicalize().unwrap();
    println!(
        "Deploying {} -> {} ...",
        template_path.to_str().unwrap(),
        link_path.to_str().unwrap()
    );
    let _ = fs::remove_file(&link_path);
    fs::create_dir_all(link_path.parent().unwrap()).unwrap();
    fs::hard_link(&template_path, &link_path).unwrap();
}

fn get_link_path(
    template_path: &path::PathBuf,
    templates_dir: &path::Path,
    home_dir: &path::Path,
) -> path::PathBuf {
    let link_file_name = template_path
        .file_stem()
        .unwrap()
        .to_os_string()
        .into_string()
        .unwrap();
    let link_file_name = link_file_name.trim_end_matches(format!("-{}", TargetOS::Common).as_str());
    let link_file_name = link_file_name.trim_end_matches(format!("-{}", TargetOS::Linux).as_str());
    let link_file_name = link_file_name.trim_end_matches(format!("-{}", TargetOS::MacOS).as_str());
    let link_file_name = match template_path.extension() {
        Some(extension) => format!("{}.{}", link_file_name, extension.to_str().unwrap()),
        None => String::from(link_file_name),
    };
    let mut link_path = path::PathBuf::from(template_path);
    link_path.set_file_name(link_file_name);
    let link_path = link_path.strip_prefix(templates_dir).unwrap();
    path::PathBuf::from(format!(
        "{}/.{}",
        home_dir.to_str().unwrap(),
        link_path.to_str().unwrap()
    ))
}
