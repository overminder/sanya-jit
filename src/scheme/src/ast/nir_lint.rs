use super::nir::*;

use std::collections::HashSet;

pub fn lint_scdefns(defns: &mut [ScDefn]) -> Result<(), String> {
    let toplevel_names: HashSet<String> = defns.iter().map(|d| d.name().to_owned()).collect();
    for defn in defns {
        try!(defn.body_mut().foreach_readglobal(&mut |g| {
            if toplevel_names.contains(g) {
                Ok(())
            } else {
                Err(format!("No global name `{}` found", g))
            }
        }));
    }
    Ok(())
}
