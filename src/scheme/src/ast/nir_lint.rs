use super::nir::*;
use super::nir::RawNode::*;

use std::collections::HashSet;

pub fn lint_scdefns(defns: &mut [ScDefn]) -> Result<(), String> {
    let toplevel_names: HashSet<String> = defns.iter().map(|d| d.name().to_owned()).collect();
    if !toplevel_names.contains("main") {
        return Err("main not defined".to_owned());
    }

    let mut checker = CheckGlobal(&toplevel_names);
    for defn in defns {
        try!(defn.body_mut().traverse(&mut checker));
    }
    Ok(())
}

struct CheckGlobal<'a>(&'a HashSet<String>);

impl<'a> NodeTraverser<String> for CheckGlobal<'a> {
    fn before(&mut self, node: &mut RawNode) -> Result<TraversalDirection, String> {
        match node {
            &mut NReadGlobal(ref mut g) => {
                if !self.0.contains(g) {
                    return Err(format!("No global name `{}` found", g));
                }
            }
            _ => {}
        }
        Ok(TraversalDirection::Forward)
    }
}
