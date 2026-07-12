use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

use lsp_types::{FileChangeType, Uri};

use crate::lsp::{AnalysisContext, Location, LspError, LspErrorType, UNUSED_DEF_FMT};

#[derive(Debug, Clone, PartialEq)]
pub struct DocumentLocation {
    pub uri: Uri,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct RuleDefinition {
    pub location: DocumentLocation,
    pub text: String,
}

#[derive(Debug)]
struct WorkspaceRoot {
    uri: Uri,
    path: PathBuf,
}

#[derive(Debug)]
struct DocumentState {
    uri: Uri,
    path: Option<PathBuf>,
    disk_text: Option<String>,
    open_text: Option<String>,
    analysis: Option<AnalysisContext>,
    syntax_error: Option<LspError>,
}

impl DocumentState {
    fn new(uri: Uri, path: Option<PathBuf>) -> Self {
        Self {
            uri,
            path,
            disk_text: None,
            open_text: None,
            analysis: None,
            syntax_error: None,
        }
    }

    fn set_disk_text(&mut self, text: String) {
        self.disk_text = Some(text.clone());
        if self.open_text.is_none() {
            self.apply_text(text);
        }
    }

    fn set_open_text(&mut self, text: String) {
        self.open_text = Some(text.clone());
        self.apply_text(text);
    }

    fn close(&mut self) {
        self.open_text = None;
        if let Some(text) = self.disk_text.clone() {
            self.apply_text(text);
        }
    }

    fn apply_text(&mut self, text: String) {
        let analysis = AnalysisContext::from_src(text);
        if let Some(error) = analysis.syntax_error_to_lsp_error() {
            self.syntax_error = Some(error);
        } else {
            self.analysis = Some(analysis);
            self.syntax_error = None;
        }
    }

    fn is_open(&self) -> bool {
        self.open_text.is_some()
    }
}

#[derive(Debug, Default)]
pub struct WorkspaceState {
    roots: Vec<WorkspaceRoot>,
    documents: HashMap<Uri, DocumentState>,
}

impl WorkspaceState {
    pub fn add_root(&mut self, uri: Uri) -> Vec<String> {
        let uri = normalize_uri(&uri);
        if self.roots.iter().any(|root| root.uri == uri) {
            return Vec::new();
        }

        let path = match uri_to_path(&uri) {
            Ok(path) => path,
            Err(error) => return vec![error],
        };
        self.roots.push(WorkspaceRoot {
            uri,
            path: path.clone(),
        });
        self.roots.sort_by(|a, b| a.uri.cmp(&b.uri));
        self.scan_path(&path)
    }

    pub fn remove_root(&mut self, uri: &Uri) {
        let uri = normalize_uri(uri);
        self.roots.retain(|root| root.uri != uri);
        let roots = &self.roots;
        self.documents.retain(|_, document| {
            document.is_open()
                || document
                    .path
                    .as_ref()
                    .is_some_and(|path| roots.iter().any(|root| path.starts_with(&root.path)))
        });
    }

    pub fn open_document(&mut self, uri: Uri, text: String) {
        let uri = normalize_uri(&uri);
        let path = uri_to_path(&uri).ok();
        self.documents
            .entry(uri.clone())
            .or_insert_with(|| DocumentState::new(uri, path))
            .set_open_text(text);
    }

    pub fn change_document(&mut self, uri: Uri, text: String) {
        self.open_document(uri, text);
    }

    pub fn close_document(&mut self, uri: &Uri) -> Vec<String> {
        let uri = normalize_uri(uri);
        let mut errors = Vec::new();
        let disk_result = self
            .documents
            .get(&uri)
            .and_then(|document| document.path.clone())
            .map(|path| fs::read_to_string(&path).map(|text| (path, text)));

        if let Some(document) = self.documents.get_mut(&uri) {
            match disk_result {
                Some(Ok((_, text))) => document.disk_text = Some(text),
                Some(Err(error)) if error.kind() == std::io::ErrorKind::NotFound => {
                    document.disk_text = None;
                }
                Some(Err(error)) => {
                    errors.push(format!("Failed to reload {}: {error}", uri.as_str()))
                }
                None => {}
            }
            document.close();
        }

        let should_remove = self.documents.get(&uri).is_some_and(|document| {
            !document.is_open()
                && (document.disk_text.is_none() || !self.belongs_to_any_root(document))
        });
        if should_remove {
            self.documents.remove(&uri);
        }
        errors
    }

    pub fn update_watched_file(&mut self, uri: Uri, change_type: FileChangeType) -> Vec<String> {
        let uri = normalize_uri(&uri);
        if change_type == FileChangeType::DELETED {
            if self
                .documents
                .get(&uri)
                .is_some_and(|document| document.is_open())
            {
                if let Some(document) = self.documents.get_mut(&uri) {
                    document.disk_text = None;
                }
            } else {
                self.documents.remove(&uri);
            }
            return Vec::new();
        }

        let path = match uri_to_path(&uri) {
            Ok(path) => path,
            Err(error) => return vec![error],
        };
        if !is_ebnf_file(&path) || self.root_for_path(&path).is_none() {
            return Vec::new();
        }
        match fs::read_to_string(&path) {
            Ok(text) => {
                self.documents
                    .entry(uri.clone())
                    .or_insert_with(|| DocumentState::new(uri, Some(path)))
                    .set_disk_text(text);
                Vec::new()
            }
            Err(error) => vec![format!("Failed to read {}: {error}", path.display())],
        }
    }

    pub fn document(&self, uri: &Uri) -> Option<&AnalysisContext> {
        self.documents.get(&normalize_uri(uri))?.analysis.as_ref()
    }

    pub fn syntax_error(&self, uri: &Uri) -> Option<LspError> {
        self.documents
            .get(&normalize_uri(uri))?
            .syntax_error
            .clone()
    }

    pub fn symbol(&self, uri: &Uri, location: &Location) -> Option<&str> {
        self.document(uri)?.symbol(location)
    }

    pub fn definitions(&self, uri: &Uri, name: &str) -> Vec<DocumentLocation> {
        let mut definitions = self
            .namespace_documents(uri)
            .into_iter()
            .flat_map(|document| {
                document
                    .analysis
                    .as_ref()
                    .into_iter()
                    .flat_map(|analysis| analysis.definition_occurrences())
                    .filter(move |(definition_name, _)| definition_name == name)
                    .map(|(_, location)| DocumentLocation {
                        uri: document.uri.clone(),
                        location,
                    })
            })
            .collect::<Vec<_>>();
        sort_locations(&mut definitions);
        definitions
    }

    pub fn references(&self, uri: &Uri, name: &str) -> Vec<DocumentLocation> {
        let mut references = self
            .namespace_documents(uri)
            .into_iter()
            .flat_map(|document| {
                document
                    .analysis
                    .as_ref()
                    .into_iter()
                    .flat_map(|analysis| analysis.reference_occurrences())
                    .filter(move |(reference_name, _)| reference_name == name)
                    .map(|(_, location)| DocumentLocation {
                        uri: document.uri.clone(),
                        location,
                    })
            })
            .collect::<Vec<_>>();
        sort_locations(&mut references);
        references
    }

    pub fn hovers(&self, uri: &Uri, name: &str) -> Vec<RuleDefinition> {
        let mut hovers = self
            .namespace_documents(uri)
            .into_iter()
            .flat_map(|document| {
                document
                    .analysis
                    .as_ref()
                    .into_iter()
                    .flat_map(|analysis| analysis.hover_occurrences())
                    .filter(move |(definition_name, _, _)| definition_name == name)
                    .map(|(_, location, text)| RuleDefinition {
                        location: DocumentLocation {
                            uri: document.uri.clone(),
                            location,
                        },
                        text,
                    })
            })
            .collect::<Vec<_>>();
        hovers.sort_by(|a, b| {
            a.location
                .uri
                .cmp(&b.location.uri)
                .then(a.location.location.line.cmp(&b.location.location.line))
                .then(a.location.location.col.cmp(&b.location.location.col))
        });
        hovers
    }

    pub fn completions(&self, uri: &Uri) -> Vec<(String, String)> {
        let mut completions = BTreeMap::new();
        for document in self.namespace_documents(uri) {
            if let Some(analysis) = &document.analysis {
                for (name, _, text) in analysis.hover_occurrences() {
                    completions.entry(name).or_insert(text);
                }
            }
        }
        completions.into_iter().collect()
    }

    pub fn diagnostics(&self, uri: &Uri) -> Vec<LspError> {
        let uri = normalize_uri(uri);
        let documents = self.namespace_documents(&uri);
        let definitions = workspace_definitions(&documents);
        let definition_names = definitions
            .iter()
            .map(|definition| definition.name.as_str())
            .collect::<BTreeSet<_>>();
        let references = workspace_references(&documents);
        let reference_names = references
            .iter()
            .map(|reference| reference.name.as_str())
            .collect::<BTreeSet<_>>();

        let unused = definitions
            .iter()
            .filter(|definition| {
                !reference_names.contains(definition.name.as_str()) && !definition.suppressed
            })
            .collect::<Vec<_>>();

        let mut diagnostics = Vec::new();
        if unused.len() > 1 {
            diagnostics.extend(
                unused
                    .into_iter()
                    .filter(|definition| definition.uri == uri)
                    .map(|definition| LspError {
                        message: format!("{}{}", UNUSED_DEF_FMT, definition.name),
                        start: definition.location.clone(),
                        end: Location {
                            line: definition.location.line,
                            col: definition.location.col + definition.name.len(),
                        },
                        error_type: LspErrorType::UnusedDefinition,
                    }),
            );
        }

        diagnostics.extend(
            references
                .into_iter()
                .filter(|reference| {
                    reference.uri == uri && !definition_names.contains(reference.name.as_str())
                })
                .map(|reference| LspError {
                    message: format!("Undefined reference: {}", reference.name),
                    start: reference.location.clone(),
                    end: Location {
                        line: reference.location.line,
                        col: reference.location.col + reference.name.len(),
                    },
                    error_type: LspErrorType::UndefinedReference,
                }),
        );

        if let Some(error) = self.syntax_error(&uri) {
            diagnostics.push(error);
        }
        diagnostics
    }

    pub fn root_rule(&self, uri: &Uri) -> Option<DocumentLocation> {
        let uri = normalize_uri(uri);
        let documents = self.namespace_documents(&uri);
        let definitions = workspace_definitions(&documents);
        let reference_names = workspace_references(&documents)
            .into_iter()
            .map(|reference| reference.name)
            .collect::<BTreeSet<_>>();
        let mut roots = definitions.into_iter().filter(|definition| {
            !reference_names.contains(&definition.name) && !definition.suppressed
        });
        let root = roots.next()?;
        if roots.next().is_some() || root.uri != uri {
            return None;
        }
        Some(DocumentLocation {
            uri: root.uri,
            location: root.location,
        })
    }

    fn scan_path(&mut self, root: &Path) -> Vec<String> {
        let mut files = Vec::new();
        let mut errors = Vec::new();
        collect_ebnf_files(root, &mut files, &mut errors);
        files.sort();
        for path in files {
            match fs::read_to_string(&path) {
                Ok(text) => match path_to_uri(&path) {
                    Ok(uri) => {
                        self.documents
                            .entry(uri.clone())
                            .or_insert_with(|| DocumentState::new(uri, Some(path)))
                            .set_disk_text(text);
                    }
                    Err(error) => errors.push(error),
                },
                Err(error) => errors.push(format!("Failed to read {}: {error}", path.display())),
            }
        }
        errors
    }

    fn namespace_documents(&self, uri: &Uri) -> Vec<&DocumentState> {
        let uri = normalize_uri(uri);
        let Some(document) = self.documents.get(&uri) else {
            return Vec::new();
        };
        let Some(path) = &document.path else {
            return vec![document];
        };
        let root = self.root_for_path(path);
        let mut documents = match root {
            Some(root) => self
                .documents
                .values()
                .filter(|candidate| {
                    candidate.path.as_ref().is_some_and(|path| {
                        self.root_for_path(path).is_some_and(|r| r.uri == root.uri)
                    })
                })
                .collect::<Vec<_>>(),
            None => vec![document],
        };
        documents.sort_by(|a, b| a.uri.cmp(&b.uri));
        documents
    }

    fn root_for_path(&self, path: &Path) -> Option<&WorkspaceRoot> {
        self.roots
            .iter()
            .filter(|root| path.starts_with(&root.path))
            .max_by_key(|root| root.path.components().count())
    }

    fn belongs_to_any_root(&self, document: &DocumentState) -> bool {
        document
            .path
            .as_ref()
            .is_some_and(|path| self.root_for_path(path).is_some())
    }
}

#[derive(Debug)]
struct DefinitionOccurrence {
    uri: Uri,
    name: String,
    location: Location,
    suppressed: bool,
}

#[derive(Debug)]
struct ReferenceOccurrence {
    uri: Uri,
    name: String,
    location: Location,
}

fn workspace_definitions(documents: &[&DocumentState]) -> Vec<DefinitionOccurrence> {
    documents
        .iter()
        .flat_map(|document| {
            document
                .analysis
                .as_ref()
                .into_iter()
                .flat_map(move |analysis| {
                    analysis
                        .definition_occurrences()
                        .into_iter()
                        .map(move |(name, location)| DefinitionOccurrence {
                            uri: document.uri.clone(),
                            suppressed: analysis.is_unused_suppressed(&name),
                            name,
                            location,
                        })
                })
        })
        .collect()
}

fn workspace_references(documents: &[&DocumentState]) -> Vec<ReferenceOccurrence> {
    documents
        .iter()
        .flat_map(|document| {
            document
                .analysis
                .as_ref()
                .into_iter()
                .flat_map(move |analysis| {
                    analysis
                        .reference_occurrences()
                        .into_iter()
                        .map(move |(name, location)| ReferenceOccurrence {
                            uri: document.uri.clone(),
                            name,
                            location,
                        })
                })
        })
        .collect()
}

fn sort_locations(locations: &mut [DocumentLocation]) {
    locations.sort_by(|a, b| {
        a.uri
            .cmp(&b.uri)
            .then(a.location.line.cmp(&b.location.line))
            .then(a.location.col.cmp(&b.location.col))
    });
}

fn collect_ebnf_files(path: &Path, files: &mut Vec<PathBuf>, errors: &mut Vec<String>) {
    let entries = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(error) => {
            errors.push(format!(
                "Failed to read directory {}: {error}",
                path.display()
            ));
            return;
        }
    };
    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(error) => {
                errors.push(format!("Failed to read directory entry: {error}"));
                continue;
            }
        };
        let file_type = match entry.file_type() {
            Ok(file_type) => file_type,
            Err(error) => {
                errors.push(format!(
                    "Failed to inspect {}: {error}",
                    entry.path().display()
                ));
                continue;
            }
        };
        if file_type.is_dir() {
            collect_ebnf_files(&entry.path(), files, errors);
        } else if file_type.is_file() && is_ebnf_file(&entry.path()) {
            files.push(entry.path());
        }
    }
}

fn is_ebnf_file(path: &Path) -> bool {
    path.extension()
        .is_some_and(|extension| extension == "ebnf")
}

pub fn uri_to_path(uri: &Uri) -> Result<PathBuf, String> {
    let url = url::Url::parse(uri.as_str())
        .map_err(|error| format!("Invalid URI {}: {error}", uri.as_str()))?;
    url.to_file_path()
        .map_err(|_| format!("URI is not a file path: {}", uri.as_str()))
}

pub fn path_to_uri(path: &Path) -> Result<Uri, String> {
    let url = url::Url::from_file_path(path)
        .map_err(|_| format!("Path cannot be converted to URI: {}", path.display()))?;
    url.as_str()
        .parse()
        .map_err(|error| format!("Invalid file URI {}: {error}", url.as_str()))
}

fn normalize_uri(uri: &Uri) -> Uri {
    uri_to_path(uri)
        .and_then(|path| path_to_uri(&path))
        .unwrap_or_else(|_| uri.clone())
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicUsize, Ordering};

    use super::*;

    static TEMP_DIRECTORY_ID: AtomicUsize = AtomicUsize::new(0);

    struct TestDirectory(PathBuf);

    impl TestDirectory {
        fn new() -> Self {
            let id = TEMP_DIRECTORY_ID.fetch_add(1, Ordering::Relaxed);
            let path = std::env::temp_dir()
                .join(format!("ebnfer-workspace-test-{}-{id}", std::process::id()));
            fs::create_dir_all(&path).unwrap();
            Self(path)
        }

        fn write(&self, relative_path: &str, content: &str) -> PathBuf {
            let path = self.0.join(relative_path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(&path, content).unwrap();
            path
        }

        fn uri(&self) -> Uri {
            path_to_uri(&self.0).unwrap()
        }
    }

    impl Drop for TestDirectory {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn resolves_rules_across_recursively_indexed_files() {
        let directory = TestDirectory::new();
        let start_path = directory.write("start.ebnf", "Start = Item;");
        let item_path = directory.write("grammar/item.ebnf", "Item = \"item\";");
        let start_uri = path_to_uri(&start_path).unwrap();
        let item_uri = path_to_uri(&item_path).unwrap();

        let mut workspace = WorkspaceState::default();
        assert!(workspace.add_root(directory.uri()).is_empty());

        assert_eq!(
            workspace.definitions(&start_uri, "Item"),
            vec![DocumentLocation {
                uri: item_uri.clone(),
                location: Location { line: 0, col: 0 },
            }]
        );
        assert_eq!(
            workspace.references(&item_uri, "Item"),
            vec![DocumentLocation {
                uri: start_uri.clone(),
                location: Location { line: 0, col: 8 },
            }]
        );
        assert!(workspace.diagnostics(&start_uri).is_empty());
        assert_eq!(
            workspace.root_rule(&start_uri),
            Some(DocumentLocation {
                uri: start_uri.clone(),
                location: Location { line: 0, col: 0 },
            })
        );
        assert_eq!(
            workspace
                .completions(&start_uri)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            vec!["Item".to_string(), "Start".to_string()]
        );
    }

    #[test]
    fn keeps_workspace_roots_isolated() {
        let first = TestDirectory::new();
        let second = TestDirectory::new();
        let first_path = first.write("first.ebnf", "Start = Shared;");
        second.write("second.ebnf", "Shared = \"shared\";");
        let first_uri = path_to_uri(&first_path).unwrap();

        let mut workspace = WorkspaceState::default();
        assert!(workspace.add_root(first.uri()).is_empty());
        assert!(workspace.add_root(second.uri()).is_empty());

        assert!(workspace.definitions(&first_uri, "Shared").is_empty());
        assert_eq!(
            workspace
                .diagnostics(&first_uri)
                .into_iter()
                .map(|error| error.message)
                .collect::<Vec<_>>(),
            vec!["Undefined reference: Shared".to_string()]
        );
    }

    #[test]
    fn preserves_duplicate_definitions_as_alternatives() {
        let directory = TestDirectory::new();
        let first_path = directory.write("first.ebnf", "Shared = \"first\";");
        let second_path = directory.write("second.ebnf", "Shared = \"second\";");
        let first_uri = path_to_uri(&first_path).unwrap();
        let second_uri = path_to_uri(&second_path).unwrap();

        let mut workspace = WorkspaceState::default();
        workspace.add_root(directory.uri());

        assert_eq!(
            workspace.definitions(&first_uri, "Shared"),
            vec![
                DocumentLocation {
                    uri: first_uri.clone(),
                    location: Location { line: 0, col: 0 },
                },
                DocumentLocation {
                    uri: second_uri,
                    location: Location { line: 0, col: 0 },
                },
            ]
        );
        assert_eq!(workspace.hovers(&first_uri, "Shared").len(), 2);
    }

    #[test]
    fn open_buffer_overrides_disk_and_close_restores_it() {
        let directory = TestDirectory::new();
        let path = directory.write("grammar.ebnf", "Disk = \"disk\";");
        let uri = path_to_uri(&path).unwrap();

        let mut workspace = WorkspaceState::default();
        workspace.add_root(directory.uri());
        workspace.open_document(uri.clone(), "Open = \"open\";".to_string());

        assert_eq!(
            workspace
                .completions(&uri)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            vec!["Open".to_string()]
        );

        assert!(workspace.close_document(&uri).is_empty());
        assert_eq!(
            workspace
                .completions(&uri)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            vec!["Disk".to_string()]
        );
    }

    #[test]
    fn watched_file_events_refresh_the_workspace() {
        let directory = TestDirectory::new();
        let anchor_path = directory.write("anchor.ebnf", "Root = Added;");
        let anchor_uri = path_to_uri(&anchor_path).unwrap();
        let added_path = directory.0.join("nested/added.ebnf");
        let added_uri = path_to_uri(&added_path).unwrap();

        let mut workspace = WorkspaceState::default();
        workspace.add_root(directory.uri());
        assert_eq!(workspace.diagnostics(&anchor_uri).len(), 1);

        directory.write("nested/added.ebnf", "Added = \"added\";");
        assert!(workspace
            .update_watched_file(added_uri.clone(), FileChangeType::CREATED)
            .is_empty());
        assert!(workspace.diagnostics(&anchor_uri).is_empty());

        directory.write("nested/added.ebnf", "Renamed = \"renamed\";");
        assert!(workspace
            .update_watched_file(added_uri.clone(), FileChangeType::CHANGED)
            .is_empty());
        assert_eq!(workspace.diagnostics(&anchor_uri).len(), 2);

        fs::remove_file(&added_path).unwrap();
        assert!(workspace
            .update_watched_file(added_uri, FileChangeType::DELETED)
            .is_empty());
        assert_eq!(workspace.diagnostics(&anchor_uri).len(), 1);
    }

    #[test]
    fn closing_a_deleted_open_file_removes_its_analysis() {
        let directory = TestDirectory::new();
        let path = directory.write("deleted.ebnf", "Deleted = \"value\";");
        let uri = path_to_uri(&path).unwrap();

        let mut workspace = WorkspaceState::default();
        workspace.add_root(directory.uri());
        workspace.open_document(uri.clone(), "Open = \"value\";".to_string());
        fs::remove_file(&path).unwrap();
        workspace.update_watched_file(uri.clone(), FileChangeType::DELETED);

        assert!(workspace.close_document(&uri).is_empty());
        assert!(workspace.document(&uri).is_none());
    }

    #[test]
    fn normalizes_equivalent_file_uris() {
        let directory = TestDirectory::new();
        let path = directory.write("grammar.ebnf", "Disk = \"disk\";");
        let uri = path_to_uri(&path).unwrap();
        let encoded_uri: Uri = uri
            .as_str()
            .replace("grammar.ebnf", "%67rammar.ebnf")
            .parse()
            .unwrap();

        let mut workspace = WorkspaceState::default();
        workspace.add_root(directory.uri());
        workspace.open_document(encoded_uri, "Open = \"open\";".to_string());

        assert_eq!(
            workspace
                .completions(&uri)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            vec!["Open".to_string()]
        );
    }
}
