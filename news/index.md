# Changelog

## rairai (development version)

## rairai 0.2.6

- Source navigation, R6 tools, test lookup, and overview source
  summaries now reject discovered symlink escapes before reads or
  cached-fact reuse.

## rairai 0.2.5

- Every package-local MCP tool now accepts a call-local absolute
  `package_root` override and reports its canonical effective root,
  while `serve(path = ...)` remains the immutable default.

## rairai 0.2.4

- `package_news()` now uses Markdown outline ancestry to keep nested
  categories within releases while leaving generic sibling sections
  outside release ranges.

## rairai 0.2.3

- `definition()` and `refactor_context()` now expose the resolved
  definition only once instead of duplicating it in singleton
  collections.

## rairai 0.2.2

- Package-local MCP tools now inspect the active `serve(path = ...)`
  root regardless of the process working directory.

## rairai 0.2.1

- Source-index tools now reuse cached per-file facts across repeated
  calls, improving warm
  [`symbols()`](https://rdrr.io/r/graphics/symbols.html),
  `definition()`, `references()`, `function_formals()`, and
  `refactor_context()` performance while preserving same-service
  freshness.

## rairai 0.2.0

- [`serve()`](https://tosidata.github.io/rairai/reference/serve.md) now
  runs fully in-process without a language-server runtime; the public
  `diagnostics()` tool was removed and package-qualified
  `function_formals()` now returns a static limitation with help-tool
  next steps.
- Code-location tools now keep `location` exact and add optional
  pathless `leading_comment_lines` for immediately preceding comments on
  source-object, test-block, and R6 records.
- MCP tool schemas now correctly mark optional/defaulted arguments as
  optional.
- Dedicated R6 tools now use Tree-sitter syntax facts, inspect flat
  `R/*.[Rr]` package source, report Tree-sitter parse-error partiality
  without text fallback, and include `r6_method_references()`
  target-resolution metadata.
- Local `function_formals()` now resolves unqualified package helpers
  through package-wide Tree-sitter lookup; source-index navigation
  outputs now include truncation metadata, R6 member reference caveats,
  and no public declaration-inclusion option.
- Local [`symbols()`](https://rdrr.io/r/graphics/symbols.html),
  `definition()`, `references()`, `function_formals()`, and
  `refactor_context()` now use a Tree-sitter source index for
  package-local navigation and require R \>= 4.3.0 with `treesitter` and
  `treesitter.r`.
- Removed the direct `dplyr` dependency.

## rairai 0.1.2

- MCP tool-call errors now use JSON text thrown from rairai tools and
  handled by `mcplite` as `isError = true` results.

## rairai 0.1.1

- LSP-backed tools now normalize in-root absolute paths and `file://`
  URIs to package-root-relative paths and reject out-of-root paths with
  structured errors.
- `symbols(path = "R")` now treats the source directory as a flat
  `R/*.R` prefix scan instead of trying to open `R` as a document.
- [`serve()`](https://tosidata.github.io/rairai/reference/serve.md) now
  accepts a `path` argument for explicitly selecting the target package
  root without calling [`setwd()`](https://rdrr.io/r/base/getwd.html)
  first.
- Documentation tools now include install guidance when a requested
  package is not installed or has no Rd database.

## rairai 0.1.0

- Top-level MCP tool failures now return error tool results so clients
  can set `isError = true`, while successful and partial-success
  payloads remain JSON values.
- New `cran_search()` and `cran_package()` tools search ranked CRAN
  package metadata and return DESCRIPTION-like package details using
  pkgsearch.
- Improve tool performance.

## rairai 0.0.1

- `package_dependencies()`, `package_overview()`, and `package_files()`
  now report structured partial errors for malformed metadata,
  unavailable dependency metadata, and unreadable line counts.
- `function_formals()` source fallback now recognizes same-line `<-`
  function assignments when language-server signature help is
  unavailable.
- `package_vignettes()` now falls back to exact article-like installed
  help topics when packages ship those instead of standard vignettes,
  using stable topic names and reporting ambiguous matches.
- `r6_method_references()` now reports distinct source lines for
  repeated method calls in the same file.
- `tests_for()` now groups repeated hits by enclosing test block while
  preserving concrete hit locations.
- `refactor_context()` now exposes static test text hits separately from
  language-server references.
- `package_overview()` now previews source objects by default while
  preserving full source-object counts.
- [`symbols()`](https://rdrr.io/r/graphics/symbols.html) now reports
  `file_outline` status for file-scoped outline requests.
- `help_search()` now suggests follow-up tools when local Rd search
  finds no matches.
- `r6_classes()` and top-level `r6_class()` locations now report full
  class definition line ranges when available.
- `package_docs()` and `help_search()` now report structured local Rd
  limitations when generated source documentation is missing or
  unreadable.
- New `package_docs()` tool returns a compact documentation map with
  help topics, vignette summaries, NEWS summaries, and next-tool hints.
- `help_search()` now requires a non-empty query, supports optional
  package scoping, and returns compact ranked matches with match
  metadata.
- `help_page()` now reports available, requested, matched, and missing
  sections when retrieving section-limited help pages.
- `package_namespace()` now returns a merged package-keyed import map
  with compact default object lists, focused package expansion, S3
  method registrations, and dynamic library declarations.
- `package_overview()` now returns the redesigned six-section
  orientation map with full `DESCRIPTION` fields, namespace exports,
  compact file layout, documentation/test summaries, and complete
  top-level source object indexing.
- `package_dependencies()` now returns all dependency varieties as
  concise strings with version requirements, plus top-level `remotes`
  and `Config/Needs/*` entries.
- `package_files(scope = "all")` now returns every non-gitignored file
  under the package root instead of only the focused `R`, `tests`,
  `man`, and `vignettes` scopes.
- Empty [`symbols()`](https://rdrr.io/r/graphics/symbols.html) results
  now include structured `next_tools` hints for fallback discovery
  routes.
- `references()` and related refactor outputs now filter internal
  synthetic signature documents from returned reference locations.
- Location-bearing tool outputs now use compact exact `location` strings
  such as `R/foo.R:12-18` instead of separate path and line range
  fields.
- `r6_class()` now reports member source locations for fields, methods,
  and active bindings.
- `r6_method()` now returns exact file-sliced method source and reports
  a structured error when exact source cannot be resolved.
- [`serve()`](https://tosidata.github.io/rairai/reference/serve.md) now
  exposes a single concise public toolset and no longer registers
  backend-shaped LSP tools such as `workspace_symbols()`,
  `document_symbols()`, `find_symbol()`, or positional
  definition/reference tools.
- `refactor_context()` now includes related tests, namespace export
  metadata, and structured next-tool hints for safe-change workflows.
- New [`symbols()`](https://rdrr.io/r/graphics/symbols.html),
  `definition()`, and `references()` tools provide the redesigned
  name/path-based public navigation interface.
- New `package_overview()` tool provides a compact first-look package
  orientation summary, and `tests_for()` replaces the public
  `find_test_for_symbol()` MCP tool.
- `package_news()` replaces `pkg_news()` and now returns structured NEWS
  overviews by default, with version and keyword filters for focused
  entries.
- `help_search()` now returns bounded results by default and supports
  `limit` and `offset` pagination metadata for large result sets.
- Removed the redundant
  [`serve()`](https://tosidata.github.io/rairai/reference/serve.md)
  profile argument;
  [`serve()`](https://tosidata.github.io/rairai/reference/serve.md) now
  exposes the complete public tool surface without profiles.
- New `r6_method(class, method)` tool: returns exact file-sliced source
  for a specific method on a named R6 class. Reports the section
  (public, private, or active), signature, and source location.
- New `r6_method_references(class, method)` tool: searches the codebase
  for call sites of the form `obj$method()` using AST analysis on
  parseable files and heuristic pattern matching as a fallback. Returns
  concrete call-site locations, the receiver object, and call text.
- New `r6_classes()` tool: lists all R6 classes in the package’s R/
  directory with exact source locations and inheritance.
- New `r6_class(name)` tool: returns the full structure of a named R6
  class, including all public, private, and active members with their
  kinds and signatures.
- New `package_news()` tool: reads structured NEWS.md or NEWS content
  for a package. Supports optional `version` and `keyword` filtering to
  narrow output. Works for the local package under development and
  installed packages.
- New `package_vignettes()` tool: lists available vignettes for a
  package and retrieves their content as plain text. Works for the local
  package under development and installed packages.
- `diagnostics()` gains an optional `severity` argument (`"Error"`,
  `"Warning"`, `"Information"`, `"Hint"`) to filter results by severity
  level. `NULL`, `""`, and `"all"` return all severities (P9-safe).
- Definition location results from `symbol_definition()`,
  `symbol_definition_by_name()`, and `refactor_context()` use compact
  exact source-object locations.
- New `tests_for()` tool: searches test files for mentions of a symbol
  by name and returns compact 1-based locations. Supports W6 (add or
  update tests).
- New `help_page()` tool: retrieves and renders an R help page as plain
  readable text. Searches the package under development first (in-source
  Rd files), then falls back to installed packages. Supports optional
  `sections` filtering and `max_chars` truncation to control response
  length.
- New `help_search()` tool: searches R help pages for a query string and
  returns matching topics with their package and a brief description.
- New `refactor_context()` tool: returns definition location, all
  references, namespace export status, and diagnostics for the
  definition file — all in one call. Supports W3 (refactor without
  guessing).
