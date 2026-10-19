# MCP tool schemas and example outputs

This vignette shows what an MCP client can expose to an AI agent when
the `rairai` server is used against this package. It prints all 21
current tool schemas and representative inputs. The 19 package-local
examples run during rendering; the two CRAN examples show schemas and
inputs only, without network calls or fabricated responses.

Schemas and local responses come from freshly loaded package code, not
hand-written snapshots. Example arguments are maintained here. Displayed
JSON replaces the canonical build root with `/path/to/rairai`, including
occurrences in nested values or keys. This is a publication-only
redaction: runtime responses still disclose canonical absolute host
paths. Source-relative locations are unchanged.

### How to read this vignette

Each section below is produced from the live tool definitions in this
package:

- The schema comes from the
  [`mcplite::tool()`](https://tosidata.github.io/mcplite/reference/tool.html)
  object after conversion to MCP tool JSON for protocol version
  `2025-11-25`.
- The example input is a small argument list chosen to exercise the tool
  against this repository.
- Each local response is produced by calling the tool during rendering.
  A failed call fails the render instead of being presented as a normal
  response.
- CRAN inputs are illustrative and are not executed.

Loading `rairai` here runs the documentation generator. The tools
inspect package files and metadata statically; they do not execute
inspected source to obtain facts. The 19 local schemas expose optional
`package_root`; the CRAN schemas do not. See the [toolset
overview](https://tosidata.github.io/rairai/articles/tools.html#tool-surface-and-package-roots)
for root selection and filesystem access.

``` r

emit_tool_group(
  "Static package tools",
  c(
    "package_overview",
    "package_dependencies",
    "package_files",
    "package_namespace",
    "tests_for",
    "package_docs",
    "help_search",
    "help_page",
    "package_vignettes",
    "package_news"
  )
)
```

## Static package tools

### `package_overview`

Get a compact orientation map of the local package, including metadata,
namespace, file layout, docs, tests, and source objects.

#### Schema visible to the MCP client

``` json
{
  "name": "package_overview",
  "description": "Get a compact orientation map of the local package, including metadata, namespace, file layout, docs, tests, and source objects.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "description": {
    "fields": {
      "Package": "rairai",
      "Title": "'R' Package Intelligence MCP Server",
      "Version": "0.2.6",
      "Authors@R": "person(\"Juha\", \"Itkonen\", , \"juha@tosidata.com\", role = c(\"aut\", \"cre\"))",
      "Description": "Helps artificial intelligence (AI) agents develop 'R' packages\nmore efficiently and make better-informed code changes. Provides focused,\npackage-aware context through the Model Context Protocol (MCP) so agents\ncan understand existing behavior and assess changes with less manual\nexploration. Inspects packages without loading or executing their code.",
      "License": "MIT + file LICENSE",
      "URL": "https://github.com/tosidata/rairai,\nhttps://tosidata.github.io/rairai/",
      "BugReports": "https://github.com/tosidata/rairai/issues",
      "Encoding": "UTF-8",
      "Depends": [
        "R (>= 4.3.0)"
      ],
      "Imports": [
        "mcplite",
        "cachem",
        "jsonlite",
        "stringr",
        "purrr",
        "glue",
        "rlang",
        "fs",
        "gert",
        "R6",
        "frontmatter",
        "rvest",
        "pkgsearch",
        "treesitter",
        "treesitter.r"
      ],
      "Suggests": [
        "knitr",
        "quarto",
        "testthat (>= 3.0.0)",
        "withr",
        "pkgload"
      ],
      "Config/testthat/edition": "3",
      "VignetteBuilder": "quarto",
      "Config/roxygen2/version": "8.1.0",
      "Config/roxygen2/markdown": "TRUE",
      "Enhances": [],
      "LinkingTo": [],
      "Remotes": []
    },
    "more": {
      "tool": "package_dependencies",
      "reason": "Use for dependency tree expansion and dependency-specific analysis."
    }
  },
  "namespace": {
    "exports": [
      "serve"
    ],
    "imports_count": 8,
    "s3_methods_count": 0,
    "use_dyn_lib_count": 0,
    "more": {
      "tool": "package_namespace",
      "reason": "Use for full import declarations, importFrom directives, S3 method registrations, native-code declarations, and namespace parse details."
    }
  },
  "files": {
    "summary": {
      "total_file_count": 71,
      "top_level_dir_count": 5,
      "top_level_file_count": 9
    },
    "has_package_files": {
      "DESCRIPTION": true,
      "NAMESPACE": true,
      "README.md": true,
      "README.Rmd": false,
      "NEWS.md": true,
      "NEWS": false,
      "LICENSE": true,
      "LICENSE.md": true,
      ".Rbuildignore": true,
      "_pkgdown.yml": true
    },
    "has_package_dirs": {
      "R": true,
      "tests": true,
      "tests/testthat": true,
      "man": true,
      "vignettes": true,
      "data": false,
      "data-raw": false,
      "src": false,
      "inst": false
    },
    "layout": [
      {
        "path": ".github",
        "direct_file_count": 1,
        "recursive_file_count": 5,
        "children": [
          {
            "path": ".github/workflows",
            "direct_file_count": 4,
            "recursive_file_count": 4,
            "children": []
          }
        ]
      },
      {
        "path": "man",
        "direct_file_count": 4,
        "recursive_file_count": 4,
        "children": []
      },
      {
        "path": "R",
        "direct_file_count": 25,
        "recursive_file_count": 25,
        "children": []
      },
      {
        "path": "tests",
        "direct_file_count": 1,
        "recursive_file_count": 22,
        "children": [
          {
            "path": "tests/testthat",
            "direct_file_count": 21,
            "recursive_file_count": 21,
            "children": []
          }
        ]
      },
      {
        "path": "vignettes",
        "direct_file_count": 6,
        "recursive_file_count": 6,
        "children": []
      }
    ],
    "more": {
      "tool": "package_files",
      "reason": "Use for complete file listings by scope and optional metadata such as line counts, sizes, or modification times."
    }
  },
  "documentation": {
    "man_pages_count": 4,
    "vignettes_count": 3,
    "more": {
      "tools": [
        "package_docs",
        "help_page",
        "help_search",
        "package_vignettes",
        "package_news"
      ],
      "reason": "Use these for documentation maps, rendered help topics, help discovery, vignette contents, and NEWS entries."
    }
  },
  "tests": {
    "frameworks": [
      "testthat"
    ],
    "test_count": 278,
    "snapshot_count": 0,
    "helper_file_count": 1,
    "fixture_dir_count": 0,
    "more": {
      "tool": "tests_for",
      "reason": "Use before behavior changes to find tests related to a symbol."
    }
  },
  "source_objects": {
    "counts": {
      "functions": 275,
      "r6_classes": 2,
      "other_objects": 4
    },
    "total_objects": 281,
    "listed_objects": 20,
    "objects": [
      {
        "name": "internal_cache_env",
        "kind": "other_object",
        "location": "R/cache.R:1"
      },
      {
        "name": "cached_value",
        "kind": "function",
        "location": "R/cache.R:3-23"
      },
      {
        "name": "reset_internal_cache",
        "kind": "function",
        "location": "R/cache.R:25-28"
      },
      {
        "name": "read_package_description",
        "kind": "function",
        "location": "R/description.R:1-31"
      },
      {
        "name": "json_response",
        "kind": "function",
        "leading_comment_lines": "1-29",
        "location": "R/json-response.R:30-32"
      },
      {
        "name": "tool_value_response",
        "kind": "function",
        "leading_comment_lines": "34-41",
        "location": "R/json-response.R:42-44"
      },
      {
        "name": "tool_error_response",
        "kind": "function",
        "leading_comment_lines": "46-53",
        "location": "R/json-response.R:54-56"
      },
      {
        "name": "format_location",
        "kind": "function",
        "location": "R/location-format.R:1-42"
      },
      {
        "name": "normalize_location_part",
        "kind": "function",
        "location": "R/location-format.R:44-55"
      },
      {
        "name": "compact_location_fields",
        "kind": "function",
        "location": "R/location-format.R:57-103"
      },
      {
        "name": "compact_location_records",
        "kind": "function",
        "location": "R/location-format.R:105-127"
      },
      {
        "name": "format_same_file_line_range",
        "kind": "function",
        "location": "R/location-format.R:129-140"
      },
      {
        "name": "add_leading_comment_lines",
        "kind": "function",
        "location": "R/location-format.R:142-164"
      },
      {
        "name": "leading_comment_block_range",
        "kind": "function",
        "location": "R/location-format.R:166-188"
      },
      {
        "name": "read_source_lines",
        "kind": "function",
        "location": "R/location-format.R:190-199"
      },
      {
        "name": "source_path_for_record",
        "kind": "function",
        "location": "R/location-format.R:201-210"
      },
      {
        "name": "is_comment_line",
        "kind": "function",
        "location": "R/location-format.R:212-214"
      },
      {
        "name": "is_file_uri",
        "kind": "function",
        "location": "R/path-uri.R:1-5"
      },
      {
        "name": "path_to_uri",
        "kind": "function",
        "location": "R/path-uri.R:7-16"
      },
      {
        "name": "normalize_root_relative_path",
        "kind": "function",
        "location": "R/path-uri.R:18-68"
      }
    ],
    "objects_truncated": true,
    "parse_errors": [],
    "more": {
      "tools": [
        "symbols",
        "definition",
        "r6_classes",
        "r6_class"
      ],
      "reason": "Source objects are previewed only. Use symbols for searchable source symbols, definition for a specific object, and R6 tools for class structure."
    }
  }
}
```

### `package_dependencies`

Read dependency fields from DESCRIPTION.

#### Schema visible to the MCP client

``` json
{
  "name": "package_dependencies",
  "description": "Read dependency fields from DESCRIPTION.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "depth": {
        "type": "integer",
        "description": "Dependency depth. Use 1 for direct dependencies only. Default: 1."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "depth": 1
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "dependencies": {
    "depends": [
      "R (>= 4.3.0)"
    ],
    "imports": [
      "mcplite",
      "cachem",
      "jsonlite",
      "stringr",
      "purrr",
      "glue",
      "rlang",
      "fs",
      "gert",
      "R6",
      "frontmatter",
      "rvest",
      "pkgsearch",
      "treesitter",
      "treesitter.r"
    ],
    "suggests": [
      "knitr",
      "quarto",
      "testthat (>= 3.0.0)",
      "withr",
      "pkgload"
    ],
    "enhances": [],
    "linkingto": []
  },
  "remotes": [],
  "config_needs": []
}
```

### `package_files`

List package files by development area.

#### Schema visible to the MCP client

``` json
{
  "name": "package_files",
  "description": "List package files by development area.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "scope": {
        "type": "string",
        "description": "File area to list: 'R', 'tests', 'docs', or 'all'. Default: 'R'."
      },
      "include": {
        "type": "array",
        "items": {
          "type": "string"
        },
        "description": "Extra metadata to include: 'lines', 'size', or 'modified'."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "scope": "R",
  "include": ["lines", "size"]
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "scope": "R",
  "files": [
    {
      "path": "R/cache.R",
      "lines": 28,
      "size": 634
    },
    {
      "path": "R/description.R",
      "lines": 31,
      "size": 725
    },
    {
      "path": "R/json-response.R",
      "lines": 56,
      "size": 1493
    },
    {
      "path": "R/location-format.R",
      "lines": 214,
      "size": 4733
    },
    {
      "path": "R/path-uri.R",
      "lines": 135,
      "size": 3781
    },
    {
      "path": "R/r6-syntax-facts.R",
      "lines": 519,
      "size": 14077
    },
    {
      "path": "R/rairai-package.R",
      "lines": 7,
      "size": 221
    },
    {
      "path": "R/serve.R",
      "lines": 239,
      "size": 6718
    },
    {
      "path": "R/source-index.R",
      "lines": 730,
      "size": 21162
    },
    {
      "path": "R/tool-cran.R",
      "lines": 246,
      "size": 6167
    },
    {
      "path": "R/tool-definitions.R",
      "lines": 249,
      "size": 6528
    },
    {
      "path": "R/tool-doc-errors.R",
      "lines": 15,
      "size": 387
    },
    {
      "path": "R/tool-find-test.R",
      "lines": 213,
      "size": 5473
    },
    {
      "path": "R/tool-help.R",
      "lines": 843,
      "size": 20301
    },
    {
      "path": "R/tool-helpers.R",
      "lines": 76,
      "size": 1643
    },
    {
      "path": "R/tool-package-dependencies.R",
      "lines": 266,
      "size": 6463
    },
    {
      "path": "R/tool-package-docs.R",
      "lines": 215,
      "size": 5416
    },
    {
      "path": "R/tool-package-files.R",
      "lines": 363,
      "size": 8728
    },
    {
      "path": "R/tool-package-namespace.R",
      "lines": 542,
      "size": 13384
    },
    {
      "path": "R/tool-package-overview.R",
      "lines": 541,
      "size": 14434
    },
    {
      "path": "R/tool-package-vignettes.R",
      "lines": 408,
      "size": 10144
    },
    {
      "path": "R/tool-pkg-news.R",
      "lines": 513,
      "size": 13152
    },
    {
      "path": "R/tool-r6-classes.R",
      "lines": 558,
      "size": 13936
    },
    {
      "path": "R/tool-service.R",
      "lines": 669,
      "size": 19881
    },
    {
      "path": "R/zzz.R",
      "lines": 3,
      "size": 67
    }
  ],
  "count": 25,
  "include": [
    "lines",
    "size"
  ]
}
```

### `package_namespace`

Read exports, imports, S3 registrations, and native-code declarations
from NAMESPACE. Imported object lists are compact by default.

#### Schema visible to the MCP client

``` json
{
  "name": "package_namespace",
  "description": "Read exports, imports, S3 registrations, and native-code declarations from NAMESPACE. Imported object lists are compact by default.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "package": {
        "type": "string",
        "description": "Imported package to inspect in full, such as 'dplyr' or 'stringr'. Leave empty for the namespace summary."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "exports": [
    "serve"
  ],
  "imports": {
    "glue": [
      "glue",
      "glue_collapse"
    ],
    "jsonlite": [
      "fromJSON",
      "toJSON"
    ],
    "purrr": [
      "%>%",
      "%||%",
      "accumulate",
      "accumulate2",
      "array_branch",
      "array_tree",
      "as_mapper",
      "as_vector",
      "assign_in",
      "attr_getter",
      "auto_browse",
      "chuck",
      "compact",
      "compose",
      "cross",
      "cross_df",
      "cross2",
      "cross3",
      "detect",
      "detect_index"
    ],
    "R6": [
      "R6Class"
    ],
    "stats": [
      "setNames"
    ],
    "stringr": [
      "%>%",
      "boundary",
      "coll",
      "fixed",
      "invert_match",
      "regex",
      "str_c",
      "str_conv",
      "str_count",
      "str_detect",
      "str_dup",
      "str_ends",
      "str_equal",
      "str_escape",
      "str_extract",
      "str_extract_all",
      "str_flatten",
      "str_flatten_comma",
      "str_glue",
      "str_glue_data"
    ],
    "treesitter": [
      "is_language",
      "is_node",
      "is_parser",
      "is_point",
      "is_query",
      "is_range",
      "is_tree",
      "language_field_count",
      "language_field_id_for_name",
      "language_field_name_for_id",
      "language_name",
      "language_next_state",
      "language_state_count",
      "language_symbol_count",
      "language_symbol_for_name",
      "language_symbol_name",
      "node_child",
      "node_child_by_field_id",
      "node_child_by_field_name",
      "node_child_count"
    ],
    "utils": [
      "available.packages",
      "head"
    ]
  },
  "s3_methods": [],
  "use_dyn_lib": [],
  "truncated": {
    "purrr": {
      "shown": 20,
      "total": 184
    },
    "stringr": {
      "shown": 20,
      "total": 63
    },
    "treesitter": {
      "shown": 20,
      "total": 89
    }
  }
}
```

### `tests_for`

Find test files and test blocks that reference a symbol. Use this before
changing behavior to locate related tests.

#### Schema visible to the MCP client

``` json
{
  "name": "tests_for",
  "description": "Find test files and test blocks that reference a symbol. Use this before changing behavior to locate related tests.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "name": {
        "type": "string",
        "description": "Symbol name to search for in tests."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "name"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "name": "serve_tools"
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "name": "serve_tools",
  "count": 11,
  "matches": [
    {
      "location": "tests/testthat/test-root-isolation.R:93",
      "desc": {},
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-root-isolation.R:93"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:56-66",
      "desc": "serve_tools exposes the single target public toolset",
      "hit_count": 2,
      "hits": [
        "tests/testthat/test-serve-tools.R:56",
        "tests/testthat/test-serve-tools.R:57"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:68-109",
      "desc": "served tool schemas mark only required public arguments",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-serve-tools.R:69"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:111-140",
      "desc": "only package-local tools expose the optional package root",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-serve-tools.R:112"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:142-158",
      "desc": "reference tools omit the declaration toggle",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-serve-tools.R:143"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:160-197",
      "desc": "served tool errors are MCP tool-call errors with JSON text",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-serve-tools.R:162"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:261-301",
      "desc": "invalid JSON-like errors become structured MCP errors",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-serve-tools.R:266"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:337",
      "desc": {},
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-serve-tools.R:337"
      ]
    },
    {
      "location": "tests/testthat/test-tool-find-test.R:263-269",
      "desc": "tests_for is the registered public test-discovery tool",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-tool-find-test.R:264"
      ]
    },
    {
      "location": "tests/testthat/test-tool-package-overview.R:828-833",
      "desc": "package_overview tool is in the served toolset",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-tool-package-overview.R:829"
      ]
    },
    {
      "location": "tests/testthat/test-tool-r6-classes.R:529-536",
      "desc": "tool_r6_classes is in the served toolset",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-tool-r6-classes.R:530"
      ]
    }
  ]
}
```

### `package_docs`

Summarize package documentation, including help pages, aliases, titles,
vignettes, and NEWS.

#### Schema visible to the MCP client

``` json
{
  "name": "package_docs",
  "description": "Summarize package documentation, including help pages, aliases, titles, vignettes, and NEWS.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "package": {
        "type": "string",
        "description": "Package to inspect. Leave empty for the local source package."
      },
      "limit": {
        "type": "integer",
        "description": "Maximum number of help topics."
      },
      "offset": {
        "type": "integer",
        "description": "Number of help topics to skip."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "package": "rairai",
  "limit": 10
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "package": "rairai",
  "source": "local",
  "help": {
    "total_count": 4,
    "count": 4,
    "offset": 0,
    "limit": 10,
    "has_more": false,
    "topics": [
      {
        "topic": "json_response",
        "title": "Create a JSON response",
        "aliases": [
          "json_response"
        ],
        "sections": [
          "Description",
          "Usage",
          "Arguments",
          "Value",
          "Examples"
        ]
      },
      {
        "topic": "serve",
        "title": "Serve the MCP tools",
        "aliases": [
          "serve"
        ],
        "sections": [
          "Description",
          "Usage",
          "Arguments",
          "Details",
          "Value",
          "See Also",
          "File access and security"
        ]
      },
      {
        "topic": "tool_error_response",
        "title": "Create an MCP tool error response",
        "aliases": [
          "tool_error_response"
        ],
        "sections": [
          "Description",
          "Usage",
          "Arguments",
          "Value"
        ]
      },
      {
        "topic": "tool_value_response",
        "title": "Create a successful MCP tool response",
        "aliases": [
          "tool_value_response"
        ],
        "sections": [
          "Description",
          "Usage",
          "Arguments",
          "Value"
        ]
      }
    ]
  },
  "vignettes": {
    "available": true,
    "count": 3,
    "items": {
      "/path/to/rairai/vignettes/design.qmd": {
        "name": "design",
        "file": "vignettes/design.qmd",
        "title": "Design Principles"
      },
      "/path/to/rairai/vignettes/mcp-tool-schemas.qmd": {
        "name": "mcp-tool-schemas",
        "file": "vignettes/mcp-tool-schemas.qmd",
        "title": "MCP tool schemas and example outputs"
      },
      "/path/to/rairai/vignettes/tools.qmd": {
        "name": "tools",
        "file": "vignettes/tools.qmd",
        "title": "Toolset overview"
      }
    }
  },
  "news": {
    "available": true,
    "source": "NEWS.md",
    "latest_version": "development version",
    "latest_heading": "rairai (development version)",
    "version_count": 12
  },
  "next_tools": [
    {
      "tool": "help_page",
      "reason": "Read one help topic from the documentation map.",
      "arguments": {
        "topic": "<topic>",
        "package": "rairai"
      }
    },
    {
      "tool": "help_search",
      "reason": "Search this package's help topics by name, alias, title, or concept.",
      "arguments": {
        "query": "<query>",
        "package": "rairai"
      }
    },
    {
      "tool": "package_vignettes",
      "reason": "Retrieve long-form package documentation.",
      "arguments": {
        "package": "rairai"
      }
    },
    {
      "tool": "package_news",
      "reason": "Inspect release history or behavior changes.",
      "arguments": {
        "package": "rairai"
      }
    }
  ]
}
```

### `help_search`

Search R help topics by name, alias, title, or concept.

#### Schema visible to the MCP client

``` json
{
  "name": "help_search",
  "description": "Search R help topics by name, alias, title, or concept.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": {
        "type": "string",
        "description": "Search query."
      },
      "package": {
        "type": "string",
        "description": "Package to search within."
      },
      "limit": {
        "type": "integer",
        "description": "Maximum number of matches."
      },
      "offset": {
        "type": "integer",
        "description": "Number of matches to skip."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "query"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "query": "serve",
  "package": "rairai",
  "limit": 5
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "query": "serve",
  "package": "rairai",
  "search_scope": "local_package",
  "total_count": 1,
  "count": 1,
  "offset": 0,
  "limit": 5,
  "has_more": false,
  "matches": [
    {
      "rank": 1,
      "topic": "serve",
      "package": "rairai",
      "title": "Serve the MCP tools",
      "description": "Runs a Model Context Protocol (MCP) server over standard input and output.",
      "source": "local",
      "score": 101,
      "matched": [
        {
          "field": "topic",
          "type": "exact"
        },
        {
          "field": "alias",
          "type": "exact"
        },
        {
          "field": "topic",
          "type": "prefix"
        },
        {
          "field": "alias",
          "type": "prefix"
        },
        {
          "field": "title",
          "type": "words"
        },
        {
          "field": "title",
          "type": "substring"
        },
        {
          "field": "description",
          "type": "substring"
        }
      ],
      "next_tools": [
        {
          "tool": "help_page",
          "arguments": {
            "topic": "serve",
            "package": "rairai"
          }
        }
      ]
    }
  ]
}
```

### `help_page`

Read one R help page as plain text.

#### Schema visible to the MCP client

``` json
{
  "name": "help_page",
  "description": "Read one R help page as plain text.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "topic": {
        "type": "string",
        "description": "Help topic or alias."
      },
      "package": {
        "type": "string",
        "description": "Package containing the topic."
      },
      "sections": {
        "type": "array",
        "items": {
          "type": "string"
        },
        "description": "Help sections to include, such as Usage, Arguments, Details, Value, or Examples."
      },
      "max_chars": {
        "type": "integer",
        "description": "Maximum number of characters to return."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "topic"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "topic": "serve",
  "package": "rairai",
  "sections": ["Description", "Usage", "Arguments", "Examples"],
  "max_chars": 2000
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "topic": "serve",
  "package": "rairai",
  "source": "local",
  "title": "Serve the MCP tools",
  "sections": [
    "Description",
    "Usage",
    "Arguments",
    "Details",
    "Value",
    "File access and security",
    "See Also"
  ],
  "requested_sections": [
    "Description",
    "Usage",
    "Arguments",
    "Examples"
  ],
  "matched_sections": [
    "Description",
    "Usage",
    "Arguments"
  ],
  "missing_sections": [
    "Examples"
  ],
  "text": "Description:\n\n     Runs a Model Context Protocol (MCP) server over standard input and\n     output.\n\nUsage:\n\n     serve(path = \".\")\n     \nArguments:\n\n    path: Existing package directory to inspect by default. Accepts\n          absolute, relative, and tilde paths. Defaults to the current\n          working directory.\n",
  "truncated": false
}
```

### `package_vignettes`

List package vignettes or read one vignette.

#### Schema visible to the MCP client

``` json
{
  "name": "package_vignettes",
  "description": "List package vignettes or read one vignette.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "package": {
        "type": "string",
        "description": "Package to inspect. Leave empty for the local source package."
      },
      "name": {
        "type": "string",
        "description": "Vignette name to read. Leave empty to list vignettes."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "package": "rairai"
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "package": "rairai",
  "source": "local",
  "count": 3,
  "vignettes": {
    "/path/to/rairai/vignettes/design.qmd": {
      "name": "design",
      "file": "vignettes/design.qmd",
      "title": "Design Principles"
    },
    "/path/to/rairai/vignettes/mcp-tool-schemas.qmd": {
      "name": "mcp-tool-schemas",
      "file": "vignettes/mcp-tool-schemas.qmd",
      "title": "MCP tool schemas and example outputs"
    },
    "/path/to/rairai/vignettes/tools.qmd": {
      "name": "tools",
      "file": "vignettes/tools.qmd",
      "title": "Toolset overview"
    }
  }
}
```

### `package_news`

Read package NEWS entries for release history and behavior changes.

#### Schema visible to the MCP client

``` json
{
  "name": "package_news",
  "description": "Read package NEWS entries for release history and behavior changes.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "package": {
        "type": "string",
        "description": "Package to inspect. Leave empty for the local source package."
      },
      "version": {
        "type": "string",
        "description": "NEWS version or heading to read. Leave empty for an overview."
      },
      "keyword": {
        "type": "string",
        "description": "Keyword or regular expression to search."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "package": "rairai"
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "package": "rairai",
  "source": "NEWS.md",
  "mode": "overview",
  "latest": {
    "heading": "rairai (development version)",
    "version": "development version",
    "location": "NEWS.md:1-2",
    "entry_count": 0,
    "returned_count": 0,
    "truncated": false,
    "entries": []
  },
  "count": 12,
  "versions": [
    {
      "heading": "rairai (development version)",
      "version": "development version",
      "location": "NEWS.md:1-2"
    },
    {
      "heading": "rairai 0.2.6",
      "version": "0.2.6",
      "location": "NEWS.md:3-6"
    },
    {
      "heading": "rairai 0.2.5",
      "version": "0.2.5",
      "location": "NEWS.md:7-10"
    },
    {
      "heading": "rairai 0.2.4",
      "version": "0.2.4",
      "location": "NEWS.md:11-14"
    },
    {
      "heading": "rairai 0.2.3",
      "version": "0.2.3",
      "location": "NEWS.md:15-18"
    },
    {
      "heading": "rairai 0.2.2",
      "version": "0.2.2",
      "location": "NEWS.md:19-22"
    },
    {
      "heading": "rairai 0.2.1",
      "version": "0.2.1",
      "location": "NEWS.md:23-26"
    },
    {
      "heading": "rairai 0.2.0",
      "version": "0.2.0",
      "location": "NEWS.md:27-36"
    },
    {
      "heading": "rairai 0.1.2",
      "version": "0.1.2",
      "location": "NEWS.md:37-40"
    },
    {
      "heading": "rairai 0.1.1",
      "version": "0.1.1",
      "location": "NEWS.md:41-47"
    },
    {
      "heading": "rairai 0.1.0",
      "version": "0.1.0",
      "location": "NEWS.md:48-53"
    },
    {
      "heading": "rairai 0.0.1",
      "version": "0.0.1",
      "location": "NEWS.md:54-111"
    }
  ]
}
```

``` r

emit_tool_group(
  "R6 structure tools",
  c(
    "r6_classes",
    "r6_class",
    "r6_method",
    "r6_method_references"
  )
)
```

## R6 structure tools

### `r6_classes`

List R6 classes defined in the package, including locations and
inheritance.

#### Schema visible to the MCP client

``` json
{
  "name": "r6_classes",
  "description": "List R6 classes defined in the package, including locations and inheritance.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "count": 2,
  "classes": [
    {
      "name": "SourceIndex",
      "location": "R/source-index.R:3-730"
    },
    {
      "name": "PackageToolService",
      "location": "R/tool-service.R:1-669"
    }
  ]
}
```

### `r6_class`

Inspect an R6 class, including public, private, and active members,
source locations, and method signatures.

#### Schema visible to the MCP client

``` json
{
  "name": "r6_class",
  "description": "Inspect an R6 class, including public, private, and active members, source locations, and method signatures.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "name": {
        "type": "string",
        "description": "R6 class name."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "name"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "name": "PackageToolService"
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "name": "PackageToolService",
  "public": [
    {
      "name": "root_path",
      "kind": "field",
      "default": "NULL",
      "location": "R/tool-service.R:4"
    },
    {
      "name": "source_index",
      "kind": "field",
      "default": "NULL",
      "location": "R/tool-service.R:5"
    },
    {
      "name": "initialize",
      "kind": "method",
      "signature": "initialize(root_path)",
      "leading_comment_lines": "7",
      "location": "R/tool-service.R:8-11"
    },
    {
      "name": "package_overview",
      "kind": "method",
      "signature": "package_overview()",
      "location": "R/tool-service.R:13-15"
    },
    {
      "name": "package_dependencies",
      "kind": "method",
      "signature": "package_dependencies(depth = 1L)",
      "location": "R/tool-service.R:17-19"
    },
    {
      "name": "package_files",
      "kind": "method",
      "signature": "package_files(scope = \"R\", include = character())",
      "location": "R/tool-service.R:21-27"
    },
    {
      "name": "package_namespace",
      "kind": "method",
      "signature": "package_namespace(package = NULL)",
      "location": "R/tool-service.R:29-31"
    },
    {
      "name": "package_docs",
      "kind": "method",
      "signature": "package_docs(package = NULL, limit = 20L, offset = 0L)",
      "location": "R/tool-service.R:33-40"
    },
    {
      "name": "package_vignettes",
      "kind": "method",
      "signature": "package_vignettes(package = NULL, name = NULL)",
      "location": "R/tool-service.R:42-48"
    },
    {
      "name": "package_news",
      "kind": "method",
      "signature": "package_news(package = NULL, version = NULL, keyword = NULL)",
      "location": "R/tool-service.R:50-57"
    },
    {
      "name": "tests_for",
      "kind": "method",
      "signature": "tests_for(name)",
      "location": "R/tool-service.R:59-61"
    },
    {
      "name": "help_page",
      "kind": "method",
      "signature": "help_page(topic, package = NULL, sections = NULL, max_chars = NULL)",
      "location": "R/tool-service.R:63-76"
    },
    {
      "name": "help_search",
      "kind": "method",
      "signature": "help_search(query, package = NULL, limit = 20L, offset = 0L)",
      "location": "R/tool-service.R:78-86"
    },
    {
      "name": "r6_classes",
      "kind": "method",
      "signature": "r6_classes()",
      "location": "R/tool-service.R:88-90"
    },
    {
      "name": "r6_class",
      "kind": "method",
      "signature": "r6_class(name)",
      "location": "R/tool-service.R:92-94"
    },
    {
      "name": "r6_method",
      "kind": "method",
      "signature": "r6_method(class, method)",
      "location": "R/tool-service.R:96-98"
    },
    {
      "name": "r6_method_references",
      "kind": "method",
      "signature": "r6_method_references(class, method)",
      "location": "R/tool-service.R:100-106"
    },
    {
      "name": "symbols",
      "kind": "method",
      "signature": "symbols(query = NULL, path = NULL, kind = NULL, exact = TRUE, limit = 50L)",
      "location": "R/tool-service.R:108-165"
    },
    {
      "name": "definition",
      "kind": "method",
      "signature": "definition(name, kind = NULL, path = NULL, exact = TRUE, limit = 20L)",
      "location": "R/tool-service.R:167-201"
    },
    {
      "name": "references",
      "kind": "method",
      "signature": "references(name, kind = NULL, path = NULL, exact = TRUE, limit = 100L)",
      "location": "R/tool-service.R:203-260"
    },
    {
      "name": "function_formals",
      "kind": "method",
      "signature": "function_formals(name, package = NULL, context_path = NULL)",
      "location": "R/tool-service.R:262-346"
    },
    {
      "name": "refactor_context",
      "kind": "method",
      "signature": "refactor_context(name, kind = NULL, path = NULL, exact = TRUE, limit = 100L)",
      "location": "R/tool-service.R:348-425"
    }
  ],
  "private": [
    {
      "name": "function_reference",
      "kind": "method",
      "signature": "function_reference(name, package = NULL)",
      "location": "R/tool-service.R:428-469"
    },
    {
      "name": "parse_qualified_reference",
      "kind": "method",
      "signature": "parse_qualified_reference(name)",
      "location": "R/tool-service.R:471-481"
    },
    {
      "name": "is_valid_function_reference",
      "kind": "method",
      "signature": "is_valid_function_reference(reference)",
      "location": "R/tool-service.R:483-487"
    },
    {
      "name": "invalid_function_reference_error",
      "kind": "method",
      "signature": "invalid_function_reference_error()",
      "location": "R/tool-service.R:489-497"
    },
    {
      "name": "external_function_formals_response",
      "kind": "method",
      "signature": "external_function_formals_response(call_reference)",
      "location": "R/tool-service.R:499-535"
    },
    {
      "name": "r6_member_reference_match",
      "kind": "method",
      "signature": "r6_member_reference_match(resolved)",
      "location": "R/tool-service.R:537-551"
    },
    {
      "name": "related_tests_for_symbol",
      "kind": "method",
      "signature": "related_tests_for_symbol(name)",
      "location": "R/tool-service.R:553-575"
    },
    {
      "name": "is_symbol_exported",
      "kind": "method",
      "signature": "is_symbol_exported(name)",
      "location": "R/tool-service.R:577-589"
    },
    {
      "name": "refactor_context_next_tools",
      "kind": "method",
      "signature": "refactor_context_next_tools(status, has_tests = FALSE)",
      "location": "R/tool-service.R:591-625"
    },
    {
      "name": "symbols_no_match_next_tools",
      "kind": "method",
      "signature": "symbols_no_match_next_tools(query = NULL, exact = TRUE)",
      "location": "R/tool-service.R:627-667"
    }
  ],
  "active": [],
  "location": "R/tool-service.R:1-669"
}
```

### `r6_method`

Read the source and location for one R6 class method.

#### Schema visible to the MCP client

``` json
{
  "name": "r6_method",
  "description": "Read the source and location for one R6 class method.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "class": {
        "type": "string",
        "description": "R6 class name."
      },
      "method": {
        "type": "string",
        "description": "Method name."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "class",
      "method"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "class": "PackageToolService",
  "method": "symbols"
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "class": "PackageToolService",
  "method": "symbols",
  "section": "public",
  "signature": "symbols(query = NULL, path = NULL, kind = NULL, exact = TRUE, limit = 50L)",
  "source": "symbols = function(\n      query = NULL,\n      path = NULL,\n      kind = NULL,\n      exact = TRUE,\n      limit = 50L\n    ) {\n      query <- normalize_optional_string(query, default = NULL)\n      path <- normalize_optional_string(path, default = NULL)\n      kind <- normalize_optional_string(kind, default = NULL)\n      if (identical(str_to_lower(kind %||% \"\"), \"all\")) {\n        kind <- NULL\n      }\n      exact <- normalize_logical_value(exact, default = TRUE)\n      limit <- normalize_limit_value(limit, default = 50L)\n\n      result <- self$source_index$symbols(\n        query = query,\n        path = path,\n        kind = kind,\n        exact = exact,\n        limit = limit\n      )\n      if (has_tool_error(result)) {\n        return(tool_error_response(error = result$error))\n      }\n\n      response_symbols <- compact_location_records(\n        result$symbols,\n        include_leading_comment = TRUE,\n        source_root = self$root_path\n      )\n      response <- list(\n        query = result$query,\n        status = result$status,\n        exact = exact,\n        filters = result$filters,\n        count = length(response_symbols),\n        total_count = result$total_count,\n        truncated = result$truncated,\n        limit = result$limit,\n        symbols = response_symbols\n      )\n      if (!is.null(result$path)) {\n        response$path <- result$path\n      }\n      if (length(result$parse_errors %||% list()) > 0L) {\n        response$parse_errors <- result$parse_errors\n      }\n      if (identical(result$status, \"none\")) {\n        response$next_tools <- private$symbols_no_match_next_tools(\n          query = query,\n          exact = exact\n        )\n      }\n\n      tool_value_response(!!!response)\n    }",
  "location": "R/tool-service.R:108-165"
}
```

### `r6_method_references`

Find \$method() call sites that may refer to an R6 method. Results are
name-based and may include calls on other object types.

#### Schema visible to the MCP client

``` json
{
  "name": "r6_method_references",
  "description": "Find $method() call sites that may refer to an R6 method. Results are name-based and may include calls on other object types.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "class": {
        "type": "string",
        "description": "R6 class name."
      },
      "method": {
        "type": "string",
        "description": "Method name."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "class",
      "method"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "class": "PackageToolService",
  "method": "symbols"
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "class": "PackageToolService",
  "method": "symbols",
  "target_resolution": {
    "class": {
      "name": "PackageToolService",
      "status": "unique",
      "count": 1,
      "matches": [
        {
          "name": "PackageToolService",
          "location": "R/tool-service.R:1-669"
        }
      ]
    },
    "method": {
      "name": "symbols",
      "status": "unique",
      "count": 1,
      "matches": [
        {
          "class": "PackageToolService",
          "method": "symbols",
          "section": "public",
          "location": "R/tool-service.R:108-165"
        }
      ]
    }
  },
  "note": "Results are name-only syntax matches for $method() calls and do not prove the receiver type or class membership. Calls on unrelated object types may be included.",
  "count": 23,
  "references": [
    {
      "object": "selected_service",
      "call": "selected_service$symbols(\n            query = query,\n            path = path,\n            kind = kind,\n            exact = exact,\n            limit = limit\n          )",
      "location": "R/tool-definitions.R:15"
    },
    {
      "object": "self$source_index",
      "call": "self$source_index$symbols(\n        query = query,\n        path = path,\n        kind = kind,\n        exact = exact,\n        limit = limit\n      )",
      "location": "R/tool-service.R:124"
    },
    {
      "object": "s",
      "call": "s$symbols()",
      "location": "tests/testthat/test-source-containment.R:111"
    },
    {
      "object": "s",
      "call": "s$symbols(path = \"R/core.R\")",
      "location": "tests/testthat/test-source-containment.R:145"
    },
    {
      "object": "s",
      "call": "s$symbols(kind = \"Function\")",
      "location": "tests/testthat/test-source-containment.R:212"
    },
    {
      "object": "s",
      "call": "s$symbols(query = \"target\")",
      "location": "tests/testthat/test-source-containment.R:302"
    },
    {
      "object": "s",
      "call": "s$symbols(query = \"target\")",
      "location": "tests/testthat/test-source-containment.R:307"
    },
    {
      "object": "s",
      "call": "s$symbols()",
      "location": "tests/testthat/test-source-containment.R:309"
    },
    {
      "object": "s",
      "call": "s$symbols()",
      "location": "tests/testthat/test-source-containment.R:379"
    },
    {
      "object": "s",
      "call": "s$symbols()",
      "location": "tests/testthat/test-source-containment.R:451"
    },
    {
      "object": "service",
      "call": "service$symbols(path = \"R/frequency.R\", exact = TRUE, limit = 100)",
      "location": "tests/testthat/test-source-index.R:49"
    },
    {
      "object": "service",
      "call": "service$symbols(kind = \"Class\", exact = TRUE, limit = 100)",
      "location": "tests/testthat/test-source-index.R:91"
    },
    {
      "object": "service",
      "call": "service$symbols(kind = \"Class\", exact = TRUE, limit = 100)",
      "location": "tests/testthat/test-source-index.R:113"
    },
    {
      "object": "service",
      "call": "service$symbols(query = \"Fake\", kind = \"Object\", exact = TRUE, limit = 10)",
      "location": "tests/testthat/test-source-index.R:117"
    },
    {
      "object": "service",
      "call": "service$symbols(limit = 100)",
      "location": "tests/testthat/test-source-index.R:169"
    },
    {
      "object": "service",
      "call": "service$symbols(\n      query = \"validate_serialized_frequency\",\n      kind = \"Function\",\n      path = \"R\",\n      limit = 10\n    )",
      "location": "tests/testthat/test-source-index.R:270"
    },
    {
      "object": "service",
      "call": "service$symbols(path = \"R/frequency.R\", exact = TRUE, limit = 2)",
      "location": "tests/testthat/test-source-index.R:428"
    },
    {
      "object": "service",
      "call": "service$symbols(kind = \"Function\", limit = 100)",
      "location": "tests/testthat/test-source-index.R:595"
    },
    {
      "object": "service",
      "call": "service$symbols(kind = \"Function\", limit = 100)",
      "location": "tests/testthat/test-source-index.R:617"
    },
    {
      "object": "service",
      "call": "service$symbols(kind = \"Function\", limit = 100)",
      "location": "tests/testthat/test-source-index.R:624"
    },
    {
      "object": "source_index_service(root_a)",
      "call": "source_index_service(root_a)$symbols(kind = \"Function\", limit = 100)",
      "location": "tests/testthat/test-source-index.R:647"
    },
    {
      "object": "source_index_service(root_b)",
      "call": "source_index_service(root_b)$symbols(kind = \"Function\", limit = 100)",
      "location": "tests/testthat/test-source-index.R:651"
    },
    {
      "object": "service",
      "call": "service$symbols(path = tempfile(tmpdir = dirname(root)))",
      "location": "tests/testthat/test-tool-function-formals.R:163"
    }
  ]
}
```

``` r

emit_tool_group(
  "Source and workflow tools",
  c(
    "symbols",
    "definition",
    "references",
    "function_formals",
    "refactor_context"
  )
)
```

## Source and workflow tools

### `symbols`

Find source symbols by name, kind, or file.

#### Schema visible to the MCP client

``` json
{
  "name": "symbols",
  "description": "Find source symbols by name, kind, or file.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": {
        "type": "string",
        "description": "Symbol name or query."
      },
      "path": {
        "type": "string",
        "description": "File path for an outline, or path prefix to limit the search."
      },
      "kind": {
        "type": "string",
        "description": "Symbol kind, such as Function, Method, Class, or Variable."
      },
      "exact": {
        "type": "boolean",
        "description": "Require an exact symbol-name match. Default: true."
      },
      "limit": {
        "type": "integer",
        "description": "Maximum number of symbols. Default: 50."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "query": "serve",
  "kind": "Function",
  "limit": 20
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "query": "serve",
  "status": "unique",
  "exact": true,
  "filters": {
    "kind": "Function"
  },
  "count": 1,
  "total_count": 1,
  "truncated": false,
  "limit": 20,
  "symbols": [
    {
      "name": "serve",
      "kind_name": "Function",
      "source": "tree_sitter_source_index",
      "referencable": true,
      "signature": {
        "label": "serve(path = \".\")",
        "documentation": "",
        "parameters": [
          {
            "label": "path = \".\"",
            "name": "path",
            "default": "\".\""
          }
        ]
      },
      "leading_comment_lines": "1-29",
      "location": "R/serve.R:30:1-39:2"
    }
  ]
}
```

### `definition`

Resolve a package-local symbol to one canonical Tree-sitter definition.

#### Schema visible to the MCP client

``` json
{
  "name": "definition",
  "description": "Resolve a package-local symbol to one canonical Tree-sitter definition.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "name": {
        "type": "string",
        "description": "Symbol name or query."
      },
      "kind": {
        "type": "string",
        "description": "Symbol kind, such as Function, Method, Class, or Variable."
      },
      "path": {
        "type": "string",
        "description": "Path prefix to limit candidates."
      },
      "exact": {
        "type": "boolean",
        "description": "Require an exact symbol-name match. Default: true."
      },
      "limit": {
        "type": "integer",
        "description": "Maximum number of candidates. Default: 20."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "name"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "name": "serve",
  "kind": "Function",
  "exact": true
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "query": "serve",
  "status": "unique",
  "exact": true,
  "filters": {
    "kind": "Function"
  },
  "count": 1,
  "total_count": 1,
  "truncated": false,
  "limit": 20,
  "resolved": {
    "name": "serve",
    "kind_name": "Function",
    "source": "tree_sitter_source_index",
    "referencable": true,
    "signature": {
      "label": "serve(path = \".\")",
      "documentation": "",
      "parameters": [
        {
          "label": "path = \".\"",
          "name": "path",
          "default": "\".\""
        }
      ]
    },
    "leading_comment_lines": "1-29",
    "location": "R/serve.R:30:1-39:2"
  }
}
```

### `references`

Find Tree-sitter syntactic impact references for a package-local symbol.
The resolved declaration is reported separately.

#### Schema visible to the MCP client

``` json
{
  "name": "references",
  "description": "Find Tree-sitter syntactic impact references for a package-local symbol. The resolved declaration is reported separately.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "name": {
        "type": "string",
        "description": "Symbol name or query."
      },
      "kind": {
        "type": "string",
        "description": "Symbol kind, such as Function, Method, Class, or Variable."
      },
      "path": {
        "type": "string",
        "description": "Path prefix to limit candidates."
      },
      "exact": {
        "type": "boolean",
        "description": "Require an exact symbol-name match. Default: true."
      },
      "limit": {
        "type": "integer",
        "description": "Maximum number of references. Default: 100."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "name"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "name": "serve",
  "kind": "Function",
  "exact": true,
  "limit": 20
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "query": "serve",
  "status": "unique",
  "exact": true,
  "filters": {
    "kind": "Function"
  },
  "count": 4,
  "total_count": 4,
  "truncated": false,
  "limit": 20,
  "resolved": {
    "name": "serve",
    "kind_name": "Function",
    "source": "tree_sitter_source_index",
    "referencable": true,
    "signature": {
      "label": "serve(path = \".\")",
      "documentation": "",
      "parameters": [
        {
          "label": "path = \".\"",
          "name": "path",
          "default": "\".\""
        }
      ]
    },
    "leading_comment_lines": "1-29",
    "location": "R/serve.R:30:1-39:2"
  },
  "reference_semantics": "syntactic_impact_references",
  "binding_identity": "not_proven",
  "declaration": "resolved_only",
  "reference_scope": {
    "paths": [
      "R",
      "tests/testthat"
    ],
    "includes_tests": true
  },
  "references": [
    {
      "name": "serve",
      "source": "tree_sitter_source_index",
      "role": "namespace_call",
      "location": "tests/testthat/test-serve-tools.R:368:21-368:26"
    },
    {
      "name": "serve",
      "source": "tree_sitter_source_index",
      "role": "namespace_call",
      "location": "tests/testthat/test-serve-tools.R:386:21-386:26"
    },
    {
      "name": "serve",
      "source": "tree_sitter_source_index",
      "role": "namespace_call",
      "location": "tests/testthat/test-serve-tools.R:405:15-405:20"
    },
    {
      "name": "serve",
      "source": "tree_sitter_source_index",
      "role": "namespace_call",
      "location": "tests/testthat/test-serve-tools.R:413:15-413:20"
    }
  ]
}
```

### `function_formals`

Show a local function signature from package source, or report that
external/package-qualified signatures require help tools.

#### Schema visible to the MCP client

``` json
{
  "name": "function_formals",
  "description": "Show a local function signature from package source, or report that external/package-qualified signatures require help tools.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "name": {
        "type": "string",
        "description": "Function name, optionally qualified like 'stats::lm'."
      },
      "package": {
        "type": "string",
        "description": "Package used to identify an external function name."
      },
      "context_path": {
        "type": "string",
        "description": "Source file used to validate local path context."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "name"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "name": "serve"
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "query": "serve",
  "status": "unique",
  "source": "tree_sitter_source_index",
  "resolved": {
    "name": "serve",
    "kind_name": "Function",
    "source": "tree_sitter_source_index",
    "referencable": true,
    "signature": {
      "label": "serve(path = \".\")",
      "documentation": "",
      "parameters": [
        {
          "label": "path = \".\"",
          "name": "path",
          "default": "\".\""
        }
      ]
    },
    "leading_comment_lines": "1-29",
    "location": "R/serve.R:30:1-39:2"
  },
  "count": 1,
  "signatures": [
    {
      "label": "serve(path = \".\")",
      "documentation": "",
      "parameters": [
        {
          "label": "path = \".\"",
          "name": "path",
          "default": "\".\""
        }
      ]
    }
  ]
}
```

### `refactor_context`

Gather refactor-readiness context for a symbol: definition, references,
tests, and export status.

#### Schema visible to the MCP client

``` json
{
  "name": "refactor_context",
  "description": "Gather refactor-readiness context for a symbol: definition, references, tests, and export status.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "name": {
        "type": "string",
        "description": "Symbol name."
      },
      "kind": {
        "type": "string",
        "description": "Symbol kind, such as Function, Method, Class, or Variable."
      },
      "path": {
        "type": "string",
        "description": "Path prefix to limit candidates."
      },
      "exact": {
        "type": "boolean",
        "description": "Require an exact symbol-name match. Default: true."
      },
      "limit": {
        "type": "integer",
        "description": "Maximum number of references. Default: 100."
      },
      "package_root": {
        "type": "string",
        "description": "Absolute path to the target package root."
      }
    },
    "required": [
      "name"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "name": "serve",
  "kind": "Function",
  "exact": true,
  "limit": 20
}
```

#### Example response

``` json
{
  "package_root": "/path/to/rairai",
  "query": "serve",
  "status": "unique",
  "exact": true,
  "filters": {
    "kind": "Function"
  },
  "resolved": {
    "name": "serve",
    "kind_name": "Function",
    "source": "tree_sitter_source_index",
    "referencable": true,
    "signature": {
      "label": "serve(path = \".\")",
      "documentation": "",
      "parameters": [
        {
          "label": "path = \".\"",
          "name": "path",
          "default": "\".\""
        }
      ]
    },
    "leading_comment_lines": "1-29",
    "location": "R/serve.R:30:1-39:2"
  },
  "reference_count": 4,
  "reference_total_count": 4,
  "references_truncated": false,
  "reference_limit": 20,
  "references": [
    {
      "name": "serve",
      "source": "tree_sitter_source_index",
      "role": "namespace_call",
      "location": "tests/testthat/test-serve-tools.R:368:21-368:26"
    },
    {
      "name": "serve",
      "source": "tree_sitter_source_index",
      "role": "namespace_call",
      "location": "tests/testthat/test-serve-tools.R:386:21-386:26"
    },
    {
      "name": "serve",
      "source": "tree_sitter_source_index",
      "role": "namespace_call",
      "location": "tests/testthat/test-serve-tools.R:405:15-405:20"
    },
    {
      "name": "serve",
      "source": "tree_sitter_source_index",
      "role": "namespace_call",
      "location": "tests/testthat/test-serve-tools.R:413:15-413:20"
    }
  ],
  "reference_semantics": "syntactic_impact_references",
  "binding_identity": "not_proven",
  "declaration": "resolved_only",
  "reference_scope": {
    "paths": [
      "R",
      "tests/testthat"
    ],
    "includes_tests": true
  },
  "exported": true,
  "namespace": {
    "exported": true
  },
  "related_test_count": 12,
  "related_tests": [
    {
      "location": "tests/testthat/test-serve-tools.R:358-376",
      "desc": "serve uses explicit path without changing working directory",
      "hit_count": 2,
      "hits": [
        "tests/testthat/test-serve-tools.R:358",
        "tests/testthat/test-serve-tools.R:368"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:378-393",
      "desc": "serve defaults to the current working directory",
      "hit_count": 2,
      "hits": [
        "tests/testthat/test-serve-tools.R:378",
        "tests/testthat/test-serve-tools.R:386"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:395-400",
      "desc": "serve path normalization expands common path forms",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-serve-tools.R:395"
      ]
    },
    {
      "location": "tests/testthat/test-serve-tools.R:402-418",
      "desc": "serve path must be one existing directory",
      "hit_count": 4,
      "hits": [
        "tests/testthat/test-serve-tools.R:402",
        "tests/testthat/test-serve-tools.R:405",
        "tests/testthat/test-serve-tools.R:411",
        "tests/testthat/test-serve-tools.R:413"
      ]
    },
    {
      "location": "tests/testthat/test-tool-help.R:59-77",
      "desc": "help_page returns text and metadata for in-source topic",
      "hit_count": 3,
      "hits": [
        "tests/testthat/test-tool-help.R:60",
        "tests/testthat/test-tool-help.R:62",
        "tests/testthat/test-tool-help.R:67"
      ]
    },
    {
      "location": "tests/testthat/test-tool-help.R:175-179",
      "desc": "help_page does not truncate when max_chars exceeds text length",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-tool-help.R:176"
      ]
    },
    {
      "location": "tests/testthat/test-tool-help.R:219-227",
      "desc": "help_page treats NULL and empty package as unspecified",
      "hit_count": 2,
      "hits": [
        "tests/testthat/test-tool-help.R:220",
        "tests/testthat/test-tool-help.R:221"
      ]
    },
    {
      "location": "tests/testthat/test-tool-help.R:288-306",
      "desc": "help_search returns ranked scoped matches for a valid query",
      "hit_count": 3,
      "hits": [
        "tests/testthat/test-tool-help.R:289",
        "tests/testthat/test-tool-help.R:291",
        "tests/testthat/test-tool-help.R:302"
      ]
    },
    {
      "location": "tests/testthat/test-tool-package-docs.R:51-94",
      "desc": "package_docs returns a local documentation map",
      "hit_count": 2,
      "hits": [
        "tests/testthat/test-tool-package-docs.R:83",
        "tests/testthat/test-tool-package-docs.R:84"
      ]
    },
    {
      "location": "tests/testthat/test-tool-package-namespace.R:13-23",
      "desc": "read_package_namespace returns merged package-keyed imports",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-tool-package-namespace.R:16"
      ]
    },
    {
      "location": "tests/testthat/test-tool-package-overview.R:155-175",
      "desc": "package_overview exposes the namespace contract",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-tool-package-overview.R:170"
      ]
    },
    {
      "location": "tests/testthat/test-tool-package-overview.R:279-349",
      "desc": "package_overview exposes source object contracts and stable serve object",
      "hit_count": 1,
      "hits": [
        "tests/testthat/test-tool-package-overview.R:279"
      ]
    }
  ],
  "test_text_hit_count": 23,
  "test_text_hits": [
    "tests/testthat/test-serve-tools.R:358",
    "tests/testthat/test-serve-tools.R:368",
    "tests/testthat/test-serve-tools.R:378",
    "tests/testthat/test-serve-tools.R:386",
    "tests/testthat/test-serve-tools.R:395",
    "tests/testthat/test-serve-tools.R:402",
    "tests/testthat/test-serve-tools.R:405",
    "tests/testthat/test-serve-tools.R:411",
    "tests/testthat/test-serve-tools.R:413",
    "tests/testthat/test-tool-help.R:60",
    "tests/testthat/test-tool-help.R:62",
    "tests/testthat/test-tool-help.R:67",
    "tests/testthat/test-tool-help.R:176",
    "tests/testthat/test-tool-help.R:220",
    "tests/testthat/test-tool-help.R:221",
    "tests/testthat/test-tool-help.R:289",
    "tests/testthat/test-tool-help.R:291",
    "tests/testthat/test-tool-help.R:302",
    "tests/testthat/test-tool-package-docs.R:83",
    "tests/testthat/test-tool-package-docs.R:84",
    "tests/testthat/test-tool-package-namespace.R:16",
    "tests/testthat/test-tool-package-overview.R:170",
    "tests/testthat/test-tool-package-overview.R:279"
  ],
  "next_tools": [
    {
      "tool": "definition",
      "reason": "Inspect the resolved definition before editing source."
    },
    {
      "tool": "references",
      "reason": "Review call sites and dependent code before refactoring."
    },
    {
      "tool": "package_namespace",
      "reason": "Check whether the symbol is part of the exported API."
    },
    {
      "tool": "tests_for",
      "reason": "Inspect related tests before changing behavior."
    }
  ]
}
```

``` r

emit_tool_group("CRAN metadata tools", cran_tools)
```

## CRAN metadata tools

### `cran_search`

Search CRAN packages by name, title, description, author, and
maintainer. Use cran_package for exact package metadata.

#### Schema visible to the MCP client

``` json
{
  "name": "cran_search",
  "description": "Search CRAN packages by name, title, description, author, and maintainer. Use cran_package for exact package metadata.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": {
        "type": "string",
        "description": "Search query."
      },
      "n_results": {
        "type": "integer",
        "description": "Maximum number of packages. Default: 20; maximum: 50."
      }
    },
    "required": [
      "query"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "query": "static analysis",
  "n_results": 5
}
```

Network example not run during rendering; no response is shown.

### `cran_package`

Look up CRAN metadata for one exact package name.

#### Schema visible to the MCP client

``` json
{
  "name": "cran_package",
  "description": "Look up CRAN metadata for one exact package name.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "package_name": {
        "type": "string",
        "description": "Exact package name."
      }
    },
    "required": [
      "package_name"
    ],
    "additionalProperties": false
  }
}
```

#### Example input

``` json
{
  "package_name": "treesitter"
}
```

Network example not run during rendering; no response is shown.

### Regenerating the content

Because this vignette evaluates the current package code, refreshing it
is just a normal vignette render. If a tool schema changes, if a tool
response shape changes, or if the package gains new tools, the generated
sections above should reflect those changes after updating the
`example_args` list as needed.
