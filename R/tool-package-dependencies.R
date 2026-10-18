read_package_dependencies <- function(depth = 1L, root_path = ".") {
  depth <- normalize_limit_value(depth, default = 1L)
  dependency_types <- c(
    "Depends",
    "Imports",
    "Suggests",
    "Enhances",
    "LinkingTo"
  )

  description_result <- read_package_description(root_path = root_path)
  if (!is.null(description_result$error)) {
    return(tool_error_response(error = description_result$error))
  }

  desc <- description_result$description
  direct_dependency_records <- dependency_types |>
    set_names(str_to_lower(dependency_types)) |>
    map(\(dependency_type) {
      parse_description_dependencies(desc_field(desc, dependency_type))
    })

  response <- list(
    dependencies = map(
      direct_dependency_records,
      format_dependency_entries
    ),
    remotes = parse_remote_dependencies(desc_field(desc, "Remotes")),
    config_needs = parse_config_needs(desc)
  )

  if (depth > 1L) {
    if (!has_resolvable_dependencies(direct_dependency_records)) {
      response$indirect_dependencies <- empty_indirect_dependencies(
        direct_dependency_records
      )
    } else {
      metadata_result <- read_dependency_metadata()
      if (!is.null(metadata_result$error)) {
        response$indirect_dependencies <- empty_indirect_dependencies(
          direct_dependency_records
        )
        response$dependency_metadata_error <- metadata_result$error
      } else {
        response$indirect_dependencies <- imap(
          direct_dependency_records,
          \(direct_dependencies, dependency_type) {
            resolve_indirect_dependencies(
              direct_dependencies,
              dependency_type = dependency_type,
              depth = depth,
              db = metadata_result$metadata
            )
          }
        )
      }
    }
  }

  tool_value_response(!!!response)
}

parse_description_dependencies <- function(field_value) {
  if (is.na(field_value) || !nzchar(str_trim(field_value))) {
    return(list())
  }

  field_value |>
    str_split_1(",") |>
    str_trim() |>
    discard(~ !nzchar(.x)) |>
    map(parse_dependency_entry)
}

parse_dependency_entry <- function(entry) {
  parts <- str_match(entry, "^([^()]+?)(?:\\(([^)]+)\\))?$")
  version_requirement <- parts[[3L]]

  list(
    package = str_trim(parts[[2L]]),
    version_requirement = if (!is.na(version_requirement)) {
      str_trim(version_requirement)
    }
  )
}

format_dependency_entries <- function(entries) {
  entries |>
    map_chr(format_dependency_entry) |>
    as.list()
}

format_dependency_entry <- function(entry) {
  if (is.null(entry$version_requirement)) {
    return(entry$package)
  }

  glue::glue("{entry$package} ({entry$version_requirement})") |>
    as.character()
}

desc_field <- function(desc, field) {
  if (!field %in% colnames(desc)) {
    return(NA_character_)
  }

  desc[1, field]
}

parse_remote_dependencies <- function(field_value) {
  if (is.na(field_value) || !nzchar(str_trim(field_value))) {
    return(list())
  }

  field_value |>
    str_split_1(",") |>
    str_trim() |>
    discard(~ !nzchar(.x)) |>
    map(\(remote) {
      list(
        remote = remote,
        package = parse_remote_package_name(remote)
      )
    })
}

parse_remote_package_name <- function(remote) {
  remote |>
    str_remove("@.*$") |>
    str_remove("#.*$") |>
    str_split_1("/") |>
    pluck(-1)
}

parse_config_needs <- function(desc) {
  config_needs_fields <- colnames(desc) |>
    str_subset("^Config/Needs/")

  if (length(config_needs_fields) == 0L) {
    return(list())
  }

  config_needs_fields |>
    set_names(str_remove(
      config_needs_fields,
      "^Config/Needs/"
    )) |>
    map(\(field) {
      parse_config_needs_packages(desc_field(desc, field))
    })
}

parse_config_needs_packages <- function(field_value) {
  if (is.na(field_value) || !nzchar(str_trim(field_value))) {
    return(list())
  }

  field_value |>
    str_split_1(",") |>
    str_trim() |>
    discard(~ !nzchar(.x)) |>
    as.list()
}

empty_indirect_dependencies <- function(direct_dependency_records) {
  direct_dependency_records |>
    map(\(direct_dependencies) list())
}

has_resolvable_dependencies <- function(direct_dependency_records) {
  direct_dependency_records |>
    list_flatten() |>
    map_chr("package") |>
    setdiff("R") |>
    length() >
    0L
}

dependency_metadata <- function() {
  available.packages()
}

read_dependency_metadata <- function() {
  tryCatch(
    list(metadata = dependency_metadata(), error = NULL),
    error = function(cnd) {
      list(
        metadata = NULL,
        error = list(
          code = "dependency_metadata_unavailable",
          message = paste(
            "Indirect dependencies could not be resolved because dependency",
            "metadata is unavailable."
          ),
          detail = conditionMessage(cnd)
        )
      )
    }
  )
}

resolve_indirect_dependencies <- function(
  direct_dependencies,
  dependency_type,
  depth,
  db
) {
  if (depth <= 1L || length(direct_dependencies) == 0L) {
    return(list())
  }

  direct_names <-
    direct_dependencies |>
    map_chr("package") |>
    setdiff("R")

  if (length(direct_names) == 0L) {
    return(list())
  }

  indirect_dependencies <- tools::package_dependencies(
    direct_names,
    db = db,
    which = dependency_type_description_name(dependency_type),
    recursive = depth > 2L
  )

  if (length(indirect_dependencies) == 0L) {
    return(list())
  }

  indirect_dependencies |>
    map(as.list)
}

dependency_type_description_name <- function(dependency_type) {
  dependency_type |>
    str_replace("^depends$", "Depends") |>
    str_replace("^imports$", "Imports") |>
    str_replace("^suggests$", "Suggests") |>
    str_replace("^enhances$", "Enhances") |>
    str_replace("^linkingto$", "LinkingTo")
}

tool_package_dependencies <- function(service) {
  mcplite::tool(
    function(depth = 1L, package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$package_dependencies(depth = depth)
        }
      )
    },
    name = "package_dependencies",
    description = "Read dependency fields from DESCRIPTION.",
    arguments = list(
      depth = mcplite::type_integer(
        "Dependency depth. Use 1 for direct dependencies only. Default: 1.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}
