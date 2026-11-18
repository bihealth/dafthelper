.mock_script_yaml <- function(dir = ".") {
  # generate a mock YAML header for a script file.

  description <- "Description of the script"
  comment <- c("This is a comment for the YAML header.",
               "It can be used to provide additional information about the script.")
  params <- list(
    param1 = list(value = "value1", description = "Description of param1"),
    param2 = list(value = "value2", description = "Description of param2")
  )
  inputs <- list(
    list(file = "input_file1.rds", description = "Description of input_file1"),
    list(file = "input_file2.csv", description = "Description of input_file2")
  )
  outputs <- list(
    list(file = "output_file1.csv", description = "Description of output_file1"),
    list(file = "output_file2.rds", description = "Description of output_file2")
  )

  if(dir != ".") {
    inputs <- lapply(inputs, function(x) {
      x$file <- file.path(dir, x$file)
      x$file <- gsub("^\\./", "", x$file)
      x
    })
    outputs <- lapply(outputs, function(x) {
      x$file <- file.path(dir, x$file)
      x$file <- gsub("^\\./", "", x$file)
      x
    })
  }
 
  header <- list(
    comment = comment,
    description = description,
    params = params,
    inputs = inputs,
    outputs = outputs
  )
  header
}

# normalize description such that if it is too long, it gets formatted as a
# para
.normalize_desc <- function(desc) {
  desc <- paste(as.character(desc), collapse = "\n")

  desc <- paste(strwrap(desc, width = 60),
                              collapse = "\n")

  desc
}

# normalize the header to ensure that it has the correct structure.
.normalize_header <- function(header) {

  # comment is a character vector.
  if(!is.null(header$comment)) {
    header$comment <- .normalize_desc(header$comment)
  } else {
    header$comment <- ""
  }

  # description likewise
  if(!is.null(header$description)) {
    header$description <- .normalize_desc(header$description)
  }

  if(!is.null(header$params)) {
    header$params <- lapply(header$params, function(x) {
      if(is.list(x) && !is.null(x$value)) {
        x$value <- as.character(x$value)
      }
      x
    })
  }

  # ok, maybe we should have afunction for that...
  if(!is.null(header$inputs)) {
    header$inputs <- lapply(header$inputs, function(x) {
      if(is.list(x)) { 
        if(is.null(x$file)) {
          cli_abort("Input file is missing 'file' field in header: {.val {x}}")
        }
        x$file <- as.character(x$file)
        if(is.null(x$description)) {
          x$description <- "no description"
        }
        x$description <- .normalize_desc(x$description)
      } else {
        x <- list(file = as.character(x), description = "no description")
      }
      x
    })
  }

  if(!is.null(header$outputs)) {
    header$outputs <- lapply(header$outputs, function(x) {
      if(is.list(x)) { 
        if(is.null(x$file)) {
          cli_abort("Output file is missing 'file' field in header: {.val {x}}")
        }
        x$file <- as.character(x$file)
        if(is.null(x$description)) {
          x$description <- "no description"
        }
        x$description <- .normalize_desc(x$description)
      } else {
        x <- list(file = as.character(x), description = "no description")
      }
      x
    })
  }
  header
}


# create a new script. Create an empty text file in the specified directory
# with the specified name. Add a template YAML header to the file. If the
# file already exists, do not overwrite it, but print a message to the console.

#' Create a new script file with a YAML header
#'
#' Create a new script file in the specified directory with the specified name.
#' Add a template YAML header to the file. If the file already exists, it
#' will not be overwritten. If all parameters are NULL, an example YAML header will be created.
#'
#' Please note that when the default YAML header is created, the input and output
#' files inserted into the header have paths relative to the current
#' directory. So for example if you created the DAFT structure in directory
#' `foo` (e.g. with `use_daft(dir = "foo")`, and you call
#' `daft_new_script()` without changing to `foo` first, the input and
#' output files will have the paths like `foo/01_data/input_file1.rds`.
#' This may not be what you want if you plan on calling the script from
#' within the `foo` directory.
#' 
#' If you specify the input and output files via parameters, the file
#' paths are used as is.
#' @param file The name of the script file to create.
#' @param dir The directory in which to create the script file. Defaults to the current
#' working directory. For example, use "01_analyses" to create the script
#' in the "01_analyses" subdirectory.
#' @param comment An optional comment to include in the YAML header.
#'        Character vector with one element for each line of the comment.
#' @param description A description of the script.
#' @param params A list of parameters for the script.
#' @param inputs A list of input files for the script. Each input file should be
#'        specified as a list with two elements: "file" (the name of the input
#' file) and "description" (a description of the input file).
#' @param outputs A list of output files for the script. Each output file should be
#'       specified as a list with two elements: "file" (the name of the output
#' file) and "description" (a description of the output file).
#' @return NULL. The function creates a new script file with the specified name and
#' YAML header.
#' @importFrom cli cli_alert_success cli_alert_warning
#' @export
daft_new_script <- function(file,
                             dir = ".",
                             description = NULL,
                             comment = NULL,
                             params = NULL,
                             inputs = NULL,
                             outputs = NULL) {

  script_file <- file.path(dir, file)

  # the user might have included the path in the file name.
  dir <- dirname(script_file)

  if(!dir.exists(dir)) {
    cli_abort("Directory does not exist: {.file {dir}}. Please create it first.")
  }

  if(file.exists(script_file)) {
    cli_alert_warning("Script file already exists: {.file {script_file}}, not overwriting.")
    return(invisible(NULL))
  }

  if(is.null(comment) &&
     is.null(description) &&
     is.null(params) &&
     is.null(inputs) &&
     is.null(outputs)) {
    header <- .mock_script_yaml(dir)
  } else {
    header <- list(
      comment = comment,
      description = description,
      params = params,
      inputs = inputs,
      outputs = outputs
    )
  }

  header <- .normalize_header(header)

  comment <- header$comment %||% ""
  header$comment <- NULL  # remove comment from header before converting to YAML

  header_yaml <- yaml::as.yaml(header)
  # put a "#: " at the beginning of each line
  header_yaml <- paste0("#: ", unlist(strsplit(header_yaml, "\n")), collapse = "\n")
  header_text <- paste("#: # --", 
    paste0("#: # ", comment, collapse = "\n"),
    header_yaml, 
    "#: # For use with the dafthelper package.",
    "#: # See ?dafthelper::daft_make_script for details.",
    "#: # --\n\n\n", sep = "\n")

  writeLines(header_text, con = script_file)
  cli_alert_success("Created new script file: {.file {script_file}}")
  invisible(NULL)
}

#' Create a new daft project directory structure
#'
#' Create a new directory structure for a daft project. The default
#' structure includes subdirectories for data, analysis, figures, and text.
#' @param format A string specifying the format of the subdirectory names.
#'        Evaluated by `glue`. The default is `"{number}_{name}"`.
#' @param dir The directory in which to create the subdirectories. Defaults
#'        to the current working directory.
#' @param scheme "daft", "mdaft", "dat" or "custom". If "custom", you need to
#'        provide a vector of names for the subdirectories with the `names`
#'        argument. The default is "daft" (`data`, `analysis`, `figures`,
#'        and `text`); "mdaft" adds a `meta` subdirectory; `dat` is a
#'        shortened version with only `data`, `analysis`, and `text`.
#' @param number_format A string specifying the format of the subdirectory.
#'       Evaluated by `sprintf`. The default is "%02d", which will create
#'       subdirectory names like "01_data", "02_analysis", etc.
#' @param start_with An integer specifying the starting number for the
#'       subdirectory names. The default is 1.
#' @return NULL. The function creates the specified subdirectories in the specified directory.
#' @export
#' @importFrom glue glue
use_daft <- function(dir = ".",
                     format = "{number}_{name}",
                     number_format = "%02d",
                     start_with = 1,
                     scheme = c("daft", "mdaft", "dat", "custom"),
                     names = NULL
                     ) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cli_alert_success("Created directory: {.file {dir}}")
  }

  scheme <- match.arg(scheme)
  if(scheme == "daft") {
    names <- c("data", "analysis", "figures", "text")
  } else if(scheme == "mdaft") {
    names <- c("meta", "data", "analysis", "figures", "text")
  } else if(scheme == "dat") {
    names <- c("data", "analysis", "text")
  } else if(scheme == "custom") {
    if(is.null(names)) {
      stop("For scheme = 'custom', you must provide a vector of names for the subdirectories.")
    }
  } else {
    cli_abort("Invalid scheme: {.val {scheme}}. Must be one of 'daft', 'mdaft', or 'custom'.")
  }

  for(i in seq_along(names)) {
    number <- i + start_with - 1
    number <- sprintf(number_format, number)
    subdir <- glue::glue(format, number = number, name = names[i])
    subdir_path <- file.path(dir, subdir)
    if(!dir.exists(subdir_path)) {
      dir.create(subdir_path)
      cli_alert_success("Created subdirectory: {.file {subdir_path}}")
    } else {
      cli_alert_warning("Subdirectory already exists: {.file {subdir_path}}")
    }
  }

  if(file.exists(file.path(dir, "README.md"))) {
    cli_alert_warning("README.md file already exists, not overwriting.")
  } else {
    # copy the README_project_template.md from extdata
    # to the current directory and rename it to README.md
    template_path <- system.file("templates", "README_project_template.md",
                                 package = "dafthelper", mustWork = TRUE)
    file.copy(template_path, file.path(dir, "README.md"))
    cli_alert_success("Created README.md file. Please edit it.")
  }

  invisible(NULL)
}
