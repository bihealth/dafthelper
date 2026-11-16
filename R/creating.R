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
      x
    })
    outputs <- lapply(outputs, function(x) {
      x$file <- file.path(dir, x$file)
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


# create a new script. Create an empty text file in the specified directory
# with the specified name. Add a template YAML header to the file. If the
# file already exists, do not overwrite it, but print a message to the console.

#' Create a new script file with a YAML header
#' Create a new script file in the specified directory with the specified name.
#' Add a template YAML header to the file. If the file already exists, it
#' will not be overwritten. If all parameters are NULL, an example YAML header will be created.
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
daft_make_script <- function(file,
                             dir = ".",
                             comment = NULL,
                             description = NULL,
                             params = NULL,
                             inputs = NULL,
                             outputs = NULL) {

  script_file <- file.path(dir, file)

  # the user might have included the path in the file name.
  dir <- dirname(script_file)

  if(file.exists(script_file)) {
    cli_alert_warning("Script file already exists: {.file {script_file}}, not overwriting.")
    return(invisible(NULL))
  }

  if(is.null(description) && is.null(params) && is.null(inputs) && is.null(outputs)) {
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

  comment <- header$comment %||% ""
  header$comment <- NULL  # remove comment from header before converting to YAML

  header_yaml <- yaml::as.yaml(header)
  # put a "#: " at the beginning of each line
  header_yaml <- paste0("#: ", unlist(strsplit(header_yaml, "\n")))
  header_text <- paste("#: # ------------ YAML HEADER ------------ # :#", 
    paste0("#: # ", header$comment, collapse = "\n"),
    header_yaml, 
    "#: # For use with the dafthelper package. See ?dafthelper::daft_make_script for details.",
    "#: # ------------------------------ # :#\n\n\n", sep = "\n")

  writeLines(header_text, con = script_file)
  cli_alert_success("Created new script file: {.file {script_file}}")
  invisible(NULL)
}

#' Create a new daft project directory structure
#'
#' Create a new directory structure for a daft project. The default
#' structure includes subdirectories for data, analysis, figures, and text.
#' @param format A string specifying the format of the subdirectory names.
#'        Evaluated by `glue`. The default is "{number}_{name}".
#' @param dir The directory in which to create the subdirectories. Defaults
#'        to the current working directory.
#' @param names A character vector of names for the subdirectories. The
#' default is `data`, `analysis`, `figures`, and `text`.
#' @param number_format A string specifying the format of the subdirectory.
#'       Evaluated by `sprintf`. The default is "%02d", which will create
#'       subdirectory names like "01_data", "02_analysis", etc.
#' @return NULL. The function creates the specified subdirectories in the specified directory.
#' @export
#' @importFrom glue glue
use_daft <- function(format = "{number}_{name}", dir = ".",
                     number_format = "%02d",
                     names = c("data", "analysis", "figures", "text")) {
  if(!dir.exists(dir)) {
    cli_alert_success("Created directory: {.file {dir}}")
    dir.create(dir, recursive = TRUE)
  }

  for(i in seq_along(names)) {
    number <- sprintf(number_format, i)
    subdir <- glue::glue(format, number = number, name = names[i])
    subdir_path <- file.path(dir, subdir)
    if(!dir.exists(subdir_path)) {
      dir.create(subdir_path)
      cli_alert_success("Created subdirectory: {.file {subdir_path}}")
    } else {
      cli_alert_warning("Subdirectory already exists: {.file {subdir_path}}")
    }
  }

  invisible(NULL)
}
