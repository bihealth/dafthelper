# make sure that the file is a list with file and description fields. If
# the file is a character vector, convert it to a list with file and
# description fields. If the file is not a character vector or a list with
# file and description fields, return NULL and print a warning message.
.normalize_file <- function(x, script_file) {

  if(is.character(x)) {
    ret <- list(file = x, description = "")
  } else if(is.list(x) && !is.null(x$file) && !is.null(x$description)) {
    ret <- x
  } else {
    warning("Invalid yaml header format in script file: ", script_file)
    return(NULL)
  }

  if(file.exists(ret$file)) {
    ret$mtime <- file.info(ret$file)$mtime
  } else {
    ret$mtime <- NA
  }

  ret
}


#' Get the YAML header from a script file
#'
#' Given a script file, this function reads the file and extracts the YAML header
#' from the comments at the top of the file. The YAML header is expected to be
#' in the format of comments starting with "#: " followed by YAML content. The function
#' returns a list containing the parsed YAML content.
#' @param file The path to the script file from which to extract the YAML header.
#' @param quiet If TRUE, suppress warnings when no YAML header is found.
#' @return A list containing the parsed YAML content from the header.
#' If no YAML header is found, returns NULL.
#' @importFrom yaml yaml.load
#' @importFrom stringr str_subset str_replace
#' @export
daft_get_header <- function(file, quiet=TRUE) {
  # read the file, select only lines that start with "#: ", remove the "#:
  # " prefix, and parse the resulting YAML into a list
  yaml_lines <- readLines(file) |>
    str_subset("^#:\\s") |>
    str_replace("^#:\\s", "")

  if(length(yaml_lines) == 0 && !quiet) {
    warning("No YAML header found in file: ", file)
  }
  ret <- yaml::yaml.load(paste(yaml_lines, collapse = "\n"))

  if(length(ret$outputs) == 0L) {
    ret$outputs <- NULL
  }

  # make sure that each element of the outputs has a file and description
  # field. If the outputs is a character vector (with file names), convert
  # it to a list of lists with file and description fields
  if(!is.null(ret$outputs)) {
    ret$outputs <- lapply(ret$outputs, function(x) {
      .normalize_file(x, file)
    })
  }

  if(length(ret$inputs) == 0L) {
    ret$inputs <- NULL
  }

  if(!is.null(ret$inputs)) {
    ret$inputs <- lapply(ret$inputs, function(x) {
      .normalize_file(x, file)
    })
  }

  ret
}


#' Report potential script files that are missing YAML headers
#'
#' This function scans the specified directory for R script files and checks
#' for the presence of YAML headers in each file. It returns a character vector
#' containing the paths of script files that are missing YAML headers.
#' @param dir The directory to scan for R script files. Defaults to the current working
#' directory.
#' @param pattern A regular expression pattern to match R script files.
#' @param recursive If TRUE, scan subdirectories recursively.
#' @return A character vector containing the paths of script files that are missing YAML headers.
#' @export
daft_missing_headers <- function(dir=".", pattern="\\.R$", recursive=TRUE) {
  files <- list.files(dir, pattern=pattern, recursive=recursive, full.names=TRUE)
  missing <- c()
  for(file in files) {
    header <- daft_get_header(file, quiet=TRUE)
    if(is.null(header)) {
      missing <- c(missing, file)
    }
  }
  missing
}

.get_all_yaml_headers <- function(dir=".", pattern="\\.R$", recursive=TRUE) {
  files <- list.files(dir, pattern=pattern, recursive=recursive, full.names=TRUE)
  files <- gsub("^\\./", "", files)  # remove leading "./" from file paths
  headers <- lapply(files, daft_get_header)
  names(headers) <- files
  headers <- headers[!sapply(headers, is.null)]
  headers
}

# This function returns a tibble with the following columns:
# - file: the output file name
# - description: the description of the output file
# - script: the script that produces the output file

#' Find all output or input files specified in the R script files
#'
#' Scan the specified directory for R script files, parse the yaml headers
#' and extract the output and/or input files.
#' 
#' * **daft_files** searches for both input and output files.
#' * **daft_output_files** searches for output files only.
#' * **daft_input_files** searches for input files only.
#' @param dir The directory to scan for R script files.
#' @param pattern A regular expression pattern to match R script files.
#' @param recursive If TRUE, scan subdirectories recursively.
#' @return A tibble with the following columns:
#' - file: the output or input file name
#' - description: the description of the output or input file
#' - script: the script that produces or uses the output or input file
#' @importFrom tibble tibble
#' @importFrom purrr imap_dfr
#' @export
daft_output_files <- function(dir=".", pattern="\\.R$", recursive=TRUE) {
  headers <- .get_all_yaml_headers(dir, pattern, recursive)

  if(length(headers) == 0L) {
    return(tibble(file=character(0), description=character(0), script=character(0)))
  }

  outputs <- imap_dfr(headers, \(header, script) {
    if(!is.null(header$outputs)) {
      tibble(
        file = sapply(header$outputs, \(x) x$file),
        description = sapply(header$outputs, \(x) x$description),
        script = script
      )
    } else {
      tibble(file=character(0), description=character(0), script=character(0))
    }
  })
  outputs
}

#' @rdname daft_output_files
#' @export
daft_input_files <- function(dir=".", pattern="\\.R$", recursive=TRUE) {
  headers <- .get_all_yaml_headers(dir, pattern, recursive)
  if(length(headers) == 0L) {
    return(tibble(file=character(0), description=character(0), script=character(0)))
  }
  inputs <- imap_dfr(headers, \(header, script) {
    if(!is.null(header$inputs)) {
      tibble(
        file = sapply(header$inputs, \(x) x$file),
        description = sapply(header$inputs, \(x) x$description),
        script = script
      )
    } else {
      tibble(file=character(0), description=character(0), script=character(0))
    }
  })
  inputs
}

#' @rdname daft_output_files
#' @importFrom dplyr bind_rows mutate
#' @export
daft_files <- function(dir=".", pattern="\\.R$", recursive=TRUE) {
  inputs <- daft_input_files(dir, pattern, recursive) |>
    mutate(type = "input")
  outputs <- daft_output_files(dir, pattern, recursive) |>
    mutate(type = "output")
  bind_rows(
    inputs |> mutate(type = "input"),
    outputs |> mutate(type = "output")
  )
}

# given an input or output file, find where this file is used or produced

#' Explain where a file is used or produced in the scripts
#'
#' Search for a file name in the input and output files and scripts. Show
#' how the file is produced or used in the scripts. If the file is a
#' script, show which files it produces and which files it uses.
#' @param file The file name to search for.
#' @return NULL. Prints the results to the console.
#' @importFrom dplyr filter mutate
#' @importFrom cli cli_text cli_alert_warning
#' @export
daft_show <- function(file) {

  file_df <- daft_files()
  
  for(f in file) {
    .show_one_file(f, file_df)
  }

}



#' Find all files which match a pattern
#'
#' Find all files which match a pattern in the input or output files and scripts.
#'
#' This function searches for files that match a specified pattern in the input and output files of scripts. It prints information about each matching file, including which scripts produce or use the file.
#' @param pattern A regular expression pattern to match file names.
#' @return A tibble containing the matching files and their associated
#' scripts, or NULL if no matches are found. The function also prints
#' information about each matching file to the console.
#' @importFrom dplyr filter mutate .data
#' @importFrom cli cli_text cli_alert_warning
#' @export
daft_find <- function(pattern) {
  df <- daft_files() |>
    filter(grepl(pattern, file))
  for(f in unique(df$file)) {
    .show_one_file(f, df)
  }
  return(invisible(df))
}
