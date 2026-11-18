daft_theme <- list(
  ".daft-file" = list(
    color = "cyan",
    "font-weight" = "bold"
  ),
  ".daft-output" = list(
    color = "green",
    "font-weight" = "bold"
  ),
  ".daft-input" = list(
    color = "blue",
    "font-weight" = "bold"
  ),
  ".daft-muted" = list(
    color = "grey"
  )
)


#' @importFrom dplyr .data
#' @importFrom cli cli_text cli_alert_warning cli_div cli_rule cli_end symbol
.show_one_file <- function(file, file_df = NULL) {
  cli_div(theme = daft_theme)
  query_file <- file

  if(is.null(file_df)) {
    file_df <- daft_files()
  }

  if(file.exists(file)) {
    mark <- cli::col_green(cli::symbol$tick)
  } else {
    mark <- cli::col_red(cli::symbol$cross)
  }

  if(file %in% file_df$script) {
    .print_script_file(file)
    return(invisible(NULL))
  }

  df <- file_df |>
      filter(file == query_file)

  cli_rule(left = "{.strong File: {.file {file}} {mark}}")
  d0 <- cli_div(theme = list(div = list("margin-left" = 2)))

  if(nrow(df) == 0) {
    cli_alert_warning("This file is not used as input or produced as output in any script.\n")
    return(invisible(NULL))
  }

  input_rows  <- filter(df, .data[["type"]] == "input")
  output_rows <- filter(df, .data[["type"]] == "output")

  w <- options()$width - 6

  # show which files produce the file as output
  if(nrow(output_rows) > 1) {
    cli_warning("This file is produced as output in multiple scripts\n")
  } 

  if(nrow(output_rows) > 0) {
    cli_text("{.daft-output {symbol$arrow_left} Produced as output in:}")

    for(i in seq_len(nrow(output_rows))) {
      script <- output_rows$script[i]
      description <- output_rows$description[i]

      cli_text("- {.file {script}}:")
      d <- cli_div(theme = list(div = list("margin-left" = 2)))
      cli_text("{.description {description}}")
      cli_end(d)
    }
  } else {
    cli_text("{.daft-muted Not produced as output in any script.}")
  }

  # show in which scripts file is used as input
  if(nrow(input_rows) > 0) {
    cli_text("{.daft-input {symbol$arrow_right} Used as input in:}")
    for(i in seq_len(nrow(input_rows))) {
      script <- input_rows$script[i]
      description <- input_rows$description[i]
      cli_text("- {.file {script}}:")
      d <- cli_div(theme = list(div = list("margin-left" = 2)))
      cli_text("{.description {description}}")
      cli_end(d)
    }
  } else {
    cli_text("{.emph Not used as input in any script.}")
  }

}

.print_script_file <- function(script_file) {
  cli_rule(left = "{.strong Script: {.file {script_file}}}")
  header <- daft_get_header(script_file)

  if(is.null(header)) {
    cli_alert_warning("No YAML header found in script file: {.file {script_file}}")
    return(invisible(NULL))
  }

  if(!is.null(header$description)) {
    cli_text("{.field Description:}")
    cli_div(theme = list(div = list("margin-left" = 2)))
    cli_text("{header$description}")
    cli_end()
  }

  if(!is.null(header$params)) {
    cli_text("{.field Parameters:}")
    cli_div(theme = list(div = list("margin-left" = 2)))
    for(param in names(header$params)) {
      value <- header$params[[param]]
      if(is.list(value) && !is.null(value$value)) {
        value <- value$value
      }
      cli_text("  - {.var {param}}: {value}")
    }
    cli_end()
  }
  if(!is.null(header$inputs)) {
    cli_text("{.field Inputs:}")
    cli_div(theme = list(div = list("margin-left" = 2)))
    for(input in header$inputs) {
      if(file.exists(input$file)) {
        mark <- cli::col_green(cli::symbol$tick)
      } else {
        mark <- cli::col_red(cli::symbol$cross)
      }
      cli_text("  - {mark} {.file {input$file}}: {input$description}")
    }
    cli_end()
  }
  if(!is.null(header$outputs)) {
    cli_text("{.field Outputs:}")
    cli_div(theme = list(div = list("margin-left" = 2)))
    for(output in header$outputs) {
      if(file.exists(output$file)) {
        mark <- cli::col_green(cli::symbol$tick)
      } else {
        mark <- cli::col_red(cli::symbol$cross)
      }

      cli_text("  - {mark} {.file {output$file}}: {output$description}")
    }
    cli_end()
  }
}
