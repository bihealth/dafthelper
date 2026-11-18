



daft_update <- function(file, description = NULL, params = NULL, inputs = NULL, outputs = NULL) {
  header <- daft_get_header(file)

  if(is.null(header)) {
    cli_alert_warning("No YAML header found in script file: {.file {file}}")
    return(invisible(NULL))
  }

  if(!is.null(description)) {
    header$description <- description
  }
  if(!is.null(params)) {
    header$params <- params
  }
  if(!is.null(inputs)) {
    header$inputs <- inputs
  }
  if(!is.null(outputs)) {
    header$outputs <- outputs
  }

  .write_yaml_header(file, header)
}
