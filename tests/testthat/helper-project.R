# Each test gets its own project and restores the working directory afterwards.
local_project <- function(.local_envir = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_dir(path, .local_envir = .local_envir)
  withr::local_options(
    list(cli.num_colors = 0, cli.unicode = FALSE, width = 120),
    .local_envir = .local_envir
  )
  invisible(path)
}

write_header <- function(file, header) {
  lines <- strsplit(yaml::as.yaml(header), "\n", fixed = TRUE)[[1]]
  writeLines(c(paste0("#: ", lines), "", "invisible(NULL)"), file)
  invisible(file)
}

make_project <- function() {
  dir.create("analysis")
  dir.create("analysis/nested")
  write_header("analysis/prepare.R", list(
    description = "Prepare measurements",
    params = list(threshold = 0.5, method = list(value = "median", description = "Summary")),
    inputs = list(list(file = "raw.csv", description = "Raw measurements")),
    outputs = list(list(file = "clean.csv", description = "Clean measurements"))
  ))
  write_header("analysis/nested/plot.R", list(
    description = "Plot measurements",
    inputs = list("clean.csv"),
    outputs = list(list(file = "figure.pdf", description = "Summary plot"))
  ))
  write_header("analysis/other.r", list(outputs = list("other.txt")))
  write_header("analysis/metadata.R", list(description = "Metadata only"))
  writeLines("x <- 1", "analysis/plain.R")
  writeLines("x <- 2", "analysis/nested/plain.R")
  writeLines("notes", "analysis/notes.txt")
}

console_text <- function(code) {
  paste(testthat::capture_messages(code), collapse = "\n")
}
