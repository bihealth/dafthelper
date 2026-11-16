test_that("headers parse metadata and normalize both file representations", {
  local_project()
  writeLines("data", "existing.csv")
  writeLines(c(
    "# Ordinary comment", "#: description: Example analysis",
    "#: params:", "#:   threshold: 0.5",
    "#: inputs:", "#:   - existing.csv",
    "#: outputs:", "#:   - file: missing.pdf", "#:     description: A plot",
    "x <- 1", "# description: ignored", "  #: ignored: true"
  ), "script.R")

  header <- daft_get_header("script.R")
  expect_identical(header$description, "Example analysis")
  expect_identical(header$params, list(threshold = 0.5))
  expect_named(header, c("description", "params", "inputs", "outputs"))
  expect_identical(header$inputs[[1]]$file, "existing.csv")
  expect_identical(header$inputs[[1]]$description, "")
  expect_equal(header$inputs[[1]]$mtime, file.info("existing.csv")$mtime)
  expect_identical(header$outputs[[1]], list(
    file = "missing.pdf", description = "A plot", mtime = NA
  ))
})

test_that("absent headers are quiet by default and can emit a warning", {
  local_project()
  for (lines in list(character(), c("# A comment", "x <- 1"), "#:no-space")) {
    writeLines(lines, "plain.R")
    expect_no_warning(expect_null(daft_get_header("plain.R")))
    expect_warning(expect_null(daft_get_header("plain.R", quiet = FALSE)),
                   "No YAML header found in file: plain.R", fixed = TRUE)
  }
})

test_that("omitted, null and empty input/output sections are normalized", {
  local_project()
  for (section in c("", "#: inputs: null\n#: outputs: null",
                    "#: inputs: []\n#: outputs: []")) {
    writeLines(c("#: description: Metadata only", section), "script.R")
    header <- daft_get_header("script.R")
    expect_identical(header$description, "Metadata only")
    expect_null(header$inputs)
    expect_null(header$outputs)
  }
})

test_that("invalid file entries warn and malformed YAML fails", {
  local_project()
  for (entry in list(42, TRUE, list(file = "missing.csv"), list(description = "No file"))) {
    for (field in c("inputs", "outputs")) {
      write_header("invalid.R", setNames(list(list(entry)), field))
      expect_warning(header <- daft_get_header("invalid.R"),
                     "Invalid yaml header format in script file: invalid.R", fixed = TRUE)
      expect_identical(header[[field]], list(NULL))
    }
  }
  writeLines("#: inputs: [unterminated", "malformed.R")
  expect_error(daft_get_header("malformed.R"))
})

test_that("file normalization preserves metadata and refreshes timestamps", {
  local_project()
  writeLines("data", "data.csv")
  entry <- list(file = "data.csv", description = "Measurements", extra = "kept", mtime = NA)
  normalized <- .normalize_file(entry, "script.R")
  expect_identical(normalized[c("file", "description", "extra")], entry[c("file", "description", "extra")])
  expect_equal(normalized$mtime, file.info("data.csv")$mtime)
  expect_identical(.normalize_file("absent.csv", "script.R"),
                   list(file = "absent.csv", description = "", mtime = NA))
})

test_that("missing header discovery respects recursion and file patterns", {
  local_project()
  make_project()
  expect_setequal(daft_missing_headers("analysis"),
                  c("analysis/plain.R", "analysis/nested/plain.R"))
  expect_identical(daft_missing_headers("analysis", recursive = FALSE), "analysis/plain.R")
  expect_identical(daft_missing_headers("analysis", pattern = "\\.txt$"), "analysis/notes.txt")
  dir.create("empty")
  expect_length(daft_missing_headers("empty"), 0L)
})

test_that("header collection names scripts and excludes scripts without headers", {
  local_project()
  make_project()
  headers <- .get_all_yaml_headers()
  expect_setequal(names(headers), c("analysis/prepare.R", "analysis/nested/plot.R", "analysis/metadata.R"))
  expect_identical(headers[["analysis/prepare.R"]]$description, "Prepare measurements")
  expect_named(.get_all_yaml_headers("analysis", pattern = "\\.r$"), "analysis/other.r")
  expect_setequal(names(.get_all_yaml_headers("analysis", recursive = FALSE)),
                  c("analysis/prepare.R", "analysis/metadata.R"))
})

test_that("input and output indexes associate files with the correct scripts", {
  local_project()
  make_project()
  inputs <- daft_input_files()
  outputs <- daft_output_files()
  expect_s3_class(inputs, "tbl_df")
  expect_s3_class(outputs, "tbl_df")
  expect_equal(inputs, tibble::tibble(
    file = c("clean.csv", "raw.csv"), description = c("", "Raw measurements"),
    script = c("analysis/nested/plot.R", "analysis/prepare.R")
  ))
  expect_equal(outputs, tibble::tibble(
    file = c("figure.pdf", "clean.csv"), description = c("Summary plot", "Clean measurements"),
    script = c("analysis/nested/plot.R", "analysis/prepare.R")
  ))
  expect_equal(daft_files(), dplyr::bind_rows(
    dplyr::mutate(inputs, type = "input"), dplyr::mutate(outputs, type = "output")
  ))
})

test_that("indexing forwards directory, pattern and recursion arguments", {
  local_project()
  make_project()
  expect_identical(daft_input_files("analysis", recursive = FALSE)$file, "raw.csv")
  expect_identical(daft_output_files("analysis", recursive = FALSE)$file, "clean.csv")
  expect_identical(daft_files("analysis", recursive = FALSE)$file, c("raw.csv", "clean.csv"))
  expect_identical(daft_output_files("analysis", pattern = "\\.r$")$file, "other.txt")
  expect_identical(daft_files("analysis", pattern = "\\.r$")$type, "output")
  expect_equal(nrow(daft_input_files("analysis", pattern = "\\.r$")), 0L)
})

test_that("indexes retain their schema when no files are declared", {
  local_project()
  for (fixture in c("empty", "plain", "metadata")) {
    if (fixture == "plain") writeLines("x <- 1", "plain.R")
    if (fixture == "metadata") write_header("metadata.R", list(description = "Only metadata"))
    for (fun in list(daft_input_files, daft_output_files)) {
      result <- fun()
      expect_equal(result, tibble::tibble(file = character(), description = character(), script = character()))
    }
    expect_equal(daft_files(), tibble::tibble(
      file = character(), description = character(), script = character(), type = character()
    ))
  }
})
