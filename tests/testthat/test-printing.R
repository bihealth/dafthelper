test_that("show reports producers, consumers and unknown files", {
  local_project()
  make_project()
  text <- console_text(result <- daft_show("clean.csv"))
  expect_null(result)
  for (expected in c("clean.csv", "Produced as output in:", "Used as input in:",
                     "analysis/prepare.R", "analysis/nested/plot.R", "Clean measurements")) {
    expect_match(text, expected, fixed = TRUE)
  }
  text <- console_text(daft_show(c("raw.csv", "figure.pdf", "unknown.csv")))
  for (expected in c("Raw measurements", "Summary plot", "Not produced as output in any script.",
                     "Not used as input in any script.", "unknown.csv",
                     "This file is not used as input or produced as output in any script.")) {
    expect_match(text, expected, fixed = TRUE)
  }
})

test_that("show prints script metadata and both parameter representations", {
  local_project()
  make_project()
  text <- console_text(daft_show("analysis/prepare.R"))
  for (expected in c("Script:", "Prepare measurements", "Parameters:", "threshold", "0.5",
                     "method", "median", "Inputs:", "raw.csv", "Raw measurements",
                     "Outputs:", "clean.csv", "Clean measurements")) {
    expect_match(text, expected, fixed = TRUE)
  }
})

test_that("script printing handles missing headers and optional metadata", {
  local_project()
  writeLines("x <- 1", "plain.R")
  text <- console_text(result <- .print_script_file("plain.R"))
  expect_null(result)
  expect_match(text, "No YAML header found", fixed = TRUE)
  write_header("minimal.R", list(description = "Minimal script"))
  text <- console_text(.print_script_file("minimal.R"))
  expect_match(text, "Minimal script", fixed = TRUE)
  expect_false(grepl("Parameters:|Inputs:|Outputs:", text))
  write_header("output.R", list(outputs = list("result.csv")))
  text <- console_text(.print_script_file("output.R"))
  expect_match(text, "result.csv", fixed = TRUE)
  expect_false(grepl("Description:|Parameters:|Inputs:", text))
})

test_that("the file display helper can discover its own index", {
  local_project()
  make_project()
  text <- console_text(.show_one_file("raw.csv"))
  expect_match(text, "analysis/prepare.R", fixed = TRUE)
  expect_match(text, "Used as input in:", fixed = TRUE)
})

test_that("find uses regular expressions and invisibly returns every matching relationship", {
  local_project()
  make_project()
  text <- console_text(result <- withVisible(daft_find("^clean\\.csv$")))
  expect_false(result$visible)
  expect_equal(result$value, dplyr::filter(daft_files(), file == "clean.csv"))
  expect_equal(nrow(result$value), 2L)
  expect_equal(lengths(regmatches(text, gregexpr("File:", text, fixed = TRUE))), 1L)
  expect_match(text, "Produced as output in:", fixed = TRUE)
  expect_match(text, "Used as input in:", fixed = TRUE)
  console_text(result <- daft_find("\\.(csv|pdf)$"))
  expect_equal(result, daft_files())
})

test_that("find and show handle unmatched queries and empty projects", {
  local_project()
  for (populated in c(FALSE, TRUE)) {
    if (populated) make_project()
    text <- console_text(result <- withVisible(daft_find("no-match")))
    expect_identical(text, "")
    expect_false(result$visible)
    expect_s3_class(result$value, "tbl_df")
    expect_equal(nrow(result$value), 0L)
    expect_named(result$value, c("file", "description", "script", "type"))
    text <- console_text(daft_show("unknown.csv"))
    expect_match(text, "This file is not used", fixed = TRUE)
    expect_identical(console_text(expect_null(daft_show(character()))), "")
  }
})
