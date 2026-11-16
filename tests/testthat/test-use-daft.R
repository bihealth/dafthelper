test_that("use_daft creates the default structure and returns invisibly", {
  local_project()

  text <- console_text(result <- withVisible(use_daft()))

  expected <- c("01_data", "02_analysis", "03_figures", "04_text")
  expect_identical(list.files(), expected)
  expect_true(all(dir.exists(expected)))
  expect_null(result$value)
  expect_false(result$visible)
  expect_match(text, "Created subdirectory:", fixed = TRUE)
})

test_that("use_daft creates missing parent directories at the requested destination", {
  local_project()

  text <- console_text(use_daft(dir = "projects/new analysis"))

  expected <- c("01_data", "02_analysis", "03_figures", "04_text")
  expect_identical(list.files("projects/new analysis"), expected)
  expect_true(all(dir.exists(file.path("projects/new analysis", expected))))
  expect_identical(list.files(), "projects")
  expect_match(text, "Created directory:", fixed = TRUE)
})

test_that("use_daft respects custom names, templates and numbering", {
  local_project()

  console_text(use_daft(
    names = c("raw data", "results"),
    format = "{name}-{number}", number_format = "%03d"
  ))

  expected <- c("raw data-001", "results-002")
  expect_identical(list.files(), expected)
  expect_true(all(dir.exists(expected)))

  console_text(use_daft(dir = "unnumbered", names = c("source", "reports"),
                        format = "{name}"))
  expect_identical(list.files("unnumbered"), c("reports", "source"))
  expect_true(all(dir.exists(file.path("unnumbered", c("reports", "source")))))
})

test_that("use_daft preserves existing contents and creates only missing directories", {
  local_project()
  dir.create("01_data")
  writeLines("Keep these measurements", "01_data/measurements.txt")
  writeLines("Keep these notes", "notes.txt")

  text <- console_text(use_daft())

  expect_match(text, "Subdirectory already exists:", fixed = TRUE)
  expect_match(text, "Created subdirectory:", fixed = TRUE)
  expect_true(all(dir.exists(c("01_data", "02_analysis", "03_figures", "04_text"))))
  expect_identical(readLines("01_data/measurements.txt"), "Keep these measurements")
  expect_identical(readLines("notes.txt"), "Keep these notes")

  before <- list.files(recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  text <- console_text(use_daft())
  expect_identical(list.files(recursive = TRUE, include.dirs = TRUE, all.files = TRUE), before)
  expect_identical(readLines("01_data/measurements.txt"), "Keep these measurements")
  expect_match(text, "Subdirectory already exists:", fixed = TRUE)
  expect_false(grepl("Created", text, fixed = TRUE))
})

test_that("use_daft accepts an empty set of directory names", {
  local_project()

  expect_identical(console_text(use_daft(names = character())), "")
  expect_length(list.files(all.files = TRUE, no.. = TRUE), 0L)

  console_text(use_daft(dir = "empty project", names = character()))
  expect_true(dir.exists("empty project"))
  expect_length(list.files("empty project", all.files = TRUE, no.. = TRUE), 0L)
})
