test_that("default templates can be parsed as both YAML metadata and R code", {
  local_project()
  text <- console_text(result <- withVisible(daft_make_script("new.R")))
  expect_null(result$value)
  expect_false(result$visible)
  expect_match(text, "Created new script file", fixed = TRUE)
  expect_true(file.exists("new.R"))
  expect_no_error(parse("new.R"))
  expect_true(any(grepl("^#: ", readLines("new.R"))))
  header <- daft_get_header("new.R")
  expect_identical(header$description, "Description of the script")
  expect_named(header$params, c("param1", "param2"))
  expect_identical(header$params$param1$value, "value1")
  expect_identical(vapply(header$inputs, `[[`, "", "file"), c("input_file1.rds", "input_file2.csv"))
  expect_identical(vapply(header$outputs, `[[`, "", "file"), c("output_file1.csv", "output_file2.rds"))
})

test_that("custom metadata round trips through a script in the requested directory", {
  local_project()
  dir.create("analysis")
  description <- "Summary: 'quoted' values\nand a second line"
  params <- list(cutoff = 0.25, method = list(value = "mean", description = "Average"))
  inputs <- list(list(file = "raw data.csv", description = "Raw: measurements"))
  outputs <- list(list(file = "plot.pdf", description = "Summary figure"))
  console_text(daft_make_script("custom.R", dir = "analysis", description = description,
                               params = params, inputs = inputs, outputs = outputs))
  expect_true(file.exists("analysis/custom.R"))
  expect_false(file.exists("custom.R"))
  expect_no_error(parse("analysis/custom.R"))
  header <- daft_get_header("analysis/custom.R")
  expect_identical(header$description, "Summary: 'quoted' values and a second line")
  expect_identical(header$params, params)
  expect_identical(header$inputs[[1]][c("file", "description")], inputs[[1]])
  expect_identical(header$outputs[[1]][c("file", "description")], outputs[[1]])
})

test_that("partial metadata does not populate example fields", {
  local_project()
  console_text(daft_make_script("partial.R", description = "Just a description"))
  header <- daft_get_header("partial.R")
  expect_identical(header$description, "Just a description")
  expect_null(header$params)
  expect_null(header$inputs)
  expect_null(header$outputs)
})

test_that("existing scripts are never overwritten", {
  local_project()
  writeLines(c("# Keep this script", "x <- 42"), "existing.R")
  before <- readBin("existing.R", "raw", n = file.info("existing.R")$size)
  text <- console_text(result <- withVisible(daft_make_script("existing.R", description = "Replacement")))
  expect_match(text, "not overwriting", fixed = TRUE)
  expect_identical(readBin("existing.R", "raw", n = file.info("existing.R")$size), before)
  expect_null(result$value)
  expect_false(result$visible)
})

test_that("template file paths follow the script directory", {
  local_project()
  dir.create("analysis")
  for (file_includes_path in c(FALSE, TRUE)) {
    if (file_includes_path) {
      console_text(daft_make_script("analysis/path.R"))
      script <- "analysis/path.R"
    } else {
      console_text(daft_make_script("dir.R", dir = "analysis"))
      script <- "analysis/dir.R"
    }
    header <- daft_get_header(script)
    expect_identical(sub("^\\./", "", vapply(header$inputs, `[[`, "", "file")),
                     c("analysis/input_file1.rds", "analysis/input_file2.csv"))
    expect_identical(sub("^\\./", "", vapply(header$outputs, `[[`, "", "file")),
                     c("analysis/output_file1.csv", "analysis/output_file2.rds"))
  }
})

test_that("comments are written as YAML comments and do not become metadata", {
  local_project()
  comments <- c("Notes: [not YAML", "Second line with # and 'quotes'")
  console_text(daft_make_script("comments.R", comment = comments, description = "Example"))
  expect_true(all(paste0("#: # ", comments) %in% readLines("comments.R")))
  header <- daft_get_header("comments.R")
  expect_null(header$comment)
  expect_identical(header$description, "Example")
  expect_no_error(parse("comments.R"))

  console_text(daft_make_script("comment-only.R", comment = comments))
  expect_true(all(paste0("#: # ", comments) %in% readLines("comment-only.R")))
  header <- daft_get_header("comment-only.R")
  expect_null(header$params)
  expect_null(header$inputs)
  expect_null(header$outputs)
})

test_that("long descriptions wrap without losing their text", {
  local_project()
  description <- paste(rep("A description with several words.", 5), collapse = " ")
  console_text(daft_make_script("wrapped.R", description = description))
  header <- daft_get_header("wrapped.R")
  lines <- strsplit(header$description, "\n", fixed = TRUE)[[1]]
  expect_gt(length(lines), 1L)
  expect_true(all(nchar(lines) <= 60L))
  expect_identical(paste(lines, collapse = " "), description)
})
