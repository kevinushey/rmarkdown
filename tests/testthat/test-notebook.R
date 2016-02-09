context("Notebook")

test_path <- function(path) {
  if (file.exists("DESCRIPTION"))
    file.path("tests/testthat", path)
  else
    path
}

test_that("we can round-trip through reading, writing a notebook", {

  input <- test_path("resources/notebook/example-notebook.Rmd")
  output <- test_path("resources/notebook/example-notebook.Rnb")

  rnbData <- rnb_read(input)
  rnbDoc <- rnb_prepare(rnbData)
  cat(rnbDoc, file = output, sep = "\n")

})
