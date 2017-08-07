library(researchTools)
context("Weasel words")

### weasels()
test_that("weasels prints the default list of weasel words.", {
  words <- weasels()
  expect_equal(typeof(words), "character")
})

### find_weasels()
txt <- "there are a number of excellent weasel words that are very hard to
  resist. Clearly the vast literature is filled with such a vast amount of words."

test_that("find_weasels counts number of occurrence of each phrase", {
  expected_return_value <- list(
    "excellent" = 1,
    "are a number" = 1,
    "clearly" = 1,
    "very" = 1,
    "vast" = 2)
  words <- c("excellent", "are a number", "clearly", "very", "vast")
  res <- find_weasels(txt, words)
  expect_equal(sum(sapply(res, function(x) { res[x] != expected_return_value[x]})), 0)
  expect_equal(sum(sapply(expected_return_value, function(x) { res[x] != expected_return_value[x]})), 0)
})

test_that("find_weasels uses default weasel words if none is given.", {
  res <- try(find_weasels(txt), silent = TRUE)
  expect_equal(typeof(res), "integer")
})

test_that("find_weasels writes to a file if specified.", {
  output_file <- "test_weasels.txt"
  res <- find_weasels(txt, outfile = output_file)
  expect_true(file.exists(output_file))
  rm(output_file)
})

test_that("find_weasels takes a txt file as an argument.", {
  test_file <- "test_txt.Rmd"
  getwd()
  expected_return_value <- list(
    "excellent" = 1,
    "respectively" = 1,
    "very" = 1)
  res <- find_weasels(test_file)
  expect_equal(sum(sapply(res, function(x) { res[x] != expected_return_value[x]})), 0)
})




