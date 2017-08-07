library(researchTools)
context("Weasel words")

data(weasel_words)
test_that("weasels counts number of occurrence of each phrase", {
  txt <- "there are a number of excellent weasel words that are very hard to
  resist. Clearly the vast literature is filled with such a vast amount of words."
  expected_return_value <- list(
    "excellent" = 1,
    "are a number" = 1,
    "clearly" = 1,
    "very" = 1,
    "vast" = 2)
  res <- find_weasels(txt, weasel_words)
  expect_equal(sum(sapply(res, function(x) { res[x] != expected_return_value[x]})), 0)
  expect_equal(sum(sapply(expected_return_value, function(x) { res[x] != expected_return_value[x]})), 0)
})
