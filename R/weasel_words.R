#' counts the number of 'weasel words' in a document.
#'
#' @export
#' @importFrom stringr str_count str_to_lower
#' @importFrom readr write_csv
#' @param txt text to evaluate.
#' @param weasel_words list of words or phrases to look for.
#' @param outfile the name of a file to save the results to.
#' @return a list of weasel phrases found and their frequency.
find_weasels <- function(txt, weasels, outfile = NULL) {
  if(file.exists(txt)) { #If a file, open it and read the text
    txt <- paste(readLines(txt), collapse="")
  }

  if(missing(weasels)) weasels <- weasel_words #use package default if non provided

  txt <- str_to_lower(txt) #lower all case for matching
  res <- sapply(weasel_words, function(x) { str_count(txt, x) })
  res2 <- res[res != 0] #we only want words that were found

  if(is.null(outfile) == FALSE) {
    dat <- data.frame(weasel_words = names(res2), count = res2, stringsAsFactors = FALSE)
    write_csv(dat, outfile)
  }

  return(res2)
}

#' The default weasel words.
#'
#' Provides a way of viewing the internal package list of weasel words.
#'
#' @export
#' @return the list of weasel words internal to the package.
#'
weasels <- function() {
  weasel_words
}
