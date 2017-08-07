#' counts the number of 'weasel words' in a document.
#'
#' @export
#' @importFrom stringr str_count str_to_lower
#' @importFrom readr write_csv
#' @param txt text to evaluate.
#' @param weasels list of words or phrases to look for.
#' @return a list of weasel phrases found and their frequency.
find_weasels <- function(txt, weasels, outfile = FALSE) {
  if(file.exists(txt)) { #If a file, open it and read the text
    txt <- readLines(txt)
  }

  txt <- str_to_lower(txt) #lower all case for matching
  res <- sapply(weasels, function(x) { str_count(txt, x) })
  res2 <- res[res != 0] #we only want words that were found

  if(outfile) {
    dat <- data.frame(weasels = names(res2), count = res2, stringsAsFactors = FALSE)
    write_csv(dat, outfile)
  }

  return(res2)
}

