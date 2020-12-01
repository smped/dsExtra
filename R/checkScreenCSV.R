#' @title Check the provided CSV file
#' @description Check the provided CSV file for existence and the Compound_ID
#' column
#' @details
#' This function initially checks for the existence of the file.
#' Once confirmed, the first line is loaded and the requested column either
#' detected, or not.
#'
#' @param x Path to the csv file
#' @param name The expected column name. Defaults to 'Compound_ID'
#'
#' @return
#' Multiple messages may be produced depending on the input.
#' Eventually returns \code{invisible(logical(1))}.
#' @examples
#' fl <- system.file("extdata", "test.csv", package = "dsExtra")
#' checkScreenCSV(fl, "Col1")
#' @export
checkScreenCSV <- function(x, name = "Compound_ID"){

  if (!file.exists(x)) {
    stop(
      "Couldn't find the file\n", x,
      "\nHave you specified the path/name correctly"
    )
  }

  l <- readLines(x, n = 1)
  l <- strsplit(l, ",")[[1]]
  if (name %in% l){
    message(
      "The column '", name,
      "' is in this file.\nAll subsequent code should work without modification"
    )
    return(invisible(TRUE))
  }
  message(
    "The column '", name,
    "' is NOT in this file.\nSearching for possible alternatives."
  )
  partialMatch <- l[grepl(name, l)]
  if (length(partialMatch) == 1){
    message(
      "Found the column '", partialMatch,
      "'. Is this correct?",
      "\nIf so, either modify the initial file or your code to reflect this name"
    )
    return(invisible(TRUE))
  }
  if (length(partialMatch) > 1){
    message(
      "Possible matches found as '",
      paste(partialMatch, collapse = "' or '"),
      "'\nPlease decide on the correct column and either modify your original",
      " file or the subsequent code."
    )
    return(invisible(TRUE))
  }
  if (length(partialMatch) == 0){
    message(
      "Couldn't find a column matching '", name,
      "'.\nTry searching for something else, or inspecting your data closely."
    )
    return(invisible(FALSE))
  }
}
