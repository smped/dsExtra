#' @title Show all Non-Numeric values in a column
#' @description Show all Non-Numeric values in a column
#' @details
#' Looks in a column, converts everything to a numeric and returns the values
#' which could not be converted to a numeric value.
#' This is particualrly useful as many screening compounds are simply given
#' a numeric identifier. Better known compounds are generally named.
#'
#' This is useful just for quickly checking values when constructing
#' abbreviated labels for common compounds.
#'
#' @param df The data frame with named compounds
#' @param col The column name to show values from
#'
#' @return
#' Sorted, unique values from the requested column as a character vector.
#' Only values which cannot be transformed into a numeric value are returned.
#'
#' @examples
#' df <- data.frame(Compound_ID = c(1:10, "DHT"))
#' showNamedCompounds(df)
#'
#' @export
showNamedCompounds <- function(df, col = "Compound_ID"){

  ## Check the column name is in the df
  if (!col %in% colnames(df))
    stop("Couldn't find any column named ", col)
  ## Grab the column as a character or grab the factor levels
  if (is.character(df[[col]])) allVals <- unique(df[[col]])
  if (is.factor(df[[col]]))  allVals <- levels(df[[col]])
  ## Convert to numeric & grab the NA values
  textVals <- suppressWarnings(is.na(as.numeric(allVals)))
  ## Return the vector
  sort(allVals[textVals])

}
