#' @title Get Predictions For Each Test Compound
#' @description Get Best Predictions For Each Test Compound
#' @details
#' After training a model based on the selected reference compounds, obtain the
#' predictions for the remaining test compounds.
#' The best distance measure can be chosen, and concordance across both
#' components is provided in the output
#' @return
#' A \code{link{tibble}} containing all original metadata along with
#' predictions in the first few columns
#'
#' @param compound Character vector of compounds on which predictions are to be
#' performed
#' @param fit The object returned when fitting the training dataset using
#' \code{link{splsda}}
#' @param data Data as imported with all measurements used in the training
#' model
#' @param column The column in \code{data} which identifies the compounds
#' @param dist The distance measure to use when classifying compounds
#'
#' @import rlang
#' @import tibble
#' @importFrom methods is
#' @importFrom dplyr filter select case_when
#' @importFrom tidyselect all_of any_of contains everything starts_with
#'
#' @export
getPredictions <- function(
  compound, fit, data, column = "Label",
  dist = c("mahalanobis.dist", "mahalanobis.dist", "max.dist")
){

  ## Check the input data
  if (!column %in% colnames(data)) stop(
    paste0("Cannot find the column '", column, "' in your data")
  )
  if (!all(compound %in% data[[column]])) stop(
    paste0("Requested compound(s) not found in the column '", column, "':\n"),
    paste(setdiff(compound, data[[column]]), sep = ", ")
  )
  stopifnot("Concentration_Index" %in% colnames(data))
  ## Choose the distance measure
  dist <- match.arg(dist)

  ## Check the fitted model & the supplied data
  stopifnot(is(fit, "mixo_spls"))
  param <- colnames(fit$X)
  missingParam <- setdiff(param, colnames(data))
  if (length(missingParam) > 0) stop(
    "Parameters used to train the model appear to be missing from your data"
  )
  ## Avoid an R CMD check
  ## These are the columns returned in the output from `predict()`
  comp1 <- comp2 <- c()

  ## Now subset out the training data
  testData <- dplyr::filter(data, !!sym(column) %in% compound)
  X <- dplyr::select(testData, all_of(param))
  predictions <- predict(fit, as.matrix(X))
  predict_df <- testData %>%
    dplyr::select(-any_of(param)) %>%
    cbind(predictions$MajorityVote[[dist]]) %>%
    as_tibble() %>%
    mutate(
      concordant = comp1 == comp2,
      prediction = case_when(
        concordant ~ comp1,
        !concordant ~ NA_character_
      )
    )
  ## And return the output
  dplyr::select(
    predict_df, all_of(column), contains("Concentration"),
    starts_with("comp", ignore.case = FALSE),
    all_of(c("concordant", "prediction")), everything()
  )
}
