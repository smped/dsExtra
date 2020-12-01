#' @title Plot Test Compounds
#' @description Plot the test compounds overlaid on the trained model
#' @details
#' This takes the full dataset and the trained model, fitting where each
#' selected test compound lies on the trained PLS model and overlaying each
#' concentration for the test compound.
#' Concentrations are overlaid as labels indicating the concentration index
#' level.
#'
#' @return
#' Produces a facetted ggplot object, with each test compound in an individual
#' facet.
#' Can be modified, saved and exported like any ggplot object.
#'
#' @param compound Character vector of compounds to be plotted
#' @param fit The object returned when fitting the training dataset using
#' \code{link{splsda}}
#' @param data The original imported CSV. Must contain the testing compounds,
#' but training compounds are optional
#' @param column The column which identifies the compounds
#' @param pal The colour palette for plotting the training compounds
#' @param shapes The shapes to use when plotting the training compounds
#' @param size The point size when plotting the training compounds
#'
#' @import mixOmics
#' @import rlang
#' @import tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices hcl.colors
#' @importFrom tidyselect any_of all_of
#' @importFrom dplyr select bind_rows mutate filter
#' @importFrom magrittr `%>%`
#' @importFrom methods is
#' @importFrom stats setNames predict
#' @importFrom zoo na.locf
#'
#' @export
plotTestCompounds <- function(
  compound, fit, data, column = "Label", pal, shapes, size = 3
){

  # Check the input data
  if (!column %in% colnames(data)) stop(
    paste0("Cannot find the column '", column, "' in your data")
  )
  if (!all(compound %in% data[[column]])) stop(
    paste0("Requested compound(s) not found in the column '", column, "':\n"),
    paste(setdiff(compound, data[[column]]), sep = ", ")
  )
  stopifnot("Concentration_Index" %in% colnames(data))

  # Check the fitted model & the supplied data
  stopifnot(is(fit, "mixo_spls"))
  param <- colnames(fit$X)
  missingParam <- setdiff(param, colnames(data))
  if (length(missingParam) > 0) stop(
    "Parameters used to train the model appear to be missing from your data"
  )

  ## Check the plotting parameters
  labels <- fit$Y
  if (!is.factor(labels)) labels <- as.factor(labels)
  refs <- levels(labels)
  nRef <- length(refs)
  if (missing(pal) & nRef <= 9) {
    pal <- brewer.pal(nRef, "Set1")
    names(pal) <- refs
  }
  if (missing(pal) & nRef > 9) {
    pal <- hcl.colors(nRef, "Plasma")
    names(pal) <- refs
  }
  if (missing(shapes)) {
    shapes <- seq(0, by = 1, length.out = nRef)
    names(shapes) <- refs
  }
  if (!all(refs %in% names(pal))) stop(
    "You have not assigned a colour for each reference compound:\n",
    paste(setdiff(refs, names(pal)), collapse = ", ")
  )
  if (!all(refs %in% names(pal))) stop(
    "You have not assigned a shape for each reference compound:\n",
    paste(setdiff(refs, names(pal)), collapse = ", ")
  )

  ## Extract the x & Y co-ordinates from the training dataset
  refPoints <- setNames(tibble(fit$Y), column)
  refPoints <- cbind(refPoints, fit$variates$X)
  refPoints <- droplevels(refPoints)

  ## Fit the new compounds
  ind <- which(data[[column]] %in% compound)
  test_df <- data[ind,]
  test_pred <- predict(fit, dplyr::select(test_df, any_of(param)))
  testPoints <- dplyr::select(test_df, -any_of(param))
  testPoints <- cbind(testPoints, test_pred$variates)
  names(testPoints) <- gsub("dim", "comp", names(testPoints))
  testPoints <- droplevels(testPoints)

  ## Now split each compound, add the training points,
  ## define plotGroups & merge
  allPred <- testPoints %>%
    dplyr::select(
      all_of(c(column, "comp1", "comp2", "Concentration_Index"))
    ) %>%
    split(f = testPoints[[column]]) %>%
    lapply(function(x){
      x[["plotGroup"]] <- x[[column]]
      x <- bind_rows(x, refPoints)
      x
    }) %>%
    bind_rows() %>%
    as_tibble()
  allPred[["plotGroup"]] <- zoo::na.locf(allPred[["plotGroup"]])

  x <- sym("comp1")
  y <- sym("comp2")
  lab <- sym("Concentration_Index")
  plotGroup <- c() # Avoids an R CMD Check error

  ## Plot each set of training points first, then add the test points
  allPred[allPred[[column]] %in% refs,] %>%
    ggplot(aes(!!x, !!y)) +
    geom_point(
      aes_string(shape = column, colour = column),
      size = size
    ) +
    stat_ellipse(
      aes_string(colour = column),
      size = 1, show.legend = FALSE
    ) +
    ## The test points as labels
    geom_label(
      aes(label = !!lab),
      data = dplyr::filter(allPred, !allPred[[column]] %in% refs),
      colour = "black",
      alpha = 0.5,
      show.legend = FALSE
    ) +
    labs(
      x = "PLS-DA-1",
      y = "PLS-DA-2",
      colour = "Compound",
      shape = "Compound"
    ) +
    facet_wrap(~plotGroup) +
    scale_colour_manual(values = pal) +
    scale_shape_manual(values = shapes)


}
