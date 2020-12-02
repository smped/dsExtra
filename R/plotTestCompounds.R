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
#' @param lab_col Text colour for plotted labels
#' @param lab_bg Background colour for plotted labels
#' @param lab_alpha Transparency for the label background
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
#' @importFrom stats predict as.formula
#' @importFrom zoo na.locf
#'
#' @export
plotTestCompounds <- function(
  compound, fit, data, column = "Label", pal, shapes, size = 3,
  lab_col = "black", lab_bg = "white", lab_alpha = 0.5
){

  # Check the input data
  if (!column %in% colnames(data)) stop(
    paste0("Cannot find the column '", column, "' in your data")
  )
  if (!all(compound %in% data[[column]])) stop(
    paste0("Requested compound(s) not found in the column '", column, "':\n"),
    paste(setdiff(compound, data[[column]]), sep = ", ")
  )
  concInd <- "Concentration_Index"
  stopifnot(concInd %in% colnames(data))

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
  refPoints <- tibble(!!column := fit$Y)
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
  x <- sym("comp1")
  y <- sym("comp2")
  allPred <- testPoints %>%
    dplyr::select(
      all_of(c(column, as_string(x), as_string(y), concInd))
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

  ## The Facetting formula
  fm <- as.formula("~plotGroup")
  ## The column as a symbol for parsing with rlang
  colassym <- sym(column)

  ## Plot each set of training points first, then add the test points
  allPred %>%
    dplyr::filter(!!colassym %in% refs) %>%
    ggplot(aes(!!x, !!y)) +
    geom_point(
      aes(shape = !!colassym, colour = !!colassym),
      size = size
    ) +
    stat_ellipse(
      aes(colour = !!colassym),
      size = 1, show.legend = FALSE
    ) +
    ## The test points as labels
    geom_label(
      aes(label = !!sym(concInd)),
      data = dplyr::filter(allPred, !(!!colassym %in% refs)),
      colour = lab_col,
      alpha = lab_alpha,
      fill = lab_bg,
      show.legend = FALSE
    ) +
    labs(
      x = "PLS-DA-1",
      y = "PLS-DA-2",
      colour = "Compound",
      shape = "Compound"
    ) +
    facet_wrap(fm) +
    scale_colour_manual(values = pal) +
    scale_shape_manual(values = shapes)


}
