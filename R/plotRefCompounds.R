#' @title Plot the Reference Compounds
#' @description Plot the reference compounds after training the model
#' @details
#' Takes a trained model with the reference compounds only and plots the PLS
#' fit as a gglot2 object
#'
#' @param fit The object returned when fitting the training dataset using
#' \code{link{splsda}}
#' @param pal The colour palette for each reference compound
#' @param shapes The shapes for each reference compound. (Optional)
#' @param size Size of points on the final plot
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices hcl.colors
#' @import rlang
#' @import ggplot2
#' @import mixOmics
#' @import tibble
#'
#' @export
plotRefCompounds <- function(fit, pal, shapes, size = 1) {

  ## Check the fitted model & the supplied data
  stopifnot(is(fit, "mixo_spls"))

  ## Deal with missing palettes and shapes
  refs <- fit$Y
  if (is.character(refs)) refs <- unique(refs)
  if (is.factor(refs)) refs <- levels(refs)
  nRef <- length(refs)
  if (missing(pal)){
    if (nRef <= 9) pal <- brewer.pal(nRef, "Set1")
    if (nRef > 9) pal <- hcl.colors(nRef, "Plasma")
    names(pal) <- refs
  }
  if (missing(shapes)) {
    shapes <- seq(0, by = 1, length.out = nRef)
    names(shapes) <- refs
  }

  ## Check the palette & shapes have a value for each ref compound
  stopifnot(all(refs %in% names(pal)))
  stopifnot(all(refs %in% names(shapes)))

  ## Define the values based on the returned values
  x <- sym(colnames(fit$variates$X)[[1]])
  y <- sym(colnames(fit$variates$X)[[2]])
  nm <- sym("Label")

  tibble(!!nm := fit$Y) %>%
    cbind(fit$variates$X) %>%
    as_tibble() %>%
    ggplot(aes(!!x, !!y, colour = !!nm, shape = !!nm)) +
    geom_point(size = size) +
    stat_ellipse(size = 1, show.legend = FALSE) +
    scale_shape_manual(values = shapes) +
    scale_colour_manual(values= pal) +
    labs(
      x = "PLS-DA-1",
      y = "PLS-DA-2",
      colour = "Compound",
      shape = "Compound"
    )
}
