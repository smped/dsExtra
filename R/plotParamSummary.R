#' @title Produce a Summary Plot Of All Parameters
#' @description Produce a summary plot of all parameters used for training
#' the model
#' @details
#' Given a character vector of parameeters used for fitting the model, this
#' functions assumes they have a common separator and are listed in a
#' hierarchical manner.
#' A summary plot of all measured parameters, along with marginal values is
#' produced
#'
#' @return A cowplot object composed of four panels
#'
#' @param x Character vector of colnames from the data used for training the
#' model
#' @param sep The common separator for the parameters
#' @param .into The column names after separating the parameters into a df
#' @param barCol Colour of the marginal barplots drawn at top & right
#' @param cirCol The colour for the circles
#' @param rng The size range for the plotted circles
#' @param nudge_top Value between 0 & 0.5. Lower values are better.
#' This can be tweaked to ensure the top marginal plot alings the y-axis with
#' the y-axis of the lower plot. As these tend to shift with resizing the plot
#' this may take manual tweaking.
#' @param nudge_labs Nudge the labels away from the bars in the marginal
#' barplots
#' @param ... Passed to \code{\link{scale_size}}
#'
#' @importFrom stringr str_detect
#' @importFrom tidyr separate
#' @importFrom dplyr group_by mutate summarise n
#' @importFrom cowplot plot_grid
#' @importFrom forcats fct_rev
#' @importFrom grDevices rgb
#' @import rlang
#' @import tibble
#' @import ggplot2
#'
#' @examples
#' mat <- cbind("Median", rep(c("Membrane", "Nucleus"), each = 5),
#' rep(c("Colour", "Texture"), each = 10), rep(letters[1:10], times = 2))
#' param <- apply(mat[1:18,], MARGIN = 1, FUN = paste, collapse = "_")
#' plotParamSummary(param, nudge_top = 0.07)
#'
#' @export
plotParamSummary <- function(
  x, sep = "_", .into = c("Stat", "Target", "Feature", "Value"),
  barCol = "grey70", cirCol = rgb(0, 0.5, 0.5, 0.2),
  rng = c(5, 25), nudge_top = 0.15, nudge_labs = 20, ...
){

  ## Checks
  stopifnot(is.character(x))
  if (!all(str_detect(x, sep)))
    stop("Some values do not contain the separator ", sep)
  stopifnot(nudge_top < 0.5 & nudge_top > 0)

  ## Create the variable names using NSE
  var1 <- sym(.into[[2]])
  var2 <- sym(.into[[3]])

  ## Dummy variable for R CMD check
  parameters <- c()

  ## Form summary
  paramSummary <- tibble(parameters = x) %>%
    separate(
      col = parameters,
      into = .into,
      sep = sep,
      extra = "merge"
    ) %>%
    group_by(!!var1, !!var2) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      ## This will always name them as Feature & Target
      !!var2 := as.factor(!!var2),
      !!var1 := fct_rev(!!var1)
    )

  # Now make the four plots
  a <- paramSummary %>%
    ggplot(aes(!!var2, y = n)) +
    geom_col(fill = barCol) +
    geom_text(
      aes(label = n),
      data = paramSummary %>%
        group_by(!!var2) %>%
        summarise(n = sum(n), .groups = "drop"),
      nudge_y = nudge_labs
    ) +
    scale_y_continuous(expand = expansion(c(0, 0.1))) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  b <- ggplot() + theme_void()
  c <- paramSummary %>%
    ggplot(
      aes(!!var2, !!var1)
    ) +
    geom_point(aes(size = n), shape = 21, fill = cirCol) +
    geom_text(aes(label = n)) +
    scale_size(range = rng, ...) +
    theme(
      legend.position = "none",
      panel.grid = element_blank()
    )
  d <- paramSummary %>%
    ggplot(aes(!!var1, y = n)) +
    geom_col(fill = barCol) +
    geom_text(
      aes(label = n),
      data = paramSummary %>%
        group_by(!!var1) %>%
        summarise(n = sum(n), .groups = "drop"),
      nudge_y = nudge_labs
    ) +
    coord_flip() +
    scale_y_continuous(expand = expansion(c(0, 0.15))) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  plot_grid(
    # The top row with the Feature Marginals
    plot_grid(
      b, a, b,
      nrow = 1, rel_widths = c(nudge_top, 0.85 - nudge_top, 0.15),
      align = "v"
    ),
    # The main plot with target marginals
    plot_grid(
      c, d,
      nrow = 1,
      rel_widths = c(0.85, 0.15),
      align = "h", axis = "tblr"
    ),
    nrow = 2,
    rel_heights = c(0.2, 0.8)
  )
}
