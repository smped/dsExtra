#' @title Plot a colour palette
#'
#' @description Plot a colour palette as a strip plot
#'
#' @details
#' Takes a named vector of colours and produces a single row plot showing each
#' name and the corresponding colour
#'
#' @param pal Character vector of colours. Can be in any recosgnised R format
#' @param alpha Include transparency in the colours
#'
#' @importFrom grDevices col2rgb rgb
#' @importFrom dplyr mutate
#' @importFrom magrittr `%>%`
#' @importFrom stats setNames
#'
#' @return
#' A ggplot object showing the colours provided in the palette
#'
#' @examples
#' x <- c(A = "blue", B = "yellow", C = "green")
#' plotPalette(x)
#'
#' @export
plotPalette <- function(pal, alpha = TRUE){

  ## The palette must be named
  stopifnot(!is.null(names(pal)))

  ## Avoid R CMD issues
  red <- green <- blue <- c()

  ## Make a tibble. This will fail if any invalid colours are used
  df <- col2rgb(pal, alpha) %>% ## Failure will occur here
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Label")
  if (!alpha) df[["alpha"]] <- 255
  df <- mutate(
    df, RGB = rgb(red, green, blue, alpha, maxColorValue = 255)
  )
  rgb <- setNames(df$RGB, df$Label)

  ## Draw a heatmap
  ggplot(df, aes_string("Label", 1, fill = "Label")) +
    geom_raster() +
    scale_x_discrete(expand = expansion(c(0, 0))) +
    scale_y_continuous(expand = expansion(c(0, 0))) +
    scale_fill_manual(values = rgb) +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    )
}
