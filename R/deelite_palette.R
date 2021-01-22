deelite_palette <- c(
  '#48448e',
  '#fc4d97',
  '#f5f72d',
  '#fe910e',
  '#8ccc58',
  '#b82578',
  '#f14019',
  '#51d4e9',
  '#41a15a',
  '#f95a7c',
  '#b23a76'
)

#' @title deelite palette
#' @description deelite palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname deelite_pal
#' @examples
#' library(scales)
#' show_col(deelite_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

deelite_pal <- function(n, type = c("discrete", "continuous"),
                        reverse = FALSE){
  deelite <- deelite_palette
  
  if (reverse == TRUE) {
    deelite <- rev(deelite)
  }
  
  if (missing(n)) {
    n <- length(deelite)
  }
  
  type <- match.arg(type)
  
  if (type == "discrete" && n > length(deelite)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(deelite)}!"))
  }
  
  deelite <- switch(type,
                    continuous = grDevices::colorRampPalette(deelite)(n),
                    discrete = deelite[1:n])
  
  deelite <- scales::manual_pal(deelite)
  
  return(deelite)
}

#' @title scale_color_deelite
#' @rdname deelite_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_deelite()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_deelite <- function(n, type = "discrete",
                                reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "deelite",
                            deelite_pal(n = n, type = type,
                                        reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = deelite_pal(n = n, type = type,
                                                        reverse = reverse)(256))
  }
}

#' @title scale_colour_deelite
#' @rdname deelite_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_deelite()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_deelite <- scale_color_deelite

#' @title scale_fill_deelite
#' @rdname deelite_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_deelite()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_deelite <- function(n, type = "discrete",
                               reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "deelite",
                            deelite_pal(n = n, type = type,
                                        reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = deelite_pal(n = n, type = type,
                                                       reverse = reverse)(256))
  }
}