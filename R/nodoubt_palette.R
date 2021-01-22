nodoubt_palette <- c(
  "#385581",
  "#6dbac6",
  "#dac190",
  "#c9052c",
  "#9db635",
  "#4d855d",
  "#898074",
  "#469ba7",
  "#252464",
  "#5fa2c5"
)

#' @title nodoubt palette
#' @description nodoubt palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname nodoubt_pal
#' @examples
#' library(scales)
#' show_col(nodoubt_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

nodoubt_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
    nodoubt <- nodoubt_palette
    
    if (reverse == TRUE) {
        nodoubt <- rev(nodoubt)
    }
    
    if (missing(n)) {
        n <- length(nodoubt)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(nodoubt)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(nodoubt)}!"))
    }
    
    nodoubte <- switch(type,
                   continuous = grDevices::colorRampPalette(nodoubt)(n),
                   discrete = nodoubt[1:n])
    
    nodoubt <- scales::manual_pal(nodoubt)
    
    return(nodoubt)
}

#' @title scale_color_nodoubt
#' @rdname nodoubt_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_nodoubt()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_nodoubt <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "nodoubt",
                                nodoubt_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = nodoubt_pal(n = n, type = type,
                                                         reverse = reverse)(256))
    }
}

#' @title scale_colour_nodoubt
#' @rdname nodoubt_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_nodoubt()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_nodoubt <- scale_color_nodoubt

#' @title scale_fill_nodoubt
#' @rdname nodoubt_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_nodoubt()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_nodoubt <- function(n, type = "discrete",
                            reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "nodoubt",
                                nodoubt_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = nodoubt_pal(n = n, type = type,
                                                        reverse = reverse)(256))
    }
}
