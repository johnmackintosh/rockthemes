herb_palette <- c(
    "#eae6e0", 
    "#a1ae8c", 
    "#a09983", 
    "#d0924c", 
    "#c36b43",
    "#85826f",
    "#94b432",
    "#56a225",
    "#645e21",
    "#030100"
)

#' @title herb alpert palette
#' @description herb alpert palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname herb_pal
#' @examples
#' library(scales)
#' show_col(herb_pal()(5))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

herb_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
    herb <- herb_palette
    
    if (reverse == TRUE) {
        herb <- rev(herb)
    }
    
    if (missing(n)) {
        n <- length(herb)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(herb)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(herb)}!"))
    }
    
    herb <- switch(type,
                   continuous = grDevices::colorRampPalette(herb)(n),
                   discrete = herb[1:n])
    
    herb <- scales::manual_pal(herb)
    
    return(herb)
}

#' @title scale_color_herb
#' @rdname herb_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_herb()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_herb <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "herb",
                                herb_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = herb_pal(n = n, type = type,
                                                         reverse = reverse)(8))
    }
}

#' @title scale_colour_herb
#' @rdname herb_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_herb()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_herb <- scale_color_herb

#' @title scale_fill_herb
#' @rdname herb_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_herb()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_herb <- function(n, type = "discrete",
                            reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "herb",
                                herb_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = herb_pal(n = n, type = type,
                                                        reverse = reverse)(8))
    }
}