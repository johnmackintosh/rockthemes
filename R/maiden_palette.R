maiden_palette <- c(
    "#c6e4ec", "#dae3c7", "#97b7c1", "#68a0d6", "#6195cb",
    "#828282", "#868b72", "#7e6e52", "#4c5777", "#205970"
)

#' @title maiden palette
#' @description maiden palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname maiden_pal
#' @examples
#' library(scales)
#' show_col(maiden_pal()(5))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

maiden_pal <- function(n, type = c("discrete", "continuous"),
                       reverse = FALSE){
    maiden <- maiden_palette
    
    if (reverse == TRUE) {
        maiden <- rev(maiden)
    }
    
    if (missing(n)) {
        n <- length(maiden)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(maiden)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(maiden)}!"))
    }
    
    maiden <- switch(type,
                     continuous = grDevices::colorRampPalette(maiden)(n),
                     discrete = maiden[1:n])
    
    maiden <- scales::manual_pal(maiden)
    
    return(maiden)
}

#' @title scale_color_maiden
#' @rdname maiden_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_maiden()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_maiden <- function(n, type = "discrete",
                               reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "maiden",
                                maiden_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = maiden_pal(n = n, type = type,
                                                           reverse = reverse)(8))
    }
}

#' @title scale_colour_maiden
#' @rdname maiden_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_maiden()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_maiden <- scale_color_maiden

#' @title scale_fill_maiden
#' @rdname maiden_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_maiden()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_maiden <- function(n, type = "discrete",
                              reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "maiden",
                                maiden_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = maiden_pal(n = n, type = type,
                                                          reverse = reverse)(8))
    }
}