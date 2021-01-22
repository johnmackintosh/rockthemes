electric_palette <- c(
    "#e5e4e3", "#999a9c", "#bf776f", "#7a8283", "#7f8b65",
    "#6a6a6f", "#a67730", "#715745", "#734d37","#1f1f1f"
)

#' @title electric palette
#' @description electric palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname electric_pal
#' @examples
#' library(scales)
#' show_col(electric_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

electric_pal <- function(n, type = c("discrete", "continuous"),
                       reverse = FALSE){
    electric <- electric_palette
    
    if (reverse == TRUE) {
        electric <- rev(electric)
    }
    
    if (missing(n)) {
        n <- length(electric)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(electric)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(electric)}!"))
    }
    
    electric <- switch(type,
                     continuous = grDevices::colorRampPalette(electric)(n),
                     discrete = electric[1:n])
    
    electric <- scales::manual_pal(electric)
    
    return(electric)
}

#' @title scale_color_electric
#' @rdname electric_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_electric()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_electric <- function(n, type = "discrete",
                               reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "electric",
                                electric_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = electric_pal(n = n, type = type,
                                                           reverse = reverse)(256))
    }
}

#' @title scale_colour_electric
#' @rdname electric_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_electric()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_electric <- scale_color_electric

#' @title scale_fill_electric
#' @rdname electric_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_electric()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_electric <- function(n, type = "discrete",
                              reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "electric",
                                electric_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = electric_pal(n = n, type = type,
                                                          reverse = reverse)(256))
    }
}