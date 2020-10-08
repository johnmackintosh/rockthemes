husker_palette <- c(
    "#94A643",
    "#7163B7", 
    "#2D203E", 
    "#8A3E65",
    "#D3506C"
)

#' @title husker palette
#' @description husker palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname husker_pal
#' @examples
#' library(scales)
#' show_col(husker_pal()(5))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

husker_pal <- function(n, type = c("discrete", "continuous"),
                          reverse = FALSE){
    husker <- husker_palette
    
    if (reverse == TRUE) {
        husker <- rev(husker)
    }
    
    if (missing(n)) {
        n <- length(husker)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(husker)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(husker)}!"))
    }
    
    husker <- switch(type,
                        continuous = grDevices::colorRampPalette(husker)(n),
                        discrete = husker[1:n])
    
    husker <- scales::manual_pal(husker)
    
    return(husker)
}

#' @title scale_color_husker
#' @rdname husker_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_husker()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_husker <- function(n, type = "discrete",
                                  reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "husker",
                                husker_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = husker_pal(n = n, type = type,
                                                              reverse = reverse)(8))
    }
}

#' @title scale_colour_husker
#' @rdname husker_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_husker()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_husker <- scale_color_husker

#' @title scale_fill_husker
#' @rdname husker_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_husker()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_husker <- function(n, type = "discrete",
                                 reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "husker",
                                husker_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = husker_pal(n = n, type = type,
                                                             reverse = reverse)(8))
    }
}