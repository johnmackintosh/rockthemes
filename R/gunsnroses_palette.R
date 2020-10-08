gunsnroses_palette <- c(
    "#5C6FA3", 
    "#BA6449", 
    "#685751",
    "#7D8E92",
    "#0D0C0E"
    
)

#' @title gunsnroses palette
#' @description gunsnroses palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname gunsnroses_pal
#' @examples
#' library(scales)
#' show_col(gunsnroses_pal()(5))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

gunsnroses_pal <- function(n, type = c("discrete", "continuous"),
                          reverse = FALSE){
    gunsnroses <- gunsnroses_palette
    
    if (reverse == TRUE) {
        gunsnroses <- rev(gunsnroses)
    }
    
    if (missing(n)) {
        n <- length(gunsnroses)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(gunsnroses)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(gunsnroses)}!"))
    }
    
    gunsnroses <- switch(type,
                        continuous = grDevices::colorRampPalette(gunsnroses)(n),
                        discrete = gunsnroses[1:n])
    
    gunsnroses <- scales::manual_pal(gunsnroses)
    
    return(gunsnroses)
}

#' @title scale_color_gunsnroses
#' @rdname gunsnroses_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_gunsnroses()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_gunsnroses <- function(n, type = "discrete",
                                  reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "gunsnroses",
                                gunsnroses_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = gunsnroses_pal(n = n, type = type,
                                                              reverse = reverse)(8))
    }
}

#' @title scale_colour_gunsnroses
#' @rdname gunsnroses_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_gunsnroses()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_gunsnroses <- scale_color_gunsnroses

#' @title scale_fill_gunsnroses
#' @rdname gunsnroses_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_gunsnroses()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_gunsnroses <- function(n, type = "discrete",
                                 reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "gunsnroses",
                                gunsnroses_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = gunsnroses_pal(n = n, type = type,
                                                             reverse = reverse)(8))
    }
}