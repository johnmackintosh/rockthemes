californication_palette <- c(
    "#604835", 
    "#E9DCC5", 
    "#608DB6", 
    "#0F3C5E",
    "#CF5D2A"
)

#' @title californication palette
#' @description californication palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname californication_pal
#' @examples
#' library(scales)
#' show_col(californication_pal()(5))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

californication_pal <- function(n, type = c("discrete", "continuous"),
                          reverse = FALSE){
    californication <- californication_palette
    
    if (reverse == TRUE) {
        californication <- rev(californication)
    }
    
    if (missing(n)) {
        n <- length(californication)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(californication)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(californication)}!"))
    }
    
    californication <- switch(type,
                        continuous = grDevices::colorRampPalette(californication)(n),
                        discrete = californication[1:n])
    
    californication <- scales::manual_pal(californication)
    
    return(californication)
}

#' @title scale_color_californication
#' @rdname californication_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_californication()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_californication <- function(n, type = "discrete",
                                  reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "californication",
                                californication_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = californication_pal(n = n, type = type,
                                                              reverse = reverse)(8))
    }
}

#' @title scale_colour_californication
#' @rdname californication_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_californication()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_californication <- scale_color_californication

#' @title scale_fill_californication
#' @rdname californication_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_californication()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_californication <- function(n, type = "discrete",
                                 reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "californication",
                                californication_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = californication_pal(n = n, type = type,
                                                             reverse = reverse)(8))
    }
}