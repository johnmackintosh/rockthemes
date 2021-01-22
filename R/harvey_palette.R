harvey_palette <- c(
    "#efefef",
    "#aac2ce",
    "#b9ab90", 
    "#cbb87d",
    "#719fb1",
    "#5e98b2",
    "#b76c41",
    "#49909c",
    "#8a211d",
    "#5d2115"
)

#' @title harvey palette
#' @description harvey palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname harvey_pal
#' @examples
#' library(scales)
#' show_col(harvey_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

harvey_pal <- function(n, type = c("discrete", "continuous"),
                          reverse = FALSE){
    harvey <- harvey_palette
    
    if (reverse == TRUE) {
        harvey <- rev(harvey)
    }
    
    if (missing(n)) {
        n <- length(harvey)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(harvey)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(harvey)}!"))
    }
    
    harvey <- switch(type,
                        continuous = grDevices::colorRampPalette(harvey)(n),
                        discrete = harvey[1:n])
    
    harvey <- scales::manual_pal(harvey)
    
    return(harvey)
}

#' @title scale_color_harvey
#' @rdname harvey_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_harvey()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_harvey <- function(n, type = "discrete",
                                  reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "harvey",
                                harvey_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = harvey_pal(n = n, type = type,
                                                              reverse = reverse)(256))
    }
}

#' @title scale_colour_harvey
#' @rdname harvey_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_harvey()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_harvey <- scale_color_harvey

#' @title scale_fill_harvey
#' @rdname harvey_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_harvey()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_harvey <- function(n, type = "discrete",
                                 reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "harvey",
                                harvey_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = harvey_pal(n = n, type = type,
                                                             reverse = reverse)(256))
    }
}
