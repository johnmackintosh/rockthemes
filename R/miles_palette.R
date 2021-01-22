miles_palette <- c(
    "#cbcbcb", 
    "#83c7f5", 
    "#d59f87", 
    "#728eb7",
    "#625fc7",
    "#a5697b",
    "#d27236",
    "#a23852",
    "#2c687e",
    "#371836"
)

#' @title miles palette
#' @description miles palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname miles_pal
#' @examples
#' library(scales)
#' show_col(miles_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

miles_pal <- function(n, type = c("discrete", "continuous"),
                      reverse = FALSE){
    miles <- miles_palette
    
    if (reverse == TRUE) {
        miles <- rev(miles)
    }
    
    if (missing(n)) {
        n <- length(miles)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(miles)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(miles)}!"))
    }
    
    miles <- switch(type,
                    continuous = grDevices::colorRampPalette(miles)(n),
                    discrete = miles[1:n])
    
    miles <- scales::manual_pal(miles)
    
    return(miles)
}

#' @title scale_color_miles
#' @rdname miles_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_miles()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_miles <- function(n, type = "discrete",
                              reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "miles",
                                miles_pal(n = n, type = type,
                                          reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = miles_pal(n = n, type = type,
                                                          reverse = reverse)(256))
    }
}

#' @title scale_colour_miles
#' @rdname miles_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_miles()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_miles <- scale_color_miles

#' @title scale_fill_miles
#' @rdname miles_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_miles()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_miles <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "miles",
                                miles_pal(n = n, type = type,
                                          reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = miles_pal(n = n, type = type,
                                                         reverse = reverse)(256))
    }
}