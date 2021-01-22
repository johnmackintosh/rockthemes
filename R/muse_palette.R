muse_palette <- c(
    "#e4d4d2", 
    "#cf80d5", 
    "#cb4f66", 
    "#a25c69",
    "#6e6789",
    "#ae365e",
    "#881029",
    "#5c2f5a",
    "#2f1f47",
    "#2a0d31"
)

#' @title muse palette
#' @description muse palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname muse_pal
#' @examples
#' library(scales)
#' show_col(muse_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

muse_pal <- function(n, type = c("discrete", "continuous"),
                          reverse = FALSE){
    muse <- muse_palette
    
    if (reverse == TRUE) {
        muse <- rev(muse)
    }
    
    if (missing(n)) {
        n <- length(muse)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(muse)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(muse)}!"))
    }
    
    muse <- switch(type,
                        continuous = grDevices::colorRampPalette(muse)(n),
                        discrete = muse[1:n])
    
    muse <- scales::manual_pal(muse)
    
    return(muse)
}

#' @title scale_color_muse
#' @rdname muse_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_muse()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_muse <- function(n, type = "discrete",
                                  reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "muse",
                                muse_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = muse_pal(n = n, type = type,
                                                              reverse = reverse)(256))
    }
}

#' @title scale_colour_muse
#' @rdname muse_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_muse()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_muse <- scale_color_muse

#' @title scale_fill_muse
#' @rdname muse_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_muse()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_muse <- function(n, type = "discrete",
                                 reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "muse",
                                muse_pal(n = n, type = type,
                                              reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = muse_pal(n = n, type = type,
                                                             reverse = reverse)(256))
    }
}