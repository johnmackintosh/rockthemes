real_thing_palette <- c(
    "#e2d5cb","#bea3a8","#d7b674","#a27b74","#94797d",
    "#dc4f2e", "#c49f1e", "#7a6178", "#42420b", "#2b1c1a"
)

#' @title real_thing palette
#' @description real_thing palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname real_thing_pal
#' @examples
#' library(scales)
#' show_col(real_thing_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

real_thing_pal <- function(n, type = c("discrete", "continuous"),
                         reverse = FALSE){
    real_thing <- real_thing_palette
    
    if (reverse == TRUE) {
        real_thing <- rev(real_thing)
    }
    
    if (missing(n)) {
        n <- length(real_thing)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(real_thing)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(real_thing)}!"))
    }
    
    real_thing <- switch(type,
                       continuous = grDevices::colorRampPalette(real_thing)(n),
                       discrete = real_thing[1:n])
    
    real_thing <- scales::manual_pal(real_thing)
    
    return(real_thing)
}

#' @title scale_color_real_thing
#' @rdname real_thing_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_real_thing()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_real_thing <- function(n, type = "discrete",
                                 reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "real_thing",
                                real_thing_pal(n = n, type = type,
                                             reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = real_thing_pal(n = n, type = type,
                                                             reverse = reverse)(256))
    }
}

#' @title scale_colour_real_thing
#' @rdname real_thing_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_real_thing()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_real_thing <- scale_color_real_thing

#' @title scale_fill_real_thing
#' @rdname real_thing_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_real_thing()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_real_thing <- function(n, type = "discrete",
                                reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "real_thing",
                                real_thing_pal(n = n, type = type,
                                             reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = real_thing_pal(n = n, type = type,
                                                            reverse = reverse)(256))
    }
}