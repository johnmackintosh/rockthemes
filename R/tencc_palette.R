tencc_palette <- c(
    "#e9926d", 
    "#fcd456", 
    "#a38b95",
    "#c55e3b", 
    "#a56e4a",
    "#b48616",
    "#702d2b",
    "#444655",
    "#492c1f",
    "#2f241b"
)

#' @title tencc palette
#' @description tencc palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname tencc_pal
#' @examples
#' library(scales)
#' show_col(tencc_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

tencc_pal <- function(n, type = c("discrete", "continuous"),
                                reverse = FALSE){
    tencc <- tencc_palette
    
    if (reverse == TRUE) {
        tencc <- rev(tencc)
    }
    
    if (missing(n)) {
        n <- length(tencc)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(tencc)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(tencc)}!"))
    }
    
    tencc <- switch(type,
                              continuous = grDevices::colorRampPalette(tencc)(n),
                              discrete = tencc[1:n])
    
    tencc <- scales::manual_pal(tencc)
    
    return(tencc)
}

#' @title scale_color_tencc
#' @rdname tencc_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_tencc()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_tencc <- function(n, type = "discrete",
                                        reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "tencc",
                                tencc_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = tencc_pal(n = n, type = type,
                                                                    reverse = reverse)(8))
    }
}

#' @title scale_colour_tencc
#' @rdname tencc_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_tencc()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_tencc <- scale_color_tencc

#' @title scale_fill_tencc
#' @rdname tencc_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_tencc()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_tencc <- function(n, type = "discrete",
                                       reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "tencc",
                                tencc_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = tencc_pal(n = n, type = type,
                                                                   reverse = reverse)(8))
    }
}