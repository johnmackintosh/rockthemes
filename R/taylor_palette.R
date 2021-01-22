taylor_palette <- c(
    "#ebebe9", 
    "#d3d8c6", 
    "#dfc7b7", 
    "#d0c2ac", 
    "#9c9496",
    "#b85f46",
    "#a07051",
    "#845a77",
    "#795148",
    "#2f333f"
)

#' @title taylor palette
#' @description taylor palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname taylor_pal
#' @examples
#' library(scales)
#' show_col(taylor_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

taylor_pal <- function(n, type = c("discrete", "continuous"),
                      reverse = FALSE){
    taylor <- taylor_palette
    
    if (reverse == TRUE) {
        taylor <- rev(taylor)
    }
    
    if (missing(n)) {
        n <- length(taylor)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(taylor)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(taylor)}!"))
    }
    
    taylor <- switch(type,
                    continuous = grDevices::colorRampPalette(taylor)(n),
                    discrete = taylor[1:n])
    
    taylor <- scales::manual_pal(taylor)
    
    return(taylor)
}

#' @title scale_color_taylor
#' @rdname taylor_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_taylor()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_taylor <- function(n, type = "discrete",
                              reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "taylor",
                                taylor_pal(n = n, type = type,
                                          reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = taylor_pal(n = n, type = type,
                                                          reverse = reverse)(256))
    }
}

#' @title scale_colour_taylor
#' @rdname taylor_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_taylor()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_taylor <- scale_color_taylor

#' @title scale_fill_taylor
#' @rdname taylor_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_taylor()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_taylor <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "taylor",
                                taylor_pal(n = n, type = type,
                                          reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = taylor_pal(n = n, type = type,
                                                         reverse = reverse)(256))
    }
}