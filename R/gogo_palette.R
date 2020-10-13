gogo_palette <- c(
    "#fbfcfa", 
    "#cba592", 
    "#93b0bf",
    "#b86c6a", 
    "#6092ab",
    "#3c69cd",
    "#2a7ac8",
    "#714141",
    "#5d4a3f",
    "#251a1a"
)

#' @title gogo palette
#' @description gogo palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname gogo_pal
#' @examples
#' library(scales)
#' show_col(gogo_pal()(5))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

gogo_pal <- function(n, type = c("discrete", "continuous"),
                      reverse = FALSE){
    gogo <- gogo_palette
    
    if (reverse == TRUE) {
        gogo <- rev(gogo)
    }
    
    if (missing(n)) {
        n <- length(gogo)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(gogo)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(gogo)}!"))
    }
    
    gogo <- switch(type,
                    continuous = grDevices::colorRampPalette(gogo)(n),
                    discrete = gogo[1:n])
    
    gogo <- scales::manual_pal(gogo)
    
    return(gogo)
}

#' @title scale_color_gogo
#' @rdname gogo_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_gogo()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_gogo <- function(n, type = "discrete",
                              reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "gogo",
                                gogo_pal(n = n, type = type,
                                          reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = gogo_pal(n = n, type = type,
                                                          reverse = reverse)(8))
    }
}

#' @title scale_colour_gogo
#' @rdname gogo_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_gogo()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_gogo <- scale_color_gogo

#' @title scale_fill_gogo
#' @rdname gogo_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_gogo()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_gogo <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "gogo",
                                gogo_pal(n = n, type = type,
                                          reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = gogo_pal(n = n, type = type,
                                                         reverse = reverse)(8))
    }
}