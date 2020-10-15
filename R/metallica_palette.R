metallica_palette <- c(
    "#d6cbc0", 
    "#aea6ad", 
    "#878f94", 
    "#eb9621",
    "#cf8728",
    "#726448",
    "#704647",
    "#a50b0a",
    "#532912",
    "#28070c"
)

#' @title metallica palette
#' @description metallica palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname metallica_pal
#' @examples
#' library(scales)
#' show_col(metallica_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

metallica_pal <- function(n, type = c("discrete", "continuous"),
                       reverse = FALSE){
    metallica <- metallica_palette
    
    if (reverse == TRUE) {
        metallica <- rev(metallica)
    }
    
    if (missing(n)) {
        n <- length(metallica)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(metallica)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(metallica)}!"))
    }
    
    metallica <- switch(type,
                     continuous = grDevices::colorRampPalette(metallica)(n),
                     discrete = metallica[1:n])
    
    metallica <- scales::manual_pal(metallica)
    
    return(metallica)
}

#' @title scale_color_metallica
#' @rdname metallica_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_metallica()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_metallica <- function(n, type = "discrete",
                               reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "metallica",
                                metallica_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = metallica_pal(n = n, type = type,
                                                           reverse = reverse)(8))
    }
}

#' @title scale_colour_metallica
#' @rdname metallica_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_metallica()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_metallica <- scale_color_metallica

#' @title scale_fill_metallica
#' @rdname metallica_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_metallica()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_metallica <- function(n, type = "discrete",
                              reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "metallica",
                                metallica_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = metallica_pal(n = n, type = type,
                                                          reverse = reverse)(8))
    }
}