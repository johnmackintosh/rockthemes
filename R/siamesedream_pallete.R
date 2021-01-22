siamesedream_palette <- c(
    "#2f1f16",
    "#cd8e40",
    "#a34d2c",
    "#f4efec",
    "#e4ca97",
    "#9b8984",
    "#cea585",
    "#846c74",
    "#6c8436",
    "#c9bfbd"
    )

#' @title siamesedream palette
#' @description siamesedream palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname siamesedream_pal
#' @examples
#' library(scales)
#' show_col(siamesedream_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

siamesedream_pal <- function(n, type = c("discrete", "continuous"),
                             reverse = FALSE){
    siamesedream <- siamesedream_palette
    
    if (reverse == TRUE) {
        siamesedream <- rev(siamesedream)
    }
    
    if (missing(n)) {
        n <- length(siamesedream)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(siamesedream)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(siamesedream)}!"))
    }
    
    siamesedream <- switch(type,
                           continuous = grDevices::colorRampPalette(siamesedream)(n),
                           discrete = siamesedream[1:n])
    
    siamesedream <- scales::manual_pal(siamesedream)
    
    return(siamesedream)
}

#' @title scale_color_siamesedream
#' @rdname siamesedream_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_siamesedream()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_siamesedream <- function(n, type = "discrete",
                                     reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "siamesedream",
                                siamesedream_pal(n = n, type = type,
                                                 reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = siamesedream_pal(n = n, type = type,
                                                                 reverse = reverse)(256))
    }
}

#' @title scale_colour_siamesedream
#' @rdname siamesedream_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_siamesedream()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_siamesedream <- scale_color_siamesedream

#' @title scale_fill_siamesedream
#' @rdname siamesedream_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_siamesedream()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_siamesedream <- function(n, type = "discrete",
                                    reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "siamesedream",
                                siamesedream_pal(n = n, type = type,
                                                 reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = siamesedream_pal(n = n, type = type,
                                                                reverse = reverse)(256))
    }
}