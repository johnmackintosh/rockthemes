melloncollie_palette <- c(
    "#c69648",
    "#44485d",
    "#85725a",
    "#201d1a",
    "#68757c",
    "#5c6a84",
    "#9ba48c",
    "#ae4329",
    "#1b1e2c",
    "#949cac"
)

#' @title melloncollie palette
#' @description melloncollie palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname melloncollie_pal
#' @examples
#' library(scales)
#' show_col(melloncollie_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

melloncollie_pal <- function(n, type = c("discrete", "continuous"),
                                reverse = FALSE){
    melloncollie <- melloncollie_palette
    
    if (reverse == TRUE) {
        melloncollie <- rev(melloncollie)
    }
    
    if (missing(n)) {
        n <- length(melloncollie)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(melloncollie)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(melloncollie)}!"))
    }
    
    melloncollie <- switch(type,
                              continuous = grDevices::colorRampPalette(melloncollie)(n),
                              discrete = melloncollie[1:n])
    
    melloncollie <- scales::manual_pal(melloncollie)
    
    return(melloncollie)
}

#' @title scale_color_melloncollie
#' @rdname melloncollie_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_melloncollie()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_melloncollie <- function(n, type = "discrete",
                                        reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "melloncollie",
                                melloncollie_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = melloncollie_pal(n = n, type = type,
                                                                    reverse = reverse)(256))
    }
}

#' @title scale_colour_melloncollie
#' @rdname melloncollie_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_melloncollie()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_melloncollie <- scale_color_melloncollie

#' @title scale_fill_melloncollie
#' @rdname melloncollie_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_melloncollie()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_melloncollie <- function(n, type = "discrete",
                                       reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "melloncollie",
                                melloncollie_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = melloncollie_pal(n = n, type = type,
                                                                   reverse = reverse)(256))
    }
}