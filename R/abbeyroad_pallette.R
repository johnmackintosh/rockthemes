abbeyroad_palette <- c(
    "#dcdecc",
    "#8cbee3",
    "#779cb0",
    "#c99156",
    "#b0956c",
    "#528390",
    "#5d7a5c",
    "#51766f",
    "#45625b",
    "#12292d"
    
)

#' @title abbeyroad palette
#' @description abbeyroad palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname abbeyroad_pal
#' @examples
#' library(scales)
#' show_col(abbeyroad_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

abbeyroad_pal <- function(n, type = c("discrete", "continuous"),
                             reverse = FALSE){
    abbeyroad <- abbeyroad_palette
    
    if (reverse == TRUE) {
        abbeyroad <- rev(abbeyroad)
    }
    
    if (missing(n)) {
        n <- length(abbeyroad)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(abbeyroad)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(abbeyroad)}!"))
    }
    
    abbeyroad <- switch(type,
                           continuous = grDevices::colorRampPalette(abbeyroad)(n),
                           discrete = abbeyroad[1:n])
    
    abbeyroad <- scales::manual_pal(abbeyroad)
    
    return(abbeyroad)
}

#' @title scale_color_abbeyroad
#' @rdname abbeyroad_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_abbeyroad()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_abbeyroad <- function(n, type = "discrete",
                                     reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "abbeyroad",
                                abbeyroad_pal(n = n, type = type,
                                                 reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = abbeyroad_pal(n = n, type = type,
                                                                 reverse = reverse)(8))
    }
}

#' @title scale_colour_abbeyroad
#' @rdname abbeyroad_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_abbeyroad()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_abbeyroad <- scale_color_abbeyroad

#' @title scale_fill_abbeyroad
#' @rdname abbeyroad_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_abbeyroad()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_abbeyroad <- function(n, type = "discrete",
                                    reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "abbeyroad",
                                abbeyroad_pal(n = n, type = type,
                                                 reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = abbeyroad_pal(n = n, type = type,
                                                                reverse = reverse)(8))
    }
}