dirt_palette <- c(
    "#ED631B", 
    "#DB5018", 
    "#681E10", 
    "#781D0E",
    "#110201",
    "#A33138",
    "#E7885C",
    "#F4B46E",
    "#F9DBA2",
    "#F0BB7F"
)

#' @title dirt palette
#' @description Alice In Chains - Dirt palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname dirt_pal
#' @examples
#' library(scales)
#' show_col(dirt_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

dirt_pal <- function(n, type = c("discrete", "continuous"),
                                reverse = FALSE){
    dirt <- dirt_palette
    
    if (reverse == TRUE) {
        dirt <- rev(dirt)
    }
    
    if (missing(n)) {
        n <- length(dirt)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(dirt)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(dirt)}!"))
    }
    
    dirt <- switch(type,
                              continuous = grDevices::colorRampPalette(dirt)(n),
                              discrete = dirt[1:n])
    
    dirt <- scales::manual_pal(dirt)
    
    return(dirt)
}

#' @title scale_color_dirt
#' @rdname dirt_pal
#' @export
#' @examples
#'
#'library(ggplot2)
#'ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'     scale_color_dirt()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_dirt <- function(n, type = "discrete",
                                        reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "dirt",
                                dirt_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = dirt_pal(n = n, type = type,
                                                                    reverse = reverse)(256))
    }
}

#' @title scale_colour_dirt
#' @rdname dirt_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_dirt()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_dirt <- scale_color_dirt

#' @title scale_fill_dirt
#' @rdname dirt_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_dirt()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_dirt <- function(n, type = "discrete",
                                       reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "dirt",
                                dirt_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = dirt_pal(n = n, type = type,
                                                                   reverse = reverse)(256))
    }
}