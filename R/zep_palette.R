ledzep_palette <- c(
    "#5B837A",
    "#2E2F44", 
    "#F2CB18", 
    "#009496",
    "#312F41",
    "#409A8E",
    "#549D89",
    "#E2C42E",
    "#D81D24",
    "#E69023"
)

#' @title ledzep_palette
#' @description Led Zeppelin - Celebration Day colour palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname ledzep_pal
#' @examples
#' library(scales)
#' show_col(ledzep_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

ledzep_pal <- function(n, type = c("discrete", "continuous"),
                                reverse = FALSE){
    ledzep <- ledzep_palette
    
    if (reverse == TRUE) {
        ledzep <- rev(ledzep)
    }
    
    if (missing(n)) {
        n <- length(ledzep)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(ledzep)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(ledzep)}!"))
    }
    
    ledzep <- switch(type,
                              continuous = grDevices::colorRampPalette(ledzep)(n),
                              discrete = ledzep[1:n])
    
    ledzep <- scales::manual_pal(ledzep)
    
    return(ledzep)
}

#' @title scale_color_ledzep
#' @rdname ledzep_pal
#' @export
#' @examples
#'
#'library(ggplot2)
#'ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'     scale_color_ledzep()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_ledzep <- function(n, type = "discrete",
                                        reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "ledzep",
                                ledzep_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = ledzep_pal(n = n, type = type,
                                                                    reverse = reverse)(256))
    }
}

#' @title scale_colour_ledzep
#' @rdname ledzep_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_ledzep()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_ledzep <- scale_color_ledzep

#' @title scale_fill_ledzep
#' @rdname ledzep_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_ledzep()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_ledzep <- function(n, type = "discrete",
                                       reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "ledzep",
                                ledzep_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = ledzep_pal(n = n, type = type,
                                                                   reverse = reverse)(256))
    }
}