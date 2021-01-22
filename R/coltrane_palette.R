coltrane_palette <- c(
    "#b2c3be",
    "#6b9e76",
    "#2ea9d7",
    "#597172",
    "#047d8e",
    "#0e6c83",
    "#056161",
    "#043f4d",
    "#041d32",
    "#051b21"
)

#' @title coltrane palette
#' @description coltrane palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname coltrane_pal
#' @examples
#' library(scales)
#' show_col(coltrane_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

coltrane_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
    coltrane <- coltrane_palette
    
    if (reverse == TRUE) {
        coltrane <- rev(coltrane)
    }
    
    if (missing(n)) {
        n <- length(coltrane)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(coltrane)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(coltrane)}!"))
    }
    
    coltrane <- switch(type,
                   continuous = grDevices::colorRampPalette(coltrane)(n),
                   discrete = coltrane[1:n])
    
    coltrane <- scales::manual_pal(coltrane)
    
    return(coltrane)
}

#' @title scale_color_coltrane
#' @rdname coltrane_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_coltrane()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_coltrane <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "coltrane",
                                coltrane_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = coltrane_pal(n = n, type = type,
                                                         reverse = reverse)(256))
    }
}

#' @title scale_colour_coltrane
#' @rdname coltrane_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_coltrane()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_coltrane <- scale_color_coltrane

#' @title scale_fill_coltrane
#' @rdname coltrane_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_coltrane()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_coltrane <- function(n, type = "discrete",
                            reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "coltrane",
                                coltrane_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = coltrane_pal(n = n, type = type,
                                                        reverse = reverse)(256))
    }
}