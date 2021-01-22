janelle_palette <- c(
    "#e5d7cc", 
    "#b0bebe", 
    "#a89cc4", 
    "#f6b663",
    "#d66852",
    "#ac5964",
    "#595564",
    "#8e1217",
    "#851a5a",
    "#3e2637"
)

#' @title janelle palette
#' @description janelle palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname janelle_pal
#' @examples
#' library(scales)
#' show_col(janelle_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

janelle_pal <- function(n, type = c("discrete", "continuous"),
                                reverse = FALSE){
    janelle <- janelle_palette
    
    if (reverse == TRUE) {
        janelle <- rev(janelle)
    }
    
    if (missing(n)) {
        n <- length(janelle)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(janelle)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(janelle)}!"))
    }
    
    janelle <- switch(type,
                              continuous = grDevices::colorRampPalette(janelle)(n),
                              discrete = janelle[1:n])
    
    janelle <- scales::manual_pal(janelle)
    
    return(janelle)
}

#' @title scale_color_janelle
#' @rdname janelle_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_janelle()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_janelle <- function(n, type = "discrete",
                                        reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "janelle",
                                janelle_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = janelle_pal(n = n, type = type,
                                                                    reverse = reverse)(256))
    }
}

#' @title scale_colour_janelle
#' @rdname janelle_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_janelle()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_janelle <- scale_color_janelle

#' @title scale_fill_janelle
#' @rdname janelle_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_janelle()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_janelle <- function(n, type = "discrete",
                                       reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "janelle",
                                janelle_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = janelle_pal(n = n, type = type,
                                                                   reverse = reverse)(256))
    }
}