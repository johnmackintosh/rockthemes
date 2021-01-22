peacesells_palette <- c(
    "#d06c53",
    "#6d4454",
    "#b85551",
    "#43383f",
    "#9c4f52",
    "#edc65e",
    "#70497f",
    "#977a69",
    "#bca782",
    "#8c81a3"
)

#' @title peacesells palette
#' @description peacesells palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname peacesells_pal
#' @examples
#' library(scales)
#' show_col(peacesells_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

peacesells_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
    peacesells <- peacesells_palette
    
    if (reverse == TRUE) {
        peacesells <- rev(peacesells)
    }
    
    if (missing(n)) {
        n <- length(peacesells)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(peacesells)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(peacesells)}!"))
    }
    
    peacesells <- switch(type,
                   continuous = grDevices::colorRampPalette(peacesells)(n),
                   discrete = peacesells[1:n])
    
    peacesells <- scales::manual_pal(peacesells)
    
    return(peacesells)
}

#' @title scale_color_peacesells
#' @rdname peacesells_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_peacesells()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_peacesells <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "peacesells",
                                peacesells_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = peacesells_pal(n = n, type = type,
                                                         reverse = reverse)(256))
    }
}

#' @title scale_colour_peacesells
#' @rdname peacesells_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_peacesells()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_peacesells <- scale_color_peacesells

#' @title scale_fill_peacesells
#' @rdname peacesells_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_peacesells()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_peacesells <- function(n, type = "discrete",
                            reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "peacesells",
                                peacesells_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = peacesells_pal(n = n, type = type,
                                                        reverse = reverse)(256))
    }
}