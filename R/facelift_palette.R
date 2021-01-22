facelift_palette <- c(
    "#b6cbd5",
    "#e9b929",
    "#d47330",
    "#8e6e96",
    "#c5347f",
    "#2e6ea0",
    "#b63d18",
    "#318d80",
    "#6a2b3a",
    "#171422"
)

#' @title facelift palette
#' @description facelift palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname facelift_pal
#' @examples
#' library(scales)
#' show_col(facelift_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

facelift_pal <- function(n, type = c("discrete", "continuous"),
                         reverse = FALSE){
    facelift <- facelift_palette
    
    if (reverse == TRUE) {
        facelift <- rev(facelift)
    }
    
    if (missing(n)) {
        n <- length(facelift)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(facelift)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(facelift)}!"))
    }
    
    facelift <- switch(type,
                       continuous = grDevices::colorRampPalette(facelift)(n),
                       discrete = facelift[1:n])
    
    facelift <- scales::manual_pal(facelift)
    
    return(facelift)
}

#' @title scale_color_facelift
#' @rdname facelift_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_facelift()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_facelift <- function(n, type = "discrete",
                                 reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "facelift",
                                facelift_pal(n = n, type = type,
                                             reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = facelift_pal(n = n, type = type,
                                                             reverse = reverse)(256))
    }
}

#' @title scale_colour_facelift
#' @rdname facelift_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_facelift()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_facelift <- scale_color_facelift

#' @title scale_fill_facelift
#' @rdname facelift_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_facelift()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_facelift <- function(n, type = "discrete",
                                reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "facelift",
                                facelift_pal(n = n, type = type,
                                             reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = facelift_pal(n = n, type = type,
                                                            reverse = reverse)(256))
    }
}