heep_palette <- c(
    "#e7e4e3",
    "#e1959e",
    "#90b2a1",
    "#6d9dd5",
    "#4582b5",
    "#da1a27",
    "#886362",
    "#547c63",
    "#231b55",
    "#353225"
)

#' @title heep palette
#' @description heep palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname heep_pal
#' @examples
#' library(scales)
#' show_col(heep_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

heep_pal <- function(n, type = c("discrete", "continuous"),
                       reverse = FALSE){
    heep <- heep_palette
    
    if (reverse == TRUE) {
        heep <- rev(heep)
    }
    
    if (missing(n)) {
        n <- length(heep)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(heep)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(heep)}!"))
    }
    
    heep <- switch(type,
                     continuous = grDevices::colorRampPalette(heep)(n),
                     discrete = heep[1:n])
    
    heep <- scales::manual_pal(heep)
    
    return(heep)
}

#' @title scale_color_heep
#' @rdname heep_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_heep()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_heep <- function(n, type = "discrete",
                               reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "heep",
                                heep_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = heep_pal(n = n, type = type,
                                                           reverse = reverse)(256))
    }
}

#' @title scale_colour_heep
#' @rdname heep_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_heep()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_heep <- scale_color_heep

#' @title scale_fill_heep
#' @rdname heep_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_heep()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_heep <- function(n, type = "discrete",
                              reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "heep",
                                heep_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = heep_pal(n = n, type = type,
                                                          reverse = reverse)(256))
    }
}