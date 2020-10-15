nevermind_palette <- c(
    "#d5d9e0", 
    "#50b3dc",
    "#5a86aa",
    "#5d7ba5",
    "#2775b7",
    "#465f71",
    "#1366a1",
    "#273f79",
    "#0b1941",
    "#05070a"
)

#' @title Nevermind palette
#' @description Nevermind palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname nevermind_pal
#' @examples
#' library(scales)
#' show_col(nevermind_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

nevermind_pal <- function(n, type = c("discrete", "continuous"),
                              reverse = FALSE){
    nevermind <- nevermind_palette
    
    if (reverse == TRUE) {
        nevermind <- rev(nevermind)
    }
    
    if (missing(n)) {
        n <- length(nevermind)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(nevermind)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(nevermind)}!"))
    }
    
    nevermind <- switch(type,
                            continuous = grDevices::colorRampPalette(nevermind)(n),
                            discrete = nevermind[1:n])
    
    nevermind <- scales::manual_pal(nevermind)
    
    return(nevermind)
}

#' @title scale_color_nevermind
#' @rdname nevermind_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_nevermind()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_nevermind <- function(n, type = "discrete",
                                      reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "nevermind",
                                nevermind_pal(n = n, type = type,
                                                  reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = nevermind_pal(n = n, type = type,
                                                                  reverse = reverse)(8))
    }
}

#' @title scale_colour_nevermind
#' @rdname nevermind_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_nevermind()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_nevermind <- scale_color_nevermind

#' @title scale_fill_nevermind
#' @rdname nevermind_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_nevermind()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_nevermind <- function(n, type = "discrete",
                                     reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "nevermind",
                                nevermind_pal(n = n, type = type,
                                                  reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = nevermind_pal(n = n, type = type,
                                                                 reverse = reverse)(8))
    }
}