hellawaits_palette <- c(
    "#9f4c23",
    "#2a1f15",
    "#cebc85",
    "#b2883c",
    "#5e371d",
    "#d78433",
    "#7b6c62",
    "#9f9796",
    "#7e1a17",
    "#444c3a"
)

#' @title hellawaits palette
#' @description hellawaits palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname hellawaits_pal
#' @examples
#' library(scales)
#' show_col(hellawaits_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

hellawaits_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
    hellawaits <- hellawaits_palette
    
    if (reverse == TRUE) {
        hellawaits <- rev(hellawaits)
    }
    
    if (missing(n)) {
        n <- length(hellawaits)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(hellawaits)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(hellawaits)}!"))
    }
    
    hellawaits <- switch(type,
                   continuous = grDevices::colorRampPalette(hellawaits)(n),
                   discrete = hellawaits[1:n])
    
    hellawaits <- scales::manual_pal(hellawaits)
    
    return(hellawaits)
}

#' @title scale_color_hellawaits
#' @rdname hellawaits_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_hellawaits()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_hellawaits <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "hellawaits",
                                hellawaits_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = hellawaits_pal(n = n, type = type,
                                                         reverse = reverse)(256))
    }
}

#' @title scale_colour_hellawaits
#' @rdname hellawaits_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_hellawaits()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_hellawaits <- scale_color_hellawaits

#' @title scale_fill_hellawaits
#' @rdname hellawaits_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_hellawaits()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_hellawaits <- function(n, type = "discrete",
                            reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "hellawaits",
                                hellawaits_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = hellawaits_pal(n = n, type = type,
                                                        reverse = reverse)(256))
    }
}