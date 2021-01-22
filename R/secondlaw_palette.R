second_law_palette <- c(
    "#963A82",
    "#0EAB4A",
    "#474095",
    "#40A7C5",
    "#149357",
    "#E2E37D",
    "#213980",
    "#D23522",
    "#328D71",
    "#E85733"
)

#' @title second_law_palette
#' @description Muse Second Law Palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname second_law_pal
#' @examples
#' library(scales)
#' show_col(second_law_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

second_law_pal <- function(n, type = c("discrete", "continuous"),
                                reverse = FALSE){
    second_law <- second_law_palette

    if (reverse == TRUE) {
        second_law <- rev(second_law)
    }

    if (missing(n)) {
        n <- length(second_law)
    }

    type <- match.arg(type)

    if (type == "discrete" && n > length(second_law)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(second_law)}!"))
    }

    second_law <- switch(type,
                              continuous = grDevices::colorRampPalette(second_law)(n),
                              discrete = second_law[1:n])

    second_law <- scales::manual_pal(second_law)

    return(second_law)
}

#' @title scale_color_second_law
#' @rdname second_law_pal
#' @export
#' @examples
#'
#'library(ggplot2)
#'ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'     scale_color_second_law()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_second_law <- function(n, type = "discrete",
                                        reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "second_law",
                                second_law_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = second_law_pal(n = n, type = type,
                                                                    reverse = reverse)(256))
    }
}

#' @title scale_colour_second_law
#' @rdname second_law_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_second_law()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_second_law <- scale_color_second_law

#' @title scale_fill_second_law
#' @rdname second_law_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_second_law()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_second_law <- function(n, type = "discrete",
                                       reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "second_law",
                                second_law_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = second_law_pal(n = n, type = type,
                                                                   reverse = reverse)(256))
    }
}
