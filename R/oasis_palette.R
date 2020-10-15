oasis_palette <- c(
    "#eff0eb", 
    "#c5b283",
    "#a69372",
    "#868c82",
    "#678c86",
    "#3388bd",
    "#479f8e",
    "#746464",
    "#554d40",
    "#343c46"
)

#' @title oasis palette
#' @description oasis palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname oasis_pal
#' @examples
#' library(scales)
#' show_col(oasis_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

oasis_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
    oasis <- oasis_palette
    
    if (reverse == TRUE) {
        oasis <- rev(oasis)
    }
    
    if (missing(n)) {
        n <- length(oasis)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(oasis)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(oasis)}!"))
    }
    
    oasis <- switch(type,
                   continuous = grDevices::colorRampPalette(oasis)(n),
                   discrete = oasis[1:n])
    
    oasis <- scales::manual_pal(oasis)
    
    return(oasis)
}

#' @title scale_color_oasis
#' @rdname oasis_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_oasis()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_oasis <- function(n, type = "discrete",
                             reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "oasis",
                                oasis_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = oasis_pal(n = n, type = type,
                                                         reverse = reverse)(8))
    }
}

#' @title scale_colour_oasis
#' @rdname oasis_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_oasis()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_oasis <- scale_color_oasis

#' @title scale_fill_oasis
#' @rdname oasis_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_oasis()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_oasis <- function(n, type = "discrete",
                            reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "oasis",
                                oasis_pal(n = n, type = type,
                                         reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = oasis_pal(n = n, type = type,
                                                        reverse = reverse)(8))
    }
}