heap_palette <- c(
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

#' @title heap palette
#' @description heap palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname heap_pal
#' @examples
#' library(scales)
#' show_col(heap_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

heap_pal <- function(n, type = c("discrete", "continuous"),
                       reverse = FALSE){
    heap <- heap_palette
    
    if (reverse == TRUE) {
        heap <- rev(heap)
    }
    
    if (missing(n)) {
        n <- length(heap)
    }
    
    type <- match.arg(type)
    
    if (type == "discrete" && n > length(heap)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(heap)}!"))
    }
    
    heap <- switch(type,
                     continuous = grDevices::colorRampPalette(heap)(n),
                     discrete = heap[1:n])
    
    heap <- scales::manual_pal(heap)
    
    return(heap)
}

#' @title scale_color_heap
#' @rdname heap_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_heap()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_heap <- function(n, type = "discrete",
                               reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "heap",
                                heap_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = heap_pal(n = n, type = type,
                                                           reverse = reverse)(8))
    }
}

#' @title scale_colour_heap
#' @rdname heap_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_heap()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_heap <- scale_color_heap

#' @title scale_fill_heap
#' @rdname heap_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_heap()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_heap <- function(n, type = "discrete",
                              reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "heap",
                                heap_pal(n = n, type = type,
                                           reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = heap_pal(n = n, type = type,
                                                          reverse = reverse)(8))
    }
}