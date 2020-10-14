rock_palettes <- list(
    californication = c("#BB5C33", "#15649C", "#E3D6BB", "#7296B8"),
    coltrane = c("#053138","#22A1B6","#0B8CA9","#AEC7BE"),
    electric = c("#626063","#E9DABA","#CF8B64","#5E98C6"),
    faithnomore = c("#D2BAAA", "#3B2320", "#C02F28", "#D6B63A"),
    gogo = c("#C4C9C8", "#259BDA", "#B26E47", "#87492F"),
    gunsnroses = c("#100F12", "#C28B68", "#6877A7", "#729094"),
    harvey = c("#C6D4D6", "#274C4F", "#A4432D", "#B17C51"),
    heap = c("#466E9A", "#E84D45", "#21201C", "#A1BAAC"),
    herb = c("#5A5621", "#DAD2B8", "#7EC634", "#A4A897"),
    husker = c("#422537", "#D58078","#86A556", "#624FB0"),
    janelle = c("#916364","#E2D6BF", "#21121B", "#B3ACC7"),
    maiden = c("#8FBBCE", "#265F8A", "#78704B", "#4A5372"),
    metallica = c("#652113", "#D8CABC","#D77021", "#9E8C99"),
    miles = c("#C1C6CF", "#180C18", "#933450", "#D99755"),
    muse = c("#3B1A35","#D27F9A","#885EA5","#9D94B3"),
    nevermind = c("#B5BECE","#4DB8DA","#286B9F","#121725"),
    oasis = c("#B5AF93", "#293845", "#5B4E4D", "#685F41"),
    swift = c("#CDC1B1","#444550","#865242", "#A26847"),
    tencc = c("#FADB75", "#BDA2A8", "#AB6749", "#211710")
)


#' Color Palettes based on classic rock album covers
#'
#' R package that contains color palettes based on colours on classic rock album covers.
#' 
#' 
#' See also: https://github.com/johnmackintosh/metallicaRt for metallica palettes
#'
#' @param name Name of palette. Select one:
#' \code{californication}, \code{coltrane}, \code{electric}, \code{faithnomore}, 
#' \code{gogo}, \code{gunsnroses},\code{harvey}, \code{heap},
#' \code{herb}, \code{husker}, \code{janelle}, \code{maiden},
#' \code{metallica}, \code{miles}, \code{muse},\code{nevermind},  
#'  \code{oasis}, \code{swift}, \code{tencc}
#'
#' @param n Number of colors desired. 
#' 
#' Some palettes contain 5 colors. Those beginning with 'rock' have 4
#'
#' @param type Either continuous or discrete.
#'
#' @return A vector of colors.
#' @export
#'
#' @examples
#' rock_palette("swift")
#'
rock_palette <- function(name, n, type = c("discrete", "continuous")) {
    type <- match.arg(type)
    
    pal <- rock_palettes[[name]]
    if (is.null(pal))
        stop("Palette not found")
    
    if (missing(n)) {
        n = length(pal)
    }
    
    if (type == "discrete" && n > length(pal)) {
        stop(paste("You have requested", n, "colors, but this palette only contains", length(pal), "colors."))
    }
    
    out <- switch(type,
                  continuous = grDevices::colorRampPalette(pal)(n),
                  discrete = pal[1:n]
    )
    structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
    n <- length(x)
    old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
    on.exit(par(old))
    
    image(1:n, 1, as.matrix(1:n), col = x,
          ylab = "", xaxt = "n", yaxt = "n", bty = "n")
    
    rect(0, 0.95, n + 1, 1.05, col = rgb(1, 1, 1, 0.7), border = NA)
    text((n + 1) / 2, 1, labels = attr(x,"name"), cex = 1, family = "serif")
    for (i in 1:n) {
        rect(i - .5, .65, i + .5, .75, col = rgb(1, 1, 1, 0.7), border = NA)
        text(i, .7, labels = x[i], cex = 1, family = "serif")
    }
}