rock_palettes <- list(
    alice = c("#F5A631","#E84C0D","#8D130C","#4A1010"),
    californication = c("#BB5C33", "#15649C", "#E3D6BB", "#7296B8"),
    coltrane = c("#053138","#22A1B6","#0B8CA9","#AEC7BE"),
    deelite = c("#48448e","#8ccc58", "#fc4d97", "#b82578"),
    electric = c("#626063","#E9DABA","#CF8B64","#5E98C6"),
    facelift = c("#a2da3c","#c12a45","#8b4f82","#44486d"),
    faithnomore = c("#D2BAAA", "#3B2320", "#C02F28", "#D6B63A"),
    harvey = c("#C6D4D6", "#274C4F", "#A4432D", "#B17C51"),
    heep = c("#466E9A", "#E84D45", "#21201C", "#A1BAAC"),
    hellawaits = c("#995026", "#281D13", "#D7C698", "#AC9D9A"),
    husker = c("#422537", "#D58078","#86A556", "#624FB0"),
    janelle = c("#ab9ed4","#d6e055", "#cb6f42", "#7d2d3b"),
    melloncollie = c("#484A5A", "#C69648", "#9BA48C", "#949CAC"),
    miles = c("#C1C6CF", "#180C18", "#933450", "#D99755"),
    muse = c("#3B1A35","#D27F9A","#885EA5","#9D94B3"),
    nodoubt = c("#385581", "#6dbac6", "#dac190", "#c9052c"),
    peacesells = c("#9A5155", "#483943", "#ECC463", "#A497B2"),
    secondlaw = c("#505797","#58958C","#9D7382","#100F11"),
    siamesedream = c("#F0E9E0","#2F1F16","#A34D2C", "#CD8E40"),
    swift = c("#CDC1B1","#444550","#865242", "#A26847"),
    zeppelin = c("#EFDB15","#739C9C","#0D484C","#C71A08")
)



#' Color Palettes based on classic rock album covers
#'
#' R package that contains color palettes based on colours on classic rock album covers.
#' 
#' 
#' See also: https://github.com/johnmackintosh/metallicaRt for metallica palettes
#'
#' @param name Name of palette. Select one:
#' \code{alice}, \code{californication}, \code{coltrane}, \code{deelite}, 
#' \code{electric}, \code{facelift}, \code{faithnomore}, \code{harvey},
#' \code{heep}, \code{hellawaits}, \code{husker}, \code{janelle},
#' \code{melloncollie},\code{miles}, \code{muse}, \code{nodoubt}, 
#' \code{peacesells},\code{secondlaw}, \code{siamesedream}, \code{swift},
#'  \code{zeppelin}
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
}
