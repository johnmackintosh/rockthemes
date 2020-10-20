rockthemes
================

# <img src="man/figures/logo.png" width="160px" align="right" />

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/johnmackintosh/rockthemes.svg?branch=main)](https://travis-ci.com/johnmackintosh/rockthemes)

![R-CMD-check](https://github.com/johnmackintosh/rockthemes/workflows/R-CMD-check/badge.svg)

![Render
README](https://github.com/johnmackintosh/rockthemes/workflows/Render%20README/badge.svg)

![pkgdown](https://github.com/johnmackintosh/metallicaRt/workflows/pkgdown/badge.svg)

<!-- badges: end -->

## What?

This is a collection of colour palettes based on classic rock album
covers.

Not all of the artists are ‘rock’, but they appeared in lists of classic
rock album covers and the internet is never wrong.

The albums were chosen either for their striking covers (in terms of
colour), or simply, because they are bona fide rock classics.

Your job is to guess which is which.

## Why?

Because [this repo of Metallica inspired
palettes](https://github.com/johnmackintosh/metallicaRt) has been
received quite well on various social media platforms, and I figured
that there were other albums with interesting covers that might provide
more scope for data visualisation purposes.

## Installation

This will probably not go to CRAN, so please install using the remotes
package.

``` r
#library(remotes)
#remotes::install_github("johnmackintosh/rockthemes")
library(rockthemes)
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(scales)
library(gapminder)
```

# Palettes and Themes

## Californication

Inspiration

# <img src="man/figures/californication.png" width="160px" align="left" />

``` r
rock_palette("californication")
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

## Coltrane

Inspiration

# <img src="man/figures/coltrane.png" width="160px" align="left" />

``` r
rock_palette("coltrane")
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

## Electric

Inspiration

# <img src="man/figures/electric.png" width="160px" align="left" />

``` r
rock_palette("electric")
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

## Faith No More

Inspiration

# <img src="man/figures/realthing.png" width="160px" align="left" />

``` r
rock_palette("faithnomore")
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

## Go Gos

Inspiration

# <img src="man/figures/gogos.png" width="160px" align="left" />

``` r
rock_palette("gogo")
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

## Guns N’ Roses

Inspiration

# <img src="man/figures/gunsnroses.png" width="160px" align="left" />

``` r
rock_palette("gunsnroses")
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

## Slayer

Inspiration

# <img src="man/figures/hellawaits.png" width="160px" align="left" />

``` r
rock_palette("hellawaits")
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

## Meagdeth

Inspiration

# <img src="man/figures/peacesells.png" width="160px" align="left" />

``` r
rock_palette("peacesells")
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

## PJ Harvey

Inspiration

# <img src="man/figures/harvey.png" width="160px" align="left" />

``` r
rock_palette("harvey")
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->

## Uria Heep

Inspiration

# <img src="man/figures/heep.png" width="160px" align="left" />

``` r
rock_palette("heap")
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->

## Husker Du

Inspiration

# <img src="man/figures/husker.png" width="160px" align="left" />

``` r
rock_palette("husker")
```

![](man/figures/README-unnamed-chunk-12-1.png)<!-- -->

## Janelle Monae

Inspiration

# <img src="man/figures/janelle.png" width="160px" align="left" />

``` r
rock_palette("janelle")
```

![](man/figures/README-unnamed-chunk-13-1.png)<!-- -->

## Iron Maiden

Inspiration

# <img src="man/figures/maiden.png" width="160px" align="left" />

``` r
rock_palette("maiden")
```

![](man/figures/README-unnamed-chunk-14-1.png)<!-- -->

## Metallica

Inspiration

# <img src="man/figures/metallica.png" width="160px" align="left" />

``` r
rock_palette("metallica")
```

![](man/figures/README-unnamed-chunk-15-1.png)<!-- -->

## Miles Davis

Inspiration

# <img src="man/figures/miles.png" width="160px" align="left" />

``` r
rock_palette("miles")
```

![](man/figures/README-unnamed-chunk-16-1.png)<!-- -->

## Muse

Inspiration

# <img src="man/figures/muse.png" width="160px" align="left" />

``` r
rock_palette("muse")
```

![](man/figures/README-unnamed-chunk-17-1.png)<!-- -->

## Nirvana

Inspiration

# <img src="man/figures/nirvana.png" width="160px" align="left" />

``` r
rock_palette("nevermind")
```

![](man/figures/README-unnamed-chunk-18-1.png)<!-- -->

## No Doubt

Inspiration

# <img src="man/figures/nodoubt.png" width="160px" align="left" />

``` r
rock_palette("nodoubt")
```

![](man/figures/README-unnamed-chunk-19-1.png)<!-- -->

## Oasis

Inspiration

# <img src="man/figures/oasis.png" width="160px" align="left" />

``` r
rock_palette("oasis")
```

![](man/figures/README-unnamed-chunk-20-1.png)<!-- -->

## Taylor Swift

Inspiration

# <img src="man/figures/taylor.png" width="160px" align="left" />

``` r
rock_palette("swift")
```

![](man/figures/README-unnamed-chunk-21-1.png)<!-- -->

## 10CC

Inspiration

# <img src="man/figures/tencc.png" width="160px" align="left" />

``` r
rock_palette("tencc")
```

![](man/figures/README-unnamed-chunk-22-1.png)<!-- -->

## Longer colour palettes, more suited for ggplot2 use

The following palettes share the same inspirations, but there are more
colours, which hopefully increases their utility for data visualisation.

``` r
show_col(californication_pal()(10))
```

![](man/figures/README-unnamed-chunk-23-1.png)<!-- -->

``` r
show_col(coltrane_pal()(10))
```

![](man/figures/README-unnamed-chunk-24-1.png)<!-- -->

``` r
show_col(electric_pal()(10))
```

![](man/figures/README-unnamed-chunk-25-1.png)<!-- -->

``` r
show_col(gogo_pal()(10))
```

![](man/figures/README-unnamed-chunk-26-1.png)<!-- -->

``` r
show_col(gunsnroses_pal()(10))
```

![](man/figures/README-unnamed-chunk-27-1.png)<!-- -->

``` r
show_col(hellawaits_pal()(10))
```

![](man/figures/README-unnamed-chunk-28-1.png)<!-- -->

``` r
show_col(peacesells_pal()(10))
```

![](man/figures/README-unnamed-chunk-29-1.png)<!-- -->

``` r
show_col(harvey_pal()(10))
```

![](man/figures/README-unnamed-chunk-30-1.png)<!-- -->

``` r
show_col(heap_pal()(10))
```

![](man/figures/README-unnamed-chunk-31-1.png)<!-- -->

``` r
show_col(husker_pal()(10))
```

![](man/figures/README-unnamed-chunk-32-1.png)<!-- -->

``` r
show_col(janelle_pal()(10))
```

![](man/figures/README-unnamed-chunk-33-1.png)<!-- -->

``` r
show_col(maiden_pal()(10))
```

![](man/figures/README-unnamed-chunk-34-1.png)<!-- -->

``` r
show_col(metallica_pal()(10))
```

![](man/figures/README-unnamed-chunk-35-1.png)<!-- -->

``` r
show_col(miles_pal()(10))
```

![](man/figures/README-unnamed-chunk-36-1.png)<!-- -->

``` r
show_col(muse_pal()(10))
```

![](man/figures/README-unnamed-chunk-37-1.png)<!-- -->

``` r
show_col(nevermind_pal()(10))
```

![](man/figures/README-unnamed-chunk-38-1.png)<!-- -->

``` r
show_col(nodoubt_pal()(10))
```

![](man/figures/README-unnamed-chunk-39-1.png)<!-- -->

``` r
show_col(oasis_pal()(10))
```

![](man/figures/README-unnamed-chunk-40-1.png)<!-- -->

``` r
show_col(real_thing_pal()(10))
```

![](man/figures/README-unnamed-chunk-41-1.png)<!-- -->

``` r
show_col(taylor_pal()(10))
```

![](man/figures/README-unnamed-chunk-42-1.png)<!-- -->

``` r
show_col(tencc_pal()(10))
```

![](man/figures/README-unnamed-chunk-43-1.png)<!-- -->

## Credit

[Thanks to Ryo for the tvthemes
package](https://github.com/Ryo-N7/tvthemes) which helped me get this
off the ground quickly

## Code of Conduct

Please note that the rockthemes project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).

By contributing to this project, you agree to abide by its terms.

## Contributing

PR’s are welcome, if you feel there is a glaring omission.

Please include an image of the album cover, relevant code.

Please try to keep artists / albums in chronological order in the
readme.

Don’t forget to update the name param of the rock\_palette function

## More ggplot2 examples

``` r
data <- gapminder::gapminder %>% 
    filter(country %in% c("France", "Germany", "Ireland", "Italy", "Japan")) %>% 
    mutate(year = as.Date(paste(year, "-01-01", sep = "", format = '%Y-%b-%d')))
    
ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_tencc()
```

![](man/figures/README-unnamed-chunk-44-1.png)<!-- -->

``` r
    
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_husker()
```

![](man/figures/README-unnamed-chunk-44-2.png)<!-- -->

``` r
    
    
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_janelle()
```

![](man/figures/README-unnamed-chunk-44-3.png)<!-- -->

``` r
  
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_muse()
```

![](man/figures/README-unnamed-chunk-44-4.png)<!-- -->

``` r
    
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_nodoubt()
```

![](man/figures/README-unnamed-chunk-44-5.png)<!-- -->
