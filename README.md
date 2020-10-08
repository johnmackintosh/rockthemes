rockthemes
================

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/johnmackintosh/rockthemes.svg?branch=master)](https://travis-ci.com/johnmackintosh/rockthemes)
<!-- badges: end -->

## What?

This is a collection of colour palettes based on classic rock album
covers.

Although ‘rock’ may be a stretch in some cases, the albums were chosen
for their appealing covers

## Why?

Because [this repo of Metallica inspired
palettes](https://github.com/johnmackintosh/metallicaRt) has been
received quite well on various social media platforms

## Credit

[Thanks to Ryo for the tvthemes
package](https://github.com/Ryo-N7/tvthemes) which helped me get this
off the ground quickly

## I want this

Of course you do. This will not go to CRAN, so please install using the
remotes package.

``` r
#library(remotes)
#remotes::install_github("johnmackintosh/rockthemes")
library(rockthemes)
#> Warning: replacing previous import 'vctrs::data_frame' by 'tibble::data_frame'
#> when loading 'dplyr'
```

## Data Viz Friendly

These mini palettes are lush

``` r
rock_palette("californication")
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

``` r
rock_palette("coltrane")
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

``` r
rock_palette("electric")
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

``` r
rock_palette("faithnomore")
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

``` r
rock_palette("gogo")
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

``` r
m <- outer(1:20,1:20,function(x,y) sin(sqrt(x*y)/3))
cols <- rock_palette("gogo")
Lab.palette <- colorRampPalette(cols,space = "Lab")
filled.contour(m, col = Lab.palette(20))
```

![](man/figures/README-unnamed-chunk-6-2.png)<!-- -->

``` r
rock_palette("gunsnroses")
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

``` r
rock_palette("harvey")
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

``` r
rock_palette("heap")
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

``` r
rock_palette("herb")
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->

``` r
rock_palette("husker")
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->

``` r
rock_palette("maiden")
```

![](man/figures/README-unnamed-chunk-12-1.png)<!-- -->

``` r
rock_palette("metallica")
```

![](man/figures/README-unnamed-chunk-13-1.png)<!-- -->

``` r
rock_palette("miles")
```

![](man/figures/README-unnamed-chunk-14-1.png)<!-- -->

``` r
rock_palette("nevermind")
```

![](man/figures/README-unnamed-chunk-15-1.png)<!-- -->

``` r
rock_palette("oasis")
```

![](man/figures/README-unnamed-chunk-16-1.png)<!-- -->

``` r
rock_palette("swift")
```

![](man/figures/README-unnamed-chunk-17-1.png)<!-- -->

``` r
rock_palette("tencc")
```

![](man/figures/README-unnamed-chunk-18-1.png)<!-- -->
