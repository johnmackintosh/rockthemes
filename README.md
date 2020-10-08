rockthemes
================

<!-- badges: start -->

[![Build Status](https://travis-ci.com/johnmackintosh/rockthemes.svg?branch=main)](https://travis-ci.com/johnmackintosh/rockthemes)
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

## ggplot2 use

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_californication() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-19-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_coltrane() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-20-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_electric() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-21-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_gogo() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-22-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_gunsnroses() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-23-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_harvey() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-24-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_heap() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-25-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_herb() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-26-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_husker() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-27-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_maiden() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-28-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_metallica() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-29-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_miles() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-30-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_nevermind() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-31-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_oasis() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-32-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_real_thing() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-33-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_taylor() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-34-1.png)<!-- -->

``` r
airquality %>% 
    mutate(Month = as.factor(Month)) %>% 
    ggplot(aes(x = Day, y = Temp, group = Month, color = Month)) +
    geom_line(size = 1.5) +
    scale_color_tencc() +
    theme_bw()
```

![](man/figures/README-unnamed-chunk-35-1.png)<!-- -->
