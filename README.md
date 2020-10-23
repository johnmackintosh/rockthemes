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
rock album covers and the internet is never wrong, is it ;)

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

## Abbey Road

Inspiration

# <a href="https://en.wikipedia.org/wiki/File:Beatles_-_Abbey_Road.jpg#/media/File:Beatles_-_Abbey_Road.jpg"><img src="https://upload.wikimedia.org/wikipedia/en/4/42/Beatles_-_Abbey_Road.jpg" alt="The cover of Abbey Road has no printed words. It is a photo of the Beatles, in side view, crossing the street in single file." width="160px" align="left"></a>

<br><span style="font-size: 10px;"> By Source,
<a href="//en.wikipedia.org/wiki/File:Beatles_-_Abbey_Road.jpg" title="Fair use of copyrighted material in the context of Abbey Road">Fair
use</a>,
<a href="https://en.wikipedia.org/w/index.php?curid=4897516">Link</a></span>

``` r
rock_palette("abbeyroad")
```

![](man/figures/README-abbeyroad-1.png)<!-- -->

## Californication

Inspiration

# <img src="man/figures/californication.png" width="160px" align="left" />

``` r
rock_palette("californication")
```

![](man/figures/README-californication-1.png)<!-- -->

## Coltrane

Inspiration

# <img src="man/figures/coltrane.png" width="160px" align="left" />

``` r
rock_palette("coltrane")
```

![](man/figures/README-coltrane-1.png)<!-- -->

## Electric

Inspiration

# <img src="man/figures/electric.png" width="160px" align="left" />

``` r
rock_palette("electric")
```

![](man/figures/README-electric-1.png)<!-- -->

## Faith No More

Inspiration

# <img src="man/figures/realthing.png" width="160px" align="left" />

``` r
rock_palette("faithnomore")
```

![](man/figures/README-faithnomore-1.png)<!-- -->

## Go Gos

Inspiration

# <img src="man/figures/gogos.png" width="160px" align="left" />

``` r
rock_palette("gogo")
```

![](man/figures/README-gogo-1.png)<!-- -->

## Guns N’ Roses

Inspiration

# <img src="man/figures/gunsnroses.png" width="160px" align="left" />

``` r
rock_palette("gunsnroses")
```

![](man/figures/README-gnr-1.png)<!-- -->

## PJ Harvey

Inspiration

# <img src="man/figures/harvey.png" width="160px" align="left" />

``` r
rock_palette("harvey")
```

![](man/figures/README-harvey-1.png)<!-- -->

## Uria Heep

Inspiration

# <img src="man/figures/heep.png" width="160px" align="left" />

``` r
rock_palette("heep")
```

![](man/figures/README-heep-1.png)<!-- -->

## Hell Awaits - Slayer

Inspiration

# <img src="man/figures/hellawaits.png" width="160px" align="left" />

``` r
rock_palette("hellawaits")
```

![](man/figures/README-slayer-1.png)<!-- -->

## Husker Du

Inspiration

# <img src="man/figures/husker.png" width="160px" align="left" />

``` r
rock_palette("husker")
```

![](man/figures/README-husker-1.png)<!-- -->

## Janelle Monae

Inspiration

# <img src="man/figures/janelle.png" width="160px" align="left" />

``` r
rock_palette("janelle")
```

![](man/figures/README-janelle-1.png)<!-- -->

## Iron Maiden

Inspiration

# <img src="man/figures/maiden.png" width="160px" align="left" />

``` r
rock_palette("maiden")
```

![](man/figures/README-maiden-1.png)<!-- -->

## Mellon Collie and the Infitine Sadness

Inspiration

# <a href="https://en.wikipedia.org/wiki/File:The_Smashing_Pumpkins_-_Mellon_Collie_And_The_infinite_Sadness.jpg#/media/File:The_Smashing_Pumpkins_-_Mellon_Collie_And_The_infinite_Sadness.jpg"><img src="https://upload.wikimedia.org/wikipedia/en/7/76/The_Smashing_Pumpkins_-_Mellon_Collie_And_The_infinite_Sadness.jpg" alt="A painting of women in outer space" width="160px" align="left"></a>

<br><span style="font-size: 10px;"> By
<span title="must have been published or publicly displayed outside Wikipedia">Source</span>
(<a href="//en.wikipedia.org/wiki/Wikipedia:Non-free_content_criteria#4" title="Wikipedia:Non-free content criteria">WP:NFCC\#4</a>),
<a href="//en.wikipedia.org/wiki/File:The_Smashing_Pumpkins_-_Mellon_Collie_And_The_infinite_Sadness.jpg" title="Fair use of copyrighted material in the context of Mellon Collie and the Infinite Sadness">Fair
use</a>,
<a href="https://en.wikipedia.org/w/index.php?curid=64531870">Link</a></span>

``` r
rock_palette("melloncollie")
```

![](man/figures/README-melloncollie-1.png)<!-- -->

## Metallica

Inspiration

# <img src="man/figures/metallica.png" width="160px" align="left" />

``` r
rock_palette("metallica")
```

![](man/figures/README-metallica-1.png)<!-- -->

## Miles Davis

Inspiration

# <img src="man/figures/miles.png" width="160px" align="left" />

``` r
rock_palette("miles")
```

![](man/figures/README-miles-1.png)<!-- -->

## Muse

Inspiration

# <img src="man/figures/muse.png" width="160px" align="left" />

``` r
rock_palette("muse")
```

![](man/figures/README-muse-1.png)<!-- -->

## Nirvana

Inspiration

# <img src="man/figures/nirvana.png" width="160px" align="left" />

``` r
rock_palette("nevermind")
```

![](man/figures/README-nirvana-1.png)<!-- -->

## No Doubt

Inspiration

# <img src="man/figures/nodoubt.png" width="160px" align="left" />

``` r
rock_palette("nodoubt")
```

![](man/figures/README-nodoubt-1.png)<!-- -->

## Oasis

Inspiration

# <img src="man/figures/oasis.png" width="160px" align="left" />

``` r
rock_palette("oasis")
```

![](man/figures/README-oasis-1.png)<!-- -->

## Peace Sells - Megadeth

Inspiration

# <img src="man/figures/peacesells.png" width="160px" align="left" />

``` r
rock_palette("peacesells")
```

![](man/figures/README-peacesells-1.png)<!-- -->

## Siamese Dream

Inspiration

# <a href="https://en.wikipedia.org/wiki/File:SmashingPumpkins-SiameseDream.jpg#/media/File:SmashingPumpkins-SiameseDream.jpg"><img src="https://upload.wikimedia.org/wikipedia/en/4/44/SmashingPumpkins-SiameseDream.jpg" alt="SmashingPumpkins-SiameseDream.jpg" width="160px" align="left"></a>

<br><span style="font-size: 10px;"> By Source,
<a href="//en.wikipedia.org/wiki/File:SmashingPumpkins-SiameseDream.jpg" title="Fair use of copyrighted material in the context of Siamese Dream">Fair
use</a>,
<a href="https://en.wikipedia.org/w/index.php?curid=224244">Link</a></span>

``` r
rock_palette("siamesedream")
```

![](man/figures/README-siamesedream-1.png)<!-- -->

## Taylor Swift

Inspiration

# <img src="man/figures/taylor.png" width="160px" align="left" />

``` r
rock_palette("swift")
```

![](man/figures/README-swift-1.png)<!-- -->

## 10CC

Inspiration

# <img src="man/figures/tencc.png" width="160px" align="left" />

``` r
rock_palette("tencc")
```

![](man/figures/README-tencc-1.png)<!-- -->

## Longer colour palettes, more suited for ggplot2 use

The following palettes share the same inspirations, but there are more
colours, which hopefully increases their utility for data visualisation.

``` r
show_col(abbeyroad_pal()(10))
```

![](man/figures/README-abb10-1.png)<!-- -->

``` r
show_col(californication_pal()(10))
```

![](man/figures/README-cal10-1.png)<!-- -->

``` r
show_col(coltrane_pal()(10))
```

![](man/figures/README-coltrane10-1.png)<!-- -->

``` r
show_col(electric_pal()(10))
```

![](man/figures/README-electric10-1.png)<!-- -->

``` r
show_col(gogo_pal()(10))
```

![](man/figures/README-gogo10-1.png)<!-- -->

``` r
show_col(gunsnroses_pal()(10))
```

![](man/figures/README-gnr10-1.png)<!-- -->

``` r
show_col(harvey_pal()(10))
```

![](man/figures/README-harvey10-1.png)<!-- -->

``` r
show_col(heep_pal()(10))
```

![](man/figures/README-heep10-1.png)<!-- -->

``` r
show_col(hellawaits_pal()(10))
```

![](man/figures/README-slayer10-1.png)<!-- -->

``` r
show_col(husker_pal()(10))
```

![](man/figures/README-husker10-1.png)<!-- -->

``` r
show_col(janelle_pal()(10))
```

![](man/figures/README-janell10-1.png)<!-- -->

``` r
show_col(maiden_pal()(10))
```

![](man/figures/README-maiden10-1.png)<!-- -->

``` r
show_col(melloncollie_pal()(10))
```

![](man/figures/README-melloncollie10-1.png)<!-- -->

``` r
show_col(metallica_pal()(10))
```

![](man/figures/README-metallica10-1.png)<!-- -->

``` r
show_col(miles_pal()(10))
```

![](man/figures/README-miles10-1.png)<!-- -->

``` r
show_col(muse_pal()(10))
```

![](man/figures/README-muse10-1.png)<!-- -->

``` r
show_col(nevermind_pal()(10))
```

![](man/figures/README-nevermind10-1.png)<!-- -->

``` r
show_col(nodoubt_pal()(10))
```

![](man/figures/README-nodoubt10-1.png)<!-- -->

``` r
show_col(oasis_pal()(10))
```

![](man/figures/README-oasis10-1.png)<!-- -->

``` r
show_col(peacesells_pal()(10))
```

![](man/figures/README-megadeth10-1.png)<!-- -->

``` r
show_col(real_thing_pal()(10))
```

![](man/figures/README-faithnomore10-1.png)<!-- -->

``` r
show_col(siamesedream_pal()(10))
```

![](man/figures/README-siamesedream10-1.png)<!-- -->

``` r
show_col(taylor_pal()(10))
```

![](man/figures/README-taylor10-1.png)<!-- -->

``` r
show_col(tencc_pal()(10))
```

![](man/figures/README-tencc10-1.png)<!-- -->

## Credit

[Thanks to Ryo for the tvthemes
package](https://github.com/Ryo-N7/tvthemes) which helped me get this
off the ground quickly

## Code of Conduct

Please note that the rockthemes project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project
you agree to abide by its terms.

## Contributing

See the [Contribution guide](.github/CONTRIBUTING.md)

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

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

``` r
    
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_husker()
```

![](man/figures/README-unnamed-chunk-2-2.png)<!-- -->

``` r
    
    
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_janelle()
```

![](man/figures/README-unnamed-chunk-2-3.png)<!-- -->

``` r
    
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_melloncollie()
```

![](man/figures/README-unnamed-chunk-2-4.png)<!-- -->

``` r
  
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_muse()
```

![](man/figures/README-unnamed-chunk-2-5.png)<!-- -->

``` r
    
    ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
    geom_area(alpha = 0.8) +
    scale_x_date(breaks = data$year, date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    scale_fill_nodoubt()
```

![](man/figures/README-unnamed-chunk-2-6.png)<!-- -->
