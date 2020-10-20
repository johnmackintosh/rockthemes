# Contributing to rockthemes

This outlines how to propose a change to rockthemes. 


## Submitting a palette

If you want to suggest a palette, please file an issue with the artist, album title and a small image or link to the album cover. 


### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("johnmackintosh/rockthemes", fork = TRUE)`.

*   Install all development dependences with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.


* Changes you will need to make: 
 - Add a 4 colour palette to rock_palletes.R. I use [canva's online tool](https://www.canva.com/colors/color-palette-generator/). Please ensure the new palette name is entered in the correct alphabetical order in both the palette list and the param values  
 
 - Create a ten colour palette from the image. I used [loading.io/](https://loading.io/color/random/) with variance set  to around 20 and Count set to 10 (I left all the other options at their default, ticked settings). 
 
 - Add new "album_palette.R" file where you substitute the palette name, using one of the existing ones as a guide. There are 38 instances that will need updating in each palette file.  
 

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

### Code style


*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the rockthemes project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
