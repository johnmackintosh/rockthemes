# Contributing to rockthemes

This outlines how to propose a change to rockthemes. 


## Submitting a palette

If you want to suggest a palette, please file an issue with the artist, album title and a link to the album cover. 


### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("johnmackintosh/rockthemes", fork = TRUE)`.

*   Install all development dependences with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.


* Changes you will need to make: 
 - Add a 4 colour palette to rock_palletes.R. 
 
I use [canva's online tool](https://www.canva.com/colors/color-palette-generator/), 
but you are welcome to choose whatever else works for you.

*Please* ensure the new palette name is entered in the correct alphabetical order in *both* the _palette list_ *and* the _param values_.  

Run ```devtools::document``` to update the package documentation


There is a minimal test in [the tests/testthat folder](/tests/testthat/test-rock_palettes.R)
Add your palette to that (just copy / paste an existing line and amend to your new palette name). Rerun the tests to ensure it passes (this will pick up errors where a palette is added but not added to the  function params)
 
 - Create a ten colour palette from the image. I used [loading.io/](https://loading.io/color/random/) with Count set to 10 (I left all the other options at their default, ticked settings). 
 
 - Add new "album_palette.R" file where you substitute the palette name, using one of the existing ones as a guide. There are 38 instances that will need updating in each palette file.  
 

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.


### Code style


*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We will use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the rockthemes project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
