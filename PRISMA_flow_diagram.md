PRISMA article inclusion diagram
================

``` r
#install.packages("PRISMAstatement")
library(PRISMAstatement)
library(DiagrammeRsvg)
library(rsvg)
```

Here is an example from
<https://cran.r-project.org/web/packages/PRISMAstatement/vignettes/PRISMA.html>
:

``` r
prsm <- prisma(found = 750,
               found_other = 123,
               no_dupes = 776,
               screened = 776,
               screen_exclusions = 13,
               full_text = 763,
               full_text_exclusions = 17,
               qualitative = 746,
               quantitative = 319,
               width = 200, height = 200,
               dpi = 36)
PRISMAstatement:::prisma_pdf(prsm, "test.pdf")
knitr::include_graphics("test.pdf")
```

![](test.pdf)<!-- -->
