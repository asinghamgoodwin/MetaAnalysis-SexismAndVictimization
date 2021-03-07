Calculating inter-rater reliability
================

``` r
library(psych)
library(tidyverse)
```

Cohen’s Kappa is a measure of agreement between two raters. We’re using
it to asssess inter-rater reliability at both the title/abstract
screening and the full-text screening stage.

Some notes:

  - For each stage, we’re using a combination of our studies found
    through database search and through citation searching
  - We need to make our own agreement table and ratings lists because
    Covidence (the software we used for screening) currently does not
    have a way to manually remove duplicate studies, so we kept track of
    agreement numbers ourseles and removed duplicates from the yes/no
    counts.

# Title/abstract screening phase

Expected total number of studies: 4030

``` r
agreement1 = tibble(
  my_rownames = c("A_yes", "A_no"),
  B_yes = c(93, 21),
  B_no = c(186, 3730)
) %>% column_to_rownames(., var = "my_rownames")

knitr::kable(agreement1)
```

|        | B\_yes | B\_no |
| :----- | -----: | ----: |
| A\_yes |     93 |   186 |
| A\_no  |     21 |  3730 |

``` r
frequency_list_for_repeating1 = c(agreement1["A_yes", "B_yes"],
                                  agreement1["A_yes", "B_no"],
                                  agreement1["A_no", "B_yes"],
                                  agreement1["A_no", "B_no"])

raterA1 = rep(c(1, 1, 0, 0), frequency_list_for_repeating1)
raterB1 = rep(c(1, 0, 1, 0), frequency_list_for_repeating1)

cohen.kappa(x = cbind(raterA1, raterB1))
```

    ## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha, levels = levels)
    ## 
    ## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
    ##                  lower estimate upper
    ## unweighted kappa  0.39     0.45  0.51
    ## weighted kappa    0.39     0.45  0.51
    ## 
    ##  Number of subjects = 4030

# Full-text screening phase

Expected total number of studies: 136

``` r
agreement2 = tibble(
  my_rownames = c("A_yes", "A_no"),
  B_yes = c(49, 6),
  B_no = c(2, 79)
) %>% column_to_rownames(., var = "my_rownames")

knitr::kable(agreement2)
```

|        | B\_yes | B\_no |
| :----- | -----: | ----: |
| A\_yes |     49 |     2 |
| A\_no  |      6 |    79 |

``` r
frequency_list_for_repeating2 = c(agreement2["A_yes", "B_yes"],
                                  agreement2["A_yes", "B_no"],
                                  agreement2["A_no", "B_yes"],
                                  agreement2["A_no", "B_no"])

raterA2 = rep(c(1, 1, 0, 0), frequency_list_for_repeating2)
raterB2 = rep(c(1, 0, 1, 0), frequency_list_for_repeating2)

cohen.kappa(x = cbind(raterA2, raterB2))
```

    ## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha, levels = levels)
    ## 
    ## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
    ##                  lower estimate upper
    ## unweighted kappa  0.79     0.88  0.96
    ## weighted kappa    0.79     0.88  0.96
    ## 
    ##  Number of subjects = 136
