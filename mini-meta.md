Mini-meta
================

Goal: write code to conduct a mini meta-analysis (or meta regression)
using just a few of our studies, as a proof of concept and to better
understand how to work with the data and use the relevant R packages.

``` r
library(tidyverse)
library(metafor)
library(clubSandwich)
library(robumeta)
#library(kableExtra) #--> might be helpful later for making nice tables
```

## Import & clean data

``` r
study_data = read.csv("mini-meta-practice-data.csv") %>% 
  janitor::clean_names(.) %>% 
  mutate_at(., vars(all_of(c("decade_of_majority_of_data"))), factor) %>% 
  mutate_at(., vars(all_of(c("exposure_operationalization",
                             "outcome_operationalization",
                             "source_population",
                             "which_model_is_this_from_in_their_analysis_table",
                             "confounders",
                             "notes_about_measure_of_association_type"))), list(as.character)) %>% 
  rename(.,
         sample_size = sample_size_observations,
         SE = standard_deviation_error,
         MA_type = measure_of_association_type,
         MA_number = measure_of_association_number)

knitr::kable(study_data)
```

| study\_id | authors                                               | publication\_date | title                                                                                       | decade\_of\_majority\_of\_data | survey\_or\_surveillance | ecological\_or\_cross\_level | exposure\_data\_source | exposure\_operationalization                                                                                                                                                             | exposure\_absolute\_relative\_or\_both | outcome\_data\_source                             | outcome\_operationalization                    | source\_population                                                         | unit\_of\_analysis | study\_design   | sample\_size | which\_model\_is\_this\_from\_in\_their\_analysis\_table | confounders                                                                                                                                                                                                                                     | MA\_type        | MA\_number | variance\_reported\_type |    SE | notes\_about\_measure\_of\_association\_type                                                                                                                                                                                                                                                                                                                                                                             |
| --------: | :---------------------------------------------------- | ----------------: | :------------------------------------------------------------------------------------------ | :----------------------------- | :----------------------- | :--------------------------- | :--------------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :------------------------------------- | :------------------------------------------------ | :--------------------------------------------- | :------------------------------------------------------------------------- | :----------------- | :-------------- | -----------: | :------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------------- | ---------: | :----------------------- | ----: | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
|        18 | Johnson, Richard R.                                   |              2013 | Rape and Gender Conflict in a Patriarchal State                                             | 2000                           | Surveillance             | Ecological                   | Census / gov’t         | Index: percentage of female state legislative reps, percentage of female-owned businesses, percentage of female-headed households, percentage of female law enforcement officers         | Relative                               | FBI (ex. UCR)                                     | Avg annual \# rapes reported to police / 1000  | Counties in Kansas                                                         | County             | Cross-sectional |          105 | Table 3                                                  | Concentrated disadvantage (including non-rape violent crime rates), presence of a rape crisis center, \# police officers per capita, percentage of the population that was female                                                               | Risk Difference |      0.024 | Standard error           | 0.010 | NOTE: in the text they say “to explore the multivariate effects of the exogenous variables on the logged reported rape rate of eachcounty, estimated using OLS regression” but in the variable list, they don’t take the log. I think this is OLS regression, using the log of count data: ln(y) = B0+B1\*x , but I don’t know what the coefficient represents. <http://cameron.econ.ucdavis.edu/racd/simplepoisson.pdf> |
|        19 | Kawachi I; Kennedy BP; Gupta V; Prothrow-Stith D      |              1999 | Women’s status and the health of women and men: a view from the States.                     | 1990                           | Surveillance             | Ecological                   | Census / gov’t         | Political participation: women’s voter registration, women’s voter turnout, representation in elected office, institutional resources for women                                          | Both/mix                               | CDC (ex. WISQ.)                                   | Age standardized homicide rates per 100k women | US                                                                         | State              | Cross-sectional |           50 | Table 2                                                  | Gini, Median house hold income, poverty rate                                                                                                                                                                                                    | Risk difference |    \-0.070 | Standard error           | 0.050 | Count data vs. 1stdev change in indicators                                                                                                                                                                                                                                                                                                                                                                               |
|        19 | Kawachi I; Kennedy BP; Gupta V; Prothrow-Stith D      |              1999 | Women’s status and the health of women and men: a view from the States.                     | 1990                           | Surveillance             | Ecological                   | Census / gov’t         | Employment and earnings: absolute earnings, wage gap, mgmt/professional percentage, labor force percentage                                                                               | Both/mix                               | CDC (ex. WISQ.)                                   | Age standardized homicide rates per 100k women | US                                                                         | State              | Cross-sectional |           50 | Table 3                                                  | Gini, Median house hold income, poverty rate                                                                                                                                                                                                    | Risk difference |      0.510 | Standard error           | 0.730 |                                                                                                                                                                                                                                                                                                                                                                                                                          |
|        19 | Kawachi I; Kennedy BP; Gupta V; Prothrow-Stith D      |              1999 | Women’s status and the health of women and men: a view from the States.                     | 1990                           | Surveillance             | Ecological                   | Census / gov’t         | Reproductive rights: 8 legislative/political indicators ie policies, including percent of counties with at least one abortion provider and whether governor or legislature is pro-choice | Both/mix                               | CDC (ex. WISQ.)                                   | Age standardized homicide rates per 100k women | US                                                                         | State              | Cross-sectional |           50 | Table 5                                                  | Gini, Median house hold income, poverty rate                                                                                                                                                                                                    | Risk difference |    \-0.170 | Standard error           | 0.150 |                                                                                                                                                                                                                                                                                                                                                                                                                          |
|        17 | Jackson, Aubrey L.                                    |              2016 | The Combined Effect of Women’s Neighborhood Resources and Collective Efficacy on IPV.       | 1990                           | Survey                   | Cross-level                  | Census / gov’t         | PCA/composite: percent employed pop, prof/managerial workers, & college grads who are women                                                                                              | Relative                               | Longitudinal Cohort Survey of the PHDCN (Chicago) | Severe past-year IPV victimization             | Female primary caregivers of children, married or cohabitating, in Chicago | Neighborhood       | Cross-sectional |           80 | Table 2 / Model 4                                        | Individual characteristics (age, race, marital status, social support, salary, relative salary to spouse, household size, residential stability) + other structural (concentrated disadvantage, residential stability, immigrant concentration) | Log OR          |    \-0.096 | Standard error           | 0.081 | NOTE: sample size was 2463 women / 80 neighborhoods, not sure what to do with cross-level sample size                                                                                                                                                                                                                                                                                                                    |
|        21 | Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly |              2009 | The Contributions of Female Independence and Gender Equality to Rape in Metropolitan Areas. | 2000                           | Surveillance             | Ecological                   | Census / gov’t         | Gender diff in earnings                                                                                                                                                                  | Relative                               | FBI (ex. UCR)                                     | ln(3-year average forcible rape rate)          | People from 75 of the 100 largest SMSAs                                    | City / MSA         | Cross-sectional |           75 | Table 3 / Model 4 / coef 4                               | Infant mortality rate, poverty rate, percent 18-24, percent 25-44, male:female ratio, percent black + social independence index (female divorce rate, female headed household rate, female self-employment rate)                                | Log RR          |      0.250 | Standard error           | 1.493 | “The dependent variable is the natural log of the three-year average forcible rape rate” –\> is this the log IRR?                                                                                                                                                                                                                                                                                                        |
|        21 | Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly |              2009 | The Contributions of Female Independence and Gender Equality to Rape in Metropolitan Areas. | 2000                           | Surveillance             | Ecological                   | Census / gov’t         | Gender diff in unemployment                                                                                                                                                              | Relative                               | FBI (ex. UCR)                                     | ln(3-year average forcible rape rate)          | People from 75 of the 100 largest SMSAs                                    | City / MSA         | Cross-sectional |           75 | Table 3 / Model 4 / coef 5                               | Same as above                                                                                                                                                                                                                                   | Log RR          |      0.001 | Standard error           | 0.002 |                                                                                                                                                                                                                                                                                                                                                                                                                          |
|        21 | Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly |              2009 | The Contributions of Female Independence and Gender Equality to Rape in Metropolitan Areas. | 2000                           | Surveillance             | Ecological                   | Census / gov’t         | Gender diff in Higher Ed                                                                                                                                                                 | Relative                               | FBI (ex. UCR)                                     | ln(3-year average forcible rape rate)          | People from 75 of the 100 largest SMSAs                                    | City / MSA         | Cross-sectional |           75 | Table 3 / Model 4 / coef 6                               | Same as above                                                                                                                                                                                                                                   | Log RR          |    \-0.001 | Standard error           | 0.005 |                                                                                                                                                                                                                                                                                                                                                                                                                          |
|        21 | Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly |              2009 | The Contributions of Female Independence and Gender Equality to Rape in Metropolitan Areas. | 2000                           | Surveillance             | Ecological                   | Census / gov’t         | Gender diff in Professional Employment                                                                                                                                                   | Relative                               | FBI (ex. UCR)                                     | ln(3-year average forcible rape rate)          | People from 75 of the 100 largest SMSAs                                    | City / MSA         | Cross-sectional |           75 | Table 3 / Model 4 / coef 7                               | Same as above                                                                                                                                                                                                                                   | Log RR          |      0.003 | Standard error           | 0.002 |                                                                                                                                                                                                                                                                                                                                                                                                                          |

## Look at effect sizes, transform using `escalc`

``` r
knitr::kable(select(study_data,
                    study_id,
                    sample_size,
                    MA_type,
                    MA_number,
                    SE))
```

| study\_id | sample\_size | MA\_type        | MA\_number |    SE |
| --------: | -----------: | :-------------- | ---------: | ----: |
|        18 |          105 | Risk Difference |      0.024 | 0.010 |
|        19 |           50 | Risk difference |    \-0.070 | 0.050 |
|        19 |           50 | Risk difference |      0.510 | 0.730 |
|        19 |           50 | Risk difference |    \-0.170 | 0.150 |
|        17 |           80 | Log OR          |    \-0.096 | 0.081 |
|        21 |           75 | Log RR          |      0.250 | 1.493 |
|        21 |           75 | Log RR          |      0.001 | 0.002 |
|        21 |           75 | Log RR          |    \-0.001 | 0.005 |
|        21 |           75 | Log RR          |      0.003 | 0.002 |

``` r
## Question: what are we trying to transform all of these *to*? Fisher's z? Cohen's d?
```

## Made-up dataset, for easier practicing

``` r
set.seed(123)
replication_counts = c(3, 2, 1, 1, 1)

fake_data = tibble(
  es_id = 1001:1008,
  label = c(": Edu", ": Employment", ": Gov't rep", ": IPV", ": Homicide", "", "", ""),
  study_id = rep(1:5, replication_counts),
  authors = rep(c("Author1", "Author2", "Author3", "Author4", "Author1"), replication_counts),
  pub_date = as.character(rep(2001:2005, replication_counts)),
  unit_of_analysis = rep(c("City/MSA", "County", "Neighborhood", "County", "State"), replication_counts),
  setting = rep(c("Urban", "Rural", "Urban", "Rural", "USA"), replication_counts),
  outcome_data_source = rep(c("FBI", "Survey", "Survey", "Local PD", "FBI"), replication_counts),
  sample_size = rep(c(75, 100, 30, 150, 50), replication_counts),
  result = rnorm(8, mean = 1.03, sd = 0.3), #c(0.8, 0.85, 1.01, 2, 2.1, 0.95, 0.87, 1.5), #what are these??
  error = abs(rnorm(8, mean = 0.04, sd = 0.03)) #c(0.03, 0.04, 0.2, 0.2, 0.23, 0.04, 0.05, 0.01)
)

knitr::kable(fake_data)
```

| es\_id | label        | study\_id | authors | pub\_date | unit\_of\_analysis | setting | outcome\_data\_source | sample\_size |    result |     error |
| -----: | :----------- | --------: | :------ | :-------- | :----------------- | :------ | :-------------------- | -----------: | --------: | --------: |
|   1001 | : Edu        |         1 | Author1 | 2001      | City/MSA           | Urban   | FBI                   |           75 | 0.8618573 | 0.0193944 |
|   1002 | : Employment |         1 | Author1 | 2001      | City/MSA           | Urban   | FBI                   |           75 | 0.9609468 | 0.0266301 |
|   1003 | : Gov’t rep  |         1 | Author1 | 2001      | City/MSA           | Urban   | FBI                   |           75 | 1.4976125 | 0.0767225 |
|   1004 | : IPV        |         2 | Author2 | 2002      | County             | Rural   | Survey                |          100 | 1.0511525 | 0.0507944 |
|   1005 | : Homicide   |         2 | Author2 | 2002      | County             | Rural   | Survey                |          100 | 1.0687863 | 0.0520231 |
|   1006 |              |         3 | Author3 | 2003      | Neighborhood       | Urban   | Survey                |           30 | 1.5445195 | 0.0433205 |
|   1007 |              |         4 | Author4 | 2004      | County             | Rural   | Local PD              |          150 | 1.1682749 | 0.0233248 |
|   1008 |              |         5 | Author1 | 2005      | State              | USA     | FBI                   |           50 | 0.6504816 | 0.0936074 |

## Using `metafor` and `clubsandwich`

References: 1. This working paper provides a very useful explanation and
guide: <https://www.jepusto.com/#working-papers> 2. Here’s the
clubSandwich documentation:
<https://cran.r-project.org/web/packages/clubSandwich/clubSandwich.pdf>
3. And the metafor website:
<https://wviechtb.github.io/metafor/reference/rma.mv.html>

**An inventory of decisions we’re making:**

  - What type of model?
      - Multivariate, becasuse some studies contributed multiple effect
        sizes (hence, `rma.mv`)
  - What is the correlation structure between estimates from the same
    study?
      - We don’t know\! (not enough info provided per study) –\> so we
        estimate it as constant, rho = ?? (0.6/0.8)
      - Since we might be wrong with the assumption above, we use Robust
        Variance Estimation (RVE) when conducting our statistical tests
  - What is the nested structure of our data?
      - One option would be “research group” –\> studies –\> effect
        sizes
      - For now, it’s just studies –\> effect sizes

*If we want to additionally cluster by “research group” (author? unclear
whether this is appropriate in our case), the supplementary materials
here have some guidance in S3.1: <https://osf.io/nyv4u/>*

``` r
# constant sampling correlation assumption
rho <- 0.6 #is this a good assumption for us? I think in other places it's 0.8....

########################################################################################
# STEP 1: Identify an Appropriate Working Model
# --> CHE: multilevel random effects model with constant sampling correlation working model
########################################################################################


########################################################################################
# STEP 2: Estimate the Meta-Regression while Treating the Working Model as True
########################################################################################

# constant sampling correlation working model
cov_matrix <- impute_covariance_matrix(
  vi = fake_data$error, #vector of variances
  cluster = fake_data$study_id,
  r = rho, #assuming constant. make sure we pick the right value.
  smooth_vi = FALSE #this is the default. how would we know if we want to smooth?
  #subgroup = fake_data$authors, #i *think* this is where we'd do heirarchical clustering. but not sure, so leaving it out for now
)

# fit random effects working model in metafor
multilevel_model <- rma.mv(
  # here I'm specificying a formula, rather than an argument for the Y, and for the moderators
  result ~ 0 + outcome_data_source + setting, #"use a no-intercept specificaiton so that coefficients represent avg effect sizes for the corresponding category" (pre-print p.23)
  V = cov_matrix,
  random = ~ 1 | study_id / es_id, #this designates the nested structure of the random effects. QUESTION: if we do heirarchical + clustered, how do we designate that?
  #sparse = TRUE, #this is about speeding up the process, so not sure if it's necessary. I left it out.
  #test= "t", #use t-tests instead of the default z. why? (I left it out.)
  data = fake_data
)

multilevel_model # Note that this reports model-based (not robust) standard errors
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 8; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed          factor 
    ## sigma^2.1  0.0053  0.0728      5     no        study_id 
    ## sigma^2.2  0.0363  0.1905      8     no  study_id/es_id 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 3) = 8.1567, p-val = 0.0429
    ## 
    ## Test of Moderators (coefficients 1:5):
    ## QM(df = 5) = 93.2083, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                              estimate      se    zval    pval    ci.lb   ci.ub 
    ## outcome_data_sourceFBI         0.4898  0.4356  1.1244  0.2608  -0.3640  1.3437 
    ## outcome_data_sourceLocal PD    1.1683  0.2548  4.5858  <.0001   0.6690  1.6676 
    ## outcome_data_sourceSurvey      1.0599  0.2541  4.1713  <.0001   0.5619  1.5579 
    ## settingUrban                   0.4846  0.3866  1.2536  0.2100  -0.2731  1.2424 
    ## settingUSA                     0.1606  0.5701  0.2818  0.7781  -0.9566  1.2779 
    ##  
    ## outcome_data_sourceFBI 
    ## outcome_data_sourceLocal PD  *** 
    ## outcome_data_sourceSurvey    *** 
    ## settingUrban 
    ## settingUSA 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
########################################################################################
# STEP 3: Guard Against Misspecification by Using RVE
# --> use this to calculate standard errors, hypothesis tests, or CIs for the meta-reg. betas
########################################################################################

#these tests are RVE based and are robust to mispecification of the variances and covariances

coef_test(
  obj = multilevel_model, #estimation model above
  #cluster = fake_data$study_id, #define cluster IDs (not needed, already specified in model)
  vcov = "CR2" #estimation method (CR2 is best)
)
```

    ##                         Coef. Estimate       SE   t-stat d.f. p-val (Satt) Sig.
    ## 1      outcome_data_sourceFBI    0.490 0.176778    2.771    1         0.22     
    ## 2 outcome_data_sourceLocal PD    1.168 0.000000      Inf  NaN           NA   NA
    ## 3   outcome_data_sourceSurvey    1.060 0.000216 4898.071    1       <0.001  ***
    ## 4                settingUrban    0.485 0.000216 2239.726    1       <0.001  ***
    ## 5                  settingUSA    0.161 0.176778    0.909    1         0.53

``` r
conf_int(
  obj = multilevel_model,
  vcov = "CR2"
)
```

    ##                          Coef Estimate       SE d.f. Lower 95% CI Upper 95% CI
    ## 1      outcome_data_sourceFBI    0.490 0.176778    1       -1.736        2.716
    ## 2 outcome_data_sourceLocal PD    1.168 0.000000  NaN          NaN          NaN
    ## 3   outcome_data_sourceSurvey    1.060 0.000216    1        1.057        1.063
    ## 4                settingUrban    0.485 0.000216    1        0.482        0.487
    ## 5                  settingUSA    0.161 0.176778    1       -2.065        2.386

``` r
#Not sure when we would do a wald test, or what the constraints would be....
# Wald_test(
#   obj = multilevel_model,
#   constraints = ??
#   vcov = "CR2"
# )
```

**Plots**

``` r
# For the funnel and forest plots, it makes more sense (to me) to plot a version that doesn't include moderators.
no_moderators <- rma.mv(
  result ~ 1,
  V = cov_matrix,
  random = ~ 1 | study_id / es_id, #this designates the nested structure of the random effects. QUESTION: if we do heirarchical + clustered, how do we designate that?
  data = fake_data
)
#no_moderators

funnel(no_moderators)
```

![](mini-meta_files/figure-gfm/funnel_plot-1.png)<!-- -->

``` r
ranktest(no_moderators)
```

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = 0.1429, p = 0.7195

``` r
# QUESTION1: should we also do a statistical test for publication bias? If so,is it this one?
# QUESTION2: is this still appropriately down-weighting estimates within the same study? I don't think so, because each dot just shows up... but maybe it doesn't matter?
```

Note: there are a lot of really neat options on the metafor website for
forest plots\!
<https://wviechtb.github.io/metafor/reference/forest.rma.html> The very
last example has the nested structure (which fits our data), but is also
the least beautiful. There might be a way to use some of the different
preferences and options to make ours nice.

``` r
forest(
  x = rma.mv(result ~ 1, V = cov_matrix, random = ~ 1 | study_id / es_id, data = fake_data,
             slab=paste0(authors, " (", pub_date, ")", label)),
  annotate = TRUE, #does this add numbers on to the plot?
  addfit = FALSE, #TRUE would add in one summary measure for the whole thing
  #addpred = FALSE, #the bounds of the prediction interval (might not be meaningful)
  showweights = FALSE, #TRUE would be interesting to see, but the documentation warns the weights shown don't reflect the complexity from the model with rma.mv
  #level=x$level, #this is about the CI level, which I don't think is included anywhere in our model? even though that's where the default is taken from
  refline = 1, #I put this at 1 to show the relation to the null. change if not using ORs
  #digits=2L, #rounding for annotations
  #xlab, #label for the x-axis
  #slab = fake_data$study_id, #study labels. supress with NA, or add column
  #ilab, #optional extra label for studies
  #ilab.xpos, ilab.pos,
  order = c(1:3, 8, 4:7), #specify how the studies should be ordered, including obs (effect sizes), prec (variances), or a vector with the order
  header = TRUE #can pass in a character vector for left & right headings
)
```

![](mini-meta_files/figure-gfm/forest_plot-1.png)<!-- -->

-----

## Using `robumeta`

Documentation:
<https://cran.r-project.org/web/packages/robumeta/robumeta.pdf>

**Set up regression
models**

``` r
# Meta-Regression model: Effect_Size = B0 + B1*unit_of_analysis + B2*setting + B3*outcome_data_source + u + e
# --> some are confounders, others are explanatory variables
# ACTUALLY: that fake regression model couldn't be solved (data points too similar)
# --> so I'm just doing a model with one predictor: Effect_Size = B0 + B1*outcome_data_source + u + e

fake_model = robu(
  formula = result ~ outcome_data_source,#unit_of_analysis + setting + outcome_data_source,
  data = fake_data,
  studynum = study_id, #studynum must be a numeric or factor variable that uniquely identifies each study
  var.eff.size = error, #A vector of user-calculated effect-size variances.
  modelweights = "CORR", #a choice of corr or hier. TODO: after all data is extracted, check to make sure the balance really falls towards this side in our data.
  rho = 0.8, #this is the default value. explain what it is and why the default is appropriate
  small = TRUE #this is the default. how do we know if we want the small sample correction?
  # QUESTION: how to account for heirarchical clustering in our sample (i.e. same author1/5)?
)

fake_model_INTERCEPT_ONLY = robu(
  formula = result ~ 1,
  data = fake_data,
  studynum = study_id, #studynum must be a numeric or factor variable that uniquely identifies each study
  var.eff.size = error, #A vector of user-calculated effect-size variances.
  modelweights = "CORR", #a choice of corr or hier. TODO: after all data is extracted, check to make sure the balance really falls towards this side in our data.
  rho = 0.8, #this is the default value. explain what it is and why the default is appropriate
  small = TRUE #this is the default. how do we know if we want the small sample correction?
  # QUESTION: how to account for heirarchical clustering in our sample (i.e. same author1/5)?
)
```

**Results &
plots**

``` r
# NOTE: I think we might not want to use robumeta's forest plot, because it's not very customizeable (I'd like to remove the summary measure from the bottom, and maybe also even not have the summary dashed line?)
# HOWEVER: I'm not sure if we're allowed to do our meta-regression with robumeta and make our plots a different way.

# forest plot (w/summary measure)
forest_plot = forest.robu(
  fake_model_INTERCEPT_ONLY,
  es.lab = "es_id",
  study.lab = "study_id"
)

# funnel plot (publication bias)
# NOTE: robumeta doesn't have a funnel plot function.

# meta regression coefs/results
fake_model
```
