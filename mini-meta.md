Mini-meta
================

Goal: write code to conduct a mini meta-regression using just a few of
our studies, as a proof of concept and to better understand how to work
with the data and use the relevant R packages.

<details>

<summary> Click to expand and see the initial data import and cleaning
steps & full table from the data extraction form. </summary>

``` r
library(tidyverse)
library(metafor)
library(clubSandwich)
library(robumeta)
library(kableExtra)
```

``` r
study_data = read.csv("mini-meta-practice-data2.csv") %>% 
  janitor::clean_names(.) %>% 
  rowid_to_column(., "effect_size_id") %>% 
  mutate_at(., vars(all_of(c("decade_of_majority_of_data"))), factor) %>% 
  mutate_at(., vars(all_of(c("exposure_operationalization",
                             "outcome_operationalization",
                             "source_population",
                             "table_and_model",
                             "authors",
                             "publication_date",
                             "title",
                             "label",
                             "confounders"))), list(as.character)) %>% 
  rename(., `exposure scale: higher means more sexism` = exposure_scale_higher_means_more_sexism)

kable_paper(kable(study_data),
            bootstrap_options = c("condensed", "striped"))
```

<table class=" lightable-paper" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;'>

<thead>

<tr>

<th style="text-align:right;">

effect\_size\_id

</th>

<th style="text-align:right;">

study\_id

</th>

<th style="text-align:left;">

authors

</th>

<th style="text-align:left;">

publication\_date

</th>

<th style="text-align:left;">

title

</th>

<th style="text-align:left;">

decade\_of\_majority\_of\_data

</th>

<th style="text-align:left;">

survey\_or\_surveillance

</th>

<th style="text-align:left;">

ecological\_or\_cross\_level

</th>

<th style="text-align:left;">

exposure\_data\_source

</th>

<th style="text-align:left;">

exposure\_operationalization

</th>

<th style="text-align:left;">

exposure\_absolute\_relative\_or\_both

</th>

<th style="text-align:left;">

outcome\_data\_source

</th>

<th style="text-align:left;">

outcome\_operationalization

</th>

<th style="text-align:left;">

source\_population

</th>

<th style="text-align:left;">

unit\_of\_analysis

</th>

<th style="text-align:left;">

study\_design

</th>

<th style="text-align:right;">

sample\_size

</th>

<th style="text-align:left;">

table\_and\_model

</th>

<th style="text-align:left;">

confounders

</th>

<th style="text-align:right;">

num\_predictors

</th>

<th style="text-align:left;">

exposure scale: higher means more sexism

</th>

<th style="text-align:right;">

beta

</th>

<th style="text-align:right;">

standard\_error

</th>

<th style="text-align:left;">

label

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

18

</td>

<td style="text-align:left;">

Johnson, Richard R.

</td>

<td style="text-align:left;">

2013

</td>

<td style="text-align:left;">

Rape and Gender Conflict in a Patriarchal State

</td>

<td style="text-align:left;">

2000

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Ecological

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

Index: percentage of female state legislative reps, percentage of
female-owned businesses, percentage of female-headed households,
percentage of female law enforcement officers

</td>

<td style="text-align:left;">

Relative

</td>

<td style="text-align:left;">

FBI (ex. UCR)

</td>

<td style="text-align:left;">

Avg annual \# rapes reported to police / 1000

</td>

<td style="text-align:left;">

Counties in Kansas

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

105

</td>

<td style="text-align:left;">

Table 3

</td>

<td style="text-align:left;">

Concentrated disadvantage (including non-rape violent crime rates),
presence of a rape crisis center, \# police officers per capita,
percentage of the population that was female

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

0.024

</td>

<td style="text-align:right;">

0.010

</td>

<td style="text-align:left;">

sociopolitical power

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:left;">

Kawachi I; Kennedy BP; Gupta V; Prothrow-Stith D

</td>

<td style="text-align:left;">

1999

</td>

<td style="text-align:left;">

Women’s status and the health of women and men: a view from the States.

</td>

<td style="text-align:left;">

1990

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Ecological

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

Political participation: women’s voter registration, women’s voter
turnout, representation in elected office, institutional resources for
women

</td>

<td style="text-align:left;">

Both/mix

</td>

<td style="text-align:left;">

CDC (ex. WISQ.)

</td>

<td style="text-align:left;">

Age standardized homicide rates per 100k women

</td>

<td style="text-align:left;">

US

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

Table 2

</td>

<td style="text-align:left;">

Gini, Median house hold income, poverty rate

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

\-0.070

</td>

<td style="text-align:right;">

0.050

</td>

<td style="text-align:left;">

political particip.

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:left;">

Kawachi I; Kennedy BP; Gupta V; Prothrow-Stith D

</td>

<td style="text-align:left;">

1999

</td>

<td style="text-align:left;">

Women’s status and the health of women and men: a view from the States.

</td>

<td style="text-align:left;">

1990

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Ecological

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

Employment and earnings: absolute earnings, wage gap, mgmt/professional
percentage, labor force percentage

</td>

<td style="text-align:left;">

Both/mix

</td>

<td style="text-align:left;">

CDC (ex. WISQ.)

</td>

<td style="text-align:left;">

Age standardized homicide rates per 100k women

</td>

<td style="text-align:left;">

US

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

Table 3

</td>

<td style="text-align:left;">

Gini, Median house hold income, poverty rate

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

0.510

</td>

<td style="text-align:right;">

0.730

</td>

<td style="text-align:left;">

employ. + earnings

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:left;">

Kawachi I; Kennedy BP; Gupta V; Prothrow-Stith D

</td>

<td style="text-align:left;">

1999

</td>

<td style="text-align:left;">

Women’s status and the health of women and men: a view from the States.

</td>

<td style="text-align:left;">

1990

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Ecological

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

Reproductive rights: 8 legislative/political indicators ie policies,
including percent of counties with at least one abortion provider and
whether governor or legislature is pro-choice

</td>

<td style="text-align:left;">

Both/mix

</td>

<td style="text-align:left;">

CDC (ex. WISQ.)

</td>

<td style="text-align:left;">

Age standardized homicide rates per 100k women

</td>

<td style="text-align:left;">

US

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

Table 5

</td>

<td style="text-align:left;">

Gini, Median house hold income, poverty rate

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

\-0.170

</td>

<td style="text-align:right;">

0.150

</td>

<td style="text-align:left;">

repro. rights

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

17

</td>

<td style="text-align:left;">

Jackson, Aubrey L.

</td>

<td style="text-align:left;">

2016

</td>

<td style="text-align:left;">

The Combined Effect of Women’s Neighborhood Resources and Collective
Efficacy on IPV.

</td>

<td style="text-align:left;">

1990

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Cross-level

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

PCA/composite: percent employed pop, prof/managerial workers, & college
grads who are women

</td>

<td style="text-align:left;">

Relative

</td>

<td style="text-align:left;">

Longitudinal Cohort Survey of the PHDCN (Chicago)

</td>

<td style="text-align:left;">

Severe past-year IPV victimization

</td>

<td style="text-align:left;">

Female primary caregivers of children, married or cohabitating, in
Chicago

</td>

<td style="text-align:left;">

Neighborhood

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

80

</td>

<td style="text-align:left;">

Table 2 / Model 4

</td>

<td style="text-align:left;">

Individual characteristics (age, race, marital status, social support,
salary, relative salary to spouse, household size, residential
stability) + other structural (concentrated disadvantage, residential
stability, immigrant concentration)

</td>

<td style="text-align:right;">

14

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

\-0.096

</td>

<td style="text-align:right;">

0.081

</td>

<td style="text-align:left;">

relative resources

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly

</td>

<td style="text-align:left;">

2009

</td>

<td style="text-align:left;">

The Contributions of Female Independence and Gender Equality to Rape in
Metropolitan Areas.

</td>

<td style="text-align:left;">

2000

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Ecological

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

Gender diff in earnings

</td>

<td style="text-align:left;">

Relative

</td>

<td style="text-align:left;">

FBI (ex. UCR)

</td>

<td style="text-align:left;">

ln(3-year average forcible rape rate)

</td>

<td style="text-align:left;">

People from 75 of the 100 largest SMSAs

</td>

<td style="text-align:left;">

City / MSA

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

Table 3 / Model 4 / coef 4

</td>

<td style="text-align:left;">

Infant mortality rate, poverty rate, percent 18-24, percent 25-44,
male:female ratio, percent black + social independence index (female
divorce rate, female headed household rate, female self-employment rate)

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:right;">

0.250

</td>

<td style="text-align:right;">

1.493

</td>

<td style="text-align:left;">

earnings

</td>

</tr>

<tr>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly

</td>

<td style="text-align:left;">

2009

</td>

<td style="text-align:left;">

The Contributions of Female Independence and Gender Equality to Rape in
Metropolitan Areas.

</td>

<td style="text-align:left;">

2000

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Ecological

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

Gender diff in unemployment

</td>

<td style="text-align:left;">

Relative

</td>

<td style="text-align:left;">

FBI (ex. UCR)

</td>

<td style="text-align:left;">

ln(3-year average forcible rape rate)

</td>

<td style="text-align:left;">

People from 75 of the 100 largest SMSAs

</td>

<td style="text-align:left;">

City / MSA

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

Table 3 / Model 4 / coef 5

</td>

<td style="text-align:left;">

Same as above

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:right;">

0.001

</td>

<td style="text-align:right;">

0.002

</td>

<td style="text-align:left;">

unemployment

</td>

</tr>

<tr>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly

</td>

<td style="text-align:left;">

2009

</td>

<td style="text-align:left;">

The Contributions of Female Independence and Gender Equality to Rape in
Metropolitan Areas.

</td>

<td style="text-align:left;">

2000

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Ecological

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

Gender diff in Higher Ed

</td>

<td style="text-align:left;">

Relative

</td>

<td style="text-align:left;">

FBI (ex. UCR)

</td>

<td style="text-align:left;">

ln(3-year average forcible rape rate)

</td>

<td style="text-align:left;">

People from 75 of the 100 largest SMSAs

</td>

<td style="text-align:left;">

City / MSA

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

Table 3 / Model 4 / coef 6

</td>

<td style="text-align:left;">

Same as above

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:right;">

\-0.001

</td>

<td style="text-align:right;">

0.005

</td>

<td style="text-align:left;">

higher ed.

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly

</td>

<td style="text-align:left;">

2009

</td>

<td style="text-align:left;">

The Contributions of Female Independence and Gender Equality to Rape in
Metropolitan Areas.

</td>

<td style="text-align:left;">

2000

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Ecological

</td>

<td style="text-align:left;">

Census / gov’t

</td>

<td style="text-align:left;">

Gender diff in Professional Employment

</td>

<td style="text-align:left;">

Relative

</td>

<td style="text-align:left;">

FBI (ex. UCR)

</td>

<td style="text-align:left;">

ln(3-year average forcible rape rate)

</td>

<td style="text-align:left;">

People from 75 of the 100 largest SMSAs

</td>

<td style="text-align:left;">

City / MSA

</td>

<td style="text-align:left;">

Cross-sectional

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

Table 3 / Model 4 / coef 7

</td>

<td style="text-align:left;">

Same as above

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:right;">

0.003

</td>

<td style="text-align:right;">

0.002

</td>

<td style="text-align:left;">

professional emp.

</td>

</tr>

</tbody>

</table>

</details>

## Step 1: Create a sample summary table of included studies

``` r
# choose what information to include
# condense to one line per study
# note the number of effects that that study contributed
# order alphabetically

for_summary = study_data %>%
  group_by(study_id) %>%
  mutate(`Number of effects contributed` = n()) %>%
  ungroup() %>%
  mutate(.,
         Study = paste0(authors, " (", publication_date, ")"), ) %>%
  rename(
    .,
    `Sample size` = sample_size,
    `Unit of analysis` = unit_of_analysis,
    `Decade of majority of data` = decade_of_majority_of_data,
    `Outcome data source` = outcome_data_source,
    `Survey or surveillance?` = survey_or_surveillance
  ) %>%
  select(
    .,
    Study,
    `Unit of analysis`,
    `Survey or surveillance?`,
    `Outcome data source`,
    `Decade of majority of data`,
    `Number of effects contributed`,
    `Sample size`
  ) %>%
  arrange(Study)


kable_paper(
  kable(distinct(for_summary, Study, .keep_all = TRUE),
        caption = "Descriptive Information of Included Studies"),
  bootstrap_options = "condensed"
  )
```

<table class=" lightable-paper" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;'>

<caption>

Descriptive Information of Included Studies

</caption>

<thead>

<tr>

<th style="text-align:left;">

Study

</th>

<th style="text-align:left;">

Unit of analysis

</th>

<th style="text-align:left;">

Survey or surveillance?

</th>

<th style="text-align:left;">

Outcome data source

</th>

<th style="text-align:left;">

Decade of majority of data

</th>

<th style="text-align:right;">

Number of effects contributed

</th>

<th style="text-align:right;">

Sample size

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Jackson, Aubrey L. (2016)

</td>

<td style="text-align:left;">

Neighborhood

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Longitudinal Cohort Survey of the PHDCN (Chicago)

</td>

<td style="text-align:left;">

1990

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

80

</td>

</tr>

<tr>

<td style="text-align:left;">

Johnson, Richard R. (2013)

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI (ex. UCR)

</td>

<td style="text-align:left;">

2000

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

105

</td>

</tr>

<tr>

<td style="text-align:left;">

Kawachi I; Kennedy BP; Gupta V; Prothrow-Stith D (1999)

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

CDC (ex. WISQ.)

</td>

<td style="text-align:left;">

1990

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

50

</td>

</tr>

<tr>

<td style="text-align:left;">

Lee, Daniel R.; Hilinski, Carly M.; Clevenger, Shelly (2009)

</td>

<td style="text-align:left;">

City / MSA

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI (ex. UCR)

</td>

<td style="text-align:left;">

2000

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

75

</td>

</tr>

</tbody>

</table>

## Step 2: Transform effect sizes into a uniform statistic

First, look at the information we have on these effect sizes:

``` r
kable_paper(kable(
  select(
    study_data,
    study_id,
    sample_size,
    `exposure scale: higher means more sexism`,
    num_predictors,
    beta,
    standard_error
  )
),
full_width = FALSE,
bootstrap_options = "condensed"
)
```

<table class=" lightable-paper" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<thead>

<tr>

<th style="text-align:right;">

study\_id

</th>

<th style="text-align:right;">

sample\_size

</th>

<th style="text-align:left;">

exposure scale: higher means more sexism

</th>

<th style="text-align:right;">

num\_predictors

</th>

<th style="text-align:right;">

beta

</th>

<th style="text-align:right;">

standard\_error

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

105

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

0.024

</td>

<td style="text-align:right;">

0.010

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

\-0.070

</td>

<td style="text-align:right;">

0.050

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0.510

</td>

<td style="text-align:right;">

0.730

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

\-0.170

</td>

<td style="text-align:right;">

0.150

</td>

</tr>

<tr>

<td style="text-align:right;">

17

</td>

<td style="text-align:right;">

80

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:right;">

14

</td>

<td style="text-align:right;">

\-0.096

</td>

<td style="text-align:right;">

0.081

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

0.250

</td>

<td style="text-align:right;">

1.493

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

0.001

</td>

<td style="text-align:right;">

0.002

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

\-0.001

</td>

<td style="text-align:right;">

0.005

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

0.003

</td>

<td style="text-align:right;">

0.002

</td>

</tr>

</tbody>

</table>

**We are going to convert all effect sizes into partial corerlation
coefficients.**

<details>

<summary> Click for references, instructions, & code. </summary>

References:

  - Aloe & Thompson (2013):
    <https://www.journals.uchicago.edu/doi/10.5243/jsswr.2013.24>
  - Aloe & Becker (2012):
    <https://journals.sagepub.com/doi/10.3102/1076998610396901>
  - Aloe (2013):
    <https://www.tandfonline.com/doi/abs/10.1080/00221309.2013.853021>

From the `metafor` website’s documentation on the `escalc` function:

> To compute these measures, one needs to specify `ti` for the test
> statistics (i.e., t-tests) of the regression coefficient of interest,
> `ni` for the sample sizes of the studies, `mi` for the number of
> predictors in the regression models… The options for the measure
> argument are then: “PCOR” for the partial correlation coefficient…

For the majority of our studies, we will have:

  - Beta
  - Standard error
  - Number of predictors in the regression model
  - Sample size

This means we’ll need to convert effect sizes in two steps: (1) get the
t-test statistic using Beta & standard error, (2) use `escalc` to
compute a partial correlation coefficient.

*Note: for a handful of studies, they already provide partial
correlation coefficients. For others, they provide 2x2 tables or risk
differences. In these cases, we can use other `escalc` options to
convert between d-family and r-family effects (not shown here yet).*

``` r
# Step 1: get the t-test statistic
with_t_stats = study_data %>% 
  mutate(t_test = beta / standard_error) %>% 
  # flip the sign if they measured the exposure in the opposite direction
  mutate(t_test = ifelse(`exposure scale: higher means more sexism`, t_test, t_test*-1))

# Step 2: use `escalc` to compute partial correlation coefficients
for_analysis = escalc(
  measure = "PCOR",
  data = with_t_stats,
  ti = t_test,
  ni = sample_size,
  mi = num_predictors
)
```

</details>

<br> Here is our standardized data, ready for analysis:

``` r
knitr::kable(
  select(for_analysis, effect_size_id, study_id, t_test, yi, vi),
  digits = 3,
  caption = "yi and vi are the calculated partial correlation effect size and sampling variance"
)
```

<table>

<caption>

yi and vi are the calculated partial correlation effect size and
sampling variance

</caption>

<thead>

<tr>

<th style="text-align:right;">

effect\_size\_id

</th>

<th style="text-align:right;">

study\_id

</th>

<th style="text-align:right;">

t\_test

</th>

<th style="text-align:right;">

yi

</th>

<th style="text-align:right;">

vi

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

\-2.400

</td>

<td style="text-align:right;">

\-0.234

</td>

<td style="text-align:right;">

0.009

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

1.400

</td>

<td style="text-align:right;">

0.204

</td>

<td style="text-align:right;">

0.020

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

\-0.699

</td>

<td style="text-align:right;">

\-0.102

</td>

<td style="text-align:right;">

0.021

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

1.133

</td>

<td style="text-align:right;">

0.167

</td>

<td style="text-align:right;">

0.021

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

17

</td>

<td style="text-align:right;">

1.185

</td>

<td style="text-align:right;">

0.145

</td>

<td style="text-align:right;">

0.015

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

0.167

</td>

<td style="text-align:right;">

0.021

</td>

<td style="text-align:right;">

0.016

</td>

</tr>

<tr>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

0.500

</td>

<td style="text-align:right;">

0.064

</td>

<td style="text-align:right;">

0.016

</td>

</tr>

<tr>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

\-0.200

</td>

<td style="text-align:right;">

\-0.026

</td>

<td style="text-align:right;">

0.016

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

1.500

</td>

<td style="text-align:right;">

0.189

</td>

<td style="text-align:right;">

0.015

</td>

</tr>

</tbody>

</table>

*Note: We can see here that some effects are positive \[supporting the
backlash hypothesis\] and others are negative \[supporting the
amelioration hypothesis\]. Effects from study \#19 (Kawachi et al.) have
higher variance than the rest, so later on in the forest plot we should
expect to see them have wider confidence intervals.*

## Build our regression model, conduct statistical tests, create plots

(Using the `metafor` and `clubsandwich` packages)

References:

1.  This working paper provides a very useful explanation and guide:
    <https://www.jepusto.com/#working-papers>
2.  This is a reference for the statistical motivation, from *Research
    Synthesis Methods*:
    <https://onlinelibrary.wiley.com/doi/abs/10.1002/jrsm.5>
3.  Documentation for `clubSandwich`:
    <https://cran.r-project.org/web/packages/clubSandwich/clubSandwich.pdf>
4.  Documentation for `metafor`:
    <https://wviechtb.github.io/metafor/reference/rma.mv.html>

**An inventory of choices we’re making to decide how to build the
meta-regression model:**

  - What type of model?
      - Multivariate/multi-level, becasuse some studies contributed
        multiple effect sizes (hence, `rma.mv`)
      - Random effects, because our studies represent a distribution of
        *different* true effects
  - What is the correlation structure between estimates from the same
    study?
      - We don’t know\! (not enough info provided per study) –\> so we
        estimate the fixed effects component as constant, rho = ?? (some
        references suggest `0.6`, some `0.8`, so we’ll think about it)
      - Since we might be wrong with the assumption above, we use Robust
        Variance Estimation (RVE) when building our model and conducting
        our statistical tests
  - What is the nested/heirarchcal structure of our data?
      - For now, it’s: `studies --> effect sizes`
      - Another option would be `"research group"/author --> studies -->
        effect sizes` (this is suggested when the same research group is
        looking at multiple different cohorts for different studies.
        there aren’t really “research groups”, although some authors
        frequently publish together or have contributed multiple papers
        to our analysis) *Note: If we want to do this, the supplementary
        materials here have some guidance in S3.1:
        <https://osf.io/nyv4u/>*

<!-- end list -->

``` r
# constant sampling correlation assumption
rho <- 0.6 #is this a good assumption for us? In other places it's 0.8....

# create a covariance matrix assuming constant sampling correlation (working model)
cov_matrix <- impute_covariance_matrix(
  vi = for_analysis$vi, #vector of variances
  cluster = for_analysis$study_id,
  r = rho, #assuming constant. make sure we pick the right value.
  smooth_vi = FALSE #this is the default. how would we know if we want to smooth?
  #subgroup = for_analysis$authors, #i *think* this is where we'd do heirarchical clustering. but not sure, so leaving it out for now
)

# fit random effects working model in metafor
multilevel_model <- rma.mv(
  # here I'm specificying a formula, rather than an argument for the Y, and for the moderators
  yi ~ 0 + decade_of_majority_of_data + unit_of_analysis, #"use a no-intercept specificaiton so that coefficients represent avg effect sizes for the corresponding category" (pre-print p.23)
  V = cov_matrix,
  random = ~ 1 | study_id / effect_size_id, #this designates the nested structure of the random effects. QUESTION: if we do heirarchical + clustered, how do we designate that?
  #sparse = TRUE, #this is about speeding up the process, so not sure if it's necessary. I left it out.
  #test= "t", #use t-tests instead of the default z. why would we want this? (I left it out.)
  data = for_analysis
)
```

    ## Warning in rma.mv(yi ~ 0 + decade_of_majority_of_data + unit_of_analysis, :
    ## Redundant predictors dropped from the model.

### Meta-regression output:

``` r
multilevel_model # Note that this reports model-based (not robust) standard errors
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 9; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed                   factor 
    ## sigma^2.1  0.0010  0.0316      4     no                 study_id 
    ## sigma^2.2  0.0081  0.0899      9     no  study_id/effect_size_id 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 10.6903, p-val = 0.0579
    ## 
    ## Test of Moderators (coefficients 1:4):
    ## QM(df = 4) = 4.6816, p-val = 0.3215
    ## 
    ## Model Results:
    ## 
    ##                                 estimate      se     zval    pval    ci.lb 
    ## decade_of_majority_of_data1990    0.0927  0.1379   0.6722  0.5015  -0.1776 
    ## decade_of_majority_of_data2000    0.0661  0.1194   0.5535  0.5799  -0.1679 
    ## unit_of_analysisCounty           -0.3006  0.1799  -1.6707  0.0948  -0.6532 
    ## unit_of_analysisNeighborhood      0.0527  0.2070   0.2548  0.7989  -0.3530 
    ##                                  ci.ub 
    ## decade_of_majority_of_data1990  0.3630    
    ## decade_of_majority_of_data2000  0.3001    
    ## unit_of_analysisCounty          0.0520  . 
    ## unit_of_analysisNeighborhood    0.4585    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### T-tests for each variable:

``` r
#these tests are RVE based and are robust to mispecification of the variances and covariances
coef_test(
  obj = multilevel_model, #estimation model above
  #cluster = for_analysis$study_id, #define cluster IDs (not needed, already specified in model)
  vcov = "CR2" #estimation method (CR2 is best)
)
```

    ##                            Coef. Estimate     SE t-stat d.f. p-val (Satt) Sig.
    ## 1 decade_of_majority_of_data1990   0.0927 0.0112   8.31    1       0.0763    .
    ## 2 decade_of_majority_of_data2000   0.0661 0.0158   4.18    1       0.1496     
    ## 3         unit_of_analysisCounty  -0.3006 0.0158 -18.99    1       0.0335    *
    ## 4   unit_of_analysisNeighborhood   0.0527 0.0112   4.73    1       0.1328

### Confidence intervals for those tests:

``` r
conf_int(
  obj = multilevel_model,
  vcov = "CR2"
)
```

    ##                             Coef Estimate     SE d.f. Lower 95% CI Upper 95% CI
    ## 1 decade_of_majority_of_data1990   0.0927 0.0112    1      -0.0491       0.2345
    ## 2 decade_of_majority_of_data2000   0.0661 0.0158    1      -0.1350       0.2672
    ## 3         unit_of_analysisCounty  -0.3006 0.0158    1      -0.5017      -0.0995
    ## 4   unit_of_analysisNeighborhood   0.0527 0.0112    1      -0.0891       0.1946

*Note: Here we might interpret one of these results as ???? \[of course,
this isn’t meaningful right now with just 4
studies\!\]*

### Funnel plot: publication bias

``` r
# For the funnel and forest plots, it makes more sense (to me) to plot from a regression model that doesn't include moderators.
no_moderators <- rma.mv(
  yi ~ 1,
  V = cov_matrix,
  random = ~ 1 | study_id / effect_size_id, #this designates the nested structure of the random effects. QUESTION: if we do heirarchical + clustered, how do we designate that?
  data = for_analysis
)

funnel(no_moderators)
```

![](mini-meta_files/figure-gfm/funnel_plot-1.png)<!-- -->

``` r
ranktest(no_moderators)
```

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = -0.0556, p = 0.9195

``` r
# QUESTION1: should we also do a statistical test for publication bias? If so, is it this one?
# QUESTION2: is this still appropriately down-weighting estimates within the same study? I don't think so, because each dot just shows up... but maybe it doesn't matter?
```

### Forest plot\!

*Note: there are a lot of [really neat
options](https://wviechtb.github.io/metafor/reference/forest.rma.html)
on the metafor website for forest plots\! The very last example has the
nested structure (which fits our data), but is also the least beautiful.
There might be a way to use some of the different preferences and
options to make ours nice…*

``` r
forest(
  x = rma.mv(yi ~ 1, V = cov_matrix, random = ~ 1 | study_id / effect_size_id, data = for_analysis,
             slab=paste0(word(authors, 1, sep = " |,"), " (", publication_date, "): ", label)),
  annotate = TRUE, #does this add numbers on to the plot?
  addfit = FALSE, #TRUE would add in one summary measure for the whole thing
  #addpred = FALSE, #the bounds of the prediction interval (might not be meaningful)
  showweights = FALSE, #TRUE would be interesting to see, but the documentation warns the weights shown don't reflect the complexity from the model with rma.mv
  #level=x$level, #this is about the CI level, which I don't think is included anywhere in our model? even though that's where the default is taken from
  refline = 0, #I put this at 1 to show the relation to the null. change if not using ORs
  #digits=2L, #rounding for annotations
  #xlab, #label for the x-axis
  #slab = for_analysis$study_id, #study labels. supress with NA, or add column
  #ilab, #optional extra label for studies
  #ilab.xpos, ilab.pos,
  #order = c(1:3, 8, 4:7), #specify how the studies should be ordered, including obs (effect sizes), prec (variances), or a vector with the order
  header = TRUE #c("Study (& specific effect)", "Estimate [95% CI]") #can pass in a character vector for left & right headings, or TRUE
)
```

![](mini-meta_files/figure-gfm/forest_plot-1.png)<!-- -->
