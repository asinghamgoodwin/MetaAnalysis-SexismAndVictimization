Second Draft Analysis 7/4/21
================
Alicia
7/13/2021

## Step 1: Import & clean data

<details>

<summary> Click to expand and see the initial data import and cleaning
steps & full table from the data extraction form. </summary>

``` r
library(tidyverse)
library(metafor)
library(clubSandwich)
library(robumeta)
library(kableExtra)
library(table1)
library(mice)
library(mitools)

set.seed(8929) #set a random seed for reproducibility 
```

**Import data, making sure everything is the correct data type.**

``` r
all_data = read_csv("Data/SecondDraft7-4-21.csv",
                      # up to outcome + up to exposure_reliability + the rest
                      col_types = paste0("nnccccfffccffcfffcncccc", "ffffffffffffffllcllllclfc", "fccccfnnnnllnnncnnnlflcc"))
```

**Fill in information in columns that are aggregates of others.**

``` r
domestic_perpetrator_strings = c("husband",
                                 "Family violence",
                                 "family",
                                 "Intimate partner",
                                 "IPV")

non_domestic_known_perpetrator_strings = c("known nonintimate",
                                           "acquaintance",
                                           "friend")


all_data = all_data %>% 
  group_by(Study_ID) %>% 
  mutate(Combined_survey_surveillance = paste0(unique(Survey_surveillance), collapse = " & "),
         Combined_outcome_data_source = paste0(unique(Outcome_data_source), collapse = " & "),
         Combined_exposure_level = paste0(unique(Exposure_level), collapse = " & "),
         Combined_outcome_type_factor = paste0(unique(Outcome_type_factor), collapse = " & "),
         Num_EMs_from_this_study = length(unique(Effect_ID))
  ) %>% 
  ungroup() %>% 
  mutate(Exposure_category_economic = Exposure_category_employment | Exposure_category_financial,
         Exposure_combo = (as.integer(!is.na(Exposure_labor_force_participation)) +
                             as.integer(!is.na(Exposure_employment_unemployment)) +
                             as.integer(!is.na(Exposure_income)) +
                             as.integer(!is.na(Exposure_management_professional_roles))+
                             as.integer(!is.na(Exposure_women_owned_businesses)) +
                             as.integer(!is.na(Exposure_education)) +
                             as.integer(!is.na(Exposure_poverty)) +
                             as.integer(!is.na(Exposure_representation_in_government)) +
                             as.integer(!is.na(Exposure_voter_registration_civic_participation)) +
                             as.integer(!is.na(Exposure_maternal_mortality)) +
                             as.integer(!is.na(Exposure_teen_pregnancy)) +
                             as.integer(!is.na(Exposure_other_health_outcomes)) +
                             as.integer(!is.na(Exposure_women_legal_rights))) > 1,
         Unadjusted = Confounders == "NONE (UNADJUSTED)",
         Victimization_type = as.factor(
           case_when(Outcome_type_factor %in% c("Intimate partner homicide",
                                                "Female homicide") ~ "Homicide",
                     Outcome_type_factor %in% c("Rape",
                                                "Sexual assault") ~ "Rape & sexual assault",
                     Outcome_type_factor %in% c("Assault",
                                                "IPV",
                                                "Family violence",
                                                "Harassment") ~ "IPV, harassment, & assault",
                     TRUE ~ "999")),
         Perpetrator_relationship = as.factor(
           case_when(str_detect(Outcome, paste0(domestic_perpetrator_strings, collapse = "|")) ~ "Domestic/intimate partner",
                     str_detect(Outcome, paste0(non_domestic_known_perpetrator_strings, collapse = "|")) ~ "Non-domestic known perpetrator",
                     TRUE ~ "Unknown or not specified")),
         Exposure_level_condensed = as.factor(
           case_when(Exposure_level %in% c("City", "MSA") ~ "City/MSA",
                     Exposure_level %in% c("County") ~ "County",
                     Exposure_level %in% c("State") ~ "State",
                     TRUE ~ "999")),
         Race = as.factor(
           case_when(str_detect(Outcome, "White") ~ "White",
                     str_detect(Outcome, "Black") ~ "Black",
                     TRUE ~ "999"))
  ) %>% 
  group_by(Study_ID) %>% 
  mutate(Combined_exposure_types = paste0(c(rep("Economic", min(sum(Exposure_category_economic, na.rm = TRUE), 1)),
                                            rep("Education", min(sum(Exposure_category_education, na.rm = TRUE), 1)),
                                            rep("Political representation", min(sum(Exposure_category_political, na.rm = TRUE), 1)), 
                                            rep("Legal rights", min(sum(Exposure_category_legal_rights, na.rm = TRUE), 1)),
                                            rep("Health indicators", min(sum(Exposure_category_womens_health, na.rm = TRUE), 1))
                                            ), collapse = " & ")
  ) %>% 
  ungroup() %>% 
  mutate_if(is.logical, ~replace(., is.na(.), FALSE)) %>% 
  mutate(Decade = fct_relevel(Decade, c("1970s", "1980s", "1990s", "2000s", "2010s")),
         Survey_surveillance = fct_relevel(Survey_surveillance, c("Survey", "Surveillance")),
         Study_design = fct_relevel(Study_design, c("Cross-sectional", "Serial cross-sectional", "Longitudinal", "Difference over time in exposure vs. outcome")),
         Outcome_data_source_condensed = fct_relevel(Outcome_data_source_condensed, c("Survey", "Hospital records", "FBI", "Local law enforcement", "CDC", "Other (DOE)")),
         Exposure_level = fct_relevel(Exposure_level, c("State", "City", "MSA", "Neighborhood", "County", "Occupation & Industry")),
         Ecological_cross_level = ifelse(Study_ID == 17, "Unable to determine", Ecological_cross_level),
         Ecological_cross_level = fct_recode(Ecological_cross_level, "Ecological" = "1", "Cross-level" = "3", "Unable to determine" = "2", "Unable to determine" = "Unable to determine"),
         Ecological_cross_level = fct_relevel(Ecological_cross_level, c("Ecological", "Cross-level", "Unable to determine"))

  )
```

``` r
summary(filter(all_data, Ecological_cross_level == "Ecological")$Sample_size)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    25.0    50.0   148.0   215.5   177.0  3083.0

``` r
summary(filter(all_data, Ecological_cross_level == "Cross-level")$Sample_size)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     610    1414    3916   83665    5390  487166

``` r
summary(filter(all_data, Ecological_cross_level == "Unable to determine")$Sample_size)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      50 1449494 2898939 1932643 2898939 2898939

<br> *Questions for future analysis:*

  - Do we want to do anything quantitative with `Confounders`?
  - Anything else that would make sense as a predictor that we haven’t
    captured?
      - Maybe: location (South / not South? Within 1 state vs. aross
        many?)

</details>

## Step 2: Describe the data

<details>

<summary> Click to see setup code. </summary>

``` r
# **Summary stats, study-level:**

study_level = all_data %>% 
  distinct(Study_ID, .keep_all = TRUE)
```

**Summary stats, EM-level:**

``` r
em_level = all_data %>% 
  mutate(Included_in_meta_regression = ifelse(Include_in_regression == "Yes", 1, 0),
         Included_in_meta_regression = factor(Included_in_meta_regression,
                                              levels = c(1, 0), 
                                              labels = c("Included in regression", "Excluded"))
)

label(em_level$Decade) <- "Decade of majority of datapoints"
label(em_level$Exposure_level) <- "Exposure: level of analysis"
label(em_level$Survey_surveillance) <- "Outcome: survey or surveillance?"
label(em_level$Outcome_data_source_condensed) <- "Outcome: data source"
label(em_level$Victimization_type) <- "Outcome: victimization type"
label(em_level$Perpetrator_relationship) <- "Outcome: victim/offender relationship"

# Table with more variables, that didn't make it into the table 1
table1(~ Study_design + Ecological_cross_level + Exposure_level + Exposure_combo + Exposure_category_employment + Exposure_category_education + 
         Exposure_category_political + Exposure_category_legal_rights + Exposure_category_womens_health +
         Exposure_absolute_or_relative + Outcome_type_factor +
       Sample_size + Unadjusted | Included_in_meta_regression, data = em_level)
```

    ## [1] "<table class=\"Rtable1\">\n<thead>\n<tr>\n<th class='rowlabel firstrow lastrow'></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Included in regression<br><span class='stratn'>(N=204)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Excluded<br><span class='stratn'>(N=31)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=235)</span></span></th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Study_design</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Cross-sectional</td>\n<td>180 (88.2%)</td>\n<td>21 (67.7%)</td>\n<td>201 (85.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Serial cross-sectional</td>\n<td>18 (8.8%)</td>\n<td>8 (25.8%)</td>\n<td>26 (11.1%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Longitudinal</td>\n<td>3 (1.5%)</td>\n<td>2 (6.5%)</td>\n<td>5 (2.1%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Difference over time in exposure vs. outcome</td>\n<td class='lastrow'>3 (1.5%)</td>\n<td class='lastrow'>0 (0%)</td>\n<td class='lastrow'>3 (1.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Ecological_cross_level</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Ecological</td>\n<td>195 (95.6%)</td>\n<td>25 (80.6%)</td>\n<td>220 (93.6%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Cross-level</td>\n<td>9 (4.4%)</td>\n<td>3 (9.7%)</td>\n<td>12 (5.1%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Unable to determine</td>\n<td class='lastrow'>0 (0%)</td>\n<td class='lastrow'>3 (9.7%)</td>\n<td class='lastrow'>3 (1.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Exposure: level of analysis</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>State</td>\n<td>64 (31.4%)</td>\n<td>3 (9.7%)</td>\n<td>67 (28.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>City</td>\n<td>105 (51.5%)</td>\n<td>19 (61.3%)</td>\n<td>124 (52.8%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>MSA</td>\n<td>20 (9.8%)</td>\n<td>1 (3.2%)</td>\n<td>21 (8.9%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Neighborhood</td>\n<td>3 (1.5%)</td>\n<td>1 (3.2%)</td>\n<td>4 (1.7%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>County</td>\n<td>10 (4.9%)</td>\n<td>7 (22.6%)</td>\n<td>17 (7.2%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Occupation & Industry</td>\n<td class='lastrow'>2 (1.0%)</td>\n<td class='lastrow'>0 (0%)</td>\n<td class='lastrow'>2 (0.9%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Exposure_combo</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Yes</td>\n<td>47 (23.0%)</td>\n<td>6 (19.4%)</td>\n<td>53 (22.6%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>No</td>\n<td class='lastrow'>157 (77.0%)</td>\n<td class='lastrow'>25 (80.6%)</td>\n<td class='lastrow'>182 (77.4%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Exposure_category_employment</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Yes</td>\n<td>111 (54.4%)</td>\n<td>15 (48.4%)</td>\n<td>126 (53.6%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>No</td>\n<td class='lastrow'>93 (45.6%)</td>\n<td class='lastrow'>16 (51.6%)</td>\n<td class='lastrow'>109 (46.4%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Exposure_category_education</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Yes</td>\n<td>72 (35.3%)</td>\n<td>9 (29.0%)</td>\n<td>81 (34.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>No</td>\n<td class='lastrow'>132 (64.7%)</td>\n<td class='lastrow'>22 (71.0%)</td>\n<td class='lastrow'>154 (65.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Exposure_category_political</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Yes</td>\n<td>32 (15.7%)</td>\n<td>2 (6.5%)</td>\n<td>34 (14.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>No</td>\n<td class='lastrow'>172 (84.3%)</td>\n<td class='lastrow'>29 (93.5%)</td>\n<td class='lastrow'>201 (85.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Exposure_category_legal_rights</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Yes</td>\n<td>3 (1.5%)</td>\n<td>0 (0%)</td>\n<td>3 (1.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>No</td>\n<td class='lastrow'>201 (98.5%)</td>\n<td class='lastrow'>31 (100%)</td>\n<td class='lastrow'>232 (98.7%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Exposure_category_womens_health</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Yes</td>\n<td>14 (6.9%)</td>\n<td>0 (0%)</td>\n<td>14 (6.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>No</td>\n<td class='lastrow'>190 (93.1%)</td>\n<td class='lastrow'>31 (100%)</td>\n<td class='lastrow'>221 (94.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Exposure_absolute_or_relative</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Relative</td>\n<td>159 (77.9%)</td>\n<td>18 (58.1%)</td>\n<td>177 (75.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Both/mix</td>\n<td>21 (10.3%)</td>\n<td>3 (9.7%)</td>\n<td>24 (10.2%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Relative - Difference</td>\n<td class='lastrow'>24 (11.8%)</td>\n<td class='lastrow'>10 (32.3%)</td>\n<td class='lastrow'>34 (14.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Outcome_type_factor</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Assault</td>\n<td>9 (4.4%)</td>\n<td>5 (16.1%)</td>\n<td>14 (6.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Intimate partner homicide</td>\n<td>25 (12.3%)</td>\n<td>4 (12.9%)</td>\n<td>29 (12.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Rape</td>\n<td>81 (39.7%)</td>\n<td>9 (29.0%)</td>\n<td>90 (38.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Female homicide</td>\n<td>47 (23.0%)</td>\n<td>9 (29.0%)</td>\n<td>56 (23.8%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>IPV</td>\n<td>25 (12.3%)</td>\n<td>4 (12.9%)</td>\n<td>29 (12.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Family violence</td>\n<td>3 (1.5%)</td>\n<td>0 (0%)</td>\n<td>3 (1.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Sexual assault</td>\n<td>8 (3.9%)</td>\n<td>0 (0%)</td>\n<td>8 (3.4%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Harassment</td>\n<td class='lastrow'>6 (2.9%)</td>\n<td class='lastrow'>0 (0%)</td>\n<td class='lastrow'>6 (2.6%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Sample_size</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>5110 (48100)</td>\n<td>187000 (724000)</td>\n<td>29100 (270000)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>148 [25.0, 487000]</td>\n<td class='lastrow'>177 [50.0, 2900000]</td>\n<td class='lastrow'>148 [25.0, 2900000]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Unadjusted</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Yes</td>\n<td>43 (21.1%)</td>\n<td>0 (0%)</td>\n<td>43 (18.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>No</td>\n<td class='lastrow'>161 (78.9%)</td>\n<td class='lastrow'>31 (100%)</td>\n<td class='lastrow'>192 (81.7%)</td>\n</tr>\n</tbody>\n</table>\n"

``` r
#Table 1 - descriptive variables for the sample of effect sizes,
#  split by whether or not they're included in the meta regression
print(table1(~ Decade + Exposure_level + Survey_surveillance + Outcome_data_source_condensed + 
         Victimization_type + Perpetrator_relationship | Included_in_meta_regression, 
       data = em_level,
       overall = FALSE))
```

    ## <table class="Rtable1">
    ## <thead>
    ## <tr>
    ## <th class='rowlabel firstrow lastrow'></th>
    ## <th class='firstrow lastrow'><span class='stratlabel'>Included in regression<br><span class='stratn'>(N=204)</span></span></th>
    ## <th class='firstrow lastrow'><span class='stratlabel'>Excluded<br><span class='stratn'>(N=31)</span></span></th>
    ## </tr>
    ## </thead>
    ## <tbody>
    ## <tr>
    ## <td class='rowlabel firstrow'><span class='varlabel'>Decade of majority of datapoints</span></td>
    ## <td class='firstrow'></td>
    ## <td class='firstrow'></td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>1970s</td>
    ## <td>26 (12.7%)</td>
    ## <td>1 (3.2%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>1980s</td>
    ## <td>41 (20.1%)</td>
    ## <td>15 (48.4%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>1990s</td>
    ## <td>70 (34.3%)</td>
    ## <td>11 (35.5%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>2000s</td>
    ## <td>28 (13.7%)</td>
    ## <td>2 (6.5%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel lastrow'>2010s</td>
    ## <td class='lastrow'>39 (19.1%)</td>
    ## <td class='lastrow'>2 (6.5%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel firstrow'><span class='varlabel'>Exposure: level of analysis</span></td>
    ## <td class='firstrow'></td>
    ## <td class='firstrow'></td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>State</td>
    ## <td>64 (31.4%)</td>
    ## <td>3 (9.7%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>City</td>
    ## <td>105 (51.5%)</td>
    ## <td>19 (61.3%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>MSA</td>
    ## <td>20 (9.8%)</td>
    ## <td>1 (3.2%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>Neighborhood</td>
    ## <td>3 (1.5%)</td>
    ## <td>1 (3.2%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>County</td>
    ## <td>10 (4.9%)</td>
    ## <td>7 (22.6%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel lastrow'>Occupation & Industry</td>
    ## <td class='lastrow'>2 (1.0%)</td>
    ## <td class='lastrow'>0 (0%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel firstrow'><span class='varlabel'>Outcome: survey or surveillance?</span></td>
    ## <td class='firstrow'></td>
    ## <td class='firstrow'></td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>Survey</td>
    ## <td>73 (35.8%)</td>
    ## <td>2 (6.5%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel lastrow'>Surveillance</td>
    ## <td class='lastrow'>131 (64.2%)</td>
    ## <td class='lastrow'>29 (93.5%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel firstrow'><span class='varlabel'>Outcome: data source</span></td>
    ## <td class='firstrow'></td>
    ## <td class='firstrow'></td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>Survey</td>
    ## <td>63 (30.9%)</td>
    ## <td>2 (6.5%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>Hospital records</td>
    ## <td>0 (0%)</td>
    ## <td>1 (3.2%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>FBI</td>
    ## <td>128 (62.7%)</td>
    ## <td>23 (74.2%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>Local law enforcement</td>
    ## <td>7 (3.4%)</td>
    ## <td>4 (12.9%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>CDC</td>
    ## <td>6 (2.9%)</td>
    ## <td>0 (0%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel lastrow'>Other (DOE)</td>
    ## <td class='lastrow'>0 (0%)</td>
    ## <td class='lastrow'>1 (3.2%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel firstrow'><span class='varlabel'>Outcome: victimization type</span></td>
    ## <td class='firstrow'></td>
    ## <td class='firstrow'></td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>Homicide</td>
    ## <td>72 (35.3%)</td>
    ## <td>13 (41.9%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>IPV, harassment, & assault</td>
    ## <td>43 (21.1%)</td>
    ## <td>9 (29.0%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel lastrow'>Rape & sexual assault</td>
    ## <td class='lastrow'>89 (43.6%)</td>
    ## <td class='lastrow'>9 (29.0%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel firstrow'><span class='varlabel'>Outcome: victim/offender relationship</span></td>
    ## <td class='firstrow'></td>
    ## <td class='firstrow'></td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>Domestic/intimate partner</td>
    ## <td>59 (28.9%)</td>
    ## <td>8 (25.8%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel'>Non-domestic known perpetrator</td>
    ## <td>10 (4.9%)</td>
    ## <td>0 (0%)</td>
    ## </tr>
    ## <tr>
    ## <td class='rowlabel lastrow'>Unknown or not specified</td>
    ## <td class='lastrow'>135 (66.2%)</td>
    ## <td class='lastrow'>23 (74.2%)</td>
    ## </tr>
    ## </tbody>
    ## </table>

</details>

![Table 1](Data/PlotsTables/table1.png).

<br>

<br>

<details>

<summary> Click to see Table listing all studies </summary>

``` r
for_summary = study_level %>% 
  mutate(Included_in_meta_regression = ifelse(Include_in_regression == "Yes", "Yes", "No")) %>% 
  select(Authors_abbr,
         PubYear,
         #Title,
         #Hypothesis,
         Source_population,
         Combined_survey_surveillance,
         Combined_outcome_data_source,
         Combined_outcome_type_factor,
         Combined_exposure_level,
         Combined_exposure_types,
         # NOTE: this isn't necessarily consistent per study
         Sample_size,
         Num_EMs_from_this_study,
         Included_in_meta_regression) 
  
#kable_paper(
  kable(for_summary,
        col.names = c("Authors", "Publication Year", "Source population",
                      "Outcome: survey or surveillance?", "Outcome: data source", "Outcome: victimization type", 
                      "Exposure: level of analysis", "Exposure: gender inequality types",
                      "Sample size", "Number of effects contributed", "Included in meta-regression?"))#,
```

<table>

<thead>

<tr>

<th style="text-align:left;">

Authors

</th>

<th style="text-align:left;">

Publication Year

</th>

<th style="text-align:left;">

Source population

</th>

<th style="text-align:left;">

Outcome: survey or surveillance?

</th>

<th style="text-align:left;">

Outcome: data source

</th>

<th style="text-align:left;">

Outcome: victimization type

</th>

<th style="text-align:left;">

Exposure: level of analysis

</th>

<th style="text-align:left;">

Exposure: gender inequality types

</th>

<th style="text-align:right;">

Sample size

</th>

<th style="text-align:right;">

Number of effects contributed

</th>

<th style="text-align:left;">

Included in meta-regression?

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Aizer

</td>

<td style="text-align:left;">

2010

</td>

<td style="text-align:left;">

California counties

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Hospital records

</td>

<td style="text-align:left;">

Assault

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

982

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Avakame

</td>

<td style="text-align:left;">

1998

</td>

<td style="text-align:left;">

All U.S. (surveillance)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Intimate partner homicide

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Political representation

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Bailey

</td>

<td style="text-align:left;">

1999

</td>

<td style="text-align:left;">

Cities (population \> 100,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

192

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Baron & Straus

</td>

<td style="text-align:left;">

1989

</td>

<td style="text-align:left;">

All U.S. (surveillance)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Political representation & Health indicators

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Boyle et al.

</td>

<td style="text-align:left;">

2017

</td>

<td style="text-align:left;">

Students at 413 “top” colleges in 47 states

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

DOE reports from universities

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

1644

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Brewer & Smith

</td>

<td style="text-align:left;">

1995

</td>

<td style="text-align:left;">

Cities (population \> 250,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

177

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Campbell et al.

</td>

<td style="text-align:left;">

2019

</td>

<td style="text-align:left;">

All U.S. (surveillance)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI & CDC

</td>

<td style="text-align:left;">

Intimate partner homicide & Female homicide

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Political representation & Health indicators

</td>

<td style="text-align:right;">

792

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

D’Alessio & Stolzenberg

</td>

<td style="text-align:left;">

2010

</td>

<td style="text-align:left;">

Cities (population \> 50,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

134

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

DeWees & Parker

</td>

<td style="text-align:left;">

2003

</td>

<td style="text-align:left;">

Cities (population \> 100,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education & Political representation

</td>

<td style="text-align:right;">

165

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Dugan et al.

</td>

<td style="text-align:left;">

1999

</td>

<td style="text-align:left;">

Cities (in the 25 largest MSAs)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Intimate partner homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

116

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Ellis & Beattie

</td>

<td style="text-align:left;">

1983

</td>

<td style="text-align:left;">

Large cities

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

FBI & Survey (NCVS)

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

25

</td>

<td style="text-align:right;">

14

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Eschholz & Vieraitis

</td>

<td style="text-align:left;">

2004

</td>

<td style="text-align:left;">

Cities (population \> 100,000 & Black residents \> 2,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

148

</td>

<td style="text-align:right;">

12

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Gillespie & Reckdenwald

</td>

<td style="text-align:left;">

2015

</td>

<td style="text-align:left;">

North Carolina counties

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Compilation of law enforcement records & media reports (North Carolina
Coalition Against Domestic Violence)

</td>

<td style="text-align:left;">

Intimate partner homicide

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Goodson & Bouffard

</td>

<td style="text-align:left;">

2019

</td>

<td style="text-align:left;">

Texas counties

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI & Local law enforcement

</td>

<td style="text-align:left;">

Rape & Family violence

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

254

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Gressard et al.

</td>

<td style="text-align:left;">

2015

</td>

<td style="text-align:left;">

Adolescents - All U.S. (nationally representative survey)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (YRBS)

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Education & Political representation & Health indicators

</td>

<td style="text-align:right;">

38

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Haynie & Armstrong

</td>

<td style="text-align:left;">

2006

</td>

<td style="text-align:left;">

Cities (population \> 100,000 & Black residents \> 5,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Intimate partner homicide & Female homicide

</td>

<td style="text-align:left;">

City & MSA

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

148

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Henke & Lin-chi

</td>

<td style="text-align:left;">

2020

</td>

<td style="text-align:left;">

All U.S. (surveillance)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

2898939

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Jackson

</td>

<td style="text-align:left;">

2016

</td>

<td style="text-align:left;">

Chicago neighborhoods (female primary caregivers of children, married or
cohabitating)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

Neighborhood

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

2463

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Johnson

</td>

<td style="text-align:left;">

2013

</td>

<td style="text-align:left;">

Kansas counties

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Economic & Political representation

</td>

<td style="text-align:right;">

105

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Kawachi et al.

</td>

<td style="text-align:left;">

1999

</td>

<td style="text-align:left;">

All U.S. (surveillance)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

CDC

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Political representation & Legal rights

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Kearns et al.

</td>

<td style="text-align:left;">

2020

</td>

<td style="text-align:left;">

All U.S. (nationally representative survey)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (NISVS)

</td>

<td style="text-align:left;">

Rape & Sexual assault & Harassment

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Education & Political representation & Health indicators

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

20

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Lee et al.

</td>

<td style="text-align:left;">

2009

</td>

<td style="text-align:left;">

Large MSAs (75 of the 100 largest)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

MSA

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

75

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Martin et al.

</td>

<td style="text-align:left;">

2006

</td>

<td style="text-align:left;">

Cities (population \> 100,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

228

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Okeke et al.

</td>

<td style="text-align:left;">

2019

</td>

<td style="text-align:left;">

Adolescents - All U.S. (nationally representative survey)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (STRiV)

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

Neighborhood

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

723

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Pazzani

</td>

<td style="text-align:left;">

2007

</td>

<td style="text-align:left;">

All U.S. (nationally representative survey)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (STRiV)

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Education & Political representation

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Peterson & Bailey

</td>

<td style="text-align:left;">

1992

</td>

<td style="text-align:left;">

Cities (population \> 250,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

263

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Powers et al.

</td>

<td style="text-align:left;">

2018

</td>

<td style="text-align:left;">

Los Angeles neighborhoods

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Local law enforcement

</td>

<td style="text-align:left;">

Assault

</td>

<td style="text-align:left;">

Neighborhood

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

832

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Pridemore & Freilich

</td>

<td style="text-align:left;">

2005

</td>

<td style="text-align:left;">

All U.S. (surveillance)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

CDC

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Raj et al.

</td>

<td style="text-align:left;">

2020

</td>

<td style="text-align:left;">

All employed U.S. adults (nationally representative survey)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (Stop Street Harassment)

</td>

<td style="text-align:left;">

Harassment

</td>

<td style="text-align:left;">

Occupation & Industry

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

610

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Reckdenwald & Parker

</td>

<td style="text-align:left;">

2012

</td>

<td style="text-align:left;">

Cities (population \> 100,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Intimate partner homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

172

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Smith & Chiricos

</td>

<td style="text-align:left;">

2003

</td>

<td style="text-align:left;">

Florida counties

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

Local law enforcement

</td>

<td style="text-align:left;">

Assault

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

60

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Stout

</td>

<td style="text-align:left;">

1992

</td>

<td style="text-align:left;">

All U.S. (surveillance)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Intimate partner homicide

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Political representation

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Straus

</td>

<td style="text-align:left;">

1994

</td>

<td style="text-align:left;">

All U.S. (nationally representative survey)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (National Family Violence Survey)

</td>

<td style="text-align:left;">

Assault

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Political representation & Legal rights

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Titterington

</td>

<td style="text-align:left;">

2006

</td>

<td style="text-align:left;">

Central cities

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education & Political representation

</td>

<td style="text-align:right;">

217

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Vieraitis & Williams

</td>

<td style="text-align:left;">

2002

</td>

<td style="text-align:left;">

Cities (population \> 100,000 & Black residents \> 2,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

158

</td>

<td style="text-align:right;">

12

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Vieraitis et al. (2007)

</td>

<td style="text-align:left;">

2007

</td>

<td style="text-align:left;">

All U.S. (surveillance)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

CDC

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

County

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

3083

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Vieraitis et al. (2008)

</td>

<td style="text-align:left;">

2008

</td>

<td style="text-align:left;">

Cities (population \> 100,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Intimate partner homicide & Female homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

206

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Vieraitis et al. (2015)

</td>

<td style="text-align:left;">

2015

</td>

<td style="text-align:left;">

Cities (population \> 100,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Female homicide & Intimate partner homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

165

</td>

<td style="text-align:right;">

15

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Whaley

</td>

<td style="text-align:left;">

2001

</td>

<td style="text-align:left;">

Cities (population \> 50,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Rape

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

101

</td>

<td style="text-align:right;">

20

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Whaley & Messner

</td>

<td style="text-align:left;">

2002

</td>

<td style="text-align:left;">

Cities (population \> 100,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

City

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

64

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Whaley et al.

</td>

<td style="text-align:left;">

2013

</td>

<td style="text-align:left;">

Cities (population \> 100,000)

</td>

<td style="text-align:left;">

Surveillance

</td>

<td style="text-align:left;">

FBI

</td>

<td style="text-align:left;">

Female homicide

</td>

<td style="text-align:left;">

MSA

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

208

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Willie & Kershaw

</td>

<td style="text-align:left;">

2019

</td>

<td style="text-align:left;">

All U.S. (nationally representative survey)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (NISVS)

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Education & Health indicators

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Xie et al. (2011)

</td>

<td style="text-align:left;">

2011

</td>

<td style="text-align:left;">

Largest MSAs

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (NCVS)

</td>

<td style="text-align:left;">

IPV & Assault

</td>

<td style="text-align:left;">

MSA

</td>

<td style="text-align:left;">

Economic & Education & Political representation

</td>

<td style="text-align:right;">

200

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Xie et al. (2012)

</td>

<td style="text-align:left;">

2012

</td>

<td style="text-align:left;">

Largest MSAs

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey (NCVS)

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

MSA

</td>

<td style="text-align:left;">

Economic & Education

</td>

<td style="text-align:right;">

487166

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Yllö (1983)

</td>

<td style="text-align:left;">

1983

</td>

<td style="text-align:left;">

National survey (30 states, further details unavailable)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic & Education & Political representation & Legal rights

</td>

<td style="text-align:right;">

2078

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:left;">

Yes

</td>

</tr>

<tr>

<td style="text-align:left;">

Yllö (1984)

</td>

<td style="text-align:left;">

1984

</td>

<td style="text-align:left;">

National survey (30 states, further details unavailable)

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

Survey

</td>

<td style="text-align:left;">

IPV

</td>

<td style="text-align:left;">

State

</td>

<td style="text-align:left;">

Economic

</td>

<td style="text-align:right;">

2078

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

No

</td>

</tr>

</tbody>

</table>

``` r
#        bootstrap_options = "condensed"
#)
```

</details>

## Step 3: Transform effect sizes into a uniform statistic

Make a set of the data for analysis, excluding the effect sizes for
which we don’t have enough information.

  - **Aizer, Avakame, & Henke:** Unable to determine total number of
    fixed effect betas, so unable to determine the total number of
    predictors in the regression model (necessary for calculating
    partial correlation).
  - **Reckdenwald:** I had to remove the Reckdenwald study, because
    `escalc` gave me the error that the degrees of freedom were \< 1.
    Likely because something about the fixed effects betas wasn’t fully
    captured in my reading of the text.
  - **Whaley (2013)**: Can’t capture squared term in our partial
    correlation conversion.
  - **Yllo (1984)**: Sample size not reported.

<!-- end list -->

``` r
reg_set = em_level %>% 
  #Include all of the ones we have complete info for, as well as the ones to impute (the "maybe"s)
  filter(Included_in_meta_regression == "Included in regression" | 
           startsWith(as.character(Include_in_regression), "Maybe")) %>% 
  #Make nice names for printing on figures
  mutate(Authors_abbr = case_when(Study_ID %in% 36:38 ~ "Vieraitis et al.",
                                  Study_ID %in% 43:44 ~ "Xie et al.",
                                  Study_ID %in% 45:46 ~ "Yllö",
                                  TRUE ~ Authors_abbr),
         Nickname = ifelse(is.na(Nickname), "", paste0(": ", Nickname)))
```

**Convert between different types of Standard Errors & Betas:**

To calculate a partial correlation from `escalc`, we need: **T-test
statistic, sample size, the number of predictors in the regression
model.**

Therefore, we need to do some conversions of the information we have,
when it doesn’t conform completely:

1.  *If we’re given an exponentiated Beta (ex. an Odds Ratio)*: take the
    natural log to get Beta.
2.  *If we’re given a Standard Error*: calculate the t-test with: `t =
    Beta / SE`.
3.  *If we’re given a 95% CI (in our case, this only happens with
    reported exponentiated Betas)*: calculate the SE with:
    `(ln(CI_upper) - Beta) / 1.96`. Then calculate the t-test using the
    formula from \#2.
4.  *If we’re given a z-score*: I think this is actually just the same
    as a t-test statistic.
5.  *If we’re given a p-value*: calculate the standard error using the
    formula: `SE = Beta / Z`, and using `z = abs(qnorm(p))`.
6.  *If we’re given a Pearson’s correlation coefficient*: calculate the
    t-test with: `t = r * sqrt((n - 2) / (1 - r^2))`.
7.  *If we’re given a partial correlation coefficient*: calculate the
    t-test with: `t = r * sqrt((n - 2 - k) / (1 - r^2))`, where k is the
    number of other variables we’re conditioning on. (and actually, I’ve
    checked back after calculating that the partial correlation
    calculated by `escalc` is the same as the original provided in the
    studies)

Finally, for every measure, we need to capture the direction of the
association, by taking into account whether a higher reported exposure
was defined to indicate more or less sexism.

``` r
reg_set = reg_set %>% 
  mutate(
    ES_beta = ifelse(is.na(ES_beta), log(ES_exponentiated_beta), ES_beta),
    VAR_standard_error = case_when(
      !is.na(VAR_standard_error) ~ VAR_standard_error,
      !is.na(VAR_CI_upper) ~ (log(VAR_CI_upper) - ES_beta) / 1.96,
      !is.na(VAR_p_value_exact) ~ ES_beta / abs(qnorm(VAR_p_value_exact)),
      !is.na(VAR_t_test) & !is.na(ES_beta) ~ ES_beta / VAR_t_test,
      #back-fill the standard error from the t-test and beta, to use for the imputation of standard error later on.
      TRUE ~ NA_real_
    ),
    # There are a few options for where we get the t-test values from:
    # Just use from the study if they reported a beta & t-test:
    VAR_t_test = case_when((!is.na(ES_beta) &
                              !is.na(VAR_t_test)) ~ VAR_t_test,
                           # Treat any z-scores like t-test statistics:
                           (!is.na(ES_beta) &
                              !is.na(VAR_z_score)) ~ VAR_z_score,
                           # If we have a beta and a SE (reported or calculated), we can use this simple formula:
                           (!is.na(ES_beta) &
                              !is.na(VAR_standard_error)) ~ ES_beta / VAR_standard_error,
                           # If we have a correlation, we can use this formula along with sample size:
                           (!is.na(ES_correlation)) ~ ES_correlation * sqrt((Sample_size - 2) / (1 - ES_correlation ^2)),
                           # If we have a partial correlation coefficient, we can use this formula:
                           (!is.na(ES_partial_correlation_coefficient)) ~ 
                             ES_partial_correlation_coefficient * sqrt((Sample_size - 2 - as.numeric(Num_predictors_total) - 1) / (1 - ES_partial_correlation_coefficient ^2)),
                           TRUE ~ 999
    ),
    VAR_t_test = ifelse(
      Higher_exposure_means == "LESS sexism",
      VAR_t_test,
      VAR_t_test * -1
    )
  )

# Taking a look at these newly-calculated statistics:
check = reg_set %>%
  select(
    Authors_abbr,
    ES_beta,
    ES_exponentiated_beta,
    ES_partial_correlation_coefficient,
    ES_correlation,
    VAR_CI_upper,
    VAR_z_score,
    VAR_p_value_range,
    VAR_p_value_exact,
    VAR_standard_error,
    VAR_t_test,
    Higher_exposure_means,
    Sample_size,
    Num_predictors_total
  )
```

**Impute missing variance measures**

22 effect sizes were provided with only a p-value range (ex. \<0.05), so
I am imputing the variance. Specifically, we are imputing
`VAR_standard_error` for those effect sizes where `VAR_p_value_range` is
not null. In the code below I choose which variables to use in the
imputation and not.

Use passive imputation to impute standard errors where necessary, and
add `escalc`’s `yi` and `vi` into the imputed datasets as they are
built. (following along with the code here:
<https://github.com/amices/mice/issues/34#issuecomment-362720402>)

``` r
# Make empty columns to fill up over the course of the passive imputation
yi <- NA
vi <- NA
new_t_test <- NA
reg_set_to_impute <- cbind(reg_set, new_t_test, yi, vi)

# This is the set of predictors I think are meaningful to include for imputation.
# I had to comment out three, because leaving any one of them in meant that mice's linear regression couldn't run
#   --> (error: "system is computationally singular").
# Predictors not included were ones that were either all collinear with each other,
#   that were study-specific, or that were notes and not data.
to_include_as_predictors = c(
  "Field",
  "Survey_surveillance",
  "Source_population",
  "Study_design",
  #"Outcome_data_source_condensed",
  "Decade",
  #"Exposure_level",
  "Sample_size",
  #"Outcome_type_factor",
  "Exposure_category_employment",
  "Exposure_category_financial",
  "Exposure_category_education",
  "Exposure_category_political",
  "Exposure_category_legal_rights",
  "Exposure_category_womens_health",
  "Exposure_absolute_or_relative",
  "Higher_exposure_means",
  "Num_betas_not_fixed_effects",
  "Num_betas_fixed_effects",
  "Model_type",
  "ES_beta",
  "VAR_standard_error",
  "Num_EMs_from_this_study",
  "Include_in_regression"
)

to_exclude = setdiff(colnames(reg_set_to_impute), to_include_as_predictors)

#do a fake mice setup, so that we have the scaffolding for a methods and a prediction matrix to build on:
setup = mice(reg_set_to_impute, maxit = 0) 
my_methods = setup$method
my_predMatrix = setup$predictorMatrix

#alter the prediction matrix to exclude some variables from being predictors during imputation
my_predMatrix[, to_exclude] = 0

#alter the methods matrix, to exclude any variables we don't want to impute.
#  we only want to impute VAR_standard_error, and only for those where  `VAR_p_value_range` is not null.
my_methods[] = "" #set all to be blank
my_methods["VAR_standard_error"] = "pmm"

#decide what to impute: (only standard errors for effect sizes where it wasn't provided)
vec = reg_set_to_impute$Include_in_regression == "Maybe - impute unknown variance, double-check with provided p-value range"
mat = matrix(data = FALSE, nrow = nrow(reg_set_to_impute), ncol = ncol(reg_set_to_impute))
mat[, which(colnames(reg_set_to_impute) == "VAR_standard_error")] = vec

# Do the passive imputation
ini <- mice(reg_set_to_impute,
            method = my_methods, 
            predictorMatrix = my_predMatrix,
            maxit = 0)
meth <- ini$meth

meth["new_t_test"] <- '~ case_when(
      abs(VAR_t_test) == 999 &
        Higher_exposure_means == "LESS sexism" ~ -1 * ES_beta / VAR_standard_error,
      Include_in_regression == "Maybe - impute unknown variance, double-check with provided p-value range" &
        Higher_exposure_means == "MORE sexism" ~ ES_beta / VAR_standard_error,
      TRUE ~ VAR_t_test)'
meth["yi"] <- "~ escalc(
    measure = 'PCOR',
    ti = new_t_test,
    ni = as.numeric(Sample_size),
    mi = as.numeric(Num_predictors_total)
  )$yi"
meth["vi"] <- "~ escalc(
    measure = 'PCOR',
    ti = new_t_test,
    ni = as.numeric(Sample_size),
    mi = as.numeric(Num_predictors_total)
  )$vi"

#Final mice object with our 5 imputed datasets
with_imputed_values <- mice(reg_set_to_impute, pred=my_predMatrix, meth=meth)
```

    ## 
    ##  iter imp variable
    ##   1   1  VAR_standard_error*  new_t_test  yi  vi
    ##   1   2  VAR_standard_error*  new_t_test  yi  vi
    ##   1   3  VAR_standard_error*  new_t_test  yi  vi
    ##   1   4  VAR_standard_error*  new_t_test  yi  vi
    ##   1   5  VAR_standard_error*  new_t_test  yi  vi
    ##   2   1  VAR_standard_error*  new_t_test  yi  vi
    ##   2   2  VAR_standard_error*  new_t_test  yi  vi
    ##   2   3  VAR_standard_error*  new_t_test  yi  vi
    ##   2   4  VAR_standard_error*  new_t_test  yi  vi
    ##   2   5  VAR_standard_error*  new_t_test  yi  vi
    ##   3   1  VAR_standard_error*  new_t_test  yi  vi
    ##   3   2  VAR_standard_error*  new_t_test  yi  vi
    ##   3   3  VAR_standard_error*  new_t_test  yi  vi
    ##   3   4  VAR_standard_error*  new_t_test  yi  vi
    ##   3   5  VAR_standard_error*  new_t_test  yi  vi
    ##   4   1  VAR_standard_error*  new_t_test  yi  vi
    ##   4   2  VAR_standard_error*  new_t_test  yi  vi
    ##   4   3  VAR_standard_error*  new_t_test  yi  vi
    ##   4   4  VAR_standard_error*  new_t_test  yi  vi
    ##   4   5  VAR_standard_error*  new_t_test  yi  vi
    ##   5   1  VAR_standard_error*  new_t_test  yi  vi
    ##   5   2  VAR_standard_error*  new_t_test  yi  vi
    ##   5   3  VAR_standard_error*  new_t_test  yi  vi
    ##   5   4  VAR_standard_error*  new_t_test  yi  vi
    ##   5   5  VAR_standard_error*  new_t_test  yi  vi

**Calculate partial correlation coefficients from EMs using SE & Beta:**

This has already been done for the imputed datasets, and we’re repeating
these steps for the dataset excluding effect sizes with missing variance
values.

``` r
no_imputed_values = reg_set %>% 
  filter(Include_in_regression != "Maybe - impute unknown variance, double-check with provided p-value range")

for_analysis_no_imputed_values = escalc(
    measure = "PCOR",
    data = no_imputed_values,
    ti = VAR_t_test,
    ni = as.numeric(Sample_size),
    mi = as.numeric(Num_predictors_total)
  )

paste0("Number of effects, total: ", nrow(all_data))
```

    ## [1] "Number of effects, total: 235"

``` r
paste0("Number of effects left after excluding some and imputing others: ", nrow(complete(with_imputed_values, action = 1)))
```

    ## [1] "Number of effects left after excluding some and imputing others: 226"

``` r
paste0("Number of effects left after excluding all incomplete: ", nrow(no_imputed_values))
```

    ## [1] "Number of effects left after excluding all incomplete: 204"

## Step 4: Build our regression model, conduct statistical tests, create plots

<details>

<summary> Click for references, instructions, some choices we’re making,
& set-up code. </summary>

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
        estimate the fixed effects component as constant, `rho = 0.6`
        (some references suggest `0.6`, some `0.8`)
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
        <https://osf.io/nyv4u/>* –\> NOT using this structure in our
        analysis.

**Define some constants and helper objects**

``` r
rho <- 0.6

# Make the covariance matrix for the non-imputed data
cov_matrix_no_imputed = impute_covariance_matrix(
    vi = for_analysis_no_imputed_values$vi,
    cluster = for_analysis_no_imputed_values$Study_ID,
    r = rho,
    smooth_vi = FALSE
  )

# Make the covariance matrix for the imputed data
first_imputation = complete(with_imputed_values, action = 1)

cov_matrix_first_imputation = impute_covariance_matrix(
    vi = first_imputation$vi,
    cluster = first_imputation$Study_ID,
    r = rho,
    smooth_vi = FALSE
  )
```

**First, check whether including the imputed values impacts the
analysis**

Metafor/mice reference:
<https://www.metafor-project.org/doku.php/tips:multiple_imputation_with_mice_and_metafor?s>\[\]=impute
\*
<https://www.metafor-project.org/doku.php/tips:multiple_factors_interactions>

``` r
# Create the meta regression models for both
summary_model_NO_imputation <- rma.mv(
  yi,
  V = cov_matrix_no_imputed,
  random = ~ 1 |
    Study_ID / Effect_ID,
  data = for_analysis_no_imputed_values
)

summary_model_WITH_imputation <- with(
  with_imputed_values,
  rma.mv(yi,
         V = cov_matrix_first_imputation,
         random = ~ 1 |
           Study_ID / Effect_ID)
)
```

    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.
    
    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.
    
    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.
    
    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.
    
    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.

``` r
# Look at the summary measure for both
paste0("Summary stat for complete case analysis (using just metafor):")
```

    ## [1] "Summary stat for complete case analysis (using just metafor):"

``` r
summary(summary_model_NO_imputation)
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 204; method: REML)
    ## 
    ##   logLik  Deviance       AIC       BIC      AICc 
    ##  43.3493  -86.6986  -80.6986  -70.7590  -80.5780   
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed              factor 
    ## sigma^2.1  0.0317  0.1781     34     no            Study_ID 
    ## sigma^2.2  0.0229  0.1515    204     no  Study_ID/Effect_ID 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 203) = 8488.6619, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.0087  0.0368  0.2365  0.8130  -0.0635  0.0809    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paste0("Summary stat for complete case analysis (using clubSandwich, to guard against misspecification):")
```

    ## [1] "Summary stat for complete case analysis (using clubSandwich, to guard against misspecification):"

``` r
conf_int(summary_model_NO_imputation, vcov = "CR2")
```

    ##      Coef Estimate     SE d.f. Lower 95% CI Upper 95% CI
    ## 1 intrcpt  0.00871 0.0368 31.7      -0.0663       0.0837

``` r
paste0("Summary stat for data including imputed variance values (using just metafor):")
```

    ## [1] "Summary stat for data including imputed variance values (using just metafor):"

``` r
summary(pool(summary_model_WITH_imputation),
        conf.int = TRUE,
        conf.level = 0.95)
```

    ##      term   estimate  std.error statistic       df   p.value       2.5 %
    ## 1 overall 0.01495151 0.03072834 0.4865706 213.8845 0.6270609 -0.04561765
    ##       97.5 %
    ## 1 0.07552066

``` r
summary_model_WITH_imputation2 <- with(
  data = with_imputed_values,
  rma.mv(yi,
         V = cov_matrix_first_imputation,
         random = ~ 1 |
           Study_ID / Effect_ID) %>% 
    coef_test(cluster = first_imputation$Study_ID, vcov = "CR2")
)
```

    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.
    
    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.
    
    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.
    
    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.
    
    ## Warning in rma.mv(yi, V = cov_matrix_first_imputation, random = ~1 | Study_ID/
    ## Effect_ID): Ratio of largest to smallest sampling variance extremely large. May
    ## not be able to obtain stable results.

Reference for how to pool results from imputed datasets using
cluster-robust variance estimators from clubSandwich:
<https://www.jepusto.com/mi-with-clubsandwich/>

``` r
robust_pooled <- 
  summary_model_WITH_imputation2$analyses %>%
  
  # add coefficient names as a column
  lapply(function(x) {
    x$coef <- row.names(x)
    x
  }) %>%
  bind_rows() %>%
  as.data.frame() %>%
  
  # summarize by coefficient
  group_by(coef) %>%
  summarise(
    m = n(),
    B = var(beta),
    beta_bar = mean(beta),
    V_bar = mean(SE^2),
    eta_bar = mean(df)
  ) %>%
  
  mutate(
    
    # calculate intermediate quantities to get df
    V_total = V_bar + B * (m + 1) / m,
    gamma = ((m + 1) / m) * B / V_total,
    df_m = (m - 1) / gamma^2,
    df_obs = eta_bar * (eta_bar + 1) * (1 - gamma) / (eta_bar + 3),
    df = 1 / (1 / df_m + 1 / df_obs),
    
    # calculate summary quantities for output
    se = sqrt(V_total),
    t = beta_bar / se,
    p_val = 2 * pt(abs(t), df = df, lower.tail = FALSE),
    crit = qt(0.975, df = df),
    lo95 = beta_bar - se * crit,
    hi95 = beta_bar + se * crit
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
paste0("Summary stat for data including imputed variance values (clubSandwich):")
```

    ## [1] "Summary stat for data including imputed variance values (clubSandwich):"

``` r
robust_pooled %>%
  select(coef, est = beta_bar, se, t, df, p_val, lo95, hi95, gamma) %>%
  mutate_at(vars(est:gamma), round, 3)
```

    ## # A tibble: 1 x 9
    ##   coef      est    se     t    df p_val   lo95  hi95 gamma
    ##   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1 intrcpt 0.015 0.031 0.488  32.3 0.629 -0.047 0.077  0.02

*Conclusion:* Using or excluding the imputed values would not change our
conclusion when looking at the summary statistic produced by this
meta-regression analysis. Both estimates are similarly small and
positive (`0.0087` vs. `0.015`), and the two confidence intervals nearly
completely overlap (`(-0.066, 0.084)` vs. `(-0.047, 0.077)`), both
including zero indicating an overall effect that is not statistically
significant.

Moving forward, I will use complete case analysis (only effect sizes
that were reported with an exact variance).

**Meta-regression analyses on complete cases**

Create stratified datasets:

``` r
for_analysis_ALL = for_analysis_no_imputed_values

for_analysis_BLACK_WHITE = for_analysis_no_imputed_values %>%
  filter(Race != 999)

for_analysis_SURVEY = for_analysis_no_imputed_values %>%
  filter(Survey_surveillance == "Survey")

for_analysis_SURVEILLANCE = for_analysis_no_imputed_values %>%
  filter(Survey_surveillance == "Surveillance")

for_analysis_EXPOSURE_LEVEL_CONDENSED = for_analysis_no_imputed_values %>%
  filter(Exposure_level_condensed != 999)
```

Create a covariance matrix for each subset:

``` r
cov_matrix_ALL = cov_matrix_no_imputed

cov_matrix_BLACK_WHITE <- impute_covariance_matrix(
  vi = for_analysis_BLACK_WHITE$vi, cluster = for_analysis_BLACK_WHITE$Study_ID, r = rho, smooth_vi = FALSE
)

cov_matrix_SURVEY <- impute_covariance_matrix(
  vi = for_analysis_SURVEY$vi, cluster = for_analysis_SURVEY$Study_ID, r = rho, smooth_vi = FALSE
)

cov_matrix_SURVEILLANCE <- impute_covariance_matrix(
  vi = for_analysis_SURVEILLANCE$vi, cluster = for_analysis_SURVEILLANCE$Study_ID, r = rho, smooth_vi = FALSE
)

cov_matrix_EXPOSURE_LEVEL_CONDENSED <- impute_covariance_matrix(
  vi = for_analysis_EXPOSURE_LEVEL_CONDENSED$vi, cluster = for_analysis_EXPOSURE_LEVEL_CONDENSED$Study_ID, r = rho, smooth_vi = FALSE
)
```

Create a model to look at our main moderation question of interest: is
the relationship between structural gender inequality and women’s
victimization rates moderated by the data source in the analysis being
from surveys or administrative/surveillance data?

``` r
ModerationQuestion <- rma.mv(yi = yi,
                       #vi = vi,
                       data = for_analysis_ALL,
                       V = cov_matrix_ALL,
                       mods = ~ Survey_surveillance,
                       random = ~ 1 | Study_ID / Effect_ID)

# Moderation test and other results from metafor package
ModerationQuestion
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 204; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed              factor 
    ## sigma^2.1  0.0284  0.1685     34     no            Study_ID 
    ## sigma^2.2  0.0229  0.1514    204     no  Study_ID/Effect_ID 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 202) = 5341.8396, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.8397, p-val = 0.0501
    ## 
    ## Model Results:
    ## 
    ##                                  estimate      se     zval    pval    ci.lb 
    ## intrcpt                           -0.0921  0.0627  -1.4700  0.1416  -0.2150 
    ## Survey_surveillanceSurveillance    0.1488  0.0760   1.9595  0.0501  -0.0000 
    ##                                   ci.ub 
    ## intrcpt                          0.0307    
    ## Survey_surveillanceSurveillance  0.2977  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Moderation test and confidence interval from clubSandwich package
coef_test(ModerationQuestion, vcov = "CR2")
```

    ##                             Coef. Estimate     SE t-stat  d.f. p-val (Satt)
    ## 1                         intrcpt  -0.0921 0.0350  -2.63  9.47       0.0261
    ## 2 Survey_surveillanceSurveillance   0.1488 0.0602   2.47 18.59       0.0232
    ##   Sig.
    ## 1    *
    ## 2    *

``` r
conf_int(ModerationQuestion, vcov = "CR2")
```

    ##                              Coef Estimate     SE  d.f. Lower 95% CI
    ## 1                         intrcpt  -0.0921 0.0350  9.47      -0.1707
    ## 2 Survey_surveillanceSurveillance   0.1488 0.0602 18.59       0.0227
    ##   Upper 95% CI
    ## 1      -0.0136
    ## 2       0.2750

Fit models with no moderators, to estimate the summary statistic and for
use in funnel and forest plots:

``` r
no_moderators_ALL <- rma.mv(yi ~ 1,
  V = cov_matrix_ALL, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_ALL
)

summary(no_moderators_ALL)
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 204; method: REML)
    ## 
    ##   logLik  Deviance       AIC       BIC      AICc 
    ##  43.3493  -86.6986  -80.6986  -70.7590  -80.5780   
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed              factor 
    ## sigma^2.1  0.0317  0.1781     34     no            Study_ID 
    ## sigma^2.2  0.0229  0.1515    204     no  Study_ID/Effect_ID 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 203) = 8488.6619, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.0087  0.0368  0.2365  0.8130  -0.0635  0.0809    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
no_moderators_SURVEY <- rma.mv(yi ~ 1,
  V = cov_matrix_SURVEY, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_SURVEY
)

no_moderators_SURVEILLANCE <- rma.mv(yi ~ 1,
  V = cov_matrix_SURVEILLANCE, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_SURVEILLANCE
)
```

Fit models to test each variable of interest, together and separately:

``` r
########################### Decade #####################
decade_model_ALL <- rma.mv(yi ~ 0 + Decade,
  V = cov_matrix_ALL, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_ALL
)

########################### Exposure level #####################
level_ALL <- rma.mv(yi ~ 0 + Exposure_level_condensed,
  V = cov_matrix_ALL, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_ALL
)

########################### Outcome_data_source #####################
data_SURVEILLANCE <- rma.mv(yi ~ 0 + Outcome_data_source_condensed,
  V = cov_matrix_SURVEILLANCE, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_SURVEILLANCE
)
```

    ## Warning in rma.mv(yi ~ 0 + Outcome_data_source_condensed, V =
    ## cov_matrix_SURVEILLANCE, : Redundant predictors dropped from the model.

``` r
########################### Victimization type #####################
vic_ALL <- rma.mv(yi ~ 0 + Victimization_type,
  V = cov_matrix_ALL, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_ALL
)

########################### Perpetrator relationship #####################
perp_ALL <- rma.mv(yi ~ 0 + Perpetrator_relationship,
  V = cov_matrix_ALL, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_ALL
)

########################### Exposure_level_condensed #####################
exp_EXPOSURE_LEVEL_CONDENSED <- rma.mv(yi ~ 0 + Exposure_level_condensed,
  V = cov_matrix_EXPOSURE_LEVEL_CONDENSED, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_EXPOSURE_LEVEL_CONDENSED
)
```

    ## Warning in rma.mv(yi ~ 0 + Exposure_level_condensed, V =
    ## cov_matrix_EXPOSURE_LEVEL_CONDENSED, : Redundant predictors dropped from the
    ## model.

``` r
########################### Survey / surveillance #####################
survey_surveillance_ALL <- rma.mv(yi ~ 0 + Survey_surveillance,
  V = cov_matrix_ALL, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_ALL
)

########################### Race #####################
race_BLACK_WHITE <- rma.mv(yi ~ 0 + Race,
  V = cov_matrix_BLACK_WHITE, random = ~ 1 | Study_ID / Effect_ID, data = for_analysis_BLACK_WHITE
)
```

    ## Warning in rma.mv(yi ~ 0 + Race, V = cov_matrix_BLACK_WHITE, random = ~1 | :
    ## Redundant predictors dropped from the model.

</details>

### Meta-regression output:

``` r
########################## No moderators #############################
#coef_test(no_moderators_ALL, vcov = "CR2")
conf_int(no_moderators_ALL, vcov = "CR2")
```

    ##      Coef Estimate     SE d.f. Lower 95% CI Upper 95% CI
    ## 1 intrcpt  0.00871 0.0368 31.7      -0.0663       0.0837

``` r
#coef_test(no_moderators_SURVEY, vcov = "CR2")
conf_int(no_moderators_SURVEY, vcov = "CR2")
```

    ##      Coef Estimate     SE d.f. Lower 95% CI Upper 95% CI
    ## 1 intrcpt  -0.0777 0.0289 6.13       -0.148     -0.00733

``` r
#coef_test(no_moderators_SURVEILLANCE, vcov = "CR2")
conf_int(no_moderators_SURVEILLANCE, vcov = "CR2")
```

    ##      Coef Estimate     SE d.f. Lower 95% CI Upper 95% CI
    ## 1 intrcpt    0.056 0.0496 21.5      -0.0469        0.159

``` r
########################### Survey / surveillance #####################
#coef_test(survey_surveillance_ALL, vcov = "CR2")
conf_int(survey_surveillance_ALL, vcov = "CR2")
```

    ##                              Coef Estimate    SE  d.f. Lower 95% CI
    ## 1       Survey_surveillanceSurvey  -0.0921 0.035  9.47      -0.1707
    ## 2 Survey_surveillanceSurveillance   0.0567 0.049 21.11      -0.0451
    ##   Upper 95% CI
    ## 1      -0.0136
    ## 2       0.1585

``` r
########################### Exposure_level_condensed #####################
#coef_test(exp_EXPOSURE_LEVEL_CONDENSED, vcov = "CR2")
conf_int(exp_EXPOSURE_LEVEL_CONDENSED, vcov = "CR2")
```

    ##                               Coef Estimate     SE d.f. Lower 95% CI
    ## 1 Exposure_level_condensedCity/MSA   0.0378 0.0209 14.6     -0.00676
    ## 2   Exposure_level_condensedCounty   0.0713 0.0442  2.9     -0.07217
    ## 3    Exposure_level_condensedState  -0.0453 0.1133  9.6     -0.29910
    ##   Upper 95% CI
    ## 1       0.0824
    ## 2       0.2148
    ## 3       0.2086

``` r
########################### Race #####################
#coef_test(race_BLACK_WHITE, vcov = "CR2")
conf_int(race_BLACK_WHITE, vcov = "CR2")
```

    ##        Coef Estimate    SE d.f. Lower 95% CI Upper 95% CI
    ## 1 RaceBlack    0.234 0.212 2.95       -0.448        0.916
    ## 2 RaceWhite    0.233 0.226 2.99       -0.486        0.952

``` r
########################### Decade #####################
#coef_test(decade_model_ALL, vcov = "CR2")
conf_int(decade_model_ALL, vcov = "CR2")
```

    ##          Coef Estimate     SE  d.f. Lower 95% CI Upper 95% CI
    ## 1 Decade1970s  -0.0261 0.0580  2.21      -0.2538       0.2015
    ## 2 Decade1980s  -0.0625 0.0402  6.10      -0.1605       0.0355
    ## 3 Decade1990s   0.0346 0.0247 10.92      -0.0198       0.0891
    ## 4 Decade2000s   0.0773 0.0805  7.51      -0.1104       0.2649
    ## 5 Decade2010s  -0.0877 0.0560  3.94      -0.2441       0.0688

``` r
########################### Exposure level #####################
conf_int(exp_EXPOSURE_LEVEL_CONDENSED, vcov = "CR2")
```

    ##                               Coef Estimate     SE d.f. Lower 95% CI
    ## 1 Exposure_level_condensedCity/MSA   0.0378 0.0209 14.6     -0.00676
    ## 2   Exposure_level_condensedCounty   0.0713 0.0442  2.9     -0.07217
    ## 3    Exposure_level_condensedState  -0.0453 0.1133  9.6     -0.29910
    ##   Upper 95% CI
    ## 1       0.0824
    ## 2       0.2148
    ## 3       0.2086

``` r
########################### Outcome_data_source #####################
#coef_test(data_SURVEILLANCE, vcov = "CR2")
conf_int(data_SURVEILLANCE, vcov = "CR2")
```

    ##                                                 Coef Estimate     SE  d.f.
    ## 1                   Outcome_data_source_condensedFBI   0.0330 0.0292 17.25
    ## 2 Outcome_data_source_condensedLocal law enforcement   0.0102 0.0520  1.85
    ## 3                   Outcome_data_source_condensedCDC   0.2289 0.2543  2.89
    ##   Lower 95% CI Upper 95% CI
    ## 1      -0.0286       0.0945
    ## 2      -0.2315       0.2519
    ## 3      -0.5980       1.0558

``` r
########################### Victimization type #####################
#coef_test(vic_ALL, vcov = "CR2")
conf_int(vic_ALL, vcov = "CR2")
```

    ##                                           Coef Estimate     SE  d.f.
    ## 1                   Victimization_typeHomicide   0.0977 0.0788 11.66
    ## 2 Victimization_typeIPV, harassment, & assault  -0.0676 0.0322 11.60
    ## 3      Victimization_typeRape & sexual assault  -0.0277 0.0344  9.89
    ##   Lower 95% CI Upper 95% CI
    ## 1      -0.0745      0.26984
    ## 2      -0.1380      0.00282
    ## 3      -0.1045      0.04900

``` r
########################### Perpetrator relationship #####################
#coef_test(perp_ALL, vcov = "CR2")
conf_int(perp_ALL, vcov = "CR2")
```

    ##                                                     Coef Estimate     SE d.f.
    ## 1      Perpetrator_relationshipDomestic/intimate partner  -0.0278 0.0386 16.6
    ## 2 Perpetrator_relationshipNon-domestic known perpetrator  -0.0241 0.0336  4.4
    ## 3       Perpetrator_relationshipUnknown or not specified   0.0348 0.0443 24.8
    ##   Lower 95% CI Upper 95% CI
    ## 1      -0.1094       0.0537
    ## 2      -0.1141       0.0660
    ## 3      -0.0565       0.1260

### Funnel plot: publication bias

``` r
pdf(file = 'Data/PlotsTables/funnel_plot.pdf') 
funnel(no_moderators_ALL)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
ranktest(no_moderators_ALL)
```

    ## Warning in cor.test.default(yi.star, vi, method = "kendall", exact = TRUE):
    ## Cannot compute exact p-value with ties

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = -0.0108, p = 0.8182

![Funnel Plot](Data/PlotsTables/funnel_plot.pdf).

### Forest plots

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

![Forest plot: all](Data/PlotsTables/forestplot_all.pdf).

![Forest plot: surveillance
only](Data/PlotsTables/forestplot_surveillance_no_lines.pdf).

![Forest plot: survey
only](Data/PlotsTables/forestplot_survey_no_lines.pdf).

![Forest plot: survey only](Data/PlotsTables/forestplot_survey.pdf).
