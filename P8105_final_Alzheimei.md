P8105_final_Alzheimei
================
bl3175, cw3747, lg3450, yw4662
2025-11-27

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(patchwork)
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(dplyr)


knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Data Import

``` r
raw_az_data <- read.csv("./2015-2022 Alzheimer Data.csv") |>
  janitor::clean_names()
```

    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,
    ## : EOF within quoted string

``` r
raw_az_data <- raw_az_data[-nrow(raw_az_data), ]
```

``` r
az_clean <- raw_az_data |>
  filter(
    !is.na(data_value),
    !is.na(year_start),
    !is.na(location_abbr)
  )
```

## Descriptive

``` r
az_clean |>
  summarise(
    n_rows = n(),
    n_states = n_distinct(location_abbr),
    year_min = min(year_start),
    year_max = max(year_start),
    n_topics = n_distinct(topic),
    n_questions = n_distinct(question)
  )
```

    ##   n_rows n_states year_min year_max n_topics n_questions
    ## 1  32616       59     2015     2022       39          39

We obtained data from the Centers for Disease Control and Prevention
(CDC) Alzheimer’s Disease and Healthy Aging Data, which are derived from
the Behavioral Risk Factor Surveillance System (BRFSS). After excluding
records with missing key identifiers, our analytic dataset contained
**32,616** observations from **59** states and territories, covering
survey years **2015–2022**. The dataset includes **39** distinct topics
and specific questions on health status and health-related behaviors
among middle-aged and older adults.

For this study, we focused on BRFSS items whose topic was labeled
**“cognitive decline”** or **“cognitive impairment”**. These items
capture self-reported cognitive problems among middle-aged and older
adults, and we treat them as Alzheimer’s-related cognitive indicators
rather than clinically confirmed Alzheimer’s disease.

## Data considering Alzheimei

``` r
library(stringr)

az_cog <- az_clean |>
  filter(
    str_detect(topic, regex("cognitive decline|cognitive impairment",
                            ignore_case = TRUE))
  )
```

### sex and age summary

``` r
az_cog_summary_base <- az_cog |>
  mutate(
    age_group = case_when(
      stratification_category1 == "Age Group" ~ stratification1,
      TRUE ~ "Overall"
    ),
    sex_group = case_when(
      stratification_category2 == "Sex" ~ stratification2,
      TRUE ~ "Overall"
    )
  )
```

### state × age × sex table

``` r
az_cog_state_age_sex <- az_cog_summary_base |>
  group_by(
    location_abbr, location_desc,
    age_group,
    sex_group
  ) |>
  summarise(
    n_obs = n(),
    mean_value = mean(data_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(location_abbr, age_group, sex_group)

az_cog_state_age_sex
```

    ## # A tibble: 448 × 6
    ##    location_abbr location_desc age_group         sex_group n_obs mean_value
    ##    <chr>         <chr>         <chr>             <chr>     <int>      <dbl>
    ##  1 AK            Alaska        50-64 years       Female        2       30.8
    ##  2 AK            Alaska        50-64 years       Male          1       10.5
    ##  3 AK            Alaska        50-64 years       Overall       8       29  
    ##  4 AK            Alaska        65 years or older Female        2       32.3
    ##  5 AK            Alaska        65 years or older Male          3       28.4
    ##  6 AK            Alaska        65 years or older Overall       7       29.8
    ##  7 AK            Alaska        Overall           Female        4       30.6
    ##  8 AK            Alaska        Overall           Male          4       28  
    ##  9 AK            Alaska        Overall           Overall       9       26.8
    ## 10 AL            Alabama       50-64 years       Female        5       37.0
    ## # ℹ 438 more rows
