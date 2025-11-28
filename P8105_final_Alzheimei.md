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
  arrange(desc(mean_value))

az_cog_state_age_sex
```

    ## # A tibble: 448 × 6
    ##    location_abbr location_desc age_group         sex_group n_obs mean_value
    ##    <chr>         <chr>         <chr>             <chr>     <int>      <dbl>
    ##  1 PR            Puerto Rico   50-64 years       Female        4       59.8
    ##  2 PR            Puerto Rico   50-64 years       Overall       8       58.2
    ##  3 PR            Puerto Rico   Overall           Male          4       54.5
    ##  4 PR            Puerto Rico   Overall           Overall       8       53.6
    ##  5 PR            Puerto Rico   Overall           Female        4       52.8
    ##  6 PR            Puerto Rico   65 years or older Overall       8       47.9
    ##  7 RI            Rhode Island  50-64 years       Female        4       47.2
    ##  8 OK            Oklahoma      50-64 years       Female        4       46.6
    ##  9 DE            Delaware      50-64 years       Female        4       46.1
    ## 10 PR            Puerto Rico   65 years or older Female        4       44.7
    ## # ℹ 438 more rows

### top 10 states (2015-2016)

``` r
az_cog_65_overall <- az_cog_summary_base |>
  filter(
    age_group == "65 years or older",
    sex_group == "Overall"
  )

nrow(az_cog_65_overall)
```

    ## [1] 609

``` r
sort(unique(az_cog_65_overall$year_start))
```

    ## [1] 2015 2016

The CDC Healthy Aging dataset covers survey years 2015–2022. For topics
coded as Cognitive Decline or Cognitive Impairment, data were only
available in 2015–2016; therefore, our Alzheimer’s-related analyses are
restricted to these two years.

``` r
library(ggplot2)

az_cog_65_overall <- az_cog_summary_base |>
  filter(
    age_group == "65 years or older",
    sex_group == "Overall"
  )

years_use <- c(2015, 2016)

az_cog_65_overall_15_16 <- az_cog_65_overall |>
  filter(year_start %in% years_use)
```

``` r
top10_state_15_16 <- az_cog_65_overall_15_16 |>
  group_by(location_abbr, location_desc) |>
  summarise(
    n_obs = n(),
    mean_15_16 = mean(data_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mean_15_16)) |>
  slice_head(n = 10)

top10_state_15_16
```

    ## # A tibble: 10 × 4
    ##    location_abbr location_desc  n_obs mean_15_16
    ##    <chr>         <chr>          <int>      <dbl>
    ##  1 PR            Puerto Rico        8       47.9
    ##  2 NRE           Northeast         30       33.6
    ##  3 OK            Oklahoma           8       31.5
    ##  4 MS            Mississippi       14       31.4
    ##  5 SOU           South             30       31.0
    ##  6 SD            South Dakota       8       30.9
    ##  7 MA            Massachusetts      5       30.8
    ##  8 AL            Alabama           14       30.2
    ##  9 AK            Alaska             7       29.8
    ## 10 SC            South Carolina    14       29.5

Using data from 2015–2016, we ranked states and regions by the mean
prevalence of Alzheimer’s-related cognitive indicators among adults aged
65 years and older (all sexes combined). The highest burden was observed
in Puerto Rico (mean prevalence 47.9%), which was notably higher than
any U.S. state or region. The Northeast region ranked second (33.6%),
followed by Oklahoma (31.5%), Mississippi (31.4%), and the South region
as a whole (31.0%). The remaining states in the top ten—South Dakota,
Massachusetts, Alabama, Alaska, and South Carolina—all had mean
prevalences around 30%. Overall, these results suggest substantial
geographic variation in self-reported cognitive problems among older
adults, with several Southern states and Puerto Rico showing
particularly high levels.

The Top 10 list for 2015–2016 is different from the earlier
state–age–sex table because they summarize different things. The
state–age–sex table ranks specific subgroups (for example, women aged
50–64 in Puerto Rico), so it shows where the highest subgroup
prevalences occur. In contrast, the 2015–2016 Top 10 is based only on
adults aged 65+ with sexes combined, giving one value per state. This
ranking reflects overall burden in older adults, not the maximum value
in any age–sex subgroup, and provides useful context for later
stratified analyses or adjustment for potential confounders such as age,
sex, or region.

## Distribution Difference

### prevelance ~ year

``` r
az_nat_65_overall <- az_cog_65_overall |>
  group_by(year = year_start) |>
  summarise(
    mean_prev = mean(data_value, na.rm = TRUE),
    sd_prev   = sd(data_value, na.rm = TRUE),
    .groups = "drop"
  )

az_nat_65_overall |>
  mutate(year = factor(year)) |>
  ggplot(aes(x = year, y = mean_prev)) +
  geom_col() +
  labs(
    x = "Year",
    y = "Mean prevalence (%)",
    title = "Alzheimer’s-related cognitive indicators among adults 65+, 2015–2016"
  )
```

<img src="P8105_final_Alzheimei_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

At the national level, the mean prevalence of Alzheimer’s-related
cognitive indicators among adults aged 65+ was very similar in 2015
(26.0%) and 2016 (25.8%). The small difference of 0.2 percentage points
is well within the between-state variation and does not suggest any
clear change over this two-year period.

### prevelance ~ age

``` r
az_age_desc <- az_cog_summary_base |>
  group_by(age_group) |>
  summarise(
    n_obs      = n(),
    mean_value = mean(data_value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(az_age_desc,
       aes(x = age_group, y = mean_value)) +
  geom_col() +
  labs(
    x = "Age group",
    y = "Mean prevalence (%)",
    title = "Alzheimer’s-related cognitive indicators by age group"
  )
```

<img src="P8105_final_Alzheimei_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />

Across all cognitive items, the mean prevalence was about 31.8% among
adults aged 50–64 years and 25.8% among those aged 65 years or older.

### prevelance ~ sex

``` r
az_sex_desc <- az_cog_summary_base |>
  group_by(sex_group) |>
  summarise(
    n_obs      = n(),
    mean_value = mean(data_value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(az_sex_desc,
       aes(x = sex_group, y = mean_value)) +
  geom_col() +
  labs(
    x = "Sex",
    y = "Mean prevalence (%)",
    title = "Alzheimer’s-related cognitive indicators by sex"
  )
```

<img src="P8105_final_Alzheimei_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />

Across all states and cognitive items in 2015–2016, women had a higher
mean prevalence of Alzheimer’s-related cognitive indicators than men
(30.9% vs 26.8%), suggesting a consistently greater reported cognitive
burden among female respondents.
