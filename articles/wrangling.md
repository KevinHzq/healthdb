# Data wrangling helpers

The introduction vignette
([`vignette("healthdb")`](https://kevinhzq.github.io/healthdb/articles/healthdb.md))
walks through identifying disease cases from healthcare databases. This
vignette introduces the other half of the package: helper functions for
the work that comes before and after — reporting sample sizes, computing
age, decoding codes, scoring comorbidity, and reshaping records into
episodes and periods.

We will need the following packages:

``` r

library(dplyr)
library(healthdb)
```

Throughout, we will use a small made-up cohort of clients with some
hospital records:

``` r

set.seed(2024)

n <- 10

cohort <- data.frame(
  clnt_id = 1:n,
  birth_dt = sample(seq(as.Date("1950-01-01"), as.Date("2005-12-31"), by = 1), n),
  sex = sample(c("F", "M"), n, replace = TRUE),
  index_dt = sample(seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by = 1), n)
)

head(cohort)
#>   clnt_id   birth_dt sex   index_dt
#> 1       1 1982-05-13   M 2019-08-26
#> 2       2 1985-07-23   M 2020-08-15
#> 3       3 1986-02-21   M 2019-10-11
#> 4       4 2004-05-28   M 2019-10-31
#> 5       5 1971-05-12   M 2019-08-04
#> 6       6 1986-07-11   F 2020-08-18
```

## Report sample sizes for your flowchart with `report_n()`

Most studies report how many clients remain after each inclusion or
exclusion step (the classic flowchart in epidemiological papers).
[`report_n()`](https://kevinhzq.github.io/healthdb/reference/report_n.md)
counts the distinct values of an ID column across any number of data
frames or database tables in one call, so you do not have to repeat
[`n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
for every intermediate object:

``` r

# some exclusion steps
step1 <- cohort %>% filter(birth_dt <= "2001-01-01")
step2 <- step1 %>% filter(index_dt >= "2019-07-01")

Ns <- report_n(cohort, step1, step2, on = clnt_id)
Ns
#> [1] 10  8  8
```

The result is just a vector, which is handy for inline reporting in R
Markdown, e.g., writing `` `r Ns[3]` `` in your text will show the final
sample size and stay up to date when the data change.

``` r

# differences between steps = number excluded at each step
diff(Ns)
#> [1] -2  0
```

## Compute age and age groups with `compute_duration()`

[`compute_duration()`](https://kevinhzq.github.io/healthdb/reference/compute_duration.md)
calculates the time between two dates, defaulting to years (i.e., age).
Unlike a bare subtraction, it warns you about missing values (a common
data quality issue that silent arithmetic would hide), and it can cut
the result into labelled groups in the same step:

``` r

cohort <- cohort %>%
  mutate(
    age = compute_duration(birth_dt, index_dt),
    # supply lower breaks to get age groups as a factor with auto-generated labels
    age_grp = compute_duration(birth_dt, index_dt, lower_brks = c(0, 19, 25, 35, 45, 55))
  )
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   15.43   33.75   36.18   38.50   47.85   67.47 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   15.43   33.75   36.18   38.50   47.85   67.47 
#>   <19 19-24 25-34 35-44 45-54   55+ 
#>     2     0     2     2     3     1

cohort %>% select(clnt_id, birth_dt, index_dt, age, age_grp)
#>    clnt_id   birth_dt   index_dt      age age_grp
#> 1        1 1982-05-13 2019-08-26 37.28679   35-44
#> 2        2 1985-07-23 2020-08-15 35.06366   35-44
#> 3        3 1986-02-21 2019-10-11 33.63450   25-34
#> 4        4 2004-05-28 2019-10-31 15.42505     <19
#> 5        5 1971-05-12 2019-08-04 48.22998   45-54
#> 6        6 1986-07-11 2020-08-18 34.10541   25-34
#> 7        7 1952-10-31 2020-04-20 67.46886     55+
#> 8        8 1969-10-17 2020-03-02 50.37372   45-54
#> 9        9 1973-07-08 2020-03-17 46.69131   45-54
#> 10      10 2003-01-04 2019-10-03 16.74470     <19
```

Other units are available through the `unit` argument, e.g.,
`unit = "day"` for lengths of stay.

## Decode codes with `lookup()`

Administrative data are full of codes that have to be translated for
reporting — diagnosis codes, drug identification numbers, facility
numbers, and so on.
[`lookup()`](https://kevinhzq.github.io/healthdb/reference/lookup.md)
matches a column against a look-up table inside
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), using a
formula to say which column maps to which:

``` r

# a small drug look-up table
drug_lu <- data.frame(
  din = c(594687, 545503, 522724),
  drug_name = c("methadone", "morphine", "fentanyl")
)

rx <- data.frame(
  clnt_id = c(1, 1, 2, 3),
  din = c(594687, 594687, 545503, 522724)
)

rx %>% mutate(drug_name = lookup(din, ~drug_name, drug_lu))
#>   clnt_id    din drug_name
#> 1       1 594687 methadone
#> 2       1 594687 methadone
#> 3       2 545503  morphine
#> 4       3 522724  fentanyl
```

The left-hand side of the formula can be omitted when the column has the
same name in both tables (as above); otherwise write
`name_in_lookup ~ value_to_get`. If some codes have no match, the result
will contain `NA` and a warning will tell you how many.

## Score comorbidity with `compute_comorbidity()`

[`compute_comorbidity()`](https://kevinhzq.github.io/healthdb/reference/compute_comorbidity.md)
computes the unweighted Elixhauser Comorbidity Index — 31 binary disease
categories plus a total score — from diagnosis columns. It accepts
ICD-10 (“ICD-10”) and ICD-9 (“ICD-9-CM-5digits” or “ICD-9-CM-3digits”)
codes, following Quan et al. (2005). It works on both local data frames
and database tables, so the scoring can run on the database server
without downloading the records first.

``` r

# toy hospital records with ICD-9 codes; uid identifies rows
hosp <- data.frame(
  uid = 1:10,
  clnt_id = c(1, 1, 2, 2, 3, 3, 4, 5, 6, 7),
  diagx_1 = c("193", "2780", "396", "4254", "4150", "401", "401", "0932", "5329", "2536"),
  diagx_2 = c(NA, NA, "72930", "V6542", "493", "405", "5880", "2409", "714", NA)
)

# score by client: each category counts once per client no matter
# how many qualifying records they have
hosp %>%
  compute_comorbidity(
    vars = starts_with("diagx"),
    icd_ver = "ICD-9-CM-5digits",
    clnt_id = clnt_id,
    sum_by = "clnt"
  )
#> # A tibble: 7 × 33
#>   clnt_id   chf arrhy    vd   pcd   pvd hptn_nc hptn_c  para othnd  copd diab_nc
#>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
#> 1       1     0     0     0     0     0       0      0     0     0     0       0
#> 2       2     1     0     1     0     0       0      0     0     0     0       0
#> 3       3     0     0     0     1     0       1      1     0     0     1       0
#> 4       4     0     0     0     0     0       1      0     0     0     0       0
#> 5       5     0     0     1     0     0       0      0     0     0     0       0
#> 6       6     0     0     0     0     0       0      0     0     0     0       0
#> 7       7     0     0     0     0     0       0      0     0     0     0       0
#> # ℹ 21 more variables: diab_c <dbl>, hptothy <dbl>, rf <dbl>, ld <dbl>,
#> #   pud_nb <dbl>, hiv <dbl>, lymp <dbl>, mets <dbl>, tumor <dbl>,
#> #   rheum_a <dbl>, coag <dbl>, obesity <dbl>, wl <dbl>, fluid <dbl>, bla <dbl>,
#> #   da <dbl>, alcohol <dbl>, drug <dbl>, psycho <dbl>, dep <dbl>,
#> #   total_eci <dbl>
```

Two practical notes:

- Codes must contain no dots and be in upper case (e.g., “E1152”, not
  “e11.52”). Matching is by prefix: a record matches a category if its
  code *starts with* one of the category’s codes, so “4280” is captured
  by “428” (Congestive Heart Failure). The full code lists, category
  labels, and matching rules are available as a built-in data set — see
  [`?elix_codes`](https://kevinhzq.github.io/healthdb/reference/elix_codes.md).

  ``` r

  head(elix_codes)
  #>   icd_ver category              description code match_len
  #> 1  ICD-10      chf Congestive Heart Failure  I43         3
  #> 2  ICD-10      chf Congestive Heart Failure  I50         3
  #> 3  ICD-10      chf Congestive Heart Failure I099         4
  #> 4  ICD-10      chf Congestive Heart Failure I110         4
  #> 5  ICD-10      chf Congestive Heart Failure I130         4
  #> 6  ICD-10      chf Congestive Heart Failure I132         4
  ```

- If some categories are your exposure or outcome rather than
  comorbidity, exclude them from the total score with `excl`, using the
  labels from `elix_codes`:

  ``` r

  hosp %>%
    compute_comorbidity(
      vars = starts_with("diagx"),
      icd_ver = "ICD-9-CM-5digits",
      clnt_id = clnt_id,
      sum_by = "clnt",
      excl = c("drug", "alcohol")
    ) %>%
    head()
  #> # A tibble: 6 × 33
  #>   clnt_id   chf arrhy    vd   pcd   pvd hptn_nc hptn_c  para othnd  copd diab_nc
  #>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
  #> 1       1     0     0     0     0     0       0      0     0     0     0       0
  #> 2       2     1     0     1     0     0       0      0     0     0     0       0
  #> 3       3     0     0     0     1     0       1      1     0     0     1       0
  #> 4       4     0     0     0     0     0       1      0     0     0     0       0
  #> 5       5     0     0     1     0     0       0      0     0     0     0       0
  #> 6       6     0     0     0     0     0       0      0     0     0     0       0
  #> # ℹ 21 more variables: diab_c <dbl>, hptothy <dbl>, rf <dbl>, ld <dbl>,
  #> #   pud_nb <dbl>, hiv <dbl>, lymp <dbl>, mets <dbl>, tumor <dbl>,
  #> #   rheum_a <dbl>, coag <dbl>, obesity <dbl>, wl <dbl>, fluid <dbl>, bla <dbl>,
  #> #   da <dbl>, alcohol <dbl>, drug <dbl>, psycho <dbl>, dep <dbl>,
  #> #   total_eci <dbl>
  ```

## Combine related records into episodes with `collapse_episode()`

Records that are close together in time often represent one continuous
episode of care: prescription refills a few days apart, a hospital
transfer recorded as a new admission, repeated visits during one
illness.
[`collapse_episode()`](https://kevinhzq.github.io/healthdb/reference/collapse_episode.md)
groups records of the same client into episodes when the gap between
them is no more than `gap` days:

``` r

# dispensing records: each row has a supply start and end date
rx_df <- data.frame(
  clnt_id = c(1, 1, 1, 2, 2),
  supply_from = as.Date(c(
    "2021-01-01", "2021-02-04", "2021-06-01",
    "2021-03-01", "2021-03-10"
  )),
  supply_to = as.Date(c(
    "2021-01-30", "2021-03-05", "2021-06-30",
    "2021-03-08", "2021-04-08"
  ))
)

rx_epi <- collapse_episode(rx_df,
  clnt_id = clnt_id,
  start_dt = supply_from,
  end_dt = supply_to,
  # records less than or equal to 7 days apart belong to the same episode
  gap = 7
)

rx_epi
#>   clnt_id supply_from  supply_to epi_id epi_no epi_seq epi_start_dt epi_stop_dt
#> 1       1  2021-01-01 2021-01-30      1      1       1   2021-01-01  2021-03-05
#> 2       1  2021-02-04 2021-03-05      1      1       2   2021-01-01  2021-03-05
#> 3       1  2021-06-01 2021-06-30      2      2       1   2021-06-01  2021-06-30
#> 4       2  2021-03-01 2021-03-08      3      1       1   2021-03-01  2021-04-08
#> 5       2  2021-03-10 2021-04-08      3      1       2   2021-03-01  2021-04-08
```

The output keeps every original row and adds episode identifiers:
`epi_id` (unique across the data), `epi_no` (episode number within a
client), `epi_seq` (record number within an episode), and the episode’s
overall start and stop dates. From here, one
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
per `epi_id` gives episode-level measures such as total days of supply.
For related records that should be collapsed regardless of the gap
(e.g., refills under the same prescription number), see the `overwrite`
argument. Like
[`compute_comorbidity()`](https://kevinhzq.github.io/healthdb/reference/compute_comorbidity.md),
this function also accepts database tables, so episodes can be built
without downloading the records.

## Split long periods into regular intervals with `cut_period()`

The opposite of collapsing: sometimes one long record needs to become
several short ones, e.g., dividing each client’s follow-up into calendar
years for year-by-year rates.
[`cut_period()`](https://kevinhzq.github.io/healthdb/reference/cut_period.md)
expands each row into multiple rows by a fixed interval:

``` r

followup <- data.frame(
  clnt_id = 1,
  start_date = as.Date("2019-03-15"),
  end_date = as.Date("2021-06-30")
)

cut_period(followup,
  start = start_date,
  end = end_date,
  len = 1,
  unit = "year"
)
#> # A tibble: 3 × 6
#>   clnt_id start_date end_date   segment_start segment_end segment_id
#>     <dbl> <date>     <date>     <date>        <date>           <int>
#> 1       1 2019-03-15 2021-06-30 2019-03-15    2020-03-14           1
#> 2       1 2019-03-15 2021-06-30 2020-03-15    2021-03-14           2
#> 3       1 2019-03-15 2021-06-30 2021-03-15    2021-06-30           3
```

Each segment keeps the original columns plus its own `segment_start`,
`segment_end`, and `segment_id`. Note that this function accepts local
data frames only — collect your data first if it lives in a database, as
the output has more rows than the input.

## Check date patterns directly with `if_date()`

[`if_date()`](https://kevinhzq.github.io/healthdb/reference/if_date.md)
is the logic behind
[`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md)
(see the introduction vignette), exposed as a plain function you can use
anywhere: it answers, for a vector of dates, “is there any set of `n`
dates that are at least `apart` days apart and fall within `within`
days?” This is useful when you want the test as a summary value rather
than a filter:

``` r

visits <- data.frame(
  clnt_id = rep(1:3, c(4, 3, 2)),
  visit_dt = as.Date(c(
    # client 1: two visits within a year
    "2019-01-05", "2019-04-10", "2020-08-01", "2021-02-15",
    # client 2: visits spread out more than a year apart
    "2018-01-01", "2019-06-01", "2020-12-01",
    # client 3: two visits but on the same date
    "2020-05-05", "2020-05-05"
  ))
)

visits %>%
  group_by(clnt_id) %>%
  summarise(meets_def = if_date(visit_dt, n = 2, within = 365))
#> # A tibble: 3 × 2
#>   clnt_id meets_def
#>     <int> <lgl>    
#> 1       1 TRUE     
#> 2       2 FALSE    
#> 3       3 FALSE
```

Duplicated dates are counted once by default (`dup.rm = TRUE`), which is
why client 3 does not qualify. The `apart` argument adds a minimum-gap
condition, e.g., `if_date(x, n = 2, apart = 30, within = 365)` for two
dates at least 30 days apart within a year — a very common shape for
case definitions. Note that
[`if_date()`](https://kevinhzq.github.io/healthdb/reference/if_date.md)
works on local data only; for database tables, use
[`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md).

## Where these fit in a typical workflow

The guiding principle when working with a database: stay on the database
for as long as possible, and bring the data into R only once, after
every step that can run on the server has been done and the data has
been shrunk to its final size. A common end-to-end flow looks like:

1.  On the database, identify candidate cases with
    [`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_rows.md)/[`define_case()`](https://kevinhzq.github.io/healthdb/reference/define_case.md)
    (see
    [`vignette("healthdb")`](https://kevinhzq.github.io/healthdb/articles/healthdb.md)).
2.  Still on the database, apply inclusion/exclusion steps with
    [`exclude()`](https://kevinhzq.github.io/healthdb/reference/exclude.md)
    or filtering joins
    ([`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
    and
    [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)),
    reporting
    [`report_n()`](https://kevinhzq.github.io/healthdb/reference/report_n.md)
    after each step for the flowchart.
3.  Complete the remaining database-capable steps: attach demographics
    and other covariates with
    [`fetch_var()`](https://kevinhzq.github.io/healthdb/reference/fetch_var.md),
    add comorbidity adjustment variables with
    [`compute_comorbidity()`](https://kevinhzq.github.io/healthdb/reference/compute_comorbidity.md),
    and group related records into episodes with
    [`collapse_episode()`](https://kevinhzq.github.io/healthdb/reference/collapse_episode.md)
    — all three accept database tables, so none of them require
    downloading the data.
4.  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
    the now much smaller result into R.
5.  Finish with the helpers that work on local data only: compute age
    with
    [`compute_duration()`](https://kevinhzq.github.io/healthdb/reference/compute_duration.md),
    decode codes with
    [`lookup()`](https://kevinhzq.github.io/healthdb/reference/lookup.md),
    split follow-up into periods with
    [`cut_period()`](https://kevinhzq.github.io/healthdb/reference/cut_period.md)
    for period-level rates, and use
    [`if_date()`](https://kevinhzq.github.io/healthdb/reference/if_date.md)
    for any custom date-pattern checks.
