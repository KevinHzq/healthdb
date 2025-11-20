# Make test data

Make a toy data set for testing and demo. This is for internal use
purpose and not intended to be called by users.

## Usage

``` r
make_test_dat(
  vals_kept = c("304", "305", 3040:3049, 3050:3059),
  noise_val = "999",
  IDs = 1:50,
  date_range = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1),
  nrows = 100,
  n_any = 50,
  n_all = 10,
  seed = NULL,
  answer_id = NULL,
  type = c("data.frame", "database")
)
```

## Arguments

- vals_kept:

  A vector of values that suppose to be identified.

- noise_val:

  A vector of values that are not meant to be identified.

- IDs:

  A vector of client IDs.

- date_range:

  A vector of all possible dates in the data.

- nrows:

  Number of rows of the output.

- n_any:

  Number of rows to be identified if the criteria is that if any target
  column contains certain values.

- n_all:

  Number of rows to be identified if the criteria is that if all target
  columns contain certain values.

- seed:

  Seed for random number generation.

- answer_id:

  Column name for the indicator of how the row should be identified:
  any, all, and noise.

- type:

  Output type, "data.frame" or "database".

## Value

A data.frame or remote table from 'dbplyr'.

## Examples

``` r
make_test_dat() %>% head()
#>   uid clnt_id      dates diagx diagx_1 diagx_2
#> 1  42       1 2015-03-15  3058    3041    3056
#> 2  63       1 2016-04-09   999    <NA>     999
#> 3  77       1 2018-01-04   999    <NA>    <NA>
#> 4  79       1 2019-01-08   999     999     999
#> 5  98       1 2019-05-18   999    <NA>    <NA>
#> 6  65       3 2016-05-07   999    <NA>    <NA>
```
