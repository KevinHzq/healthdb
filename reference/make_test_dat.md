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
#> 1  49       1 2016-10-08  3057    3040    3058
#> 2  58       1 2018-04-01   999     999    <NA>
#> 3  88       1 2018-07-26   999     999    <NA>
#> 4  12       1 2019-02-14  3047    3045    <NA>
#> 5  96       2 2015-01-16   999     999     999
#> 6  84       2 2017-11-21   999    <NA>     999
```
