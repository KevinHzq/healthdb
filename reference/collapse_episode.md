# Group records no more than n days apart as episodes

This function is useful for collapsing, e.g., medication dispensation or
hospitalization, records into episodes if the records' dates are no more
than n days gap apart. The length of the gap can be relaxed by another
grouping variable.

## Usage

``` r
collapse_episode(
  data,
  clnt_id,
  start_dt,
  end_dt = NULL,
  gap,
  overwrite = NULL,
  gap_overwrite = 99999,
  .dt_trans = data.table::as.IDate,
  ...
)
```

## Arguments

- data:

  A data.frame or remote table that contains the id and date variables.

- clnt_id:

  Column name of subject/person ID.

- start_dt:

  Column name of the starting date of records.

- end_dt:

  Column name of the end date of records. The default is NULL assuming
  the record last one day and only the start date will be used to
  calculate the gaps between records.

- gap:

  A number in days that will be used to separate episodes. For example,
  gap = 7 means collapsing records no more than 7 days apart. Note that
  the number of days apart will be calculated as numeric difference
  between two days, so that 2020-01-07 and 2020-01-01 is considered as 6
  days apart.

- overwrite:

  Column name of a grouping variable determining whether the consecutive
  records are related and should have a different gap value. For
  example, dispensing records may have the same original prescription
  number, and a different gap value can be assigned for such situation,
  e.g., the days between two records is \> gap, but these records still
  belong to the same prescription.

- gap_overwrite:

  A different gap value used for related records. The default is 99999,
  which practically means all records with the same overwrite variable
  will be collapsed.

- .dt_trans:

  Function to transform start_dt/end_dt. For data.frame input only.
  Default is
  [`data.table::as.IDate()`](https://rdatatable.gitlab.io/data.table/reference/IDateTime.html).

- ...:

  Additional arguments passing to the .dt_trans function. For data.frame
  input only.

## Value

The original data.frame or remote table with new columns indicating
episode grouping. The new variables include:

- epi_id: unique identifier of episodes across the whole data set

- epi_no: identifier of episodes within a client/group

- epi_seq: identifier of records within an episode

- epi_start/stop_dt: start and end dates corresponding to epi_id

## Examples

``` r
# make toy data
df <- make_test_dat() %>%
dplyr::select(clnt_id, dates)

head(df)
#>   clnt_id      dates
#> 1       2 2016-01-24
#> 2       3 2019-11-04
#> 3       4 2017-01-15
#> 4       5 2015-04-19
#> 5       5 2016-12-25
#> 6       5 2017-12-20

# collapse records no more than 90 days apart
# end_dt could be absent then it is assumed to be the same as start_dt
collapse_episode(df, clnt_id, start_dt = dates, gap = 90)
#>     clnt_id      dates epi_id epi_no epi_seq epi_start_dt epi_stop_dt
#> 1         2 2016-01-24      1      1       1   2016-01-24  2016-01-24
#> 2         3 2019-11-04      2      1       1   2019-11-04  2019-11-04
#> 3         4 2017-01-15      3      1       1   2017-01-15  2017-01-15
#> 4         5 2015-04-19      4      1       1   2015-04-19  2015-04-19
#> 5         5 2016-12-25      5      2       1   2016-12-25  2016-12-25
#> 6         5 2017-12-20      6      3       1   2017-12-20  2017-12-20
#> 7         5 2020-02-08      7      4       1   2020-02-08  2020-02-08
#> 8         6 2015-06-06      8      1       1   2015-06-06  2015-06-06
#> 9         6 2017-02-16      9      2       1   2017-02-16  2017-02-16
#> 10        6 2019-10-11     10      3       1   2019-10-11  2019-10-11
#> 11        7 2017-05-27     11      1       1   2017-05-27  2017-05-27
#> 12        7 2018-04-16     12      2       1   2018-04-16  2018-04-16
#> 13        7 2019-02-21     13      3       1   2019-02-21  2019-02-21
#> 14        7 2020-06-28     14      4       1   2020-06-28  2020-06-28
#> 15        8 2015-07-26     15      1       1   2015-07-26  2015-07-26
#> 16        8 2018-03-10     16      2       1   2018-03-10  2018-03-10
#> 17        9 2019-03-28     17      1       1   2019-03-28  2019-03-28
#> 18       10 2017-11-23     18      1       1   2017-11-23  2017-11-23
#> 19       10 2018-08-31     19      2       1   2018-08-31  2018-08-31
#> 20       10 2019-02-09     20      3       1   2019-02-09  2019-02-09
#> 21       11 2017-08-01     21      1       1   2017-08-01  2017-10-18
#> 22       11 2017-10-18     21      1       2   2017-08-01  2017-10-18
#> 23       11 2019-05-11     22      2       1   2019-05-11  2019-05-11
#> 24       11 2020-12-25     23      3       1   2020-12-25  2020-12-25
#> 25       12 2015-09-17     24      1       1   2015-09-17  2015-09-17
#> 26       12 2016-02-18     25      2       1   2016-02-18  2016-02-18
#> 27       12 2017-06-11     26      3       1   2017-06-11  2017-06-15
#> 28       12 2017-06-15     26      3       2   2017-06-11  2017-06-15
#> 29       12 2017-11-03     27      4       1   2017-11-03  2017-11-03
#> 30       13 2019-05-22     28      1       1   2019-05-22  2019-05-22
#> 31       13 2020-10-06     29      2       1   2020-10-06  2020-10-06
#> 32       14 2016-04-02     30      1       1   2016-04-02  2016-04-02
#> 33       15 2015-05-04     31      1       1   2015-05-04  2015-05-04
#> 34       16 2020-10-19     32      1       1   2020-10-19  2020-10-19
#> 35       17 2015-01-17     33      1       1   2015-01-17  2015-04-06
#> 36       17 2015-04-06     33      1       2   2015-01-17  2015-04-06
#> 37       17 2019-03-21     34      2       1   2019-03-21  2019-03-21
#> 38       18 2015-02-10     35      1       1   2015-02-10  2015-02-10
#> 39       18 2017-03-06     36      2       1   2017-03-06  2017-03-06
#> 40       18 2018-10-07     37      3       1   2018-10-07  2018-10-07
#> 41       19 2018-05-15     38      1       1   2018-05-15  2018-06-07
#> 42       19 2018-06-07     38      1       2   2018-05-15  2018-06-07
#> 43       20 2015-07-22     39      1       1   2015-07-22  2015-07-22
#> 44       20 2016-11-29     40      2       1   2016-11-29  2016-11-29
#> 45       20 2020-08-23     41      3       1   2020-08-23  2020-08-23
#> 46       21 2017-03-04     42      1       1   2017-03-04  2017-03-04
#> 47       21 2017-10-26     43      2       1   2017-10-26  2017-10-26
#> 48       21 2018-10-06     44      3       1   2018-10-06  2018-10-06
#> 49       21 2020-01-04     45      4       1   2020-01-04  2020-02-24
#> 50       21 2020-02-24     45      4       2   2020-01-04  2020-02-24
#> 51       22 2016-01-18     46      1       1   2016-01-18  2016-01-18
#> 52       22 2020-10-17     47      2       1   2020-10-17  2020-10-17
#> 53       23 2018-12-13     48      1       1   2018-12-13  2018-12-13
#> 54       23 2019-04-04     49      2       1   2019-04-04  2019-04-04
#> 55       23 2020-08-15     50      3       1   2020-08-15  2020-08-17
#> 56       23 2020-08-17     50      3       2   2020-08-15  2020-08-17
#> 57       24 2015-12-23     51      1       1   2015-12-23  2015-12-23
#> 58       24 2018-05-01     52      2       1   2018-05-01  2018-05-01
#> 59       25 2018-06-15     53      1       1   2018-06-15  2018-06-15
#> 60       25 2019-01-24     54      2       1   2019-01-24  2019-01-24
#> 61       26 2016-06-20     55      1       1   2016-06-20  2016-06-20
#> 62       26 2018-01-06     56      2       1   2018-01-06  2018-01-06
#> 63       26 2019-01-13     57      3       1   2019-01-13  2019-01-13
#> 64       27 2020-08-05     58      1       1   2020-08-05  2020-09-23
#> 65       27 2020-09-23     58      1       2   2020-08-05  2020-09-23
#> 66       28 2015-09-20     59      1       1   2015-09-20  2015-09-20
#> 67       28 2017-07-10     60      2       1   2017-07-10  2017-07-10
#> 68       28 2019-06-19     61      3       1   2019-06-19  2019-06-19
#> 69       28 2020-01-07     62      4       1   2020-01-07  2020-01-07
#> 70       29 2020-02-25     63      1       1   2020-02-25  2020-02-25
#> 71       31 2017-01-07     64      1       1   2017-01-07  2017-01-07
#> 72       32 2016-07-21     65      1       1   2016-07-21  2016-07-21
#> 73       32 2018-11-19     66      2       1   2018-11-19  2018-11-19
#> 74       33 2017-10-07     67      1       1   2017-10-07  2017-10-07
#> 75       33 2020-01-31     68      2       1   2020-01-31  2020-01-31
#> 76       34 2016-12-12     69      1       1   2016-12-12  2016-12-12
#> 77       34 2017-05-19     70      2       1   2017-05-19  2017-05-19
#> 78       34 2017-08-22     71      3       1   2017-08-22  2017-08-22
#> 79       35 2019-09-09     72      1       1   2019-09-09  2019-09-09
#> 80       38 2020-09-10     73      1       1   2020-09-10  2020-09-10
#> 81       41 2016-07-11     74      1       1   2016-07-11  2016-07-11
#> 82       41 2018-03-30     75      2       1   2018-03-30  2018-03-30
#> 83       41 2019-08-19     76      3       1   2019-08-19  2019-08-19
#> 84       42 2016-02-01     77      1       1   2016-02-01  2016-02-01
#> 85       42 2017-01-23     78      2       1   2017-01-23  2017-01-23
#> 86       42 2017-11-06     79      3       1   2017-11-06  2017-11-06
#> 87       42 2018-08-07     80      4       1   2018-08-07  2018-08-30
#> 88       42 2018-08-30     80      4       2   2018-08-07  2018-08-30
#> 89       42 2020-11-17     81      5       1   2020-11-17  2020-11-17
#> 90       43 2015-01-16     82      1       1   2015-01-16  2015-01-16
#> 91       44 2015-10-17     83      1       1   2015-10-17  2015-10-17
#> 92       44 2018-11-28     84      2       1   2018-11-28  2018-11-28
#> 93       45 2015-03-23     85      1       1   2015-03-23  2015-03-23
#> 94       46 2017-11-25     86      1       1   2017-11-25  2017-11-25
#> 95       46 2019-12-08     87      2       1   2019-12-08  2019-12-08
#> 96       47 2019-08-26     88      1       1   2019-08-26  2019-08-26
#> 97       48 2016-07-01     89      1       1   2016-07-01  2016-07-01
#> 98       48 2017-01-06     90      2       1   2017-01-06  2017-01-06
#> 99       48 2017-07-29     91      3       1   2017-07-29  2017-07-29
#> 100      48 2020-09-15     92      4       1   2020-09-15  2020-09-15
```
