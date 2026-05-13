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
  [`data.table::as.IDate()`](https://rdrr.io/pkg/data.table/man/IDateTime.html).

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
#> 1       3 2019-11-04
#> 2       4 2017-01-15
#> 3       5 2015-04-19
#> 4       5 2016-12-25
#> 5       5 2017-12-20
#> 6       5 2020-02-08

# collapse records no more than 90 days apart
# end_dt could be absent then it is assumed to be the same as start_dt
collapse_episode(df, clnt_id, start_dt = dates, gap = 90)
#>     clnt_id      dates epi_id epi_no epi_seq epi_start_dt epi_stop_dt
#> 1         3 2019-11-04      1      1       1   2019-11-04  2019-11-04
#> 2         4 2017-01-15      2      1       1   2017-01-15  2017-01-15
#> 3         5 2015-04-19      3      1       1   2015-04-19  2015-04-19
#> 4         5 2016-12-25      4      2       1   2016-12-25  2016-12-25
#> 5         5 2017-12-20      5      3       1   2017-12-20  2017-12-20
#> 6         5 2020-02-08      6      4       1   2020-02-08  2020-02-08
#> 7         6 2015-06-06      7      1       1   2015-06-06  2015-06-06
#> 8         6 2017-02-16      8      2       1   2017-02-16  2017-02-16
#> 9         6 2019-10-11      9      3       1   2019-10-11  2019-10-11
#> 10        7 2017-05-27     10      1       1   2017-05-27  2017-05-27
#> 11        7 2018-04-16     11      2       1   2018-04-16  2018-04-16
#> 12        7 2019-02-21     12      3       1   2019-02-21  2019-02-21
#> 13        7 2020-06-28     13      4       1   2020-06-28  2020-06-28
#> 14        8 2015-07-26     14      1       1   2015-07-26  2015-07-26
#> 15        8 2018-03-10     15      2       1   2018-03-10  2018-03-10
#> 16       10 2017-11-23     16      1       1   2017-11-23  2017-11-23
#> 17       10 2018-08-31     17      2       1   2018-08-31  2018-08-31
#> 18       10 2019-02-09     18      3       1   2019-02-09  2019-02-09
#> 19       11 2017-08-01     19      1       1   2017-08-01  2017-10-18
#> 20       11 2017-10-18     19      1       2   2017-08-01  2017-10-18
#> 21       11 2019-05-11     20      2       1   2019-05-11  2019-05-11
#> 22       11 2020-12-25     21      3       1   2020-12-25  2020-12-25
#> 23       12 2015-09-17     22      1       1   2015-09-17  2015-09-17
#> 24       12 2016-02-18     23      2       1   2016-02-18  2016-02-18
#> 25       12 2017-06-15     24      3       1   2017-06-15  2017-06-15
#> 26       12 2017-11-03     25      4       1   2017-11-03  2017-11-03
#> 27       13 2019-05-22     26      1       1   2019-05-22  2019-05-22
#> 28       13 2020-10-06     27      2       1   2020-10-06  2020-10-06
#> 29       14 2016-04-02     28      1       1   2016-04-02  2016-04-02
#> 30       15 2015-05-04     29      1       1   2015-05-04  2015-05-04
#> 31       16 2020-10-19     30      1       1   2020-10-19  2020-10-19
#> 32       17 2015-01-17     31      1       1   2015-01-17  2015-04-06
#> 33       17 2015-04-06     31      1       2   2015-01-17  2015-04-06
#> 34       17 2019-03-21     32      2       1   2019-03-21  2019-03-21
#> 35       18 2015-02-10     33      1       1   2015-02-10  2015-02-10
#> 36       18 2017-03-06     34      2       1   2017-03-06  2017-03-06
#> 37       18 2018-10-07     35      3       1   2018-10-07  2018-10-07
#> 38       19 2018-05-15     36      1       1   2018-05-15  2018-06-07
#> 39       19 2018-06-07     36      1       2   2018-05-15  2018-06-07
#> 40       20 2015-07-22     37      1       1   2015-07-22  2015-07-22
#> 41       20 2016-11-29     38      2       1   2016-11-29  2016-11-29
#> 42       20 2020-08-23     39      3       1   2020-08-23  2020-08-23
#> 43       21 2017-03-04     40      1       1   2017-03-04  2017-03-04
#> 44       21 2017-10-26     41      2       1   2017-10-26  2017-10-26
#> 45       21 2018-10-06     42      3       1   2018-10-06  2018-10-06
#> 46       21 2020-01-04     43      4       1   2020-01-04  2020-02-24
#> 47       21 2020-02-24     43      4       2   2020-01-04  2020-02-24
#> 48       22 2016-01-18     44      1       1   2016-01-18  2016-01-18
#> 49       22 2020-10-17     45      2       1   2020-10-17  2020-10-17
#> 50       23 2018-12-13     46      1       1   2018-12-13  2018-12-13
#> 51       23 2019-04-04     47      2       1   2019-04-04  2019-04-04
#> 52       23 2020-08-15     48      3       1   2020-08-15  2020-08-17
#> 53       23 2020-08-17     48      3       2   2020-08-15  2020-08-17
#> 54       24 2015-12-23     49      1       1   2015-12-23  2015-12-23
#> 55       24 2018-05-01     50      2       1   2018-05-01  2018-05-01
#> 56       25 2016-12-13     51      1       1   2016-12-13  2016-12-13
#> 57       25 2018-06-15     52      2       1   2018-06-15  2018-06-15
#> 58       25 2019-01-24     53      3       1   2019-01-24  2019-01-24
#> 59       26 2016-06-20     54      1       1   2016-06-20  2016-06-20
#> 60       26 2018-01-06     55      2       1   2018-01-06  2018-01-06
#> 61       26 2019-01-13     56      3       1   2019-01-13  2019-01-13
#> 62       27 2020-08-05     57      1       1   2020-08-05  2020-09-23
#> 63       27 2020-09-23     57      1       2   2020-08-05  2020-09-23
#> 64       28 2015-09-20     58      1       1   2015-09-20  2015-09-20
#> 65       28 2017-07-10     59      2       1   2017-07-10  2017-07-10
#> 66       28 2019-06-19     60      3       1   2019-06-19  2019-06-19
#> 67       28 2020-01-07     61      4       1   2020-01-07  2020-01-07
#> 68       29 2016-12-06     62      1       1   2016-12-06  2016-12-06
#> 69       29 2020-02-25     63      2       1   2020-02-25  2020-02-25
#> 70       31 2017-01-07     64      1       1   2017-01-07  2017-01-07
#> 71       32 2016-07-21     65      1       1   2016-07-21  2016-07-21
#> 72       32 2018-11-19     66      2       1   2018-11-19  2018-11-19
#> 73       33 2017-10-07     67      1       1   2017-10-07  2017-10-07
#> 74       33 2020-01-31     68      2       1   2020-01-31  2020-01-31
#> 75       34 2016-12-12     69      1       1   2016-12-12  2016-12-12
#> 76       34 2017-05-19     70      2       1   2017-05-19  2017-08-22
#> 77       34 2017-06-26     70      2       2   2017-05-19  2017-08-22
#> 78       34 2017-08-22     70      2       3   2017-05-19  2017-08-22
#> 79       35 2019-09-09     71      1       1   2019-09-09  2019-09-09
#> 80       38 2020-09-10     72      1       1   2020-09-10  2020-09-10
#> 81       41 2016-07-11     73      1       1   2016-07-11  2016-07-11
#> 82       41 2018-03-30     74      2       1   2018-03-30  2018-03-30
#> 83       41 2019-08-19     75      3       1   2019-08-19  2019-08-19
#> 84       42 2016-02-01     76      1       1   2016-02-01  2016-02-01
#> 85       42 2017-01-23     77      2       1   2017-01-23  2017-01-23
#> 86       42 2017-11-06     78      3       1   2017-11-06  2017-11-06
#> 87       42 2018-08-07     79      4       1   2018-08-07  2018-08-07
#> 88       42 2020-11-17     80      5       1   2020-11-17  2020-11-17
#> 89       43 2015-01-16     81      1       1   2015-01-16  2015-01-16
#> 90       44 2015-10-17     82      1       1   2015-10-17  2015-10-17
#> 91       44 2018-11-28     83      2       1   2018-11-28  2018-11-28
#> 92       45 2015-02-11     84      1       1   2015-02-11  2015-03-23
#> 93       45 2015-03-23     84      1       2   2015-02-11  2015-03-23
#> 94       46 2017-11-25     85      1       1   2017-11-25  2017-11-25
#> 95       46 2019-12-08     86      2       1   2019-12-08  2019-12-08
#> 96       47 2019-08-26     87      1       1   2019-08-26  2019-08-26
#> 97       48 2016-07-01     88      1       1   2016-07-01  2016-07-01
#> 98       48 2017-01-06     89      2       1   2017-01-06  2017-01-06
#> 99       48 2017-07-29     90      3       1   2017-07-29  2017-07-29
#> 100      48 2020-09-15     91      4       1   2020-09-15  2020-09-15
```
