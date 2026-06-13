# Group records no more than n days apart as episodes

This function is useful for collapsing, e.g., medication dispensation or
hospitalization, records into episodes if the records' dates are no more
than n days apart. The length of the gap can be relaxed by another
grouping variable. Records with a missing (`NA`) start or end date are
removed with a warning before episodes are derived, because the gap to
such records is undefined.

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

  Column name of the end date of records. The default is NULL, assuming
  the record lasts one day and only the start date will be used to
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
# toy dispensing records: each row has a supply start and end date
rx <- data.frame(
  clnt_id = c(1, 1, 1, 2, 2),
  start = as.Date(c(
    "2020-01-01", "2020-03-01", "2020-06-01",
    "2020-01-01", "2020-01-10"
  )),
  end = as.Date(c(
    "2020-01-15", "2020-03-15", "2020-06-15",
    "2020-01-20", "2020-01-25"
  )),
  rx_id = c("a", "a", "b", "c", "c")
)

# collapse records no more than 14 days apart into episodes
collapse_episode(rx, clnt_id, start_dt = start, end_dt = end, gap = 14)
#>   clnt_id      start        end rx_id epi_id epi_no epi_seq epi_start_dt
#> 1       1 2020-01-01 2020-01-15     a      1      1       1   2020-01-01
#> 2       1 2020-03-01 2020-03-15     a      2      2       1   2020-03-01
#> 3       1 2020-06-01 2020-06-15     b      3      3       1   2020-06-01
#> 4       2 2020-01-01 2020-01-20     c      4      1       1   2020-01-01
#> 5       2 2020-01-10 2020-01-25     c      4      1       2   2020-01-01
#>   epi_stop_dt
#> 1  2020-01-15
#> 2  2020-03-15
#> 3  2020-06-15
#> 4  2020-01-25
#> 5  2020-01-25

# end_dt may be omitted, then each record is assumed to last a single day
collapse_episode(rx, clnt_id, start_dt = start, gap = 14)
#>   clnt_id      start        end rx_id epi_id epi_no epi_seq epi_start_dt
#> 1       1 2020-01-01 2020-01-15     a      1      1       1   2020-01-01
#> 2       1 2020-03-01 2020-03-15     a      2      2       1   2020-03-01
#> 3       1 2020-06-01 2020-06-15     b      3      3       1   2020-06-01
#> 4       2 2020-01-01 2020-01-20     c      4      1       1   2020-01-01
#> 5       2 2020-01-10 2020-01-25     c      4      1       2   2020-01-01
#>   epi_stop_dt
#> 1  2020-01-01
#> 2  2020-03-01
#> 3  2020-06-01
#> 4  2020-01-10
#> 5  2020-01-10

# overwrite keeps related records (e.g. refills of the same prescription)
# in one episode regardless of the gap: consecutive records with the same
# rx_id are collapsed using gap_overwrite, while a change in rx_id falls
# back to the regular gap. Here client 1's two "a" records merge even though
# they are more than 14 days apart.
collapse_episode(rx, clnt_id,
  start_dt = start, end_dt = end, gap = 14, overwrite = rx_id
)
#>   clnt_id      start        end rx_id epi_id epi_no epi_seq epi_start_dt
#> 1       1 2020-01-01 2020-01-15     a      1      1       1   2020-01-01
#> 2       1 2020-03-01 2020-03-15     a      1      1       2   2020-01-01
#> 3       1 2020-06-01 2020-06-15     b      2      2       1   2020-06-01
#> 4       2 2020-01-01 2020-01-20     c      3      1       1   2020-01-01
#> 5       2 2020-01-10 2020-01-25     c      3      1       2   2020-01-01
#>   epi_stop_dt
#> 1  2020-03-15
#> 2  2020-03-15
#> 3  2020-06-15
#> 4  2020-01-25
#> 5  2020-01-25
```
