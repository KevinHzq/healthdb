# Find value corresponding to the input vector using a look-up table

Find value corresponding to the input vector using a look-up table

## Usage

``` r
lookup(x, link, lu, verbose = getOption("healthdb.verbose"))
```

## Arguments

- x:

  A variable name in a data.frame; this function should be called inside
  dplyr::mutate().

- link:

  A formula in the form: name_of_x_in_lu ~ name_of_target_value. The
  left-hand-side can be omitted if x's name is also x in the look-up.

- lu:

  Look-up table in data.frame class.

- verbose:

  A logical for whether warn for missing values in the output.

## Value

A vector of matched values.

## Examples

``` r

df <- data.frame(drug_code = 1:10)
lu <- data.frame(drug_id = 1:20, drug_code = as.character(1:10), drug_name = sample(letters, 20))

df %>% dplyr::mutate(
  drug_nm = lookup(drug_code, drug_id ~ drug_name, lu),
  # this will work as lu also has drug_code column
  drug_nm = lookup(drug_code, ~drug_name, lu)
)
#>    drug_code drug_nm
#> 1          1       t
#> 2          2       c
#> 3          3       d
#> 4          4       a
#> 5          5       w
#> 6          6       u
#> 7          7       s
#> 8          8       h
#> 9          9       b
#> 10        10       q
```
