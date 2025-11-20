# Build case definition function calls

This function assembles function calls from the supplied functions and
their required arguments, leaving the data argument empty for easy
re-use of the definition calls with different data and batch execution
(see
[`execute_def()`](https://kevinhzq.github.io/healthdb/reference/execute_def.md)
for detail). It is useful for defining multiple diseases/events across
multiple sources.

## Usage

``` r
build_def(def_lab, src_labs, def_fn = define_case, fn_args)
```

## Arguments

- def_lab:

  A single character label for the definition, e.g., some disease.

- src_labs:

  A character vector of place-holder names for the data sources that
  will be used to execute the definition.

- def_fn:

  A list of functions (default:
  [`define_case()`](https://kevinhzq.github.io/healthdb/reference/define_case.md))
  that will filter the source data sets and keep clients met the case
  definition. The length of the list should be either 1 or equal to the
  length of `src_labs`. If length = 1, the same function will be applied
  to all sources; otherwise, `def_fn` should match `src_lab` by
  position. User can supply custom functions but must put input data as
  the first argument and name it `data`.

- fn_args:

  A named list of arguments passing to the `def_fn`. Each element in the
  list should have the same name as an argument in the source-specific
  `def_fn`, and the element length should also be either 1 or equal to
  the number of sources. If you have `def_fn` functions taking different
  sets of arguments, include the union in one list.

## Value

A tibble with a number of rows equal to the length of `src_labs`,
containing the input arguments and the synthetic function call in the
`fn_call` column.

## Examples

``` r
sud_def <- build_def("SUD", # usually a disease name
  src_lab = c("src1", "src2"), # identify from multiple sources, e.g., hospitalization, ED visits.
  # functions that filter the data with some criteria,
  # including mean here for src2 as a trivial example
  # to show only valid arguments will be in the call
  def_fn = list(define_case, mean),
  fn_args = list(
    vars = list(starts_with("diagx"), "diagx_2"),
    match = "start", # "start" will be applied to all sources as length = 1
    vals = list(c("304"), c("305")),
    clnt_id = "clnt_id",
    # c() can be used in place of list
    # if this argument only takes one value for each source
    n_per_clnt = c(2, 3),
    x = list(1:10) # src2 with mean as def_fn will only accept this argument
  )
)

# the result is a tibble
sud_def
#> # A tibble: 2 × 5
#>   def_lab src_labs def_fn      fn_args          fn_call     
#>   <chr>   <chr>    <chr>       <named list>     <named list>
#> 1 SUD     src1     define_case <named list [6]> <language>  
#> 2 SUD     src2     mean        <named list [6]> <language>  

# the fn_call column stores the code that can be ran with execute_def
sud_def#fn_call
#> # A tibble: 2 × 5
#>   def_lab src_labs def_fn      fn_args          fn_call     
#>   <chr>   <chr>    <chr>       <named list>     <named list>
#> 1 SUD     src1     define_case <named list [6]> <language>  
#> 2 SUD     src2     mean        <named list [6]> <language>  
```
