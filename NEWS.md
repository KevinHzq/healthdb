# healthdb (development version)

## Bug fixes

-   For restrict_date(), suppressed a warning related to SQL translation of "ORDER BY" when `apart` and `within` are both present. The output is correct even when the warning is given. It could be manually avoided by using dplyr::collect() or dplyr::show_query() with argument `cte = TRUE` and likely caused by issues in `dbplyr`.

# healthdb 0.2.0

## Improvements

-   The `apart` argument in restrict_date() is now functional for both data.frame and database input.

-   restrict_date() will give warning if there are missing entries in `date_var`. The description of `dup.rm` has been re-written to clarify it is applicable only when `apart` is absent.

-   pool_case() now outputs both the number of raw (raw_in_src) and valid (valid_in_src) entries in each source.

-   List element of execute_def() output is named by definition and source labels.

-   Most of the verbose message, except data summaries, will be sent as message instead of text output, so that they can be suppressed via `message = FALSE` in Rmarkdown.

## Bug fixes

-   Fixed if_date() gave output shorter than the input when `detail` and `dup.rm` are both TRUE. if_date() now does not assume the dates are sorted and returns elements in the original unsorted order.
-   Fixed restrict_date() for remote table might miss cases with duplicated dates when n \> 2.
