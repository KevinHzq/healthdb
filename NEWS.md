# healthdb 0.3.0

## Improvements

-   fetch_var() will accept database input, but it will not prevent one-to-many joins because the checking could be slow on remote database.

-   All functions that accept database input now will NOT sort the output to avoid generating the 'ORDER BY' clause which may trigger a warning if the output was chained with subsequent operations.

-   restrict_date() will not check missing date unless the new `check_missing` argument was set to TRUE. The checking time may be not negligible for large database; the user should opt in for it.

## Bug fixes

-   Chaining pool_case() with dplyr::compute() to create table in database should no longer give errors or warnings related to window order.

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
