# healthdb 0.2.1

# healthdb 0.1.1

## Bug fixes

-   Fixed if_date() gave output shorter than the input when `detail` and `dup.rm` are both TRUE. if_date() now does not assume the dates are sorted and returns elements in the original unsorted order.
-   Fixed restrict_date for remote table might miss cases with duplicated dates when n \> 2.

## Improvements

-   restrict_date() will give warning if there are missing entries in `date_var`. The description of `dup.rm` has been re-written to clarify it is applicable only when `apart` is absent.

-   pool_case() now outputs both the number of raw (raw_in_src) and valid (valid_in_src) entries in each source.
