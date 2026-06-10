# healthdb (development version)

-   Documented case sensitivity of matching: identify_row() "like"/"start" matching is case-sensitive for data.frames but follows the database for remote tables (e.g., case-insensitive ASCII LIKE on SQLite, case-sensitive on PostgreSQL, collation-dependent on SQL Server); compute_comorbidity() expects upper-case codes.

-   identify_row() now errors informatively when `vals` is empty (e.g., `character(0)` or NULL) instead of failing with an obscure message or silently returning no rows.

-   identify_row() is now also exported as identify_rows() (consistent with it returning multiple rows); both names work and are documented on the same page.

-   identify_row() with `match = "like"` or `"start"` on remote tables now generates a single `WHERE ... LIKE ... OR ... LIKE ...` clause instead of one sub-query per pattern combined with `UNION`. The new query is simpler and typically faster. Note a subtle behavior change: `UNION` removed duplicate rows, while the new query (like all other match types) keeps them; results differ only if the source table contains fully duplicated rows.

-   The database connection check (a `SELECT 1` round trip) that runs at the start of every database-method function can now be turned off with `options(healthdb.check_con = FALSE)`, which saves one round trip to the server per step in a long pipeline.

-   Fixed report_n() (and the `report_on` argument of exclude()) erroring with "Can't coerce from a <integer64> object to an integer" on database backends whose counts come back as 64-bit integers (e.g., PostgreSQL).

-   Fixed restrict_date() with `apart` on PostgreSQL: the generated SQL summed and coalesced boolean columns, which PostgreSQL rejects ("function sum(boolean) does not exist"); the indicators are now explicit integers. Also fixed the `check_missing = TRUE` count, which could report a garbled number of removed records (e.g., "2.47e-323") or miss the warning entirely on some backends.

-   The test suite can now run against PostgreSQL (`HEALTHDB_TEST_BACKEND=postgres` plus the standard `PG*` connection variables) and SQL Server (`HEALTHDB_TEST_BACKEND=sqlserver` plus an ODBC connection string in `HEALTHDB_TEST_ODBC`), and does so on GitHub Actions via service containers, in addition to the default local SQLite.

-   **Behavior change**: compute_comorbidity() now also matches ICD-9 codes by prefix (consistent with the ICD-10 change below): codes in `data` are compared with each listed code at the code's own length, as the codes in Quan et al. (2005) cover all their subdivisions, e.g., "428" (Congestive Heart Failure, 428.x) now captures "4280" and "42800", which were previously missed by exact matching. This affects both "ICD-9-CM-5digits" and the full-code part of "ICD-9-CM-3digits". ICD-9 scores may be higher than in previous versions. The prefix matching reproduces the reference SAS implementation, which compares every code with the SAS `IN:` (starts-with) operator; the package code lists were verified code-for-code against the MCHP SAS macros (see ?elix_codes for links).

-   New exported dataset `elix_codes`: the ICD codes defining the 31 Elixhauser comorbidity categories used by compute_comorbidity(), with the category labels, full names, and matching rules (prefix/exact) for all three supported ICD versions. compute_comorbidity() is now driven by this dataset internally; its interface and results are unchanged (verified against the previous implementation over every code in the lists).

-   The confirmation prompts in execute_def(), bind_source(), and report_n() now abort with an informative error in non-interactive sessions (e.g., Rscript, knitr) instead of silently proceeding (readline() returns "" when not interactive, which was treated as consent). Interactively, only an explicit "y"/"yes" answer proceeds now; any other answer cancels.

-   restrict_date() (data.frame method) with mode = "filter" no longer warns ("no non-missing arguments to max") when no record is left from the previous steps.

-   Removed leftover commented-out code and browser() calls across the package; no functional change.

-   Fixed a bug in define_case() and define_case_with_age(): with `keep = "last"`, the first occurrence of "min" anywhere in the internally generated code was string-replaced with "max", which corrupted column names containing "min" (e.g., a `clnt_id` named "admin_id" became "admax_id" and caused an error).

-   Internal refactoring: restrict_date() (database method), define_case(), and define_case_with_age() no longer build queries by string-replacing names in deparsed code; column names are now injected as symbols with 'rlang'. The generated SQL is unchanged, but column names that contain substrings such as "clnt_id", "date_var", "_i", or "_x" no longer risk corrupting the query.

-   **Behavior change**: compute_comorbidity() now matches ICD-10 codes by prefix instead of exactly. Codes in `data` are truncated to their first 3 and 4 characters and compared with the 3-character (e.g., I50) and 4-character (e.g., E115) codes in Quan et al. (2005), respectively, which cover all their subdivisions. Previously, exact matching missed subdivisions of the listed codes, e.g., "I500" was not counted as Congestive Heart Failure ("I50" on the list), and "E1152" (ICD-10-CA) was not counted as Diabetes Complicated ("E115" on the list). ICD-10 scores may therefore be higher than in previous versions. Records with codes that belong to multiple categories in Quan et al. (e.g., I11.0 in both Congestive Heart Failure and Hypertension Complicated) are now counted in all matching categories.

-   Fixed a bug in the data.frame method of identify_row(): when the input contained columns named `rid` or `incl`, the function renamed those columns in the user's original data frame by reference (e.g., `rid` became `rid.og`) and silently overwrote then dropped them from the output. Internal working columns are now given names guaranteed not to clash with the input, and the input is never modified.

# healthdb 0.5.1

-   Updated identify_row() to accommodate changes in upcoming 'dbplyr' release - Thanks the 'dbplyr' devs for the fix!

-   fixed an error caused by changes in 'data.table'

# healthdb 0.5.0

## Bug fix

-   Fixed a bug in the apart feature in restrict_date() caused by 'dbplyr' 2.5.1 update where the input order of the SQL translation for difftime() was flipped.

## Improvements

-   Added a new define function with age restriction - define_case_with_age()

# healthdb 0.4.1

## Improvements

-   Functions that take remote table as input now checks whether connection to database is working properly.

# healthdb 0.4.0

## Improvements

-   fetch_var() documentation now indicates that it does not accept input from a pipe.
-   collapse_episode() now works for remote table input.
-   pool_case() now provides the source for the first valid and the latest records for linkage back to the source data.

# healthdb 0.3.1

## Bug fixes

-   Fixed a critical bug introduced by the new code that cleans the window order remaining in database output. The problem is related to this issue [Inability to remove window_order() leads to weird, unpredictable results](https://github.com/tidyverse/dbplyr/issues/1248).

-   bind_source() will bind correctly if every variable name argument is a single string (i.e., selecting variables with common names only).

-   Fixed a bug in pool_case() when every source had all the flags, the code that fills missing flag == 1 would trigger an error.

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
