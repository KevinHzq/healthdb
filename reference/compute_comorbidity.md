# Compute Elixhauser Comorbidity Index

This function computes unweighted Elixhauser Comorbidity Index for both
data.frame and remote table input. The ICD codes used to identify the 31
disease categories is from Quan et al. (2005).

## Usage

``` r
compute_comorbidity(
  data,
  vars,
  icd_ver = c("ICD-10", "ICD-9-CM-3digits", "ICD-9-CM-5digits"),
  clnt_id,
  uid = NULL,
  sum_by = c("row", "clnt"),
  excl = NULL
)
```

## Arguments

- data:

  Data.frames or remote tables (e.g., from
  [`dbplyr::tbl_sql()`](https://dbplyr.tidyverse.org/reference/tbl_sql.html))

- vars:

  An expression passing to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
  It can be quoted/unquoted column names, or helper functions, such as
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html).

- icd_ver:

  One of `c("ICD-10", "ICD-9-CM-3digits", "ICD-9-CM-5digits")`. Specify
  the ICD code version used in `data`. The ICD-10 and ICD-9-CM 5 digits
  version are from Quan et al. (2005). The ICD-9-CM 3 digits version is
  adopted from Manitoba Centre for Health Policy. It uses BOTH 3-digit
  and 5-digit codes in search. See their web page for cautions and
  limitations of the 3 digit version if your data only has 3-digit codes
  (<http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?printer=Y&conceptID=1436#CAUTIONS>).

- clnt_id:

  Grouping variable (quoted/unquoted).

- uid:

  Variable name for a unique row identifier. It is necessary for SQL to
  produce consistent result based on sorting.

- sum_by:

  One of "row" or "clnt". The "row" option computes total score for each
  row (default), and the "clnt" option summarizes total score by
  `clnt_id`. Each disease categories will be counted only once in the
  calculation regardless of multiple records in a category.

- excl:

  A character vector of disease categories labels that should be
  excluded in the total score calculation. This is useful when some of
  the categories are the exposure/outcome of interest, and the goal is
  to measure comorbidity excluding these disease. See detail for a list
  of the categories and labels.

## Value

A data.frame or remote table with binary indicators for each categories
as columns.

## Details

List of disease categories - labels (in quote):

1.  Congestive Heart Failure - "chf"

2.  Cardiac Arrhythmia - "arrhy"

3.  Valvular Disease - "vd"

4.  Pulmonary Circulation Disorders - "pcd"

5.  Peripheral Vascular Disorders - "pvd"

6.  Hypertension Uncomplicated - "hptn_nc"

7.  Hypertension complicated - "hptn_C"

8.  Paralysis - "para"

9.  Other Neurological Disorders - "Othnd"

10. Chronic Pulmonary Disease - "copd"

11. Diabetes Uncomplicated - "diab_nc"

12. Diabetes Complicated - "diab_c"

13. Hypothyroidism - "hptothy"

14. Renal Failure - "rf"

15. Liver Disease - "ld"

16. Peptic Ulcer Disease excluding bleeding - "pud_nb"

17. AIDS/HIV - "hiv"

18. Lymphoma - "lymp"

19. Metastatic Cancer - "mets"

20. Solid Tumor without Metastasis - "tumor"

21. Rheumatoid Arthritis/collagen - "rheum_a"

22. Coagulopathy - "coag"

23. Obesity - "obesity"

24. Weight Loss - "wl"

25. Fluid and Electrolyte Disorders - "fluid"

26. Blood Loss Anemia - "bla"

27. Deficiency Anemia - "da"

28. Alcohol Abuse - "alcohol"

29. Drug Abuse - "drug"

30. Psychoses - "psycho"

31. Depression - "dep"

## References

Quan H, Sundararajan V, Halfon P, Fong A, Burnand B, Luthi JC, Saunders
LD, Beck CA, Feasby TE, Ghali WA. Coding algorithms for defining
comorbidities in ICD-9-CM and ICD-10 administrative data. Med Care
2005;43(11):1130-1139.

## Examples

``` r
# make ICD-9 toy data
df <- data.frame(
  uid = 1:10, clnt_id = sample(1:3, 10, replace = TRUE),
  diagx_1 = c("193", "2780", "396", "4254", "4150", "401", "401", "0932", "5329", "2536"),
  diagx_2 = c(NA, NA, "72930", "V6542", "493", "405", "5880", "2409", "714", NA)
)

# compute Elixhauser Comorbidity Index by row
# uid is needed for by row calculation
# 3 categories were excluded in total_eci
compute_comorbidity(df,
  vars = starts_with("diagx"),
  icd_ver = "ICD-9-CM-5digits",
  clnt_id = clnt_id, uid = uid,
  excl = c("drug", "psycho", "dep")
)
#> # A tibble: 10 × 34
#>    clnt_id   uid   chf arrhy    vd   pcd   pvd hptn_nc hptn_c  para othnd  copd
#>      <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#>  1       1     8     0     0     1     0     0       0      0     0     0     0
#>  2       1    10     0     0     0     0     0       0      0     0     0     0
#>  3       2     1     0     0     0     0     0       0      0     0     0     0
#>  4       2     2     0     0     0     0     0       0      0     0     0     0
#>  5       2     3     0     0     1     0     0       0      0     0     0     0
#>  6       2     5     0     0     0     1     0       0      0     0     0     1
#>  7       2     6     0     0     0     0     0       1      1     0     0     0
#>  8       2     9     0     0     0     0     0       0      0     0     0     0
#>  9       3     4     1     0     0     0     0       0      0     0     0     0
#> 10       3     7     0     0     0     0     0       1      0     0     0     0
#> # ℹ 22 more variables: diab_nc <dbl>, diab_c <dbl>, hptothy <dbl>, rf <dbl>,
#> #   ld <dbl>, pud_nb <dbl>, hiv <dbl>, lymp <dbl>, mets <dbl>, tumor <dbl>,
#> #   rheum_a <dbl>, coag <dbl>, obesity <dbl>, wl <dbl>, fluid <dbl>, bla <dbl>,
#> #   da <dbl>, alcohol <dbl>, drug <dbl>, psycho <dbl>, dep <dbl>,
#> #   total_eci <dbl>

# compute ECI by person
compute_comorbidity(df,
  vars = starts_with("diagx"),
  icd_ver = "ICD-9-CM-5digits",
  clnt_id = clnt_id,
  sum_by = "clnt"
)
#> # A tibble: 3 × 33
#>   clnt_id   chf arrhy    vd   pcd   pvd hptn_nc hptn_c  para othnd  copd diab_nc
#>     <int> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
#> 1       1     0     0     1     0     0       0      0     0     0     0       0
#> 2       2     0     0     1     1     0       1      1     0     0     1       0
#> 3       3     1     0     0     0     0       1      0     0     0     0       0
#> # ℹ 21 more variables: diab_c <dbl>, hptothy <dbl>, rf <dbl>, ld <dbl>,
#> #   pud_nb <dbl>, hiv <dbl>, lymp <dbl>, mets <dbl>, tumor <dbl>,
#> #   rheum_a <dbl>, coag <dbl>, obesity <dbl>, wl <dbl>, fluid <dbl>, bla <dbl>,
#> #   da <dbl>, alcohol <dbl>, drug <dbl>, psycho <dbl>, dep <dbl>,
#> #   total_eci <dbl>
```
