# ICD codes defining the Elixhauser comorbidity categories

The International Classification of Diseases (ICD) diagnostic codes that
define the 31 Elixhauser comorbidity categories, as used by
[`compute_comorbidity()`](https://kevinhzq.github.io/healthdb/reference/compute_comorbidity.md).
The "ICD-10" and "ICD-9-CM-5digits" versions are from Quan et al.
(2005). The "ICD-9-CM-3digits" version is adopted from the Manitoba
Centre for Health Policy, which searches BOTH 3-digit and 5-digit codes
(see
<http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?printer=Y&conceptID=1436#CAUTIONS>
for cautions and limitations).

## Usage

``` r
elix_codes
```

## Format

A data frame with 893 rows and 5 columns:

- icd_ver:

  ICD code version; one of "ICD-10", "ICD-9-CM-5digits", and
  "ICD-9-CM-3digits", corresponding to the `icd_ver` argument of
  [`compute_comorbidity()`](https://kevinhzq.github.io/healthdb/reference/compute_comorbidity.md).

- category:

  Short label of the comorbidity category, e.g., "chf". These labels are
  the column names in
  [`compute_comorbidity()`](https://kevinhzq.github.io/healthdb/reference/compute_comorbidity.md)
  output and the valid values of its `excl` argument.

- description:

  Full name of the comorbidity category, e.g., "Congestive Heart
  Failure".

- code:

  The ICD code, e.g., "I50". Codes contain no dots.

- match_len:

  The number of leading characters of a diagnosis value that is compared
  with the code (prefix match), equal to the number of characters in the
  code. Codes in the sources are listed at the category/subcategory
  level and cover all their subdivisions (the ".x" notation in Quan et
  al.), hence the prefix match, e.g., ICD-10 "I50" covers "I500", and
  ICD-9 "428" covers "4280" and "42800". This reproduces the behavior of
  the reference SAS implementation, which compares every code with the
  SAS `IN:` (starts-with) operator.

## Source

The code lists were verified code-for-code against the SAS macros
published by the Manitoba Centre for Health Policy (based on Quan's
"Enhanced Elixhauser Diagnosis-Type SAS code"):

- ICD-9-CM:
  <http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS/_ElixhauserICD9CM.sas.txt>

- ICD-10:
  <http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS/_ElixhauserICD10.sas.txt>

## References

Quan H, Sundararajan V, Halfon P, Fong A, Burnand B, Luthi JC, Saunders
LD, Beck CA, Feasby TE, Ghali WA. Coding algorithms for defining
comorbidities in ICD-9-CM and ICD-10 administrative data. Med Care
2005;43(11):1130-1139.

## Examples

``` r
head(elix_codes)
#>   icd_ver category              description code match_len
#> 1  ICD-10      chf Congestive Heart Failure  I43         3
#> 2  ICD-10      chf Congestive Heart Failure  I50         3
#> 3  ICD-10      chf Congestive Heart Failure I099         4
#> 4  ICD-10      chf Congestive Heart Failure I110         4
#> 5  ICD-10      chf Congestive Heart Failure I130         4
#> 6  ICD-10      chf Congestive Heart Failure I132         4

# codes defining Congestive Heart Failure in ICD-10
subset(elix_codes, icd_ver == "ICD-10" & category == "chf")
#>    icd_ver category              description code match_len
#> 1   ICD-10      chf Congestive Heart Failure  I43         3
#> 2   ICD-10      chf Congestive Heart Failure  I50         3
#> 3   ICD-10      chf Congestive Heart Failure I099         4
#> 4   ICD-10      chf Congestive Heart Failure I110         4
#> 5   ICD-10      chf Congestive Heart Failure I130         4
#> 6   ICD-10      chf Congestive Heart Failure I132         4
#> 7   ICD-10      chf Congestive Heart Failure I255         4
#> 8   ICD-10      chf Congestive Heart Failure I420         4
#> 9   ICD-10      chf Congestive Heart Failure I425         4
#> 10  ICD-10      chf Congestive Heart Failure I426         4
#> 11  ICD-10      chf Congestive Heart Failure I427         4
#> 12  ICD-10      chf Congestive Heart Failure I428         4
#> 13  ICD-10      chf Congestive Heart Failure I429         4
#> 14  ICD-10      chf Congestive Heart Failure P290         4
```
