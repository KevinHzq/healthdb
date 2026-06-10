# healthdb: Working with Healthcare Databases

A system for identifying diseases or events from healthcare databases
and preparing data for epidemiological studies. It includes capabilities
not supported by 'SQL', such as matching strings by 'stringr' style
regular expressions, and can compute comorbidity scores (Quan et al.
(2005)
[doi:10.1097/01.mlr.0000182534.19832.83](https://doi.org/10.1097/01.mlr.0000182534.19832.83)
) directly on a database server. The implementation is based on 'dbplyr'
with full 'tidyverse' compatibility.

## Package options

The behavior of some functions can be adjusted with \[options()\]:

- healthdb.verbose:

  Whether functions explain what they are doing and give an overview of
  the results. The default is TRUE. Use \`options(healthdb.verbose =
  FALSE)\` to turn the messages off.

- healthdb.force_proceed:

  Some operations have to download data from the database first, which
  may be slow. By default (FALSE), functions ask for your confirmation
  before downloading. Use \`options(healthdb.force_proceed = TRUE)\` to
  skip the prompts.

- healthdb.check_con:

  Whether functions test that the database connection is alive (by
  sending a trivial \`SELECT 1\` query) before building queries on it.
  The default is TRUE. Use \`options(healthdb.check_con = FALSE)\` to
  skip the test; this saves one trip to the database per step, which can
  add up in long pipelines, but a dropped connection would then surface
  as a less informative error.

## See also

Useful links:

- <https://github.com/KevinHzq/healthdb>

- <https://kevinhzq.github.io/healthdb/>

- Report bugs at <https://github.com/KevinHzq/healthdb/issues>

## Author

**Maintainer**: Kevin Hu <kevin.hu@bccdc.ca>
([ORCID](https://orcid.org/0000-0003-0254-5277)) \[copyright holder\]

Authors:

- Kevin Hu <kevin.hu@bccdc.ca>
  ([ORCID](https://orcid.org/0000-0003-0254-5277)) \[copyright holder\]
