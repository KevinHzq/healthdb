#' @keywords internal
#' @section Package options:
#' The behavior of some functions can be adjusted with [options()]:
#' \describe{
#'  \item{healthdb.verbose}{Whether functions explain what they are doing and give an overview of the results. The default is TRUE. Use `options(healthdb.verbose = FALSE)` to turn the messages off.}
#'  \item{healthdb.force_proceed}{Some operations have to download data from the database first, which may be slow. By default (FALSE), functions ask for your confirmation before downloading. Use `options(healthdb.force_proceed = TRUE)` to skip the prompts.}
#'  \item{healthdb.check_con}{Whether functions test that the database connection is alive (by sending a trivial `SELECT 1` query) before building queries on it. The default is TRUE. Use `options(healthdb.check_con = FALSE)` to skip the test; this saves one trip to the database per step, which can add up in long pipelines, but a dropped connection would then surface as a less informative error.}
#' }
"_PACKAGE"

utils::globalVariables(c("patterns", "elix_codes"))

## usethis namespace: start
#' @importFrom data.table %between%
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table data.table
#' @importFrom lubridate %m-%
#' @importFrom lubridate %m+%
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom rlang .data
## usethis namespace: end
NULL
