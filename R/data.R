#' ICD codes defining the Elixhauser comorbidity categories
#'
#' @md
#' @description
#' The International Classification of Diseases (ICD) diagnostic codes that
#' define the 31 Elixhauser comorbidity categories, as used by
#' [compute_comorbidity()]. The "ICD-10" and "ICD-9-CM-5digits" versions are
#' from Quan et al. (2005). The "ICD-9-CM-3digits" version is adopted from the
#' Manitoba Centre for Health Policy, which searches BOTH 3-digit and 5-digit
#' codes (see
#' \url{http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?printer=Y&conceptID=1436#CAUTIONS}
#' for cautions and limitations).
#'
#' @format A data frame with 893 rows and 5 columns:
#' \describe{
#'  \item{icd_ver}{ICD code version; one of "ICD-10", "ICD-9-CM-5digits", and "ICD-9-CM-3digits", corresponding to the `icd_ver` argument of [compute_comorbidity()].}
#'  \item{category}{Short label of the comorbidity category, e.g., "chf". These labels are the column names in [compute_comorbidity()] output and the valid values of its `excl` argument.}
#'  \item{description}{Full name of the comorbidity category, e.g., "Congestive Heart Failure".}
#'  \item{code}{The ICD code, e.g., "I50". Codes contain no dots.}
#'  \item{match_len}{How the code is compared with the diagnosis values in data: an integer L means prefix match, i.e., a diagnosis value matches if its first L characters equal the code (codes listed at the category/subcategory level cover all their subdivisions); NA means exact match of the full code.}
#' }
#'
#' @references Quan H, Sundararajan V, Halfon P, Fong A, Burnand B, Luthi JC, Saunders LD, Beck CA, Feasby TE, Ghali WA. Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Med Care 2005;43(11):1130-1139.
#' @examples
#' head(elix_codes)
#'
#' # codes defining Congestive Heart Failure in ICD-10
#' subset(elix_codes, icd_ver == "ICD-10" & category == "chf")
"elix_codes"
