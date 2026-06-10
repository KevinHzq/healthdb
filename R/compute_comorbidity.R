#' Compute Elixhauser Comorbidity Index
#'
#' @md
#' @description
#' This function computes unweighted Elixhauser Comorbidity Index for both data.frame and remote table input. The ICD codes used to identify the 31 disease categories is from Quan et al. (2005).
#'
#' @inheritParams define_case
#' @param icd_ver One of `c("ICD-10", "ICD-9-CM-3digits", "ICD-9-CM-5digits")`. Specify the ICD code version used in `data`. The ICD-10 and ICD-9-CM 5 digits version are from Quan et al. (2005). The ICD-9-CM 3 digits version is adopted from Manitoba Centre for Health Policy. It uses BOTH 3-digit and 5-digit codes in search. See their web page for cautions and limitations of the 3 digit version if your data only has 3-digit codes (\url{http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?printer=Y&conceptID=1436#CAUTIONS}). For "ICD-10", the codes in Quan et al. (2005) are listed at the category (3-character, e.g., I50) or subcategory (4-character, e.g., I099) level and cover all their subdivisions. Therefore, matching is done by prefix: codes in `data` are truncated to the first 3 and the first 4 characters and compared with the 3- and 4-character codes in the list, respectively. For example, "E1152" in `data` would be captured by "E115" (Diabetes Complicated) in the list. Codes in `data` must not contain dots (e.g., use "E1152" not "E11.52"); otherwise, codes may not be matched correctly. Note that some codes belong to multiple categories in Quan et al. (2005), e.g., I11.0 indicates both Congestive Heart Failure (I099, I110, ...) and Hypertension Complicated (I11.x); records with such codes are counted in all the categories they match.
#' @param sum_by One of "row" or "clnt". The "row" option computes total score for each row (default), and the "clnt" option summarizes total score by `clnt_id`. Each disease categories will be counted only once in the calculation regardless of multiple records in a category.
#' @param excl A character vector of disease categories labels that should be excluded in the total score calculation. This is useful when some of the categories are the exposure/outcome of interest, and the goal is to measure comorbidity excluding these disease. See detail for a list of the categories and labels.
#'
#' @details
#' List of disease categories - labels (in quote):
#' \enumerate{
#'  \item Congestive Heart Failure - "chf"
#'  \item Cardiac Arrhythmia - "arrhy"
#'  \item Valvular Disease - "vd"
#'  \item Pulmonary Circulation Disorders - "pcd"
#'  \item Peripheral Vascular Disorders - "pvd"
#'  \item Hypertension Uncomplicated - "hptn_nc"
#'  \item Hypertension Complicated - "hptn_c"
#'  \item Paralysis - "para"
#'  \item Other Neurological Disorders - "othnd"
#'  \item Chronic Pulmonary Disease - "copd"
#'  \item Diabetes Uncomplicated - "diab_nc"
#'  \item Diabetes Complicated - "diab_c"
#'  \item Hypothyroidism - "hptothy"
#'  \item Renal Failure - "rf"
#'  \item Liver Disease - "ld"
#'  \item Peptic Ulcer Disease excluding bleeding - "pud_nb"
#'  \item AIDS/HIV - "hiv"
#'  \item Lymphoma - "lymp"
#'  \item Metastatic Cancer - "mets"
#'  \item Solid Tumor without Metastasis - "tumor"
#'  \item Rheumatoid Arthritis/collagen - "rheum_a"
#'  \item Coagulopathy - "coag"
#'  \item Obesity - "obesity"
#'  \item Weight Loss - "wl"
#'  \item Fluid and Electrolyte Disorders - "fluid"
#'  \item Blood Loss Anemia - "bla"
#'  \item Deficiency Anemia - "da"
#'  \item Alcohol Abuse - "alcohol"
#'  \item Drug Abuse - "drug"
#'  \item Psychoses - "psycho"
#'  \item Depression - "dep"
#' }
#'
#' The full ICD code lists defining these categories, including the matching
#' rule for each code (prefix vs. exact), are available in the exported
#' dataset [elix_codes].
#'
#' @seealso [elix_codes]
#'
#' @return A data.frame or remote table with binary indicators for each categories as columns.
#' @export
#'
#' @references Quan H, Sundararajan V, Halfon P, Fong A, Burnand B, Luthi JC, Saunders LD, Beck CA, Feasby TE, Ghali WA. Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Med Care 2005;43(11):1130-1139.
#' @examples
#' # make ICD-9 toy data
#' df <- data.frame(
#'   uid = 1:10, clnt_id = sample(1:3, 10, replace = TRUE),
#'   diagx_1 = c("193", "2780", "396", "4254", "4150", "401", "401", "0932", "5329", "2536"),
#'   diagx_2 = c(NA, NA, "72930", "V6542", "493", "405", "5880", "2409", "714", NA)
#' )
#'
#' # compute Elixhauser Comorbidity Index by row
#' # uid is needed for by row calculation
#' # 3 categories were excluded in total_eci
#' compute_comorbidity(df,
#'   vars = starts_with("diagx"),
#'   icd_ver = "ICD-9-CM-5digits",
#'   clnt_id = clnt_id, uid = uid,
#'   excl = c("drug", "psycho", "dep")
#' )
#'
#' # compute ECI by person
#' compute_comorbidity(df,
#'   vars = starts_with("diagx"),
#'   icd_ver = "ICD-9-CM-5digits",
#'   clnt_id = clnt_id,
#'   sum_by = "clnt"
#' )
compute_comorbidity <- function(data, vars, icd_ver = c("ICD-10", "ICD-9-CM-3digits", "ICD-9-CM-5digits"), clnt_id, uid = NULL, sum_by = c("row", "clnt"), excl = NULL) {
  if (!is.data.frame(data)) {
    check_con(data)
  }

  # input check
  icd_ver <- rlang::arg_match(icd_ver)
  sum_by <- rlang::arg_match(sum_by)

  # get variable names as text with tidyselect and NSE
  db_head <- data %>%
    dplyr::select({{ vars }}) %>%
    utils::head(n = 1) %>%
    dplyr::collect()
  vars <- colnames(db_head)

  # capture variable names
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))

  has_uid <- !rlang::quo_is_null(rlang::enquo(uid))
  if (has_uid) uid <- rlang::as_name(rlang::enquo(uid))

  # pivot data to long form using variable names captured
  data_long <- data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "diag_no",
      values_to = "diag_cd"
    )

  # determine summary by row or clnt
  if (sum_by == "row") {
    if (!has_uid) stop("\nuid must be supplied for by row summary.")
    data_long <- data_long %>%
      dplyr::group_by(.data[[clnt_id]], .data[[uid]])
  } else {
    data_long <- data_long %>%
      dplyr::group_by(.data[[clnt_id]])
  }


  # build the category indicator expressions from the elix_codes lookup;
  # codes with match_len = L are compared with the first L characters of the
  # diagnosis values (prefix match, e.g., listed "I50" covers "I501"); codes
  # with match_len = NA are compared with the full values (exact match).
  # See ?elix_codes for detail.
  lu <- elix_codes[elix_codes$icd_ver == icd_ver, ]
  full_cat <- unique(lu$category)

  # helper columns with truncated codes for prefix matching
  prefix_lens <- sort(unique(stats::na.omit(lu$match_len)))
  trunc_exprs <- lapply(prefix_lens, function(l) rlang::expr(stringr::str_sub(.data[["diag_cd"]], end = !!l)))
  # vapply, not paste0, as the latter does not preserve zero length
  # (no prefix codes, e.g., ICD-9-CM-5digits)
  names(trunc_exprs) <- vapply(prefix_lens, function(l) paste0("diag_", l, "dig"), character(1))

  # one case_when per category with a condition per match_len group
  cat_exprs <- lapply(full_cat, function(cat) {
    lu_cat <- lu[lu$category == cat, ]
    # group codes by match_len, preserving the order of first appearance
    seg_id <- match(lu_cat$match_len, unique(lu_cat$match_len))
    conds <- lapply(split(lu_cat, seg_id), function(s) {
      len <- s$match_len[1]
      if (is.na(len)) {
        rlang::expr(.data[["diag_cd"]] %in% !!s$code ~ 1L)
      } else {
        rlang::expr(!!rlang::sym(paste0("diag_", len, "dig")) %in% !!s$code ~ 1L)
      }
    })
    rlang::expr(dplyr::case_when(!!!unname(conds), .default = 0))
  })
  names(cat_exprs) <- full_cat

  data_long <- data_long %>%
    dplyr::mutate(!!!trunc_exprs) %>%
    dplyr::mutate(!!!cat_exprs)

  # set categories included in the total calculation
  incl <- full_cat[!(full_cat %in% excl)]
  total_expr <- Reduce(function(x, y) rlang::expr(!!x + !!y), rlang::syms(incl))

  data_sum <- data_long %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(full_cat), ~ max(., na.rm = TRUE))) %>%
    dplyr::mutate(total_eci = !!total_expr)

  dplyr::ungroup(data_sum)
}
