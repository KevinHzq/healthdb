identify_record <- function(dat, clnt_id_nm, var_nm_pattern, val_vector, match_type = "in", n_per_clnt = 1, fuzzy_val = NULL, collapse_by_nm = NULL, multi_var_cols = FALSE, verbose = TRUE) {
  # input checks
  if (any(sapply(list(clnt_id_nm, var_nm_pattern, collapse_by_nm), function(x) !is.null(x) & !is.character(x)))) stop("Arguments ended with _nm must be characters.")

  if (!(match_type %in% c("start", "regex", "in", "between"))) stop('match_type must be one of "start", "regex", "in", or "between"')

  if (match_type %in% c("in", "between")) {
    if (class(dat[, grep(var_nm_pattern, names(dat))]) != class(val_vector)) warning("val_vector is not the same type as the var_nm column.")
  }

  # stop if conflict
  if (all(n_per_clnt == 1, !is.null(fuzzy_val))) stop("Fuzzy val should not be supplied if only one record per client is required. If appropriate, include it in val_vector instead.")

  # use data.table to speed up the performance
  dt <- data.table::as.data.table(dat)[, rid := .I]

  # stop if n_per_clnt doesn't make sense
  if (!is.null(collapse_by_nm)) {
    max_n <- dt[, .(max_n_per_clnt = data.table::uniqueN(collapse_by_nm)), by = clnt_id_nm][, max(max_n_per_clnt)]
  } else {
    max_n <- dt[, .N, by = clnt_id_nm][, max(N)]
  }

  if (max_n < n_per_clnt) stop("The maximum of n_per_clnt in the data is", max_n, "and smaller than the target specified. Try reduce n_per_clnt.")

  # force extract match if single var col
  if (!multi_var_cols) {
    var_nm_pattern <- paste0("^", var_nm_pattern, "$")
  }

  # add fuzzy_val into code_vec or fuzzy_val won't be identify at all; not needed for range filter
  if (match_type != "between") val_vector <- c(val_vector, fuzzy_val) %>% unique()

  # code filter by match types:
  # logic to preserve original record as a row without copying the original data (use extra memory):
  # 1 get a vector of all possible values
  # 2 find the vector of matched values by match_type
  # 3 compute incl indicator by row id across var columns, so that TRUE if any col has a match
  # 4 filter rows by incl

  all_val <- dt[, unlist(.SD) %>% stats::na.omit() %>% unique(), .SDcols = patterns(var_nm_pattern)]

  if (match_type == "start") {
    match_str <- paste0("^", val_vector, collapse = "|")
    match_msg <- "satisfied regular expression"
    match_val <- all_val[data.table::like(all_val, match_str)]
  }
  if (match_type == "regex") {
    match_str <- paste0(val_vector, collapse = "|")
    match_msg <- "satisfied regular expression"
    match_val <- all_val[data.table::like(all_val, match_str)]
  }
  if (match_type == "in") {
    match_str <- deparse(val_vector)
    match_msg <- "exactly matched values in set"
    match_val <- val_vector
  }
  if (match_type == "between") {
    match_str <- deparse(val_vector)
    match_msg <- "between range (bounds included)"
    match_val <- all_val[`%between%`(all_val, val_vector)]
  }

  dt[, incl := `%in%`(unlist(.SD), match_val) %>% any(), by = "rid", .SDcols = patterns(var_nm_pattern)]

  # explain the configuration in plain language to prompt user thinking
  if (verbose) {
    cat(
      "\nSearching conditions:\nEach client has at least", n_per_clnt, "of distinct", ifelse(is.null(collapse_by_nm), "record(s)", collapse_by_nm),
      "\n  where", ifelse(multi_var_cols, "at least one of the", "the single"), var_nm_pattern, "column",
      "\n    contains a value", match_msg, match_str,
      ifelse(n_per_clnt > 1 & !is.null(fuzzy_val), paste("\nExcluding clients which all their matched values are in set", deparse(fuzzy_val)), ""), "\n"
    )
  }

  # run filter and save; the above filter only updates the dat with new columns
  dt <- dt[incl == TRUE]
  dt[, incl := NULL]
  n_row <- nrow(dt)
  if (n_row == 0) {
    warning("No match found. Check val_vector.")
  } else if (verbose) cat("\nNumber of clients that has any matches:", dt[, data.table::uniqueN(.SD), .SDcols = clnt_id_nm], "\n")

  # records cannot be all fuzzy within person
  if (!is.null(fuzzy_val)) {
    match_val_wo_fuzzy <- setdiff(match_val, fuzzy_val)
    dt[, fuzzy := `%in%`(unlist(.SD) %>% stats::na.omit(), match_val_wo_fuzzy) %>% any(), by = clnt_id_nm, .SDcols = patterns(var_nm_pattern)]
    dt <- dt[fuzzy == TRUE]
    dt[, fuzzy := NULL]
    if (n_row != 0 & nrow(dt) == 0) {
      warning("Some matches found but all excluded by fuzzy_val.")
      n_row <- nrow(dt)
    } else if (verbose) cat("\nNumber of clients with fuzz_val exclusion:", dt[, data.table::uniqueN(.SD), .SDcols = clnt_id_nm], "\n")
  }

  dt[, rid := NULL]

  # job done if getting any records
  if (n_per_clnt == 1) {
    data.table::setDF(dt)
    return(dt)
  }

  # keep records for person with number of records >= n_per_clnt
  else {
    # count differently if unit id is supplied by collaspe_by_nm
    if (!is.null(collapse_by_nm)) {
      n_filter <- dt[, .(n_collapsed = data.table::uniqueN(.SD)), by = clnt_id_nm, .SDcols = collapse_by_nm][n_collapsed >= n_per_clnt]
    } else {
      n_filter <- dt[, .N, by = clnt_id_nm][N >= n_per_clnt]
    }

    dt <- dt[.(n_filter[, ..clnt_id_nm]), on = clnt_id_nm]
    if (n_row != 0 & nrow(dt) == 0) {
      warning("Some matches found but all excluded by counting n_per_clnt.")
    } else if (verbose) cat("\nNumber of clients satisfied all conditions:", dt[, data.table::uniqueN(.SD), .SDcols = clnt_id_nm], "\n")

    # convert back to dataframe before output
    data.table::setDF(dt)
    return(dt)
  }
}
