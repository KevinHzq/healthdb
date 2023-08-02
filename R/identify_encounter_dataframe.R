identify_encounter.data.frame <- function(data, clnt_id_nm, var_nm_pattern, val_vector, match_type = "in", n_per_clnt = 1, collapse_by_nm = NULL, multi_var_cols = FALSE, verbose = TRUE) {
  # input checks
  if (any(sapply(list(clnt_id_nm, var_nm_pattern, collapse_by_nm), function(x) !is.null(x) & !is.character(x)))) stop("Arguments ended with _nm must be characters.")

  if (!(match_type %in% c("start", "regex", "in", "between"))) stop('match_type must be one of "start", "regex", "in", or "between"')

  if (match_type %in% c("in", "between")) {
   if (any(sapply(dt[, grep(var_nm_pattern, names(dt))], class) != class(val_vector))) warning("val_vector (", class(val_vector), ") is not the same type as the var_nm columns (", paste(sapply(dt[, grep(var_nm_pattern, names(dt))], class), collapse = ", "), ").")
  }

  #place holder for temp column names
  rid <- max_n_per_clnt <- N <- incl <- n_collapsed <- NULL

  # use data.table to speed up the performance
  dt <- data.table::as.data.table(data)

  #treat potential name conflicts
  temp_cols <- c("rid", "incl")
  data.table::setnames(data, old = temp_cols, new = paste(temp_cols, "og", sep = "."), skip_absent = TRUE)

  dt[, rid := .I]

  # stop if n_per_clnt doesn't make sense
  if (!is.null(collapse_by_nm)) {
    max_n <- dt[, list(max_n_per_clnt = data.table::uniqueN(collapse_by_nm)), by = clnt_id_nm][, max(max_n_per_clnt)]
  } else {
    max_n <- dt[, .N, by = clnt_id_nm][, max(N)]
  }

  if (max_n < n_per_clnt) stop("The maximum of n_per_clnt in the data is", max_n, "and smaller than the target specified. Try reduce n_per_clnt.")

  # force exact match if single var col
  if (!multi_var_cols) {
    var_nm_pattern <- paste0("^", var_nm_pattern, "$")
  }

  # code filter by match types:
  # logic to preserve original record as a row without copying the original data (use extra memory):
  # 1 get a vector of all possible values
  # 2 find the vector of matched values by match_type
  # 3 compute incl indicator by row id across var columns, so that TRUE if any col has a match
  # 4 filter rows by incl

  all_val <- dt[, unlist(.SD) %>% stats::na.omit() %>% unique(), .SDcols = patterns(var_nm_pattern)]

  if (match_type == "start") {
    #match_str/msg is for verbose
    match_str <- paste0("^", val_vector, collapse = "|")
    match_msg <- "satisfied regular expression"
    #extract matched values from all possible ones
    match_val <- all_val[data.table::like(all_val, match_str)]
  }
  if (match_type == "regex") {
    match_str <- paste0(val_vector, collapse = "|")
    match_msg <- "satisfied regular expression"
    match_val <- all_val[data.table::like(all_val, match_str)]
  }
  if (match_type == "in") {
    match_str <- deparse(substitute(val_vector))
    match_msg <- "exactly matched values in set"
    match_val <- val_vector
  }
  if (match_type == "between") {
    match_str <- deparse(substitute(val_vector))
    match_msg <- "between range (bounds included)"
    match_val <- all_val[`%between%`(all_val, val_vector)]
  }

  #use %chin% to speed up character matching
  if (all(is.character(match_val), sapply(dt[, grep(var_nm_pattern, names(dt))], is.character)))
  {
    dt[, incl := data.table::`%chin%`(unlist(.SD), match_val) %>% any(), by = "rid", .SDcols = patterns(var_nm_pattern)]
  }
  else {
    dt[, incl := `%in%`(unlist(.SD), match_val) %>% any(), by = "rid", .SDcols = patterns(var_nm_pattern)]
  }

  # explain the configuration in plain language to prompt user thinking
  if (verbose) {
    cat(
      "\nSearching conditions:\nEach client has at least", n_per_clnt, "record(s)", ifelse(is.null(collapse_by_nm), "", paste("with distinct", collapse_by_nm)),
      "\n - where", ifelse(multi_var_cols, "at least one of the", "the single"), var_nm_pattern, "column in each record",
      "\n   - contains a value", match_msg, match_str, "\n"
    )
  }

  # run filter and save; the above filter only updates the dat with new columns
  dt <- dt[incl == TRUE]

  n_row <- nrow(dt)
  if (n_row == 0) {
    warning("No match found. Check val_vector.")
  } else if (verbose) cat("\nNumber of clients that has any matches:", dt[, data.table::uniqueN(.SD), .SDcols = clnt_id_nm], "\n")

  dt[, c(temp_cols) := NULL]

  # job done if getting any records
  if (n_per_clnt == 1) {
    data.table::setDF(dt)
    return(dt)
  }

  # keep records for person with number of records >= n_per_clnt
  else {
    # count differently if unit id is supplied by collaspe_by_nm
    if (!is.null(collapse_by_nm)) {
      n_filter <- dt[, list(n_collapsed = data.table::uniqueN(.SD)), by = clnt_id_nm, .SDcols = collapse_by_nm][n_collapsed >= n_per_clnt]
    } else {
      n_filter <- dt[, .N, by = clnt_id_nm][N >= n_per_clnt]
    }

    dt <- dt[list(n_filter[, clnt_id_nm, with = FALSE]), on = clnt_id_nm]
    if (n_row != 0 & nrow(dt) == 0) {
      warning("Some matches found but all excluded by counting n_per_clnt.")
    } else if (verbose) cat("\nNumber of clients satisfied all conditions:", dt[, data.table::uniqueN(.SD), .SDcols = clnt_id_nm], "\n")

    # convert back to dataframe before output
    data.table::setDF(dt)
    return(dt)
  }
}
