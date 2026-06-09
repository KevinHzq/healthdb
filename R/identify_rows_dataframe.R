#' @export
identify_rows.data.frame <- function(data, vars, match = c("in", "start", "regex", "like", "between", "glue_sql"), vals, if_all = FALSE, verbose = getOption("healthdb.verbose"), query_only = TRUE, ...) {
  # input checks
  match <- rlang::arg_match0(match, c("in", "start", "regex", "like", "between", "glue_sql"))
  if (match == "glue_sql") stop("'glue_sql' match option cannot be applied to local data frame.")

  # get variable names as text with tidyselect and NSE
  df_head <- data %>%
    dplyr::select({{ vars }}) %>%
    utils::head(n = 1)
  vars <- names(df_head)

  if (match %in% c("in", "between")) {
    var_class <- purrr::map_chr(df_head %>% dplyr::select(dplyr::all_of(vars)), class)
    if (!any(class(vals) %in% var_class)) warning("`vals` (", class(vals), ") is not the same type as the `var` columns (", paste(var_class, collapse = ", "), ").")
  }

  # use data.table to speed up the performance
  dt <- data.table::as.data.table(data)

  # internal working columns named to never clash with user's columns;
  # the user's data must not be renamed/overwritten (data.table::setnames
  # would mutate the input by reference)
  temp_rid_nm <- setdiff(make.unique(c(names(dt), "temp_rid")), names(dt))
  temp_incl_nm <- setdiff(make.unique(c(names(dt), "temp_incl")), names(dt))
  temp_cols <- c(temp_rid_nm, temp_incl_nm)

  dt[, c(temp_rid_nm) := .I]

  # code filter by match types:
  # logic to preserve original record as a row without copying the original data (use extra memory):
  # 1 get a vector of all possible values
  # 2 find the vector of matched values by match_type
  # 3 compute incl indicator by row id across var columns, so that TRUE if any col has a match
  # 4 filter rows by incl

  all_val <- dt[, unlist(.SD) %>% stats::na.omit() %>% unique(), .SDcols = vars]

  switch(match,
    "start" = {
      # match_str/msg is for verbose
      match_str <- paste0("^", vals, collapse = "|")
      match_msg <- "satisfied regular expression:"
      # extract matched values from all possible ones
      matched_vals <- all_val[data.table::like(all_val, match_str)]
    },
    "regex" = {
      match_str <- paste0(vals, collapse = "|")
      match_msg <- "satisfied regular expression:"
      matched_vals <- all_val[stringr::str_detect(all_val, match_str)]
    },
    "like" = {
      match_str <- paste0(vals, collapse = "|")
      match_msg <- "satisfied LIKE expression:"
      matched_vals <- all_val[stringr::str_like(all_val, match_str)]
    },
    "in" = {
      match_str <- deparse(substitute(vals))
      match_msg <- "exactly matched values in set:"
      matched_vals <- vals
    },
    "between" = {
      match_str <- deparse(substitute(vals))
      match_msg <- "between range (bounds included):"
      matched_vals <- all_val[`%between%`(all_val, vals)]
    }
  )

  combine_fn <- ifelse(if_all, all, any)

  # use %chin% to speed up character matching
  if (all(is.character(matched_vals), sapply(dt[, vars, with = FALSE], is.character))) {
    dt[, c(temp_incl_nm) := data.table::`%chin%`(unlist(.SD), matched_vals) %>% combine_fn(na.rm = TRUE), by = c(temp_rid_nm), .SDcols = vars]
  } else {
    dt[, c(temp_incl_nm) := `%in%`(unlist(.SD), matched_vals) %>% combine_fn(na.rm = TRUE), by = c(temp_rid_nm), .SDcols = vars]
  }

  # run filter and save; the above filter only updates the dat with new columns
  # compute the row index outside the data.table frame, then inject it with
  # `env` so user columns can never mask it
  keep_idx <- which(dt[[temp_incl_nm]])
  dt <- dt[keep_idx, env = list(keep_idx = I(keep_idx))]

  dt[, c(temp_cols) := NULL]

  # explain the configuration in plain language to prompt user thinking
  if (verbose) {
    result_vals <- dt[, unlist(.SD), .SDcols = vars] %>% unique()

    # cat(
    #   "\nIdentify records with condition(s):",
    #   "\n - where", ifelse(if_all & length(vars) > 1, "all of the", ifelse(length(vars) > 1, "at least one of the", "the")), paste0(vars, collapse = ", "), "column(s) in each record",
    #   "\n   - contains a value", match_msg, match_str, "\n"
    # )
    rlang::inform(c(
      "i" = "Identify records with condition(s):",
      "*" = glue::glue('where {ifelse(if_all & length(vars) > 1, "all of the", ifelse(length(vars) > 1, "at least one of the", "the"))} {paste0(vars, collapse = ", ")} column(s) in each record'),
      "*" = glue::glue("contains a value {match_msg} {match_str}")
    ))

    cat(ifelse(is.numeric(result_vals), "\nSummary of values in the result", "\nAll unique value(s) and frequency in the result"), ifelse(!if_all & length(vars) > 1, "(as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns):", ":"), "\n")

    switch(match,
      "between" = {
        cat("Range: ")
        print(range(result_vals))
      },
      if (is.character(result_vals)) {
        print(summary(factor(result_vals)), max = 100)
      } else {
        print(summary(result_vals))
      }
    )
  }

  # job done
  data.table::setDF(dt)
  return(dt)
}
