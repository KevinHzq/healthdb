identify_record_sql <- function(remote_tbl, clnt_id_nm, var_nm_pattern, val_vector, match_type = "in", n_per_clnt = 1, fuzzy_val = NULL, collapse_by_nm = NULL, multi_var_cols = FALSE, verbose = TRUE, query_only = TRUE) {
  # input checks
  if (!any(class(remote_tbl) %in% c("tbl_dbi", "tbl_sql", "tbl_lazy"))) stop("remote_tbl must be a remote database table made by e.g., tbl(db_connection, in_schema('schema_name', 'table_name'))")

  if (any(sapply(list(clnt_id_nm, var_nm_pattern, collapse_by_nm), function(x) !is.null(x) & !is.character(x)))) stop("Arguments ended with _nm must be characters.")

  if (!(match_type %in% c("start", "regex", "in", "between"))) stop('match_type must be one of "start", "regex", "in", or "between"')

  if (match_type %in% c("in", "between")) {
    db_head <- utils::head(remote_tbl, n = 1) %>% dplyr::collect()
    var_class <- db_head[[grep("diagx", names(db_head))[1]]] %>% class()
    if (var_class != class(val_vector)) warning("val_vector (", class(val_vector), ") is not the same type as the var_nm column (", var_class, ").")
  }

  # stop if conflict
  if (all(n_per_clnt == 1, !is.null(fuzzy_val))) stop("Fuzzy_val should not be supplied if only one record per client is required. If appropriate, include it in val_vector instead.")

  # stop if n_per_clnt doesn't make sense
  # if (!is.null(collapse_by_nm)) {
  #   max_n <- remote_tbl %>%
  #     dplyr::group_by(.data[[clnt_id_nm]]) %>%
  #     dplyr::summarise(n = dplyr::n_distinct(.data[[collapse_by_nm]])) %>%
  #     dplyr::pull(n) %>%
  #     max()
  # } else {
  #   max_n <- remote_tbl %>%
  #     dplyr::group_by(.data[[clnt_id_nm]]) %>%
  #     dplyr::summarise(n = dplyr::n()) %>%
  #     dplyr::pull(n) %>%
  #     max()
  # }

  # if (max_n < n_per_clnt) stop("The maximum of n_per_clnt in the data is", max_n, "and smaller than the target specified. Try reduce n_per_clnt.")

  # force extract match if single var col
  if (!multi_var_cols) {
    var_nm_regex <- paste0("^", var_nm_pattern, "$")
  } else {
    var_nm_regex <- var_nm_pattern
  }

  # add fuzzy_val into code_vec or fuzzy_val won't be identify at all; not needed for range filter
  if (match_type != "between") val_vector <- c(val_vector, fuzzy_val) %>% unique()

  # code filter by match types:
  # logic to preserve original record as a row without copying the original data (use extra memory):
  # 1 get a vector of all possible values
  # 2 find the vector of matched values by match_type
  # 3 compute incl indicator by row id across var columns, so that TRUE if any col has a match
  # 4 filter rows by incl

  # Note that SQL does not support regular expression. The match is done by collecting all distinct possible values locally then using regex in R. The result is plugged into subseqent query as: WHERE var_nm IN (match_result).
  all_val <- remote_tbl %>%
    tidyr::pivot_longer(
      cols = dplyr::matches({{ var_nm_regex }}),
      names_to = "position",
      values_to = {{ var_nm_pattern }}
    ) %>%
    dplyr::select({{ var_nm_pattern }}) %>%
    dplyr::distinct() %>%
    dplyr::pull({{ var_nm_pattern }})

  # get matched values set by match_type
  if (match_type == "start") {
    # match_str/msg is for verbose
    match_str <- paste0("^", val_vector, collapse = "|")
    match_msg <- "satisfied regular expression"
    # extract matched values from all possible ones
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

  # run filter with matched value set
  q_any_match <- remote_tbl %>%
    dplyr::filter(dplyr::if_any(dplyr::matches(var_nm_regex), ~ . %in% dbplyr::sql(dbplyr::escape_ansi(match_val, collapse = ",", parens = TRUE))))

  # explain the configuration in plain language to prompt user thinking
  if (verbose) {
    cat(
      "\nSearching conditions:\nEach client has at least", n_per_clnt, "of distinct", ifelse(is.null(collapse_by_nm), "record(s)", collapse_by_nm),
      "\n  where", ifelse(multi_var_cols, "at least one of the", "the single"), var_nm_pattern, "column",
      "\n    contains a value", match_msg, match_str,
      ifelse(n_per_clnt > 1 & !is.null(fuzzy_val), paste("\nExcluding clients which all their matched values are in set", deparse(fuzzy_val)), ""), "\n"
    )

    cat("\nList of all matched value(s):\n")
    print(match_val)
  }


  # check result length
  # n_row <- q_any_match %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n)
  # if (n_row == 0) {
  #   warning("No match found. Check val_vector.")
  # } else if (verbose) cat("\nNumber of clients that has any matches:", q_any_match %>% dplyr::summarise(n = dplyr::n_distinct({{clnt_id_nm}})) %>% dplyr::pull(n), "\n")

  # records cannot be all fuzzy within person
  # because keeping original record per row, values in var cols may contain those outside the matched set, so all val == fuzzy would not give correct answer, e.g., fuzzy plus non-match would not be excluded
  # therefore, the matched set (match_val) was made in previous steps and used here to ensure non-match won't affect the result but still be kept as is in original records.
  if (!is.null(fuzzy_val)) {
    match_val_wo_fuzzy <- setdiff(match_val, fuzzy_val)

    q_any_match <- q_any_match %>%
      dplyr::filter(dplyr::if_any(dplyr::matches(var_nm_regex), ~ . %in% dbplyr::sql(dbplyr::escape_ansi(match_val_wo_fuzzy, collapse = ",", parens = TRUE))))

    #   n_row_wo_fuzzy <- q_any_match %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n)
    #
    #   if (n_row != 0 & n_row_wo_fuzzy == 0) {
    #     warning("Some matches found but all excluded by fuzzy_val.")
    #     n_row <- n_row_wo_fuzzy
    #   } else if (verbose) cat("\nNumber of clients with fuzz_val exclusion:", q_any_match %>% dplyr::summarise(n = dplyr::n_distinct({{clnt_id_nm}})) %>% dplyr::pull(n), "\n")
  }

  # job done if getting any records
  if (n_per_clnt == 1) {
    if (!query_only) q_any_match <- q_any_match %>% dplyr::collect()
    return(q_any_match)
  }

  # keep records for person with number of records >= n_per_clnt
  else {
    # count differently if unit id is supplied by collaspe_by_nm
    if (!is.null(collapse_by_nm)) {
      q_n <- q_any_match %>%
        dplyr::group_by(.data[[clnt_id_nm]]) %>%
        dbplyr::window_order(.data[[collapse_by_nm]]) %>%
        dplyr::mutate(
          n_collapsed_id = dplyr::dense_rank(.data[[collapse_by_nm]]),
          n_collapsed = max(n_collapsed_id, na.rm = TRUE)
        ) %>%
        dplyr::filter(n_collapsed >= n_per_clnt) %>%
        dplyr::select(-dplyr::starts_with("n_collapsed"))
    } else {
      q_n <- q_any_match %>%
        dplyr::group_by(.data[[clnt_id_nm]]) %>%
        dplyr::filter(dplyr::n() >= {{ n_per_clnt }})
    }

    # n_row_wo_fuzzy_n <- q_n %>% ungroup() %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n)
    #
    # if (n_row != 0 & n_row_wo_fuzzy_n == 0) {
    #   warning("Some matches found but all excluded by counting n_per_clnt.")
    # } else if (verbose) cat("\nNumber of clients satisfied all conditions:", q_n %>% ungroup() %>% dplyr::summarise(n = dplyr::n_distinct({{clnt_id_nm}})) %>% dplyr::pull(n), "\n")

    if (verbose) {
      cat(
        "\nTo see the final query generated by 'dbplyr', use dplyr::show_query() on the output\n",
        "\nTo get the SQL string, use dbplyr::remote_query(), then as.character() if needed."
      )
    }


    if (!query_only) q_n <- q_n %>% dplyr::collect()
    return(q_n)
  }
}
