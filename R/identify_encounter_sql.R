#' @export
identify_encounter.tbl_sql <- function(data, from_var, match_vals, match_type = c("in", "start", "regex", "like", "between", "glue_sql"), if_all = FALSE, verbose = TRUE, query_only = TRUE, ...) {
  # input checks
  rlang::arg_match0(match_type, c("in", "start", "regex", "between", "like", "glue_sql"))

  # get variable names as text with tidyselect and NSE
  db_head <- data %>%
    dplyr::select({{ from_var }}) %>%
    utils::head(n = 1) %>%
    dplyr::collect()
  var_nm <- names(db_head)

  if (match_type %in% c("in", "between")) {
    var_class <- purrr::map_chr(db_head %>% dplyr::select(dplyr::all_of(var_nm)), class)
    if (!any(class(match_vals) %in% var_class)) warning("`match_vals` (", class(match_vals), ") is not the same type as the `from_var` columns (", paste(var_class, collapse = ", "), ").")
  }

  # make match_str/msg for verbose
  switch(match_type,
    "start" = {
      match_str <- paste0(match_vals, "%", collapse = " OR ")
      match_msg <- "satisfied SQL LIKE pattern:"
      match_vals <- paste0(match_vals, "%")
    },
    "like" = {
      match_str <- paste0(match_vals, collapse = " OR ")
      match_msg <- "satisfied SQL LIKE pattern:"
    },
    "regex" = {
      match_str <- paste0(match_vals, collapse = "|")
      match_msg <- "satisfied regular expression:"
    },
    "in" = {
      match_str <- deparse(substitute(match_vals))
      match_msg <- "exactly matched values in set:"
    },
    "between" = {
      match_str <- deparse(substitute(match_vals))
      match_msg <- "between range (bounds included):"
    },
    "glue_sql" = {
      match_str <- glue::glue_sql_collapse(glue::glue_sql(match_vals, ..., .con = dbplyr::remote_con(data)), sep = ifelse(if_all, " AND ", " OR "))
      match_msg <- "satisfied SQL WHERE clause:"
    }
  )

  if (match_type == "regex") {
    # Note that SQL does not support regular expression. The match is done by collecting all distinct possible values locally then using regex in R. The result is plugged into subseqent query as: WHERE var_nm IN (match_result).
    all_val <- data %>%
      tidyr::pivot_longer(
        cols = var_nm,
        names_to = "position",
        values_to = "temp_val"
      ) %>%
      dplyr::select(dplyr::all_of("temp_val")) %>%
      dplyr::distinct() %>%
      dplyr::pull("temp_val")
    match_vals <- all_val[data.table::like(all_val, match_str)]
    if (length(match_vals) > 1000) warning("More than 1,000 distinct values were matched by the regular expression. The query including such long list may fail to run. Try using SQL LIKE expression with match_type = 'like'.")
  }

  # run different filter by match type
  # the action expressions were captured for replacing if_any if needed
  # altering expression approach is necessary because if_any/all cannot simply be replaced by different name because dbplyr would fail to translate
  switch(match_type,
    # left start empty so getting action from like
    "start" = ,
    "like" = act_expr <- rlang::expr({
      like_list <- lapply(match_vals, function(x) data %>% dplyr::filter(dplyr::if_any(var_nm, ~ stringr::str_like(., dbplyr::sql(dbplyr::escape_ansi(x))))))
      q_match <- Reduce(dplyr::union, like_list)
    }),
    "regex" = ,
    "in" = act_expr <- rlang::expr({
      q_match <- data %>%
        dplyr::filter(dplyr::if_any(var_nm, ~ . %in% dbplyr::sql(dbplyr::escape_ansi(match_vals, collapse = ",", parens = TRUE))))
    }),
    "between" = act_expr <- rlang::expr({
      stopifnot(
        length(match_vals) == 2,
        match_vals[1] >= match_vals[2]
      )
      q_match <- data %>% dplyr::filter(dplyr::if_any(var_nm, ~ dplyr::between(., dbplyr::escape_ansi(match_vals[1]), dbplyr::escape_ansi(match_vals[2]))))
    }),
    "glue_sql" = act_expr <- rlang::expr({
      q_match <- data %>% dplyr::filter(dbplyr::sql(match_str))
    })
  )

  if (if_all) {
    act_expr <- rlang::expr_text(act_expr) %>%
      stringr::str_replace("if_any", "if_all") %>%
      rlang::parse_expr()
  }

  eval(act_expr)

  # explain the configuration in plain language to prompt user thinking
  if (verbose) {
    # get all values in data
    matched_vals <- lapply(var_nm, function(x) {
      dplyr::select(q_match, dplyr::all_of(x)) %>%
        dplyr::distinct() %>%
        dplyr::pull()
    })
    # if ("Date" %in% var_class)
    matched_vals <- Reduce(dplyr::union, matched_vals)

    cat(
      "\nIdentify records with condition(s):",
      "\n - where", ifelse(if_all & length(var_nm) > 1, "all of the", ifelse(length(var_nm) > 1, "at least one of the", "the")), paste0(var_nm, collapse = ", "), "column(s) in each record",
      "\n   - contains a value", match_msg, match_str, "\n"
    )

    cat(ifelse(is.numeric(matched_vals), "\nRange of values in the result", "\nAll unique value(s) in the result"), ifelse(!if_all & length(var_nm) > 1, "(as the conditions require just one of the columns containing target values; irrelevant values may come from other columns):", ":"), "\n")

    switch(match_type,
      "between" = print(range(matched_vals)),
      if (is.numeric(matched_vals)) print(range(matched_vals)) else print(matched_vals, max = 100)
    )
  }

  # job done

  if (!query_only) {
    q_match <- q_match %>% dplyr::collect(cte = TRUE)
    # convert dates
  } else if (verbose) {
    cat(
      "\nTo see the final query generated by 'dbplyr', use dplyr::show_query() on the output.",
      "\nTo get the SQL string, use dbplyr::remote_query(), then as.character() if needed.\n"
    )
  }

  return(q_match)
}
