#' @export
restrict_dates.data.frame <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, uid = NULL, mode = c("flag", "filter"), align = c("left", "right"), dup.rm = TRUE, force_collect = FALSE, verbose = getOption("odcfun.verbose"), ...) {
  mode <- rlang::arg_match0(mode, c("flag", "filter"))
  align <- rlang::arg_match0(align, c("left", "right"))
  rlang::check_dots_used()

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))
  date_var <- rlang::as_name(rlang::enquo(date_var))

  # place holder for temp var names
  flag_restrict_dates <- temp.nm_keep <- temp.nm_keep_cum <- NULL

  # see if_dates for detail
  keep <- dplyr::collect(data) %>%
    dplyr::group_by(.data[[clnt_id]]) %>%
    dplyr::arrange(.data[[clnt_id]], .data[[date_var]]) %>%
    dplyr::mutate(temp.nm_keep = if_dates(.data[[date_var]], n, apart, within, detail = TRUE, align, dup.rm, ...),
                  flag_restrict_dates = as.numeric(temp.nm_keep))

  # if (strict_start) {
  #   keep <- keep %>%
  #     dplyr::mutate(temp.nm_keep_cum = cummax(temp.nm_keep))
  #   switch(mode,
  #     "flag" = {
  #       keep <- keep %>%
  #         dplyr::mutate(flag_restrict_dates = temp.nm_keep_cum)
  #     },
  #     "filter" = {
  #       keep <- keep %>% dplyr::filter(temp.nm_keep_cum %>% as.logical())
  #     }
  #   )
  # } else {
  #   switch(mode,
  #     "flag" = {
  #       keep <- keep %>%
  #         dplyr::mutate(flag_restrict_dates = as.numeric(temp.nm_keep))
  #     },
  #     "filter" = {
  #       keep <- keep %>% dplyr::filter(temp.nm_keep)
  #     }
  #   )
  # }
  n_kept <- keep %>% dplyr::filter(flag_restrict_dates == 1) %>% dplyr::n_groups()

  if (mode == "filter") {
    keep <- keep %>% dplyr::filter(max(flag_restrict_dates) > 0)
  }

  keep <- keep %>%
    dplyr::select(-dplyr::starts_with("temp.nm_")) %>%
    dplyr::ungroup()

  if (verbose) {
    initial_n <- report_n(data, on = {{ clnt_id }})
    cat("\n Of the", initial_n, "clients in the input,", initial_n - n_kept, "were", ifelse(mode == "filter", "excluded", "flagged"), "by restricting that each client must have", n, "records that were", ifelse(!is.null(apart), paste("at least", apart, "days apart"), ""), "within", within, "days."
        # , ifelse(strict_start, "Records before the earliest entries that met the condition are removed.", "")
        , "\n")
  }

  return(keep)
}
