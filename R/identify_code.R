identify_code <- function(dat, clnt_id_nm = "moh_study_id", code_col_prefix, codes_vec, match_type = c("start", "exact", "regex"), n_per_clnt = 1, fuzzy_cd = NULL, collaspe_by_nm = NULL, trans_longer = FALSE) {
  #stop if conflict
  if(all(n_per_clnt == 1, !is.null(fuzzy_cd))) stop("Fuzzy code should not be supplied if only one record per client is required. If appropriate, include it in codes_vec instead.")

  #add fuzzy_cd into code_vec so that only one filter is engouh afterward
  codes_vec <- c(codes_vec, fuzzy_cd) %>% unique()

  #use data.table and dtplyr to speed up the performance
  dat <- dtplyr::lazy_dt(dat)

  #pivot longer to simplify logic of interpreting n per client and filtering
  if (trans_longer) {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with(code_col_prefix),
        names_to = "cd_position",
        names_prefix = code_col_prefix,
        values_to = code_col_prefix,
        values_drop_na = TRUE
      ) #%>%
    #dplyr::as_tibble()
  }

  #code filter by match types
  if (match_type == "start")
  {
    dat <- dplyr::filter(dat, stringr::str_detect(.data[[code_col_prefix]], paste("^", codes_vec, collapse = "|", sep = "")))
  }
  if (match_type == "regex")
  {
    dat <- dplyr::filter(dat, stringr::str_detect(.data[[code_col_prefix]], paste(codes_vec, collapse = "|", sep = "")))
  }
  if (match_type == "exact")
  {
    dat <- dplyr::filter(dat, .data[[code_col_prefix]] %in% codes_vec)
  }

  #save intermediate data.table result
  dat <- as_tibble(dat)

  #job done if getting any records
  if (n_per_clnt == 1) return(dat)
  #keep records for person with number of records >= n_per_clnt
  else
  {
    dat <- dat %>%
      dplyr::group_by(.data[[clnt_id_nm]])

    #count differently if unit id is supplied by collaspe_by_nm
    if (!is.null(collaspe_by_nm)) dat <- dat %>% dplyr::filter(dplyr::n_distinct(.data[[collaspe_by_nm]]) >= n_per_clnt)
    else dat <- dat %>% dplyr::filter(dplyr::n() >= n_per_clnt)

    #records cannot be all fuzzy within person
    if (!is.null(fuzzy_cd)) dat <- dat %>% dplyr::filter(!all(.data[[code_col_prefix]] %in% fuzzy_cd))

    #clean up grouping before output
    dat %>% dplyr::ungroup()

  }

}

compare_dates <- function(dat, source_nm, code_col_prefix, code, start_dt_nm, end_dt_nm = NULL, person_id_nm = "moh_study_id", bd_source = NULL, bd_col_nm = NULL, .dt_transfn = NULL) {

  #get age
  if (!is.null(bd_source))
  {
    #loop over the list to apply dplyr::filter for each source
    dat <- dat %>% dplyr::left_join(bd_source %>% dplyr::select({{person_id_nm}}, {{bd_col_nm}}), by = person_id_nm)
  }

  #prep data unifying variable names

  dat <- dat %>%
    dplyr::rename(dplyr::any_of(
      c(source_id = paste0(source_nm, "_id"),
        person_id = person_id_nm,
        start_dt = start_dt_nm,
        end_dt = end_dt_nm,
        b_dt = bd_col_nm)
    )) %>%
    #remove empty diag columns
    dplyr::select(dplyr::where(~!all(is.na(.)))) %>%
    dplyr::rename_with(.fn = ~ gsub(code_col_prefix, "diag_cd", .), .col = dplyr::starts_with(code_col_prefix)) %>%
    dplyr::mutate(source = source_nm)

  #transform dates if needed
  if (!is.null(.dt_transfn))
  {
    dat <- dat %>% dplyr::mutate(dplyr::across(dplyr::ends_with("_dt"), .dt_transfn))
  }

  #compute age if bd_source is supplied
  if (all(!is.null(bd_source), !is.null(bd_col_nm)))
  {
    dat <- dat %>% dplyr::mutate(age = compute_age(b_dt, start_dt))
  }

}


# toy_dat <- data.frame(msp_id = 1:10, moh_study_id = sample(1000:9999, size = 10), start_dt_nm = sample(seq(ymd("2020-01-01"), ymd("2023-12-31"), by = 1), size = 10), diagx = sample(1000:9999, size = 10), diagx_1 = sample(1000:9999, size = 10), diagx_2 = NA, din_pin = sample(1000:9999, size = 10))

# toy_dat_t <- identify_code(toy_dat, source_nm = "msp", start_dt_nm = "start_dt_nm", end_dt_nm = NULL, diag_cd_prefix = "din_pin")

# toy_dat <- data.frame(moh_study_id = c(rep(1,4), rep(2,4)), cd_1 = c("304", "305", "50B", "123", rep("234", 4)), cd_2 = c(rep(NA, 4), "304", rep(NA,3)), date = c(rep("2020-01-01",2), "2020-01-02", "2020-01-03", paste0("2020-01-0", 1:4)))
