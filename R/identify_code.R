identify_code <- function(dat, source_nm, diag_cd_prefix, start_dt_nm, end_dt_nm = NULL, person_id_nm = "moh_study_id",
                          codes, bd_source = NULL, bd_col_nm = NULL, .dt_transfn = NULL) {

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
    dplyr::rename_with(.fn = ~ gsub(diag_cd_prefix, "diag_cd", .), .col = dplyr::starts_with(diag_cd_prefix)) %>%
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
