identify_encounter <- function(data, clnt_id_nm, var_nm_pattern, val_vector, match_type = "in", n_per_clnt = 1, collapse_by_nm = NULL, multi_var_cols = FALSE, verbose = TRUE, ...) {
  UseMethod("identify_encounter")
}
