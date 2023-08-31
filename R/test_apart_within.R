# internal test function that should be ran on make_test_dat() output
test_apart_within <- function(data, n, apart = 0, within = Inf) {
  data <- data %>% dplyr::filter(dplyr::n_distinct(.data[["dates"]]) >= n, .by = .data[["clnt_id"]])

  keep <- data %>%
    dplyr::group_by(.data[["clnt_id"]]) %>%
    dplyr::summarise(keep = utils::combn(.data[["dates"]] %>% unique(), n, function(x) all(diff(sort(x)) >= apart) & (diff(c(min(x), max(x))) <= within)) %>% any())

  keep <- keep %>%
    dplyr::filter(keep) %>%
    dplyr::pull(.data[["clnt_id"]])

  return(keep)
}
