#internal test function that should be ran on make_test_dat() output
test_apart_within <- function(data, dt = dates, n, apart = 0, within = Inf) {
  data <- data %>% filter(n_distinct({{dt}}) >= n, .by = clnt_id)

  keep <- data %>% group_by(clnt_id) %>%
    summarise(keep = combn(dates %>% unique(), n, function(x) all(diff(sort(x)) >= apart) & (diff(c(min(x), max(x))) <= within)) %>% any())

  keep <- keep %>% filter(keep) %>% pull(clnt_id)

  return(keep)
}
