compute_age <- function(bday, eday, trans = FALSE, .transfn = lubridate::ymd) {
  if (trans) lubridate::interval(.transfn(bday), .transfn(eday)) / lubridate::years(1)
  else lubridate::interval(bday, eday) / lubridate::years(1)
}

cut_age <- function(age, lower_brks = c(0, 19, 25, 35, 45, 55), ...) {
  labs <- sapply(1:length(lower_brks),
         function(i) dplyr::case_when(i == 1 ~ paste0("<", lower_brks[i+1]),
                               i == length(lower_brks) ~ paste0(lower_brks[i], "+"),
                               .default = paste(lower_brks[i], lower_brks[i+1]-1, sep = "-")))
  cut(age, breaks = c(lower_brks, Inf), labels = labs, right = FALSE, ...)
}
