compute_age <- function(bday, eday, trans = FALSE, .transfn = lubridate::ymd, verbose = TRUE) {
  #compute age with lubridate functions (more accurate than /365.25) and built-in transformations
  if (trans) age <- lubridate::interval(.transfn(bday), .transfn(eday)) / lubridate::years(1)
  else age <- lubridate::interval(bday, eday) / lubridate::years(1)

  #warnings for unreasonable values
  if (verbose)
  {
    if(is.na(min(age))) warning("Age contains missing value(s)")
    if(min(age, na.rm = TRUE) < 0) warning("Age contains negative value(s)")
    if(max(age, na.rm = TRUE) > 100) warning("Age contains value(s) > 100")
  }

  return(age)
}

cut_age <- function(age, lower_brks = c(0, 19, 25, 35, 45, 55), verbose = TRUE, ...) {
  #create labels based on breaks
  labs <- sapply(1:length(lower_brks),
         function(i) dplyr::case_when(i == 1 ~ paste0("<", lower_brks[i+1]),
                               i == length(lower_brks) ~ paste0(lower_brks[i], "+"),
                               .default = paste(lower_brks[i], lower_brks[i+1]-1, sep = "-")))

  #make age group with the lower boundary of breaks
  agegrp <- cut(age, breaks = c(lower_brks, Inf), labels = labs, right = FALSE, ...)

  #warning if the breaks don't cover the full range
  if (verbose) {
    if (sum(is.na(agegrp)) > sum(is.na(age))) warning("More NA than raw age. Breaks do not cover the full range.")
  }

  return(agegrp)
}
