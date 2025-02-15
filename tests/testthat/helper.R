letters_n <- function(nrows = 25, type = "data.frame", id = 1:50) {
  n_any <- nrows %/% 2
  n_all <- n_any %/% 3
  make_test_dat(vals_kept = letters, nrows = nrows, n_any = n_any, n_all = n_all, answer_id = "ans", type = type, IDs = id)
}

xnum_n <- function(x, nrows = 25, type = "data.frame") {
  n_any <- nrows %/% 2
  n_all <- n_any %/% 3
  make_test_dat(vals_kept = paste0(x, 1:9), nrows = nrows, n_any = n_any, n_all = n_all, answer_id = "ans", type = type)
}

btw_n <- function(date_range, n_ans = 5, type = "data.frame") {
  keep <- make_test_dat(vals_kept = letters, nrows = n_ans, n_any = n_ans, n_all = n_ans, answer_id = "ans", date_range = date_range)
  out <- letters_n()
  all <- dplyr::bind_rows(keep, out)

  if (type == "database") {
    all <- memdb_tbl(all)
  }
  return(all)
}

iclnt_jdates <- function(i, j, dup, date_range = c(as.Date("2015-01-01"), as.Date("2021-01-31")), type = "data.frame") {
  dat <- purrr::map2(i, j, ~ tidyr::expand_grid(clnt_id = .x, dates = seq(date_range[1], date_range[2], length.out = .y)))
  dat <- append(dat, purrr::map2(i, dup, ~ dplyr::tibble(clnt_id = rep(.x, each = .y), dates = date_range[1])))

  test_dat <- purrr::list_rbind(dat)

  if (type == "database") {
    test_dat <- memdb_tbl(test_dat)
  }

  return(test_dat)
}

# internal test function that should be ran on make_test_dat() output
test_apart_within <- function(data, n, apart = 0, within = Inf) {
  data <- data %>%
    dplyr::group_by(.data[["clnt_id"]]) %>%
    dplyr::filter(dplyr::n_distinct(.data[["dates"]]) >= n)

  keep <- data %>%
    dplyr::summarise(met = ifelse(dplyr::n() < n, FALSE,
      utils::combn(.data[["dates"]] %>% unique(), n, function(x) all(diff(sort(x)) >= apart) & (diff(c(min(x), max(x))) <= within)) %>% any()
    ))

  keep <- keep %>%
    dplyr::filter(met) %>%
    dplyr::pull(.data[["clnt_id"]])

  return(keep)
}

test_if_dates <- function(x, n, apart = 0, within = Inf, dup.rm = TRUE) {
  if (dup.rm) {
    utils::combn(x %>% unique(), n, function(x) all(diff(sort(x)) >= apart) & (diff(c(min(x), max(x))) <= within)) %>% any()
  } else {
    utils::combn(x, n, function(x) all(diff(sort(x)) >= apart) & (diff(c(min(x), max(x))) <= within)) %>% any()
  }
}

test_comorbidity <- function(n_row = 10, n_col = 31, n_clnt = 3, icd10 = TRUE) {
  # make answer df
  ans <- sample(0:1, 31, prob = c(0.8, 0.05), replace = TRUE)
  ans <- replicate(n_row, sample(0:1, 31, prob = c(0.8, 0.05), replace = TRUE)) %>%
    t() %>%
    as.data.frame()
  colnames(ans) <- c("chf", "arrhy", "vd", "pcd", "pvd", "hptn_nc", "hptn_c", "para", "othnd", "copd", "diab_nc", "diab_c", "hptothy", "rf", "ld", "pud_nb", "hiv", "lymp", "mets", "tumor", "rheum_a", "coag", "obesity", "wl", "fluid", "bla", "da", "alcohol", "drug", "psycho", "dep")

  # make data
  # make code pool and draw from it
  if (icd10) {
    code_list <- list(
      c(
        "I099", "I110", "I130", "I132", "I255", "I420", "I425", "I427", "I428",
        "I429", "I43", "I50", "P290"
      ),
      c(
        "I441", "I442", "I443", "I456", "I459", "I47", "I48", "I49", "R000", "R001",
        "R008", "T821", "Z450", "Z950"
      ),
      c(
        "A520", "I05", "I06", "I07", "I08", "I091", "I098", "I34", "I35", "I36", "I37",
        "I38", "I39", "Q230", "Q231", "Q232", "Q233", "Z952", "Z953", "Z954"
      ),
      c("I26", "I27", "I280", "I288", "I289"),
      c(
        "I70", "I71", "I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558",
        "K559", "Z958", "Z959"
      ),
      c("I10"),
      c("I11", "I12", "I13", "I15"),
      c(
        "G041", "G114", "G801", "G802", "G81", "G82", "G830", "G831", "G832", "G833",
        "G834", "G839"
      ),
      c(
        "G10", "G11", "G12", "G13", "G20", "G21", "G22", "G254", "G255", "G312", "G318",
        "G319", "G32", "G35", "G36", "G37", "G40", "G41", "G931", "G934", "R470", "R56"
      ),
      c(
        "I278", "I279", "J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", "J60", "J61",
        "J62", "J63", "J64", "J65", "J66", "J67", "J684", "J701", "J703"
      ),
      c(
        "E100", "E101", "E109", "E110", "E111", "E119", "E120", "E121", "E129", "E130",
        "E131", "E139", "E140", "E141", "E149"
      ),
      c(
        "E102", "E103", "E104", "E105", "E106", "E107", "E108", "E112", "E113", "E114", "E115",
        "E116", "E117", "E118", "E122", "E123", "E124", "E125", "E126", "E127", "E128", "E132",
        "E133", "E134", "E135", "E136", "E137", "E138", "E142", "E143", "E144", "E145", "E146",
        "E147", "E148"
      ), #
      c("E00", "E01", "E02", "E03", "E890"),
      c("I120", "I131", "N18", "N19", "N250", "Z490", "Z491", "Z492", "Z940", "Z992"),
      c(
        "B18", "I85", "I864", "I982", "K70", "K711", "K713", "K714", "K715", "K717", "K72", "K73",
        "K74", "K760", "K762", "K763", "K764", "K765", "K766", "K767", "K768", "K769", "Z944"
      ),
      c("K257", "K259", "K267", "K269", "K277", "K279", "K287", "K289"),
      c("B20", "B21", "B22", "B24"),
      c("C81", "C82", "C83", "C84", "C85", "C88", "C96", "C900", "C902"),
      c("C77", "C78", "C79", "C80"),
      c(
        "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13",
        "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C30",
        "C31", "C32", "C33", "C34", "C37", "C38", "C39", "C40", "C41", "C43", "C45", "C46", "C47", "C48",
        "C49", "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", "C60", "C61", "C62", "C63",
        "C64", "C65", "C66", "C67", "C68", "C69", "C70", "C71", "C72", "C73", "C74", "C75", "C76", "C97"
      ),
      c(
        "L940", "L941", "L943", "M05", "M06", "M08", "M120", "M123", "M30", "M310", "M311", "M312", "M313",
        "M32", "M33", "M34", "M35", "M45", "M461", "M468", "M469"
      ),
      c("D65", "D66", "D67", "D68", "D691", "D693", "D694", "D695", "D696"),
      c("E66"),
      c("E40", "E41", "E42", "E43", "E44", "E45", "E46", "R634", "R64"),
      c("E222", "E86", "E87"), #
      c("D500"),
      c("D508", "D509", "D51", "D52", "D53"),
      c("F10", "E52", "G621", "K292", "K700", "K703", "K709", "T51", "Z502", "Z714", "Z721"),
      c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19", "Z715", "Z722"),
      c("F20", "F22", "F23", "F24", "F25", "F28", "F29", "F302", "F312"),
      c("F204", "F313", "F314", "F32", "F33", "F341", "F412", "F432")
    )
  } else {
    code_list <- list(
      c(
        "39891", "40201", "40211", "40291", "40401", "40411", "40491", "4254", "4257", "4258", "4259", "428"
      ),
      c(
        "4260", "42613", "4267", "4269", "42610", "42612", "4270", "4271", "4272", "4273",
        "4274", "4276", "4278", "4279", "7850", "99601", "99604", "V450", "V533"
      ),
      c("0932", "394", "395", "396", "397", "424", "7463", "7464", "7465", "7466", "V422", "V433"),
      c("4150", "4151", "416", "4170", "4178", "4179"),
      c("0930", "4373", "440", "441", "4431", "4432", "4438", "4439", "4471", "5571", "5579", "V434"),
      c("401"),
      c("402", "403", "404", "405"),
      c("3341", "342", "343", "3440", "3441", "3442", "3443", "3444", "3445", "3446", "3449"),
      c(
        "3319", "3320", "3321", "3334", "3335", "33392", "334", "335", "3362", "340", "341",
        "345", "3481", "3483", "7803", "7843"
      ),
      c(
        "4168", "4169", "490", "491", "492", "493", "494", "495", "496", "500", "501", "502",
        "503", "504", "505", "5064", "5081", "5088"
      ),
      c("2500", "2501", "2502", "2503"),
      c("2504", "2505", "2506", "2507", "2508", "2509"), #
      c("2409", "243", "244", "2461", "2468"),
      c(
        "40301", "40311", "40391", "40402", "40412", "40492",
        "585", "586", "5880", "V420", "V451", "V56"
      ),
      c(
        "07022", "07023", "07032", "07033", "07044", "07054", "0706", "0709", "4560", "4561",
        "4562", "570", "571", "5722", "5723", "5724", "5728", "5733", "5734", "5738", "5739", "V427"
      ),
      c("5317", "5319", "5327", "5329", "5337", "5339", "5347", "5349"),
      c("042", "043", "044"),
      c("200", "201", "202", "2030", "2386"),
      c("196", "197", "198", "199"),
      c("140", "141", "142", "143", "144", "145", "146", "147", "148", "149", "150", "151", "152", "153", "154", "155", "156", "157", "158", "159", "160", "161", "162", "163", "164", "165", "166", "167", "168", "169", "170", "171", "172", "174", "175", "176", "177", "178", "179", "180", "181", "182", "183", "184", "185", "186", "187", "188", "189", "190", "191", "192", "193", "194", "195"),
      c(
        "446", "7010", "7100", "7101", "7102", "7103", "7104", "7108", "7109", "7112", "714",
        "7193", "720", "725", "7285", "72889", "72930"
      ),
      c("286", "2871", "2873", "2874", "2875"),
      c("2780"),
      c("260", "261", "262", "263", "7832", "7994"),
      c("2536", "276"),
      c("2800"), #
      c("2801", "2808", "2809", "281"),
      c(
        "2652", "2911", "2912", "2913", "2915", "2918", "2919", "3030", "3039", "3050",
        "3575", "5353", "5710", "5711", "5712", "5713", "980", "V113"
      ),
      c("292", "304", "3052", "3053", "3054", "3055", "3056", "3057", "3058", "3059", "V6542"),
      c("2938", "295", "29604", "29614", "29644", "29654", "297", "298"),
      c("2962", "2963", "2965", "3004", "309", "311")
    )
    # code_list <- list(
    #   c("398", "402", "425", "428"),
    #   c("426", "427"),
    #   c("394", "395", "396", "397", "424", "746"),
    #   c("415", "416", "417"),
    #   c("440", "441", "443", "447", "557"),
    #   c("401"),
    #   c("402", "403", "404", "405"),
    #   c("334", "342", "343", "344"),
    #   c("331", "332", "333", "334", "335", "336", "340", "341", "345", "348"),
    #   c("416", "490", "491", "492", "493", "494", "495", "496", "500", "501", "502", "503", "504", "505"),
    #   c("250"),
    #   c("250"),#
    #   c("240", "243", "244", "246"),
    #   c("403", "585", "586", "588", "V56"),
    #   c("070", "456", "570", "571", "572", "573"),
    #   c("531", "532", "533", "534"),
    #   c("042", "043", "044"),
    #   c("200", "201", "202", "203"),
    #   c("196", "197", "198", "199"),
    #   c("140", "141", "142", "143", "144", "145", "146", "147", "148", "149", "150", "151", "152", "153", "154", "155", "156", "157", "158", "159", "160", "161", "162", "163", "164", "165", "166", "167", "168", "169", "170", "171", "172", "174", "175", "176", "177", "178", "179", "180", "181", "182", "183", "184", "185", "186", "187", "188", "189", "190", "191", "192", "193", "194", "195"),
    #   c("446", "701", "710", "711", "714", "719", "720", "725", "728"),
    #   c("286", "287"),
    #   c("278"),
    #   c("260", "261", "262", "263"),
    #   c("276"),
    #   c("280", "281"),#
    #   c("280", "281"),
    #   c("291", "303", "980"),
    #   c("292", "304", "305"),
    #   c("293", "295", "297", "298"),
    #   c("296", "300", "309", "311")
    # )
  }


  drew_code <- purrr::map(1:n_row, function(x) purrr::map_chr(code_list[ans[x, ] == 1], ~ sample(., 1)))
  fill_code <- purrr::map(drew_code, ~ c(., rep(NA, n_col - length(.))))
  code_df <- purrr::list_c(fill_code) %>%
    matrix(nrow = n_row, ncol = n_col, byrow = TRUE) %>%
    as.data.frame()
  colnames(code_df) <- paste("diagx", 1:n_col, sep = "_")
  code_df <- code_df %>%
    dplyr::mutate(
      uid = 1:n_row,
      clnt_id = sample(1:n_clnt, n_row, replace = TRUE),
      .before = dplyr::everything()
    )

  # add total score after using ans for sampling
  ans <- ans %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_eci = sum(dplyr::c_across(dplyr::everything()))) %>%
    dplyr::ungroup()

  list(answer = ans, data = code_df)
}

episode_df <- function(n, gap, type = "data.frame") {
  answer <- data.frame(clnt = 1:n, n_epi = sample(1:5, n, replace = TRUE))
  answer <- answer %>%
    dplyr::mutate(
      epi_start = sample(seq(as.Date("2018-01-01"), as.Date("2020-12-31"), by = 1), size = n, replace = TRUE),
      epi_end = epi_start + lubridate::days(sample(365:(3 * 365), size = n, replace = TRUE))
    )
  answer <- cut_period(answer, epi_start, epi_end, gap)
  # no. gaps = n_epi - 1
  answer_with_gap <- answer %>%
    dplyr::group_by(clnt) %>%
    dplyr::slice(setdiff(dplyr::row_number(), which(1:(dplyr::n() - 1) %% 2 == 0) %>% sample(dplyr::first(n_epi) - 1)))
  # make some noise within epi that should be collapsed
  noise <- answer_with_gap %>%
    dplyr::slice_sample(prop = 0.2) %>%
    dplyr::mutate(
      noise_start = purrr::map2(segment_start, segment_end, ~ sample(seq(.x, .y, by = 1), sample(1:3, 1), replace = TRUE)),
      noise_end = purrr::map2(noise_start, segment_end, ~ purrr::map_vec(.x, function(i) i + lubridate::days(sample(0:lubridate::time_length(.y - i, unit = "day"), 1))))
    ) %>%
    tidyr::unnest(cols = c(noise_start, noise_end)) %>%
    dplyr::select(-c(segment_start, segment_end)) %>%
    dplyr::rename(
      segment_start = noise_start,
      segment_end = noise_end
    )
  data <- dplyr::bind_rows(answer_with_gap, noise) %>% dplyr::arrange(clnt, segment_start)

  if (type != "data.frame") {
    data <- memdb_tbl(data)
  }

  return(list(answer, data))
}
