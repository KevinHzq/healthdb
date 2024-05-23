#' Compute Elixhauser Comorbidity Index
#'
#' @md
#' @description
#' This function computes unweighted Elixhauser Comorbidity Index for both data.frame and remote table input. The ICD codes used to identify the 31 disease categories is from Quan et al. (2005).
#'
#' @inheritParams define_case
#' @param icd_ver One of `c("ICD-10", "ICD-9-CM-3digits", "ICD-9-CM-5digits")`. Specify the ICD code version used in `data`. The ICD-10 and ICD-9-CM 5 digits version are from Quan et al. (2005). The ICD-9-CM 3 digits version is adopted from Manitoba Centre for Health Policy. It uses BOTH 3-digit and 5-digit codes in search. See their web page for cautions and limitations of the 3 digit version if your data only has 3-digit codes (\url{http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?printer=Y&conceptID=1436#CAUTIONS}).
#' @param sum_by One of "row" or "clnt". The "row" option computes total score for each row (default), and the "clnt" option summarizes total score by `clnt_id`. Each disease categories will be counted only once in the calculation regardless of multiple records in a category.
#' @param excl A character vector of disease categories labels that should be excluded in the total score calculation. This is useful when some of the categories are the exposure/outcome of interest, and the goal is to measure comorbidity excluding these disease. See detail for a list of the categories and labels.
#'
#' @details
#' List of disease categories - labels (in quote):
#' \enumerate{
#'  \item Congestive Heart Failure - "chf"
#'  \item Cardiac Arrhythmia - "arrhy"
#'  \item Valvular Disease - "vd"
#'  \item Pulmonary Circulation Disorders - "pcd"
#'  \item Peripheral Vascular Disorders - "pvd"
#'  \item Hypertension Uncomplicated - "hptn_nc"
#'  \item Hypertension complicated - "hptn_C"
#'  \item Paralysis - "para"
#'  \item Other Neurological Disorders - "Othnd"
#'  \item Chronic Pulmonary Disease - "copd"
#'  \item Diabetes Uncomplicated - "diab_nc"
#'  \item Diabetes Complicated - "diab_c"
#'  \item Hypothyroidism - "hptothy"
#'  \item Renal Failure - "rf"
#'  \item Liver Disease - "ld"
#'  \item Peptic Ulcer Disease excluding bleeding - "pud_nb"
#'  \item AIDS/HIV - "hiv"
#'  \item Lymphoma - "lymp"
#'  \item Metastatic Cancer - "mets"
#'  \item Solid Tumor without Metastasis - "tumor"
#'  \item Rheumatoid Arthritis/collagen - "rheum_a"
#'  \item Coagulopathy - "coag"
#'  \item Obesity - "obesity"
#'  \item Weight Loss - "wl"
#'  \item Fluid and Electrolyte Disorders - "fluid"
#'  \item Blood Loss Anemia - "bla"
#'  \item Deficiency Anemia - "da"
#'  \item Alcohol Abuse - "alcohol"
#'  \item Drug Abuse - "drug"
#'  \item Psychoses - "psycho"
#'  \item Depression - "dep"
#' }
#'
#'
#' @return A data.frame or remote table with binary indicators for each categories as columns.
#' @export
#'
#' @references Quan H, Sundararajan V, Halfon P, Fong A, Burnand B, Luthi JC, Saunders LD, Beck CA, Feasby TE, Ghali WA. Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Med Care 2005;43(11):1130-1139.
#' @examples
#' # make ICD-9 toy data
#' df <- data.frame(
#'   uid = 1:10, clnt_id = sample(1:3, 10, replace = TRUE),
#'   diagx_1 = c("193", "2780", "396", "4254", "4150", "401", "401", "0932", "5329", "2536"),
#'   diagx_2 = c(NA, NA, "72930", "V6542", "493", "405", "5880", "2409", "714", NA)
#' )
#'
#' # compute Elixhauser Comorbidity Index by row
#' # uid is needed for by row calculation
#' # 3 categories were excluded in total_eci
#' compute_comorbidity(df,
#'   vars = starts_with("diagx"),
#'   icd_ver = "ICD-9-CM-5digits",
#'   clnt_id = clnt_id, uid = uid,
#'   excl = c("drug", "psycho", "dep")
#' )
#'
#' # compute ECI by person
#' compute_comorbidity(df,
#'   vars = starts_with("diagx"),
#'   icd_ver = "ICD-9-CM-5digits",
#'   clnt_id = clnt_id,
#'   sum_by = "clnt"
#' )
compute_comorbidity <- function(data, vars, icd_ver = c("ICD-10", "ICD-9-CM-3digits", "ICD-9-CM-5digits"), clnt_id, uid = NULL, sum_by = c("row", "clnt"), excl = NULL) {
  # input check
  icd_ver <- rlang::arg_match(icd_ver)
  sum_by <- rlang::arg_match(sum_by)

  # get variable names as text with tidyselect and NSE
  db_head <- data %>%
    dplyr::select({{ vars }}) %>%
    utils::head(n = 1) %>%
    dplyr::collect()
  vars <- colnames(db_head)

  # capture variable names
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))

  has_uid <- !rlang::quo_is_null(rlang::enquo(uid))
  if (has_uid) uid <- rlang::as_name(rlang::enquo(uid))

  # pivot data to long form using variable names captured
  data_long <- data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "diag_no",
      values_to = "diag_cd"
    )

  # determine summary by row or clnt
  if (sum_by == "row") {
    if (!has_uid) stop("\nuid must be supplied for by row summary.")
    data_long <- data_long %>%
      dplyr::group_by(.data[[clnt_id]], .data[[uid]])
  } else {
    data_long <- data_long %>%
      dplyr::group_by(.data[[clnt_id]])
  }


  # map diagnosis code and Elixhauser catergories by ICD icd_vers
  if (icd_ver == "ICD-10") {
    data_long <- data_long %>%
      dplyr::mutate(
        # Congestive Heart Failure
        chf = dplyr::case_when(.data[["diag_cd"]] %in% c("I099", "I110", "I130", "I132", "I255", "I420", "I425", "I426", "I427", "I428", "I429", "I43", "I50", "P290") ~ 1L, .default = 0),
        # Cardiac Arrhythmia
        arrhy = dplyr::case_when(.data[["diag_cd"]] %in% c("I441", "I442", "I443", "I456", "I459", "I47", "I48", "I49", "R000", "R001", "R008", "T821", "Z450", "Z950") ~ 1L, .default = 0),
        # valvular Disease
        vd = dplyr::case_when(.data[["diag_cd"]] %in% c("A520", "I05", "I06", "I07", "I08", "I091", "I098", "I34", "I35", "I36", "I37", "I38", "I39", "Q230", "Q231", "Q232", "Q233", "Z952", "Z953", "Z954") ~ 1L, .default = 0),
        # Pulmonary Circulation Disorders
        pcd = dplyr::case_when(.data[["diag_cd"]] %in% c("I26", "I27", "I280", "I288", "I289") ~ 1L, .default = 0),
        # Peripheral Vascular Disorders
        pvd = dplyr::case_when(.data[["diag_cd"]] %in% c("I70", "I71", "I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558", "K559", "Z958", "Z959") ~ 1L, .default = 0),
        # Hypertension Uncomplicated
        hptn_nc = dplyr::case_when(.data[["diag_cd"]] %in% c("I10") ~ 1L, .default = 0),
        # Hypertension complicated
        hptn_c = dplyr::case_when(.data[["diag_cd"]] %in% c("I11", "I12", "I13", "I15") ~ 1L, .default = 0),
        # Paralysis
        para = dplyr::case_when(.data[["diag_cd"]] %in% c("G041", "G114", "G801", "G802", "G81", "G82", "G830", "G831", "G832", "G833", "G834", "G839") ~ 1L, .default = 0),
        # Other Neurological Disorders
        othnd = dplyr::case_when(.data[["diag_cd"]] %in% c("G10", "G11", "G12", "G13", "G20", "G21", "G22", "G254", "G255", "G312", "G318", "G319", "G32", "G35", "G36", "G37", "G40", "G41", "G931", "G934", "R470", "R56") ~ 1L, .default = 0),
        # Chronic Pulmonary Disease
        copd = dplyr::case_when(.data[["diag_cd"]] %in% c("I278", "I279", "J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", "J60", "J61", "J62", "J63", "J64", "J65", "J66", "J67", "J684", "J701", "J703") ~ 1L, .default = 0),
        # Diabetes Uncomplicated
        diab_nc = dplyr::case_when(.data[["diag_cd"]] %in% c("E100", "E101", "E109", "E110", "E111", "E119", "E120", "E121", "E129", "E130", "E131", "E139", "E140", "E141", "E149") ~ 1L, .default = 0),
        # Diabetes Complicated
        diab_c = dplyr::case_when(.data[["diag_cd"]] %in% c("E102", "E103", "E104", "E105", "E106", "E107", "E108", "E112", "E113", "E114", "E115", "E116", "E117", "E118", "E122", "E123", "E124", "E125", "E126", "E127", "E128", "E132", "E133", "E134", "E135", "E136", "E137", "E138", "E142", "E143", "E144", "E145", "E146", "E147", "E148") ~ 1L, .default = 0),
        # hypothyroidism
        hptothy = dplyr::case_when(.data[["diag_cd"]] %in% c("E00", "E01", "E02", "E03", "E890") ~ 1L, .default = 0),
        # Renal Failure
        rf = dplyr::case_when(.data[["diag_cd"]] %in% c("I120", "I131", "N18", "N19", "N250", "Z490", "Z491", "Z492", "Z940", "Z992") ~ 1L, .default = 0),
        # Liver Disease
        ld = dplyr::case_when(.data[["diag_cd"]] %in% c("B18", "I85", "I864", "I982", "K70", "K711", "K713", "K714", "K715", "K717", "K72", "K73", "K74", "K760", "K762", "K763", "K764", "K765", "K766", "K767", "K768", "K769", "Z944") ~ 1L, .default = 0),
        # Peptic Ulcer Disease excluding bleeding
        pud_nb = dplyr::case_when(.data[["diag_cd"]] %in% c("K257", "K259", "K267", "K269", "K277", "K279", "K287", "K289") ~ 1L, .default = 0),
        # AIDS/HIV
        hiv = dplyr::case_when(.data[["diag_cd"]] %in% c("B20", "B21", "B22", "B24") ~ 1L, .default = 0),
        # lymphoma
        lymp = dplyr::case_when(.data[["diag_cd"]] %in% c("C81", "C82", "C83", "C84", "C85", "C88", "C96", "C900", "C902") ~ 1L, .default = 0),
        # Metastatic Cancer
        mets = dplyr::case_when(.data[["diag_cd"]] %in% c("C77", "C78", "C79", "C80") ~ 1L, .default = 0),
        # Solid Tumor without Metastasis
        tumor = dplyr::case_when(.data[["diag_cd"]] %in% c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32", "C33", "C34", "C37", "C38", "C39", "C40", "C41", "C43", "C45", "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69", "C70", "C71", "C72", "C73", "C74", "C75", "C76", "C97") ~ 1L, .default = 0),
        # Rheumatoid Arthritis/collagen
        rheum_a = dplyr::case_when(.data[["diag_cd"]] %in% c("L940", "L941", "L943", "M05", "M06", "M08", "M120", "M123", "M30", "M310", "M311", "M312", "M313", "M32", "M33", "M34", "M35", "M45", "M461", "M468", "M469") ~ 1L, .default = 0),
        # Coagulopathy
        coag = dplyr::case_when(.data[["diag_cd"]] %in% c("D65", "D66", "D67", "D68", "D691", "D693", "D694", "D695", "D696") ~ 1L, .default = 0),
        # obesity
        obesity = dplyr::case_when(.data[["diag_cd"]] %in% c("E66") ~ 1L, .default = 0),
        # Weight Loss
        wl = dplyr::case_when(.data[["diag_cd"]] %in% c("E40", "E41", "E42", "E43", "E44", "E45", "E46", "R634", "R64") ~ 1L, .default = 0),
        # Fluid and Electrolyte Disorders
        fluid = dplyr::case_when(.data[["diag_cd"]] %in% c("E222", "E86", "E87") ~ 1L, .default = 0),
        # Blood Loss Anemia
        bla = dplyr::case_when(.data[["diag_cd"]] %in% c("D500") ~ 1L, .default = 0),
        # Deficiency Anemia
        da = dplyr::case_when(.data[["diag_cd"]] %in% c("D508", "D509", "D51", "D52", "D53") ~ 1L, .default = 0),
        # Alcohol Abuse
        alcohol = dplyr::case_when(.data[["diag_cd"]] %in% c("F10", "E52", "G621", "I426", "K292", "K700", "K703", "K709", "T51", "Z502", "Z714", "Z721") ~ 1L, .default = 0),
        # Drug Abuse
        drug = dplyr::case_when(.data[["diag_cd"]] %in% c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19", "Z715", "Z722") ~ 1L, .default = 0),
        # Psychoses
        psycho = dplyr::case_when(.data[["diag_cd"]] %in% c("F20", "F22", "F23", "F24", "F25", "F28", "F29", "F302", "F312", "F315") ~ 1L, .default = 0),
        # depression
        dep = dplyr::case_when(.data[["diag_cd"]] %in% c("F204", "F313", "F314", "F315", "F32", "F33", "F341", "F412", "F432") ~ 1L, .default = 0)
      )
  } else if (icd_ver == "ICD-9-CM-5digits") {
    data_long <- data_long %>%
      dplyr::mutate(
        # Congestive Heart Failure
        chf = dplyr::case_when(.data[["diag_cd"]] %in% c("39891", "40201", "40211", "40291", "40401", "40403", "40411", "40413", "40491", "40493", "4254", "4255", "4257", "4258", "4259", "428") ~ 1L, .default = 0),
        # Cardiac Arrhythmia
        arrhy = dplyr::case_when(.data[["diag_cd"]] %in% c("4260", "42613", "4267", "4269", "42610", "42612", "4270", "4271", "4272", "4273", "4274", "4276", "4278", "4279", "7850", "99601", "99604", "V450", "V533") ~ 1L, .default = 0),
        # valvular Disease
        vd = dplyr::case_when(.data[["diag_cd"]] %in% c("0932", "394", "395", "396", "397", "424", "7463", "7464", "7465", "7466", "V422", "V433") ~ 1L, .default = 0),
        # Pulmonary Circulation Disorders
        pcd = dplyr::case_when(.data[["diag_cd"]] %in% c("4150", "4151", "416", "4170", "4178", "4179") ~ 1L, .default = 0),
        # Peripheral Vascular Disorders
        pvd = dplyr::case_when(.data[["diag_cd"]] %in% c("0930", "4373", "440", "441", "4431", "4432", "4438", "4439", "4471", "5571", "5579", "V434") ~ 1L, .default = 0),
        # Hypertension Uncomplicated
        hptn_nc = dplyr::case_when(.data[["diag_cd"]] %in% c("401") ~ 1L, .default = 0),
        # Hypertension complicated
        hptn_c = dplyr::case_when(.data[["diag_cd"]] %in% c("402", "403", "404", "405") ~ 1L, .default = 0),
        # Paralysis
        para = dplyr::case_when(.data[["diag_cd"]] %in% c("3341", "342", "343", "3440", "3441", "3442", "3443", "3444", "3445", "3446", "3449") ~ 1L, .default = 0),
        # Other Neurological Disorders
        othnd = dplyr::case_when(.data[["diag_cd"]] %in% c("3319", "3320", "3321", "3334", "3335", "33392", "334", "335", "3362", "340", "341", "345", "3481", "3483", "7803", "7843") ~ 1L, .default = 0),
        # Chronic Pulmonary Disease
        copd = dplyr::case_when(.data[["diag_cd"]] %in% c("4168", "4169", "490", "491", "492", "493", "494", "495", "496", "500", "501", "502", "503", "504", "505", "5064", "5081", "5088") ~ 1L, .default = 0),
        # Diabetes Uncomplicated
        diab_nc = dplyr::case_when(.data[["diag_cd"]] %in% c("2500", "2501", "2502", "2503") ~ 1L, .default = 0),
        # Diabetes Complicated
        diab_c = dplyr::case_when(.data[["diag_cd"]] %in% c("2504", "2505", "2506", "2507", "2508", "2509") ~ 1L, .default = 0),
        # hypothyroidism
        hptothy = dplyr::case_when(.data[["diag_cd"]] %in% c("2409", "243", "244", "2461", "2468") ~ 1L, .default = 0),
        # Renal Failure
        rf = dplyr::case_when(.data[["diag_cd"]] %in% c("40301", "40311", "40391", "40402", "40403", "40412", "40413", "40492", "40493", "585", "586", "5880", "V420", "V451", "V56") ~ 1L, .default = 0),
        # Liver Disease
        ld = dplyr::case_when(.data[["diag_cd"]] %in% c("07022", "07023", "07032", "07033", "07044", "07054", "0706", "0709", "4560", "4561", "4562", "570", "571", "5722", "5723", "5724", "5728", "5733", "5734", "5738", "5739", "V427") ~ 1L, .default = 0),
        # Peptic Ulcer Disease excluding bleeding
        pud_nb = dplyr::case_when(.data[["diag_cd"]] %in% c("5317", "5319", "5327", "5329", "5337", "5339", "5347", "5349") ~ 1L, .default = 0),
        # AIDS/HIV
        hiv = dplyr::case_when(.data[["diag_cd"]] %in% c("042", "043", "044") ~ 1L, .default = 0),
        # Lymphoma
        lymp = dplyr::case_when(.data[["diag_cd"]] %in% c("200", "201", "202", "2030", "2386") ~ 1L, .default = 0),
        # Metastatic Cancer
        mets = dplyr::case_when(.data[["diag_cd"]] %in% c("196", "197", "198", "199") ~ 1L, .default = 0),
        # Solid Tumor without Metastasis
        tumor = dplyr::case_when(.data[["diag_cd"]] %in% c("140", "141", "142", "143", "144", "145", "146", "147", "148", "149", "150", "151", "152", "153", "154", "155", "156", "157", "158", "159", "160", "161", "162", "163", "164", "165", "166", "167", "168", "169", "170", "171", "172", "174", "175", "176", "177", "178", "179", "180", "181", "182", "183", "184", "185", "186", "187", "188", "189", "190", "191", "192", "193", "194", "195") ~ 1L, .default = 0),
        # Rheumatoid Arthritis/collagen
        rheum_a = dplyr::case_when(.data[["diag_cd"]] %in% c("446", "7010", "7100", "7101", "7102", "7103", "7104", "7108", "7109", "7112", "714", "7193", "720", "725", "7285", "72889", "72930") ~ 1L, .default = 0),
        # Coagulopathy
        coag = dplyr::case_when(.data[["diag_cd"]] %in% c("286", "2871", "2873", "2874", "2875") ~ 1L, .default = 0),
        # obesity
        obesity = dplyr::case_when(.data[["diag_cd"]] %in% c("2780") ~ 1L, .default = 0),
        # Weight Loss
        wl = dplyr::case_when(.data[["diag_cd"]] %in% c("260", "261", "262", "263", "7832", "7994") ~ 1L, .default = 0),
        # Fluid and Electrolyte Disorders
        fluid = dplyr::case_when(.data[["diag_cd"]] %in% c("2536", "276") ~ 1L, .default = 0),
        # Blood Loss Anemia
        bla = dplyr::case_when(.data[["diag_cd"]] %in% c("2800") ~ 1L, .default = 0),
        # Deficiency Anemia
        da = dplyr::case_when(.data[["diag_cd"]] %in% c("2801", "2808", "2809", "281") ~ 1L, .default = 0),
        # Alcohol Abuse
        alcohol = dplyr::case_when(.data[["diag_cd"]] %in% c("2652", "2911", "2912", "2913", "2915", "2918", "2919", "3030", "3039", "3050", "3575", "4255", "5353", "5710", "5711", "5712", "5713", "980", "V113") ~ 1L, .default = 0),
        # Drug Abuse
        drug = dplyr::case_when(.data[["diag_cd"]] %in% c("292", "304", "3052", "3053", "3054", "3055", "3056", "3057", "3058", "3059", "V6542") ~ 1L, .default = 0),
        # Psychoses
        psycho = dplyr::case_when(.data[["diag_cd"]] %in% c("2938", "295", "29604", "29614", "29644", "29654", "297", "298") ~ 1L, .default = 0),
        # depression
        dep = dplyr::case_when(.data[["diag_cd"]] %in% c("2962", "2963", "2965", "3004", "309", "311") ~ 1L, .default = 0)
      )
  } else {
    data_long <- data_long %>%
      dplyr::mutate(
        # ICD-9-CM-3digits matches at both 3 and all digits
        diag_3dig = stringr::str_sub(.data[["diag_cd"]], end = 3),
        # Congestive Heart Failure
        chf = dplyr::case_when(diag_3dig %in% c("398", "402", "425", "428") ~ 1L,
          .data[["diag_cd"]] %in% c("39891", "40201", "40211", "40291", "40401", "40403", "40411", "40413", "40491", "40493", "4254", "4255", "4257", "4258", "4259", "428") ~ 1L,
          .default = 0
        ),
        # Cardiac Arrhythmia
        arrhy = dplyr::case_when(diag_3dig %in% c("426", "427") ~ 1L,
          .data[["diag_cd"]] %in% c("4260", "42613", "4267", "4269", "42610", "42612", "4270", "4271", "4272", "4273", "4274", "4276", "4278", "4279", "7850", "99601", "99604", "V450", "V533") ~ 1L,
          .default = 0
        ),
        # Valvular Disease
        vd = dplyr::case_when(diag_3dig %in% c("394", "395", "396", "397", "424", "746") ~ 1L,
          .data[["diag_cd"]] %in% c("0932", "394", "395", "396", "397", "424", "7463", "7464", "7465", "7466", "V422", "V433") ~ 1L,
          .default = 0
        ),
        # Pulmonary Circulation Disorders
        pcd = dplyr::case_when(diag_3dig %in% c("415", "416", "417") ~ 1L,
          .data[["diag_cd"]] %in% c("4150", "4151", "416", "4170", "4178", "4179") ~ 1L,
          .default = 0
        ),
        # Peripheral Vascular Disorders
        pvd = dplyr::case_when(diag_3dig %in% c("440", "441", "443", "447", "557") ~ 1L,
          .data[["diag_cd"]] %in% c("0930", "4373", "440", "441", "4431", "4432", "4438", "4439", "4471", "5571", "5579", "V434") ~ 1L,
          .default = 0
        ),
        # Hypertension Uncomplicated
        hptn_nc = dplyr::case_when(diag_3dig %in% c("401") ~ 1L, .default = 0),
        # Hypertension complicated
        hptn_c = dplyr::case_when(diag_3dig %in% c("402", "403", "404", "405") ~ 1L, .default = 0),
        # Paralysis
        para = dplyr::case_when(diag_3dig %in% c("334", "342", "343", "344") ~ 1L,
          .data[["diag_cd"]] %in% c(
            "3341", "342", "343", "3440", "3441", "3442", "3443",
            "3444", "3445", "3446", "3449"
          ) ~ 1L,
          .default = 0
        ),
        # Other Neurological Disorders
        othnd = dplyr::case_when(diag_3dig %in% c("331", "332", "333", "334", "335", "336", "340", "341", "345", "348") ~ 1L,
          .data[["diag_cd"]] %in% c("3319", "3320", "3321", "3334", "3335", "33392", "334", "335", "3362", "340", "341", "345", "3481", "3483", "7803", "7843") ~ 1L,
          .default = 0
        ),
        # Chronic Pulmonary Disease
        copd = dplyr::case_when(diag_3dig %in% c("416", "490", "491", "492", "493", "494", "495", "496", "500", "501", "502", "503", "504", "505") ~ 1L,
          .data[["diag_cd"]] %in% c(
            "4168", "4169", "490", "491", "492", "493", "494",
            "495", "496", "500", "501", "502", "503", "504",
            "505", "5064", "5081", "5088"
          ) ~ 1L,
          .default = 0
        ),
        # Diabetes Uncomplicated
        diab_nc = dplyr::case_when(.data[["diag_cd"]] %in% c("2500", "2501", "2502", "2503") ~ 1L,
          diag_3dig %in% c("250") ~ 1L,
          .default = 0
        ),
        # Diabetes Complicated
        diab_c = dplyr::case_when(.data[["diag_cd"]] %in% c("2504", "2505", "2506", "2507", "2508", "2509") ~ 1L, .default = 0),
        # hypothyroidism
        hptothy = dplyr::case_when(diag_3dig %in% c("240", "243", "244", "246") ~ 1L,
          .data[["diag_cd"]] %in% c("2409", "243", "244", "2461", "2468") ~ 1L,
          .default = 0
        ),
        # Renal Failure
        rf = dplyr::case_when(diag_3dig %in% c("403", "585", "586", "588", "V56") ~ 1L,
          .data[["diag_cd"]] %in% c("40301", "40311", "40391", "40402", "40403", "40412", "40413", "40492", "40493", "585", "586", "5880", "V420", "V451", "V56") ~ 1L,
          .default = 0
        ),
        # Liver Disease
        ld = dplyr::case_when(diag_3dig %in% c("070", "456", "570", "571", "572", "573") ~ 1L,
          .data[["diag_cd"]] %in% c("07022", "07023", "07032", "07033", "07044", "07054", "0706", "0709", "4560", "4561", "4562", "570", "571", "5722", "5723", "5724", "5728", "5733", "5734", "5738", "5739", "V427") ~ 1L,
          .default = 0
        ),
        # Peptic Ulcer Disease excluding bleeding
        pud_nb = dplyr::case_when(diag_3dig %in% c("531", "532", "533", "534") ~ 1L,
          .data[["diag_cd"]] %in% c("5317", "5319", "5327", "5329", "5337", "5339", "5347", "5349") ~ 1L,
          .default = 0
        ),
        # AIDS/HIV
        hiv = dplyr::case_when(diag_3dig %in% c("042", "043", "044") ~ 1L, .default = 0),
        # Lymphoma
        lymp = dplyr::case_when(diag_3dig %in% c("200", "201", "202", "203") ~ 1L,
          .data[["diag_cd"]] %in% c("200", "201", "202", "2030", "2386") ~ 1L,
          .default = 0
        ),
        # Metastatic Cancer
        mets = dplyr::case_when(diag_3dig %in% c("196", "197", "198", "199") ~ 1L, .default = 0),
        # Solid Tumor without Metastasis
        tumor = dplyr::case_when(diag_3dig %in% c("140", "141", "142", "143", "144", "145", "146", "147", "148", "149", "150", "151", "152", "153", "154", "155", "156", "157", "158", "159", "160", "161", "162", "163", "164", "165", "166", "167", "168", "169", "170", "171", "172", "174", "175", "176", "177", "178", "179", "180", "181", "182", "183", "184", "185", "186", "187", "188", "189", "190", "191", "192", "193", "194", "195") ~ 1L, .default = 0),
        # Rheumatoid Arthritis/collagen
        rheum_a = dplyr::case_when(diag_3dig %in% c("446", "701", "710", "711", "714", "719", "720", "725", "728") ~ 1L,
          .data[["diag_cd"]] %in% c("446", "7010", "7100", "7101", "7102", "7103", "7104", "7108", "7109", "7112", "714", "7193", "720", "725", "7285", "72889", "72930") ~ 1L,
          .default = 0
        ),
        # Coagulopathy
        coag = dplyr::case_when(diag_3dig %in% c("286", "287") ~ 1L,
          .data[["diag_cd"]] %in% c("286", "2871", "2873", "2874", "2875") ~ 1L,
          .default = 0
        ),
        # obesity
        obesity = dplyr::case_when(diag_3dig %in% c("278") ~ 1L,
          .data[["diag_cd"]] %in% c("2780") ~ 1L,
          .default = 0
        ),
        # Weight Loss
        wl = dplyr::case_when(diag_3dig %in% c("260", "261", "262", "263") ~ 1L,
          .data[["diag_cd"]] %in% c("260", "261", "262", "263", "7832", "7994") ~ 1L,
          .default = 0
        ),
        # Fluid and Electrolyte Disorders
        fluid = dplyr::case_when(diag_3dig %in% c("276") ~ 1L,
          .data[["diag_cd"]] %in% c("2536", "276") ~ 1L,
          .default = 0
        ),
        # Blood Loss Anemia
        bla = dplyr::case_when(.data[["diag_cd"]] %in% c("2800") ~ 1L, .default = 0),
        # Deficiency Anemia
        da = dplyr::case_when(.data[["diag_cd"]] %in% c("2801", "2808", "2809", "281") ~ 1L,
          diag_3dig %in% c("280", "281") ~ 1L,
          .default = 0
        ),
        # Alcohol Abuse
        alcohol = dplyr::case_when(diag_3dig %in% c("291", "303", "980") ~ 1L,
          .data[["diag_cd"]] %in% c("2652", "2911", "2912", "2913", "2915", "2918", "2919", "3030", "3039", "3050", "3575", "4255", "5353", "5710", "5711", "5712", "5713", "980", "V113") ~ 1L,
          .default = 0
        ),
        # Drug Abuse
        drug = dplyr::case_when(diag_3dig %in% c("292", "304", "305") ~ 1L,
          .data[["diag_cd"]] %in% c("292", "304", "3052", "3053", "3054", "3055", "3056", "3057", "3058", "3059", "V6542") ~ 1L,
          .default = 0
        ),
        # Psychoses
        psycho = dplyr::case_when(diag_3dig %in% c("293", "295", "297", "298") ~ 1L,
          .data[["diag_cd"]] %in% c("2938", "295", "29604", "29614", "29644", "29654", "297", "298") ~ 1L,
          .default = 0
        ),
        # depression
        dep = dplyr::case_when(diag_3dig %in% c("296", "300", "309", "311") ~ 1L,
          .data[["diag_cd"]] %in% c("2962", "2963", "2965", "3004", "309", "311") ~ 1L,
          .default = 0
        )
      )
  }

  # set categories included in the total calculation with meta programming
  full_cat <- c("chf", "arrhy", "vd", "pcd", "pvd", "hptn_nc", "hptn_c", "para", "othnd", "copd", "diab_nc", "diab_c", "hptothy", "rf", "ld", "pud_nb", "hiv", "lymp", "mets", "tumor", "rheum_a", "coag", "obesity", "wl", "fluid", "bla", "da", "alcohol", "drug", "psycho", "dep")
  incl <- full_cat[!(full_cat %in% excl)]

  sum_expr <- rlang::expr(data_sum <- data_long %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(full_cat), ~ max(., na.rm = TRUE))) %>%
    dplyr::mutate(total_eci = !!rlang::parse_expr(paste(incl, collapse = "+"))))

  eval(sum_expr)

  dplyr::ungroup(data_sum)
}
