# Build the `elix_codes` dataset: ICD codes defining the 31 Elixhauser
# comorbidity categories used by compute_comorbidity().
#
# Sources:
# - "ICD-10" and "ICD-9-CM-5digits": Quan et al. (2005), Med Care 43(11).
# - "ICD-9-CM-3digits": Manitoba Centre for Health Policy adaptation, which
#   searches BOTH 3-digit and 5-digit codes.
#
# `match_len` encodes how a code is compared with the diagnosis values:
# - an integer L: prefix match, i.e., the first L characters of the diagnosis
#   value must equal the code (codes at the category/subcategory level cover
#   all their subdivisions);
# - NA: exact match of the full code.
#
# Run this script to regenerate data/elix_codes.rda:
#   source("data-raw/elix_codes.R")

categories <- c(
  chf = "Congestive Heart Failure",
  arrhy = "Cardiac Arrhythmia",
  vd = "Valvular Disease",
  pcd = "Pulmonary Circulation Disorders",
  pvd = "Peripheral Vascular Disorders",
  hptn_nc = "Hypertension Uncomplicated",
  hptn_c = "Hypertension Complicated",
  para = "Paralysis",
  othnd = "Other Neurological Disorders",
  copd = "Chronic Pulmonary Disease",
  diab_nc = "Diabetes Uncomplicated",
  diab_c = "Diabetes Complicated",
  hptothy = "Hypothyroidism",
  rf = "Renal Failure",
  ld = "Liver Disease",
  pud_nb = "Peptic Ulcer Disease excluding bleeding",
  hiv = "AIDS/HIV",
  lymp = "Lymphoma",
  mets = "Metastatic Cancer",
  tumor = "Solid Tumor without Metastasis",
  rheum_a = "Rheumatoid Arthritis/collagen",
  coag = "Coagulopathy",
  obesity = "Obesity",
  wl = "Weight Loss",
  fluid = "Fluid and Electrolyte Disorders",
  bla = "Blood Loss Anemia",
  da = "Deficiency Anemia",
  alcohol = "Alcohol Abuse",
  drug = "Drug Abuse",
  psycho = "Psychoses",
  dep = "Depression"
)

# each category is a list of segments; a segment is list(match_len, codes)
# segment order is preserved and determines the order of conditions in the
# generated case_when()
seg <- function(match_len, codes) list(match_len = match_len, codes = codes)

icd10 <- list(
  chf = list(
    seg(3L, c("I43", "I50")),
    seg(4L, c("I099", "I110", "I130", "I132", "I255", "I420", "I425", "I426", "I427", "I428", "I429", "P290"))
  ),
  arrhy = list(
    seg(3L, c("I47", "I48", "I49")),
    seg(4L, c("I441", "I442", "I443", "I456", "I459", "R000", "R001", "R008", "T821", "Z450", "Z950"))
  ),
  vd = list(
    seg(3L, c("I05", "I06", "I07", "I08", "I34", "I35", "I36", "I37", "I38", "I39")),
    seg(4L, c("A520", "I091", "I098", "Q230", "Q231", "Q232", "Q233", "Z952", "Z953", "Z954"))
  ),
  pcd = list(
    seg(3L, c("I26", "I27")),
    seg(4L, c("I280", "I288", "I289"))
  ),
  pvd = list(
    seg(3L, c("I70", "I71")),
    seg(4L, c("I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558", "K559", "Z958", "Z959"))
  ),
  hptn_nc = list(seg(3L, "I10")),
  hptn_c = list(seg(3L, c("I11", "I12", "I13", "I15"))),
  para = list(
    seg(3L, c("G81", "G82")),
    seg(4L, c("G041", "G114", "G801", "G802", "G830", "G831", "G832", "G833", "G834", "G839"))
  ),
  othnd = list(
    seg(3L, c("G10", "G11", "G12", "G13", "G20", "G21", "G22", "G32", "G35", "G36", "G37", "G40", "G41", "R56")),
    seg(4L, c("G254", "G255", "G312", "G318", "G319", "G931", "G934", "R470"))
  ),
  copd = list(
    seg(3L, c("J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", "J60", "J61", "J62", "J63", "J64", "J65", "J66", "J67")),
    seg(4L, c("I278", "I279", "J684", "J701", "J703"))
  ),
  diab_nc = list(
    seg(4L, c("E100", "E101", "E109", "E110", "E111", "E119", "E120", "E121", "E129", "E130", "E131", "E139", "E140", "E141", "E149"))
  ),
  diab_c = list(
    seg(4L, c(
      "E102", "E103", "E104", "E105", "E106", "E107", "E108",
      "E112", "E113", "E114", "E115", "E116", "E117", "E118",
      "E122", "E123", "E124", "E125", "E126", "E127", "E128",
      "E132", "E133", "E134", "E135", "E136", "E137", "E138",
      "E142", "E143", "E144", "E145", "E146", "E147", "E148"
    ))
  ),
  hptothy = list(
    seg(3L, c("E00", "E01", "E02", "E03")),
    seg(4L, "E890")
  ),
  rf = list(
    seg(3L, c("N18", "N19")),
    seg(4L, c("I120", "I131", "N250", "Z490", "Z491", "Z492", "Z940", "Z992"))
  ),
  ld = list(
    seg(3L, c("B18", "I85", "K70", "K72", "K73", "K74")),
    seg(4L, c("I864", "I982", "K711", "K713", "K714", "K715", "K717", "K760", "K762", "K763", "K764", "K765", "K766", "K767", "K768", "K769", "Z944"))
  ),
  pud_nb = list(seg(4L, c("K257", "K259", "K267", "K269", "K277", "K279", "K287", "K289"))),
  hiv = list(seg(3L, c("B20", "B21", "B22", "B24"))),
  lymp = list(
    seg(3L, c("C81", "C82", "C83", "C84", "C85", "C88", "C96")),
    seg(4L, c("C900", "C902"))
  ),
  mets = list(seg(3L, c("C77", "C78", "C79", "C80"))),
  tumor = list(
    seg(3L, c(
      "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
      "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19",
      "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32",
      "C33", "C34", "C37", "C38", "C39", "C40", "C41", "C43", "C45", "C46",
      "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54", "C55", "C56",
      "C57", "C58", "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67",
      "C68", "C69", "C70", "C71", "C72", "C73", "C74", "C75", "C76", "C97"
    ))
  ),
  rheum_a = list(
    seg(3L, c("M05", "M06", "M08", "M30", "M32", "M33", "M34", "M35", "M45")),
    seg(4L, c("L940", "L941", "L943", "M120", "M123", "M310", "M311", "M312", "M313", "M461", "M468", "M469"))
  ),
  coag = list(
    seg(3L, c("D65", "D66", "D67", "D68")),
    seg(4L, c("D691", "D693", "D694", "D695", "D696"))
  ),
  obesity = list(seg(3L, "E66")),
  wl = list(
    seg(3L, c("E40", "E41", "E42", "E43", "E44", "E45", "E46", "R64")),
    seg(4L, "R634")
  ),
  fluid = list(
    seg(3L, c("E86", "E87")),
    seg(4L, "E222")
  ),
  bla = list(seg(4L, "D500")),
  da = list(
    seg(3L, c("D51", "D52", "D53")),
    seg(4L, c("D508", "D509"))
  ),
  alcohol = list(
    seg(3L, c("E52", "F10", "T51")),
    seg(4L, c("G621", "I426", "K292", "K700", "K703", "K709", "Z502", "Z714", "Z721"))
  ),
  drug = list(
    seg(3L, c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19")),
    seg(4L, c("Z715", "Z722"))
  ),
  psycho = list(
    seg(3L, c("F20", "F22", "F23", "F24", "F25", "F28", "F29")),
    seg(4L, c("F302", "F312", "F315"))
  ),
  dep = list(
    seg(3L, c("F32", "F33")),
    seg(4L, c("F204", "F313", "F314", "F315", "F341", "F412", "F432"))
  )
)

icd9_5dig <- list(
  chf = list(seg(NA_integer_, c("39891", "40201", "40211", "40291", "40401", "40403", "40411", "40413", "40491", "40493", "4254", "4255", "4257", "4258", "4259", "428"))),
  arrhy = list(seg(NA_integer_, c("4260", "42613", "4267", "4269", "42610", "42612", "4270", "4271", "4272", "4273", "4274", "4276", "4278", "4279", "7850", "99601", "99604", "V450", "V533"))),
  vd = list(seg(NA_integer_, c("0932", "394", "395", "396", "397", "424", "7463", "7464", "7465", "7466", "V422", "V433"))),
  pcd = list(seg(NA_integer_, c("4150", "4151", "416", "4170", "4178", "4179"))),
  pvd = list(seg(NA_integer_, c("0930", "4373", "440", "441", "4431", "4432", "4438", "4439", "4471", "5571", "5579", "V434"))),
  hptn_nc = list(seg(NA_integer_, "401")),
  hptn_c = list(seg(NA_integer_, c("402", "403", "404", "405"))),
  para = list(seg(NA_integer_, c("3341", "342", "343", "3440", "3441", "3442", "3443", "3444", "3445", "3446", "3449"))),
  othnd = list(seg(NA_integer_, c("3319", "3320", "3321", "3334", "3335", "33392", "334", "335", "3362", "340", "341", "345", "3481", "3483", "7803", "7843"))),
  copd = list(seg(NA_integer_, c("4168", "4169", "490", "491", "492", "493", "494", "495", "496", "500", "501", "502", "503", "504", "505", "5064", "5081", "5088"))),
  diab_nc = list(seg(NA_integer_, c("2500", "2501", "2502", "2503"))),
  diab_c = list(seg(NA_integer_, c("2504", "2505", "2506", "2507", "2508", "2509"))),
  hptothy = list(seg(NA_integer_, c("2409", "243", "244", "2461", "2468"))),
  rf = list(seg(NA_integer_, c("40301", "40311", "40391", "40402", "40403", "40412", "40413", "40492", "40493", "585", "586", "5880", "V420", "V451", "V56"))),
  ld = list(seg(NA_integer_, c("07022", "07023", "07032", "07033", "07044", "07054", "0706", "0709", "4560", "4561", "4562", "570", "571", "5722", "5723", "5724", "5728", "5733", "5734", "5738", "5739", "V427"))),
  pud_nb = list(seg(NA_integer_, c("5317", "5319", "5327", "5329", "5337", "5339", "5347", "5349"))),
  hiv = list(seg(NA_integer_, c("042", "043", "044"))),
  lymp = list(seg(NA_integer_, c("200", "201", "202", "2030", "2386"))),
  mets = list(seg(NA_integer_, c("196", "197", "198", "199"))),
  tumor = list(seg(NA_integer_, c(
    "140", "141", "142", "143", "144", "145", "146", "147", "148", "149",
    "150", "151", "152", "153", "154", "155", "156", "157", "158", "159",
    "160", "161", "162", "163", "164", "165", "166", "167", "168", "169",
    "170", "171", "172", "174", "175", "176", "177", "178", "179", "180",
    "181", "182", "183", "184", "185", "186", "187", "188", "189", "190",
    "191", "192", "193", "194", "195"
  ))),
  rheum_a = list(seg(NA_integer_, c("446", "7010", "7100", "7101", "7102", "7103", "7104", "7108", "7109", "7112", "714", "7193", "720", "725", "7285", "72889", "72930"))),
  coag = list(seg(NA_integer_, c("286", "2871", "2873", "2874", "2875"))),
  obesity = list(seg(NA_integer_, "2780")),
  wl = list(seg(NA_integer_, c("260", "261", "262", "263", "7832", "7994"))),
  fluid = list(seg(NA_integer_, c("2536", "276"))),
  bla = list(seg(NA_integer_, "2800")),
  da = list(seg(NA_integer_, c("2801", "2808", "2809", "281"))),
  alcohol = list(seg(NA_integer_, c("2652", "2911", "2912", "2913", "2915", "2918", "2919", "3030", "3039", "3050", "3575", "4255", "5353", "5710", "5711", "5712", "5713", "980", "V113"))),
  drug = list(seg(NA_integer_, c("292", "304", "3052", "3053", "3054", "3055", "3056", "3057", "3058", "3059", "V6542"))),
  psycho = list(seg(NA_integer_, c("2938", "295", "29604", "29614", "29644", "29654", "297", "298"))),
  dep = list(seg(NA_integer_, c("2962", "2963", "2965", "3004", "309", "311")))
)

icd9_3dig <- list(
  chf = list(
    seg(3L, c("398", "402", "425", "428")),
    seg(NA_integer_, c("39891", "40201", "40211", "40291", "40401", "40403", "40411", "40413", "40491", "40493", "4254", "4255", "4257", "4258", "4259", "428"))
  ),
  arrhy = list(
    seg(3L, c("426", "427")),
    seg(NA_integer_, c("4260", "42613", "4267", "4269", "42610", "42612", "4270", "4271", "4272", "4273", "4274", "4276", "4278", "4279", "7850", "99601", "99604", "V450", "V533"))
  ),
  vd = list(
    seg(3L, c("394", "395", "396", "397", "424", "746")),
    seg(NA_integer_, c("0932", "394", "395", "396", "397", "424", "7463", "7464", "7465", "7466", "V422", "V433"))
  ),
  pcd = list(
    seg(3L, c("415", "416", "417")),
    seg(NA_integer_, c("4150", "4151", "416", "4170", "4178", "4179"))
  ),
  pvd = list(
    seg(3L, c("440", "441", "443", "447", "557")),
    seg(NA_integer_, c("0930", "4373", "440", "441", "4431", "4432", "4438", "4439", "4471", "5571", "5579", "V434"))
  ),
  hptn_nc = list(seg(3L, "401")),
  hptn_c = list(seg(3L, c("402", "403", "404", "405"))),
  para = list(
    seg(3L, c("334", "342", "343", "344")),
    seg(NA_integer_, c("3341", "342", "343", "3440", "3441", "3442", "3443", "3444", "3445", "3446", "3449"))
  ),
  othnd = list(
    seg(3L, c("331", "332", "333", "334", "335", "336", "340", "341", "345", "348")),
    seg(NA_integer_, c("3319", "3320", "3321", "3334", "3335", "33392", "334", "335", "3362", "340", "341", "345", "3481", "3483", "7803", "7843"))
  ),
  copd = list(
    seg(3L, c("416", "490", "491", "492", "493", "494", "495", "496", "500", "501", "502", "503", "504", "505")),
    seg(NA_integer_, c("4168", "4169", "490", "491", "492", "493", "494", "495", "496", "500", "501", "502", "503", "504", "505", "5064", "5081", "5088"))
  ),
  diab_nc = list(
    seg(NA_integer_, c("2500", "2501", "2502", "2503")),
    seg(3L, "250")
  ),
  diab_c = list(seg(NA_integer_, c("2504", "2505", "2506", "2507", "2508", "2509"))),
  hptothy = list(
    seg(3L, c("240", "243", "244", "246")),
    seg(NA_integer_, c("2409", "243", "244", "2461", "2468"))
  ),
  rf = list(
    seg(3L, c("403", "585", "586", "588", "V56")),
    seg(NA_integer_, c("40301", "40311", "40391", "40402", "40403", "40412", "40413", "40492", "40493", "585", "586", "5880", "V420", "V451", "V56"))
  ),
  ld = list(
    seg(3L, c("070", "456", "570", "571", "572", "573")),
    seg(NA_integer_, c("07022", "07023", "07032", "07033", "07044", "07054", "0706", "0709", "4560", "4561", "4562", "570", "571", "5722", "5723", "5724", "5728", "5733", "5734", "5738", "5739", "V427"))
  ),
  pud_nb = list(
    seg(3L, c("531", "532", "533", "534")),
    seg(NA_integer_, c("5317", "5319", "5327", "5329", "5337", "5339", "5347", "5349"))
  ),
  hiv = list(seg(3L, c("042", "043", "044"))),
  lymp = list(
    seg(3L, c("200", "201", "202", "203")),
    seg(NA_integer_, c("200", "201", "202", "2030", "2386"))
  ),
  mets = list(seg(3L, c("196", "197", "198", "199"))),
  tumor = list(seg(3L, c(
    "140", "141", "142", "143", "144", "145", "146", "147", "148", "149",
    "150", "151", "152", "153", "154", "155", "156", "157", "158", "159",
    "160", "161", "162", "163", "164", "165", "166", "167", "168", "169",
    "170", "171", "172", "174", "175", "176", "177", "178", "179", "180",
    "181", "182", "183", "184", "185", "186", "187", "188", "189", "190",
    "191", "192", "193", "194", "195"
  ))),
  rheum_a = list(
    seg(3L, c("446", "701", "710", "711", "714", "719", "720", "725", "728")),
    seg(NA_integer_, c("446", "7010", "7100", "7101", "7102", "7103", "7104", "7108", "7109", "7112", "714", "7193", "720", "725", "7285", "72889", "72930"))
  ),
  coag = list(
    seg(3L, c("286", "287")),
    seg(NA_integer_, c("286", "2871", "2873", "2874", "2875"))
  ),
  obesity = list(
    seg(3L, "278"),
    seg(NA_integer_, "2780")
  ),
  wl = list(
    seg(3L, c("260", "261", "262", "263")),
    seg(NA_integer_, c("260", "261", "262", "263", "7832", "7994"))
  ),
  fluid = list(
    seg(3L, "276"),
    seg(NA_integer_, c("2536", "276"))
  ),
  bla = list(seg(NA_integer_, "2800")),
  da = list(
    seg(NA_integer_, c("2801", "2808", "2809", "281")),
    seg(3L, c("280", "281"))
  ),
  alcohol = list(
    seg(3L, c("291", "303", "980")),
    seg(NA_integer_, c("2652", "2911", "2912", "2913", "2915", "2918", "2919", "3030", "3039", "3050", "3575", "4255", "5353", "5710", "5711", "5712", "5713", "980", "V113"))
  ),
  drug = list(
    seg(3L, c("292", "304", "305")),
    seg(NA_integer_, c("292", "304", "3052", "3053", "3054", "3055", "3056", "3057", "3058", "3059", "V6542"))
  ),
  psycho = list(
    seg(3L, c("293", "295", "297", "298")),
    seg(NA_integer_, c("2938", "295", "29604", "29614", "29644", "29654", "297", "298"))
  ),
  dep = list(
    seg(3L, c("296", "300", "309", "311")),
    seg(NA_integer_, c("2962", "2963", "2965", "3004", "309", "311"))
  )
)

build_rows <- function(ver_list, ver_label) {
  stopifnot(identical(names(ver_list), names(categories)))
  do.call(rbind, lapply(names(ver_list), function(cat) {
    do.call(rbind, lapply(ver_list[[cat]], function(s) {
      data.frame(
        icd_ver = ver_label,
        category = cat,
        description = unname(categories[cat]),
        code = s$codes,
        match_len = s$match_len,
        stringsAsFactors = FALSE
      )
    }))
  }))
}

elix_codes <- rbind(
  build_rows(icd10, "ICD-10"),
  build_rows(icd9_5dig, "ICD-9-CM-5digits"),
  build_rows(icd9_3dig, "ICD-9-CM-3digits")
)
rownames(elix_codes) <- NULL

usethis::use_data(elix_codes, overwrite = TRUE)
