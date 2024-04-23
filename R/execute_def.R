#' Execute parameterized case definitions
#'
#' @description
#' This function executes the function calls stored in the output tibble from [build_def()] with data objects supplied through a named list and returns the results as a list. It is intended to facilitate re-use of pre-defined calls with different data.
#'
#'
#' @param def A tibble created by [build_def()].
#' @param with_data A named list which the elements are in the form of src_lab = data, where 'src_lab' corresponds to the src_labs argument from [build_def()] and 'data' is the data object that will be passed to calls stored in def. The names (and length) of `with_data` must match the unique values of src_labs in `def`.
#' @param bind A logical for whether row-binding records from multiple sources into one table. Note that the binding may fail in ways that are difficult to anticipate in advance, such as data type conflict (e.g., Date vs. character) between variables in the same name from different sources. The default is FALSE. If TRUE, the behavior is to try and return the unbinded result when failed.
#' @param force_proceed A logical for whether to ask for user input in order to proceed when remote tables are needed to be collected for binding. The default is TRUE to let user be aware of that the downloading process may be slow. Use options(healthdb.force_proceed = FALSE) to suppress the prompt once and for all.
#' @seealso [bind_sources()] for binding the output with convenient renaming features.
#'
#' @return A single (if bind = TRUE) or a list of data.frames or remote tables.
#' @export
#'
#' @examples
#' # toy data
#' sample_size <- 30
#' df <- data.frame(
#'   clnt_id = rep(1:3, each = 10),
#'   service_dt = sample(seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = 1),
#'     size = sample_size, replace = TRUE
#'   ),
#'   diagx = sample(letters, size = sample_size, replace = TRUE),
#'   diagx_1 = sample(c(NA, letters), size = sample_size, replace = TRUE),
#'   diagx_2 = sample(c(NA, letters), size = sample_size, replace = TRUE)
#' )
#'
#' # make df a database table
#' db <- dbplyr::tbl_memdb(df)
#'
#' # use build_def to make a toy definition
#' sud_def <- build_def("SUD", # usually a disease name
#'   src_lab = c("src1", "src2"), # identify from multiple sources, e.g., hospitalization, ED visits.
#'   # functions that filter the data with some criteria
#'   def_fn = define_case,
#'   fn_args = list(
#'     vars = starts_with("diagx"),
#'     match = "start", # "start" will be applied to all sources as length = 1
#'     vals = list(c("304"), c("305")),
#'     clnt_id = "clnt_id", # list()/c() could be omitted for single element
#'     # c() can be used in place of list
#'     # if this argument only takes one value for each source
#'     n_per_clnt = c(2, 3)
#'   )
#' )
#'
#' # save the definition for re-use
#' # saveRDS(sud_def, file = some_path)
#'
#' sud_def %>% execute_def(with_data = list(src1 = df, src2 = db), force_proceed = TRUE)
execute_def <- function(def, with_data, bind = FALSE, force_proceed = getOption("healthdb.force_proceed")) {
  # capture data names in the original env before any eval
  with_data_quo <- rlang::enquo(with_data)

  is_list_obj <- rlang::quo_is_symbol(with_data_quo)
  if (is_list_obj) {
    with_data_expr <- rlang::quo_get_expr(with_data_quo) %>% rlang::as_name()
    n_data <- length(with_data)
    stopifnot(rlang::is_named(with_data))
    with_data_expr <- sapply(names(with_data), function(x) glue::glue('{with_data_expr}[["{x}"]]') %>% rlang::parse_expr(), simplify = FALSE, USE.NAMES = TRUE)
  } else {
    with_data_expr <- with_data_quo %>% rlang::call_args()
    n_data <- length(with_data_expr)
  }
  #with_data_expr <- with_data_quo %>% rlang::call_args()
  with_data_env <- with_data_quo %>% rlang::quo_get_env()

  # input checks
  stopifnot(
    rlang::is_named(with_data),
    rlang::is_list(with_data),
    !rlang::is_empty(with_data),
    identical(names(def), c("def_lab", "src_labs", "def_fn", "fn_args", "fn_call")),
    all(names(with_data) %in% def[["src_labs"]])
  )

  def_check <- def %>%
    dplyr::select(dplyr::all_of(c("def_lab", "src_labs"))) %>%
    dplyr::distinct()
  if (nrow(def_check) < nrow(def)) stop("Duplicates in 'def_lab' + 'src_labs' combinations")

  n_source <- dplyr::n_distinct(def[["src_labs"]])
  n_data <- length(with_data_expr)
  if (n_data != n_source) stop("'def' has ", n_source, " sources, but ", n_data, " source datasets are supplied.")

  # ask user input to proceed as collecting remote table may be slow
  # don't ask if all table is remote/local
  is_local <- purrr::map_lgl(def[["src_labs"]], ~ is.data.frame(with_data[[.]]))
  any_local <- any(is_local)
  any_remote <- any(!is_local)

  if (!force_proceed & bind & any_local & any_remote) {
    proceed <- readline(prompt = "Remote tables have to be collected (may be slow) in order to be binded. Proceed? [y/n]")

    if (proceed == "n") stop("\nTry bind = FALSE, or supply data from the same source (i.e., either all local or all remote).")
  }

  # first alter the call to include data, then eval
  def <- def %>%
    dplyr::mutate(
      fn_call = purrr::map2(
        .data[["fn_call"]], .data[["src_labs"]],
        function(x, y) rlang::call_modify(x, data = with_data_expr[[y]])
      )
    )

  def <- def %>%
    dplyr::mutate(
      result = purrr::map2(.data[["fn_call"]], .data[["src_labs"]], function(x, y) {
        if (getOption("healthdb.verbose")) cat("\nProcessing source:", rlang::expr_deparse(with_data_expr[[y]]))
        eval(x, envir = with_data_env)
      }, .progress = TRUE),
      result = purrr::pmap(
        list(.data[["result"]], .data[["def_lab"]], .data[["src_labs"]]),
        function(dat, def, src) {
          dat %>% dplyr::mutate(
            `def` = def,
            `src` = src,
            .before = 1
          )
        }
      )
    )

  # result <- purrr::map(def[["fn_call"]], function(x) eval(x, envir = with_data_env))

  if (bind & !any_local) {
    # if the data are all remote, do union in SQL;
    # union_all not necessary as already labeled by def and src; rows would not collapse across srcs
    result <- rlang::try_fetch(purrr::reduce(def[["result"]], dplyr::union),
      error = function(cnd) {
        rlang::warn("Returned unbinded result. Binding failed probably due to combining tables from different databases, which cannot be binded without collecting. Actual error message:\n", parent = cnd)
        return(def[["result"]])
      }
    )
    # manual return here to simplify the subsequent if logic
    return(result)
  } else if (bind) {
    # if not all remote, also collect the remote ones before binding
    result <- purrr::map_if(def[["result"]], !is_local, dplyr::collect, .progress = TRUE)
  } else {
    result <- def[["result"]]
  }

  # do faster list bind on all local tables, with distinct() to match union result
  # list_rbind chosen over rbindlist for better error msg at a cost of performance
  if (bind) {
    result <- rlang::try_fetch(purrr::list_rbind(result) %>% dplyr::distinct(),
      error = function(cnd) {
        rlang::warn("Returned unbinded result. Binding failed probably due to incompatible types of the same variable from different sources. Actual error message:\n", parent = cnd)
        return(result)
      }
    )
  }
  # result <- data.table::rbindlist(def[["result"]])
  # result <- unique(result)
  # data.table::setDF(result)

  return(result)
}
