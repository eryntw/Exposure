## calc_combo_pct.R ##
## Calculate the percentage of records falling in a chosen combination of
## count columns (e.g. selected IBRA bioregions) relative to a set of
## denominator columns (e.g. all ibra_name_* columns).

#' Percentage of a column combination relative to a denominator set
#'
#' @param data Data frame / tibble containing count columns.
#' @param combos Named list. Each element is a character vector of column names
#'   (the numerator for that combination); the list name becomes the output
#'   column name. A single unnamed character vector is also accepted and is
#'   written to `new_col`.
#' @param denom_cols <tidyselect> Columns that make up the denominator,
#'   e.g. `starts_with("ibra_name_")`. Evaluated against `data`.
#' @param exclude_pattern Optional regex. Any denominator column whose name
#'   matches is dropped (e.g. "simpson|shannon|_prop|_note|_total").
#'   Applied case-insensitively.
#' @param new_col Output column name when `combos` is an unnamed vector.
#' @param scale 100 for percentages, 1 for proportions.
#' @param na_rm Drop NA cells when summing across columns? Rows where ALL
#'   denominator cells are NA always return NA regardless.
#' @param verbose Message the denominator columns that were used.
#'
#' @return `data` with one new column per combination (NA where the
#'   denominator is 0 or all-NA). Attribute "denom_cols" records the columns used.
calc_combo_pct <- function(
    data,
    combos,
    denom_cols,
    exclude_pattern = NULL,
    new_col = "pct_combo",
    scale = 100,
    na_rm = TRUE,
    verbose = TRUE
) {
  
  ## normalise combos to a named list ##
  if (!is.list(combos)) {
    combos <- stats::setNames(list(combos), new_col)
  }
  if (is.null(names(combos)) || any(names(combos) == "")) {
    stop("`combos` must be a named list (names become the output columns).")
  }
  
  ## resolve denominator columns via tidyselect ##
  denom_expr <- rlang::enquo(denom_cols)
  denom_names <- names(tidyselect::eval_select(denom_expr, data))
  
  if (!is.null(exclude_pattern)) {
    denom_names <- denom_names[
      !stringr::str_detect(denom_names, stringr::regex(exclude_pattern, ignore_case = TRUE))
    ]
  }
  if (length(denom_names) == 0) {
    stop("No denominator columns left after selection/exclusion.")
  }
  
  ## guard: denominators must be numeric counts ##
  non_numeric <- denom_names[!purrr::map_lgl(data[denom_names], is.numeric)]
  if (length(non_numeric) > 0) {
    stop(
      "Non-numeric denominator columns (exclude them with `exclude_pattern`): ",
      paste(non_numeric, collapse = ", ")
    )
  }
  
  if (verbose) {
    message(length(denom_names), " denominator columns used, e.g.: ",
            paste(utils::head(denom_names, 3), collapse = ", "))
  }
  
  ## denominator: row totals across the chosen columns ##
  denom_mat <- as.matrix(data[denom_names])
  denom_total <- rowSums(denom_mat, na.rm = na_rm)
  all_na_row <- rowSums(!is.na(denom_mat)) == 0
  denom_total[all_na_row | denom_total == 0] <- NA_real_  # avoid 0/0 -> NaN
  
  ## numerator(s): one pass per combination ##
  for (nm in names(combos)) {
    num_names <- combos[[nm]]
    
    missing_cols <- setdiff(num_names, names(data))
    if (length(missing_cols) > 0) {
      stop("Combination '", nm, "' has columns not in `data`: ",
           paste(missing_cols, collapse = ", "))
    }
    ## numerator must sit inside the denominator, otherwise % can exceed 100 ##
    outside <- setdiff(num_names, denom_names)
    if (length(outside) > 0) {
      stop("Combination '", nm, "' includes columns outside the denominator set: ",
           paste(outside, collapse = ", "))
    }
    
    num_total <- rowSums(as.matrix(data[num_names]), na.rm = na_rm)
    data[[nm]] <- scale * num_total / denom_total
  }
  
  attr(data, "denom_cols") <- denom_names
  data
}

## Example usage ##
# library(dplyr)
# rec_summary_pilot <- rec_summary_pilot |>
#   calc_combo_pct(
#     combos = list(
#       pct_mallee_mdd = c("ibra_name_Mallee", "ibra_name_Murray Darling Depression"),
#       pct_arid       = c("ibra_name_Great Victoria Desert", "ibra_name_Nullarbor",
#                          "ibra_name_Stony Plains")
#     ),
#     denom_cols      = dplyr::starts_with("ibra_name_"),
#     exclude_pattern = "simpson"
#   )
#
# ## build a numerator by prefix + regex rather than typing names ##
# arid <- grep("Desert|Nullarbor|Stony|Tanami", names(rec_summary_pilot), value = TRUE) |>
#   stringr::str_subset("^ibra_name_")
# rec_summary_pilot |>
#   calc_combo_pct(arid, dplyr::starts_with("ibra_name_"), new_col = "pct_arid")