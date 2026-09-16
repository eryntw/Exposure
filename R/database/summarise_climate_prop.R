#' Calculate proportion of records in a chosen climate category group
#'
#' Sums all climate category columns to get a total record count, sums the
#' columns matching `climate_cols` separately, then calculates that group's
#' records as a proportion of total records, rounded to 2 decimal places.
#'
#' @param climate_summary Data frame with one row per species, containing
#'   `climate_<code>` count columns (from raster point extraction).
#' @param climate_cols Tidyselect expression (unquoted) identifying which
#'   `climate_<code>` columns to sum for the group of interest, e.g.
#'   `dplyr::matches("^climate_[0-9]$")` for desert codes 0-9, or
#'   `dplyr::any_of(c("climate_12", "climate_13"))` for specific codes.
#' @param prefix Character. Prefix used to name the output columns, e.g.
#'   `"desert"` produces `desert_climate` and `desert_prop`. Default = "group"
#'
#' @return `climate_summary` with three additional columns: `total_climate`
#'   (sum across all climate categories), `<prefix>_climate` (sum across the
#'   selected columns), and `<prefix>_prop` (`<prefix>_climate / total_climate`,
#'   rounded to 2 decimal places; `NA` where total_climate is 0)
#' @export
#'
#' @examples
#' summarise_climate_prop(climate_summary, dplyr::matches("^climate_[0-9]$"), prefix = "desert")
#' summarise_climate_prop(climate_summary, dplyr::any_of(c("climate_12", "climate_13")), prefix = "temperate")
#'
summarise_climate_prop <- function(climate_summary, climate_cols, prefix = "group") {
  
  climate_cols <- rlang::enquo(climate_cols)
  
  climate_col_name <- paste0(prefix, "_climate")
  prop_col_name    <- paste0(prefix, "_prop")
  
  climate_summary |>
    dplyr::mutate(
      # ---- total records across all climate categories ----
      total_climate = rowSums(
        dplyr::across(dplyr::starts_with("climate_")),
        na.rm = TRUE
      ),
      
      # ---- records in the selected climate category group only ----
      "{climate_col_name}" := rowSums(
        dplyr::across(!!climate_cols),
        na.rm = TRUE
      )
    ) |>
    dplyr::mutate(
      # ---- selected group's records as a proportion of total; NA if no records at all ----
      "{prop_col_name}" := dplyr::if_else(
        total_climate > 0,
        .data[[climate_col_name]] / total_climate,
        NA_real_
      )
    ) |>
    dplyr::mutate(
      # ---- round to 2 decimal places ----
      "{prop_col_name}" := round(.data[[prop_col_name]], digits = 2)
    )
}