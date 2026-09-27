#' Produces a template of (species x trait) combinations needing manual
#' coding, with taxonomic/conservation context columns attached so the
#' person doing manual coding doesn't need to cross-reference a second table.
#'
#' @param animal_traits_resolved The database-derived resolved-traits table.
#' @param id_col Species identifier column.
#' @param context_cols Metadata columns to retain for reference (not pivoted).
#'
#' Wide version: one row per species, metadata columns first, then every
#' resolved trait column as-is (including non-missing values, for context) —
#' useful for reviewing a species' full profile rather than isolated gaps.
flag_missing_traits_wide <- function(
    animal_traits_resolved,
    id_col = "species",
    context_cols = c("ala_class", "ala_order", "ala_vernacular_name", "epbc")
) {
  animal_traits_resolved |>
    dplyr::select(dplyr::all_of(c(id_col, context_cols)), dplyr::matches("_resolved$|_lit$")) |>
    dplyr::filter(
      dplyr::if_any(dplyr::matches("_resolved$|_lit$"), is.na)
    )
}