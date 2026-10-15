#' Apply manual (mtable) coding onto database-resolved animal traits
#'
#' Produces the final animal trait table by combining three sources, in this
#' order of priority: manually coded values from the mtable, then values
#' inherited from the parent species, then the database-resolved values.
#'
#' @section Row identity:
#' `species` is binomial and is the key used to match the mtable.
#' `taxa` is the unique row key: `taxa == species` marks a species-level row
#' (the "parent"), and `taxa != species` (a trinomial) marks a subspecies row
#' that came in with `concern_subsp_epbc`. `species` therefore repeats by
#' design, so check duplicates on `taxa`.
#'
#' @section What happens:
#' 1. **Inherit.** Subspecies rows fill their NA cells from the parent
#'    species-level row (`inherit_cols`). Parent rows are never changed.
#' 2. **Override.** For every manually coded column in the mtable
#'    (`mtable_cols`), the mtable value wins wherever it is non-NA. Matching
#'    is by `species`, so a species coded once in the mtable is applied to all
#'    its subspecies rows. This is also how species not in `animaldb` (no
#'    parent row) get their values.
#' 3. **Restore types.** `readr::type_convert()` is applied once at the end,
#'    since both inputs are all-character.
#'
#' @section Why `context_cols` and `no_inherit` are separate:
#' `context_cols` (e.g. `epbc`) are excluded from BOTH inheriting and the mtable
#' override. They describe the specific taxon, so a subspecies must keep its own
#' value; the mtable holds one value per species and would overwrite it.
#' `no_inherit` columns (e.g. `taxa`, `kingdom`) are excluded from inheriting
#' only. Add any other subspecies-specific columns that arrive with
#' `concern_subsp_epbc` here (listing status, dates, etc.).
#'
#' @section Notes:
#' - Mtable columns missing from `animal_traits_resolved` (e.g.
#'   `extinct_mainland`)
apply_manual_overrides_animal <- function(
    animal_traits_resolved,
    animal_mtable,
    id_col = "species",
    context_cols = c("ala_class", "ala_order", "ala_vernacular_name", "epbc"),
    no_inherit = c("taxa", "kingdom")
) {
  # columns a subspecies may inherit from its parent species row
  inherit_cols <- setdiff(names(animal_traits_resolved),
                          c(id_col, context_cols, no_inherit))
  # columns coded in the mtable (manual values win)
  mtable_cols <- setdiff(names(animal_mtable), c(id_col, context_cols))
  
  # 1. subspecies (taxa != species) inherit from the parent row (taxa == species)
  parents <- animal_traits_resolved |>
    dplyr::filter(taxa == .data[[id_col]]) |>
    dplyr::distinct(.data[[id_col]], .keep_all = TRUE)
  p_idx <- match(animal_traits_resolved[[id_col]], parents[[id_col]])
  
  out <- animal_traits_resolved |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(inherit_cols),
        \(x) dplyr::coalesce(x, parents[[dplyr::cur_column()]][p_idx])
      )
    )
  
  # 2. manual values win, matched by species so subspecies get the species' coding
  m_idx <- match(out[[id_col]], animal_mtable[[id_col]])
  for (col in mtable_cols) {
    base <- if (col %in% names(out)) out[[col]] else NA_character_
    out[[col]] <- dplyr::coalesce(animal_mtable[[col]][m_idx], base)
  }
  
  # 3. restore types once
  readr::type_convert(out)
}