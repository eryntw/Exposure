#' Coalesces manually-coded values (animal_mtable) onto database-derived
#' resolved traits — manual value wins wherever it's non-NA. No var_map
#' needed: animal_mtable uses the same column names as animal_traits_resolved
#' by construction (built via create_or_update_mtable_animal()).
apply_manual_overrides_animal <- function(animal_traits_resolved, animal_mtable, id_col = "species") {
  
  trait_cols <- intersect(
    names(animal_mtable),
    animal_traits_resolved |> dplyr::select(dplyr::matches("_resolved$|_lit$")) |> names()
  )
  
  mtable_j <- animal_traits_resolved |>
    dplyr::select(dplyr::all_of(id_col)) |>
    dplyr::left_join(animal_mtable |> dplyr::select(dplyr::all_of(c(id_col, trait_cols))), by = id_col)
  
  stopifnot(nrow(mtable_j) == nrow(animal_traits_resolved))
  
  trait_cols |>
    purrr::reduce(
      \(data, col) dplyr::mutate(data, "{col}" := dplyr::coalesce(mtable_j[[col]], .data[[col]])),
      .init = animal_traits_resolved,
    )
}