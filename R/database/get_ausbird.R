get_ausbird <- function(ausbird) {
  
  ausbird <- dplyr::as_tibble(ausbird)
  
  # ---- identify habitat columns ----
  breeding_cols <- grep("BreedingHabitat", names(ausbird), value = TRUE)
  feeding_cols  <- grep("FeedingHabitat",  names(ausbird), value = TRUE)
  
  # ---- filter and replace NA with 0 (migratory birds) ----
  ausbird_proc <- ausbird |>
    dplyr::filter(Extinct4 == 0) |>
    dplyr::filter(is.na(SubspeciesName2)) |>  # most subspecies have no data
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(breeding_cols, feeding_cols, "NonBreedingOnly4")),
        ~ tidyr::replace_na(., 0)
      )
    ) |>
    dplyr::mutate(
      # ---- calculate habitat breadth ----
      BreedingHB = rowSums(dplyr::across(dplyr::all_of(breeding_cols)) == 1, na.rm = TRUE),
      FeedingHB  = rowSums(dplyr::across(dplyr::all_of(feeding_cols))  == 1, na.rm = TRUE)
    )
  
  return(ausbird_proc)
}