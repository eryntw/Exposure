## join_plant_imputed.R ##
## Rename the *imputed_value_use columns to full trait names and join them
## onto raw$matrix. The raw version of each of these traits is kept
## as "<name>_raw"; the imputed values take the full trait name.

join_plant_imputed <- function(raw = NULL, 
                               imputed = NULL) {
  
  ## acronym -> full trait name (matches raw$matrix) ##
  name_map <- c(
    SDM = "seed_dry_mass",
    LMA = "leaf_mass_per_area",
    SL  = "seedbank_longevity",
    ReM = "reproductive_maturity",
    DS  = "dispersal_syndrome",
    BBL = "bud_bank_location",
    SLC = "seedbank_longevity_class",
    SEC = "seedling_establishment_conditions",
    PS  = "pollination_syndrome",
    PPD = "plant_physical_defence_structures"
  )
  
  ## "SDM_imputed_value_use" or "imputed_value_use_SDM" -> "seed_dry_mass" ##
  use_cols   <- grep("imputed_value_use", names(imputed), value = TRUE)
  acronyms   <- stringr::str_remove_all(use_cols, "_?imputed_value_use._?")
  full_names <- unname(name_map[acronyms])
  
  imputed <- imputed |>
    dplyr::rename(!!!stats::setNames(use_cols, full_names))
  
  raw$matrix <- raw$matrix |>
    dplyr::rename_with(\(x) paste0(x, "_raw"), dplyr::any_of(full_names)) |>
    dplyr::left_join(imputed, by = "taxon_name")
  
  raw
}
