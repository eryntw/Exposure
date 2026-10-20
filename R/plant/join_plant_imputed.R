## join_plant_imputed.R ##
## 1. Keep the imputed_value_use columns and rename them to full trait names.
## 2. Join them onto raw (restricted to `concern` species if given). The raw
##    version of each of these traits is kept as "<name>_raw"; the imputed
##    values take the full trait name.
## 3. Sort the trait columns so each raw/imputed pair sits side by side.
## 4. Left join the species-level `concern` columns (they stay at the end).
## 5. Optionally add subspecies rows (`subsp`) that copy their parent species' row.

join_plant_imputed <- function(raw = NULL,
                               imputed = NULL,
                               concern = NULL,
                               subsp = NULL) {
  
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
  
  ## keep only the id and the imputed columns (drops value_use.*, ori.* etc.) ##
  imputed <- dplyr::select(imputed, taxon_name, dplyr::contains("imputed_value_use"))
  
  ## imputed values only for concern species ##
  if (!is.null(concern)) {
    imputed <- dplyr::semi_join(imputed, concern, by = c("taxon_name" = "species"))
  }
  
  ## "SDM_imputed_value_use" or "imputed_value_use_SDM" -> "seed_dry_mass" ##
  use_cols   <- grep("imputed_value_use", names(imputed), value = TRUE)
  acronyms   <- stringr::str_remove_all(use_cols, "_?imputed_value_use._?")
  full_names <- unname(name_map[acronyms])
  
  imputed <- imputed |>
    dplyr::rename(!!!stats::setNames(use_cols, full_names))
  
  raw <- raw |>
    dplyr::rename_with(\(x) paste0(x, "_raw"), dplyr::any_of(full_names)) |>
    dplyr::left_join(imputed, by = "taxon_name")
  
  ## sort: id first, then alphabetical by trait, raw before imputed ##
  is_raw <- stringr::str_detect(names(raw), "_raw$")
  base   <- stringr::str_remove(names(raw), "_raw$")
  raw    <- raw[order(names(raw) != "taxon_name", base, !is_raw, method = "radix")]
  
  ## concern columns first; concern drives the rows ##
  if (!is.null(concern)) {
    raw <- concern |>
      dplyr::left_join(raw, by = c("species" = "taxon_name")) |>
      dplyr::rename(taxon_name = species) |>
      dplyr::relocate(taxon_name)
  }
  ## subspecies: copy the parent species' row (`species`) under the subspecies name (`taxa`) ##
  if (!is.null(subsp)) {
    new_rows <- subsp |>
      dplyr::filter(!taxa %in% raw$taxon_name) |>   # leave any already present untouched
      dplyr::distinct(taxa, species) |>
      dplyr::inner_join(raw, by = c(species = "taxon_name")) |>
      dplyr::select(-species) |>
      dplyr::rename(taxon_name = taxa)
    
    raw <- dplyr::bind_rows(raw, new_rows)
  }
  
  raw <-  raw |>  
    mutate(across(where(is.numeric), round, digits = 2))
}

## Target ##
# tarchetypes::tar_file_read(
#   name = plant_trait_imputed,
#   command = "/mnt/envshare/dev/out/Trait_impute/_targets/impute_traits/objects/exp_impute",
#   read = readRDS(!!.x) |>
#     join_plant_imputed(
#       raw     = plant_trait_raw$matrix,
#       concern = concern,
#       subsp   = dplyr::filter(concern_subsp_epbc, kingdom == "Plantae")
#     )
# ),