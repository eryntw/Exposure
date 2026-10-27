## join_plant_imputed.R ##
## 1. Keep the imputed_value_use columns and rename them to full trait names.
## 2. Join them onto raw (restricted to `concern` species if given). The raw
##    version of each of these traits is kept as "<name>_raw"; the imputed
##    values take the full trait name.
## 3. Sort the trait columns so each raw/imputed pair sits side by side.
## 4. Left join onto `concern`, which has one row per species or subspecies:
##      species = parent binomial (matches taxon_name in raw/imputed)
##      taxa    = binomial or trinomial; becomes the final taxon_name
##    Subspecies therefore inherit their parent species' trait values.

join_plant_imputed <- function(raw = NULL,
                               imputed = NULL,
                               concern = NULL) {
  
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
  
  ## imputed values only for parent species in concern ##
  if (!is.null(concern)) {
    imputed <- dplyr::semi_join(imputed, concern, by = c("taxon_name" = "species"))
  }
  
  ## "imputed_value_use.SDM" or "SDM_imputed_value_use" -> "seed_dry_mass" ##
  use_cols   <- grep("imputed_value_use", names(imputed), value = TRUE)
  acronyms   <- stringr::str_remove_all(use_cols, "_?imputed_value_use[._]?")
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
  
  ## concern drives the rows: species and subspecies both pick up the parent
  ## species' trait values via `species`, then `taxa` becomes taxon_name ##
  if (!is.null(concern)) {
    raw <- concern |>
      dplyr::left_join(raw, by = c("species" = "taxon_name"), relationship = "many-to-one") |>
      dplyr::mutate(taxon_name = dplyr::coalesce(taxa, species)) |>
      dplyr::select(-taxa) |>
      dplyr::relocate(taxon_name, species)
  }
  
  raw |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, digits = 2)))
}

## Target ##
# tarchetypes::tar_file_read(
#   name = plant_trait_imputed,
#   command = "/mnt/envshare/dev/out/Trait_impute/_targets/impute_traits/objects/exp_impute",
#   read = readRDS(!!.x) |>
#     join_plant_imputed(
#       raw     = plant_trait_raw$matrix,
#       concern = dplyr::filter(concern, kingdom == "Plantae")
#     )
# ),