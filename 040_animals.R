library(targets)
library(tarchetypes)
library(geotargets)
library(crew)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 50))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()
tar_source("../Status/R/setup")

# targets -------

concern <- tar_read(concern, store = tars$setup$store)
animaldb <- tar_read(animaldb, store = tars$animaldb$store)
concern_subsp_epbc <- tar_read(concern_subsp_epbc, store = tars$setup$store)

tar_plan(
  
  ## PROCESSED ------
  
  # Full table: resolved database species + ALL concern_subsp_epbc rows
  tar_target(
    animal_traits_resolved,
    animaldb |>
      resolve_animal_traits() |>
      dplyr::mutate(taxa = species) |>
      dplyr::bind_rows(concern_subsp_epbc) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  ),
  
  # Mtable: one row per species. Database species first, so subspecies of
  # species already in animaldb are dropped here (not imputed). New species
  # not in animaldb stay and are coded manually.
  tar_target(
    animal_mtable_path,
    animal_traits_resolved |>
      dplyr::filter(is.na(kingdom) | kingdom == "Animalia") |>
      dplyr::distinct(species, .keep_all = TRUE) |>
      create_or_update_mtable_animal(path = fs::path("data", "animal_mtable.csv")),
    format = "file"
  ),
  
  ## IMPUTED ------
  tar_target(
    animal_mtable,
    readr::read_csv(animal_mtable_path, col_types = readr::cols(.default = "c"))
  ),
  
  tar_target(
    animal_traits_final,
    apply_manual_overrides_animal(
      animal_traits_resolved, animal_mtable, id_col = "species"
    )
  ), 
  
  ## SCORED ------
  
  tarchetypes::tar_file_read(
    iMeta_animal,
    "/mnt/envshare/data/traits/meta/ExposureTraits.xlsx", 
    readxl::read_excel(!!.x, sheet = "iMeta_animal")
  )
)