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
rec_summary_pilot <- tar_read(rec_summary_pilot, store = tars$setup$store)

tar_plan(
  
  ## PROCESSED ------
  
  # Full table: resolved database species
  tar_target(
    animal_trait_resolved,
    animaldb |>
      resolve_animal_traits() |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  ),
  
  # Mtable: one row per species. Database species first, so subspecies of
  # species already in animaldb are dropped here (not imputed). New species
  # not in animaldb stay and are coded manually.
  tar_target(
    animal_mtable_path,
    animal_trait_resolved |>
      dplyr::distinct(species, .keep_all = TRUE) |>
      create_or_update_mtable_animal(
        path = fs::path("data", "animal_mtable.csv"),
        id_col = "species",
        context_cols = c("class", "order", "common", "epbc_rating")),
    format = "file"
  ),
  
  ## IMPUTED ------
  tar_target(
    animal_mtable,
    readr::read_csv(animal_mtable_path, col_types = readr::cols(.default = "c"))
  ),
  
  tar_target(
    animal_trait_final,
    apply_animal_overrides(
      animal_trait_resolved,
      animal_mtable,
      id_col = "species",
      context_cols = c("class", "order", "common", "epbc_rating"),
      no_inherit = c("taxa", "kingdom"),
      add_missing_traits = TRUE
    )
  ),
  
  ## SCORED ------
  tarchetypes::tar_file_read(
    Rule_animal,
    "/mnt/envshare/data/traits/meta/ExposureTraits.xlsx", 
    readxl::read_excel(!!.x, sheet = "Rule_animal")
  ),
  
  tarchetypes::tar_file_read(
    iMeta_animal,
    "/mnt/envshare/data/traits/meta/ExposureTraits.xlsx", 
    readxl::read_excel(!!.x, sheet = "iMeta_animal")
  ), 
  
  tar_target(
    animal_trait_scored,
    {
      proc_vars <- Rule_animal$processed_var |>
        stringr::str_squish() |>
        unique()
      
      missing_vars <- setdiff(proc_vars, names(animal_trait_final))
      if (length(missing_vars) > 0) {
        stop("Rule_animal processed_var missing from animal_trait_final: ",
             paste(missing_vars, collapse = ", "))
      }
      
      animal_trait_final |> 
        dplyr::select(dplyr::any_of(c("taxa", "species", 
                                      "class", "common",
                                      proc_vars))) |> 
        recode_scores_animal(
          low      = c("irruptive", "once a year", "diurnal", "Lv2"),
          moderate = c("dependent", "crepuscular", "cathemeral", "Lv3"),
          high     = c("continuous", "nocturnal","Lv4"),
          very_high = "Lv5",
          zero     = "No exposure",
          rules = Rule_animal
        )
    }
  ) ## ==> Join Status to form iCode_animal
)