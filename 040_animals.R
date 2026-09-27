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

tar_plan(
  
  ## PROCESSED ------
  
  tar_target(
    animal_traits_resolved,
    resolve_animal_traits(animaldb) |> 
      dplyr::select(dplyr::matches("_resolved$|_lit$")),
  ),
  
  # Construct mtable for imputation:
  # New species and cols append;
  # Each species must be unique; 
  # The imputed value always win and will not be replaced by updated database value;
  tar_target(
    animal_mtable_path,
    create_or_update_mtable_animal(
      animal_traits_resolved,
      path = fs::path("data", "animal_mtable.csv"),
    ),
    format = "file",
  ),
  
  ## IMPUTED ------
  tar_target(
    animal_mtable,
    readr::read_csv(
      animal_mtable_path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    ),
  ),
  
  tar_target(
    animal_traits_final,
    apply_manual_overrides_animal(animal_traits_resolved, 
                                  animal_mtable, 
                                  id_col = "species"),
  ),

  ## SCORED ------
  
)
