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

tar_plan(
  
  ## Concerned and final species ------
  
  ## Track PIA output ----
  tar_target(cpath_usg, 
             fs::path(tars$envPIA_b$setup$store, "objects", "concern"), 
             format = "file"),
  tar_target(fpath_usg, 
             fs::path(tars$envPIA_b$final$store, "objects", "final"), 
             format = "file"),
  
  tar_target(cpath_bp, update_boundary_path(cpath_usg, 2), format = "file"),
  tar_target(fpath_bp, update_boundary_path(fpath_usg, 2), format = "file"),
  
  ## Read ----
  tar_target(concern,
             dplyr::bind_rows(
               readRDS(cpath_usg),
               readRDS(cpath_bp)
             ) |>
               dplyr::distinct() |> 
               classify_species(search_term_col = "species")),
  
  tar_target(final,
             dplyr::bind_rows(
               readRDS(fpath_usg),
               readRDS(fpath_bp)
             ) |>
               dplyr::distinct()),
  
  ## Spatial data from RecExtract ------
  
  tar_target(
    rec_summary_path,
    fs::path(tars$RecExtract$RecExtr$store, "objects", "rec_summary"),
    format = "file"
  ),
  
  tar_target(
    rec_summary_pilot, # Species Level for Trait Mapping
    {
      rec_summary_combined <- readRDS(rec_summary_path) |> 
        combine_named_list_prefixed(id_cols = "taxa")
      dplyr::left_join(concern, rec_summary_combined, 
                       by = c("species" = "taxa")) # 23 spp have no summary
    }
  )
)