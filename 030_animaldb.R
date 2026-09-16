library(targets)
library(tarchetypes)
library(geotargets)
library(crew)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 5))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()
tar_source("../Status/R/iucn")

# targets -------

concern <- tar_read(concern, store = tars$setup$store)
rec_summary_pilot <- tar_read(rec_summary_pilot, store = tars$setup$store)
climate_summary <- tar_read(climate_summary, store = tars$RecExtract$RecExtr$store)
db_dir <- "/mnt/envshare/data/traits/raw"

tar_plan(
  
  #####################
  ## Bird database ----
  #####################
  
  # birdbase
  tarchetypes::tar_file_read(
    birdbase,
    fs::path(db_dir, "bird", "cleaned", "BIRDBASE_data.csv"),
    readr::read_csv(!!.x) |>
      clean_exp_df(
        commoncol = EnglishNameBirdLifeIocClementsAviList,
        clean_names = TRUE,
        clean_names_case = "upper_camel",
        post_fn = get_birdbase,
        select = c(
          dplyr::any_of(c("Brd2", "NestType", "Mig", "NestSbs")),
          dplyr::contains("Wt")
        )
      )
  ),
  
  # avistep
  tarchetypes::tar_file_read(
    avistep,
    fs::path(db_dir, "bird", "AVISTEP", "AVISTEP_Australia_Onshore.xlsx"),
    readxl::read_excel(!!.x, sheet = "Data") |> 
      clean_exp_df(
        taxacol = ScientificName,
        commoncol = CommonName,
        clean_names = TRUE,
        clean_names_case = "upper_camel",
        select = c(dplyr::any_of("Exp"))
      )
  ),
  
  # wingspan
  tarchetypes::tar_file_read(
    wingspan,
    fs::path(db_dir, "bird", "wingspan_data.xlsx"),
    readxl::read_excel(!!.x, sheet = "wingspan data") |> 
      clean_exp_df(
        taxacol = Species1,
        select  = c(
          dplyr::any_of(c(
            "wingspan_reported_mid_all", "wingspan_predicted_global",
            "wingspan_predicted_order", "wingspan_hanzab_mid_point")),
          dplyr::matches("Trophic|Habitat.Density|Migration|
                           Primary.Lifestyle|Secondary1|
                           Wing.Length|Mass_")
        )
      )
  ),
  
  ## Ausbird
  tar_target(
    ausbird,
    traitdata::australian_birds |>
      setNames(gsub("^X\\d+_?", "", names(traitdata::australian_birds))) |> 
      clean_exp_df(
        commoncol = TaxonCommonName2,
        clean_names = TRUE,
        clean_names_case = "upper_camel",
        post_fn = get_ausbird,
        select = c(
          dplyr::matches("UrbanLands|Agricultural|NestLocationGround")
        )
      )
  ),
  
  ## Elton_Bird
  tar_target(
    elt_bird,
    traitdata::elton_birds |> 
      clean_exp_df(
        select = c(
          commoncol = "English",
          dplyr::contains("Diet"),
          dplyr::any_of("Noctornal")
        )
      )
  ),
  
  ########################
  ## Mammal database ----
  ########################

  ## Elton_Mammal
  tar_target(
    elt_mml,
    traitdata::elton_mammals |> 
      clean_exp_df(
        select = c(
          dplyr::matches("Diet|Activity")
        )
      )
  ),
  
  ## Pantheria
  tar_target(
    pantheria,
    traitdata::pantheria |> 
      clean_exp_df(
        select = c(
          dplyr::any_of(c("DietBreadth", "HabitatBreadth", 
                          "LitterSize", "LittersPerYear", "InterbirthInterval_d",
                          "TrophicLevel", "DispersalAge_d", "Terrestriality",
                          "AgeatFirstBirth_d"))
        )
      )
  ),
  
  ########################
  ## Reptile database ----
  ########################
  
  # repttraits
  tarchetypes::tar_file_read(
    reptrait,
    fs::path(db_dir, "reptile","ReptTraits_v1.2.xlsx"), 
    readxl::read_excel(!!.x, sheet = "Data") |> 
      clean_exp_df(
        clean_names = TRUE,
        clean_names_case = "upper_camel",
        select = c(
          dplyr::matches("habitat|Diet|Foraging|BodyMass|Litters")
        )
      )
  ),
  
  ########################
  ## Pooled database ----
  ########################
  
  ## Habitat List - IUCN ----
  targets::tar_target(name = iucn_habitat,
                      command = map_iucn_data(
                        concern |> dplyr::filter(kingdom == "Animalia"),
                        query_fn = get_iucn_habitat,
                        pause = 1,
                        max_retries = 5)
  ),
  
  # major habitat count, excluding "Unknown" suitability
  tar_target(
    iucn_mjHB,
    summarise_iucn_habitat(iucn_habitat[["iucn_data"]]) |> 
      clean_exp_df(taxacol = scientific_name)
  ),
  
  ## Arid specialist ------
  # proportion of climate records in desert categories (codes 0-9)
  tar_target(
    climate_desert_prop,
    summarise_climate_prop(climate_summary,
                           dplyr::matches("^climate_[0-9]$"),
                           prefix = "desert") |> 
      clean_exp_df(
        taxacol = taxa,
        select = c(
          dplyr::contains("desert"),
          dplyr::any_of("total_climate")
        )
      )
  ),
  
  ########################
  ## Join ----
  ########################
  
  tar_target(name = syn_db, 
             command = match_synonym(concern$taxa)
  ),
  
  tar_target(
    animaldb,
    concern |>
      join_database_(birdbase, prefix = "bb_", syn_db = syn_db) |>
      join_database_(avistep, prefix = "avis_", syn_db = syn_db) |>
      join_database_(wingspan, prefix = "ws_", syn_db = syn_db) |>
      join_database_(ausbird, prefix = "bub_", syn_db = syn_db) |> 
      join_database_(elt_bird, prefix = "eltb_", syn_db = syn_db) |>
      join_database_(elt_mml, prefix = "eltm_", syn_db = syn_db) |>
      join_database_(pantheria, prefix = "pan_", syn_db = syn_db) |>
      join_database_(reptrait, prefix = "rep_", syn_db = syn_db) |>
      join_database_(iucn_mjHB, prefix = "iucn_", syn_db = syn_db) |>
      join_database_(climate_desert_prop, prefix = "clim_", syn_db = syn_db)
  )
)

