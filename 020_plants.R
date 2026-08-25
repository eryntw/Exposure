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

tar_plan(
  
  ## Austraits
  
  tar_target(
    austraits,
    fs::path("/mnt/envshare/data/traits/raw/austraits/austraits-7.0.0.rds"),
    format = "file"
  ),
  
  tar_target(
    plant_trait_raw,
    extract_austraits_matrix(
      austraits_traits = readRDS(austraits)$traits,
      taxa = concern$species[concern$ala_class == "Equisetopsida"],
      traits = c("seedbank_longevity_class",
                 "seedbank_longevity",
                 "bud_bank_location",
                 "seed_dry_mass",
                 "reproductive_maturity",
                 "dispersal_syndrome",
                 "plant_growth_form",
                 "leaf_mass_per_area",
                 "recruitment_time",
                 "pollination_syndrome",
                 "plant_physical_defence_structures"
      )
    )
  )
  
)
