## ---- bind manual + source tables into one table for resolvers ----------------

#' Joins animal_mtable's manual_var columns onto animaldb by id_col, producing
#' a single bound table that all resolver functions operate on (mirrors the
#' plant pipeline's single-`data`-table pattern)
bind_animal_traits <- function(animal_mtable, animaldb, id_col = "species") {
  animaldb |>
    dplyr::left_join(
      animal_mtable |>
        dplyr::select(
          dplyr::all_of(id_col),
          hab_list_lit, dispersal_ability, breed_freq_lit, refuge_use_lit,
          oldgrowth_lit, connectivity_lit, burrow_lit, nest_sbs_lit,
          nest_location_lit, forage_ground_lit, bodymass_lit, flight_height_lit,
          bat_wingload_lit, forage_behav_lit, activity_lit, ubag_tolerance_lit,
          plant_diet_prop_lit, trophic_lit,
        ),
      by = id_col,
    )
}

#' Registry of animal trait resolver functions.
#' Each function takes the bound table and returns it with ONE new column
#' added — manual_var (literature-coded override) coalesced ahead of the
#' source_var hierarchy from animaldb, in coalesce-priority order.
animal_trait_resolver <- list(
  
  habitat_count = \(data) {
    data |>
      dplyr::mutate(
        habitat_count = dplyr::coalesce(hab_list_lit, iucn_habitat_count_mj)
      )
  },
  
  # no source_var hierarchy — manual coding is the sole value, passed through
  # unchanged so it lives alongside the other resolved columns downstream
  dispersal_ability = \(data) {
    data |>
      dplyr::mutate(
        dispersal_ability = dispersal_ability
      )
  },
  
  breed_resolved = \(data) {
    data |>
      dplyr::mutate(
        breed_resolved = dplyr::coalesce(
          breed_freq_lit,
          bb_Brd2,
          pan_LittersPerYear,
          rep_NumberOfLittersOrClutchesProducedPerYear |> as.numeric(),
        )
      )
  },
  
  structure_refuge_resolved = \(data) {
    data |>
      dplyr::mutate(
        structure_refuge_resolved = dplyr::case_when(
          !is.na(refuge_use_lit) | stringr::str_detect(bb_NestType, "CV") |
            aub_NestLocationHollow12 == 1 ~ 1,
          !is.na(bb_NestType) | !is.na(aub_NestLocationHollow12) ~ 0,
        )
      )
  },
  
  # no source_var hierarchy — manual coding only
  oldgrowth_lit = \(data) {
    data |>
      dplyr::mutate(
        oldgrowth_lit = oldgrowth_lit
      )
  },
  
  connectivity_resolved = \(data) {
    data |>
      dplyr::mutate(
        connectivity_resolved = dplyr::coalesce(connectivity_lit, bb_Mig)
      )
  },
  
  burrow_resolved = \(data) {
    data |>
      dplyr::mutate(
        structure_refuge_resolved = dplyr::case_when(
          burrow_lit == 1 | stringr::str_detect(bb_NestType, "BU") ~ 1,
          TRUE ~ 0
        )
      )
  },
  
  # source_var only — no manual override defined for this trait
  arid_prop_koppenclim = \(data) {
    data |>
      dplyr::mutate(
        arid_prop_koppenclim = clim_desert_prop
      )
  },
  
  structure_dependency_resolved = \(data) {
    data |>
      dplyr::mutate(
        # does the code list contain T and/or S at all?
        has_st = stringr::str_detect(bb_NestSbs, "T|S"),
        # does the code list contain ONLY T/S (no other codes)?
        only_st = stringr::str_detect(bb_NestSbs, "^[TS,]+$"),
        
        structure_dependency_resolved = dplyr::case_when(
          nest_sbs_lit == "High" | eltm_ForStrat.Value == "Ar" | only_st ~ "High",
          nest_sbs_lit == "Moderate" | (has_st & !only_st) ~ "Moderate",
          nest_sbs_lit == "Low" | !has_st ~ "Low",
        )
      )
  },
  
  nest_location_ground_resolved = \(data) {
    data |>
      dplyr::mutate(
        nest_location_ground_resolved = dplyr::coalesce(
          nest_location_lit,
          aub_NestLocationGroundLevel12,
        )
      )
  },
  
  ground_foraging_resolved = \(data) {
    data |>
      dplyr::mutate(
        forage_ground = dplyr::case_when(
          eltm_ForStrat.Value %in% c("G", "S") | amp_Ter == 1 ~ 100,
          eltm_ForStrat.Value == "Ar" ~ 30,
          !is.na(eltm_ForStrat.Value) | !is.na(amp_Ter) ~ 0,
        ),
        ground_foraging_resolved = dplyr::coalesce(
          forage_ground_lit,
          eltb_ForStrat.ground,
          forage_ground,
        )
      )
  },
  
  # source_var only — ALA taxonomic class, no manual override
  taxonomic_class = \(data) {
    data |>
      dplyr::mutate(
        taxonomic_class = ala_class
      )
  },
  
  body_mass_value = \(data) {
    data |>
      dplyr::mutate(
        body_mass_value = dplyr::coalesce(
          bodymass_lit,
          ws_Mass_AVONET,
          eltm_BodyMass.Value,
          as.numeric(rep_MaximumBodyMassG),
          amp_Body_mass_g,
        )
      )
  },
  
  flight_height_resolved = \(data) {
    data |>
      dplyr::mutate(
        flight_height_resolved = dplyr::coalesce(flight_height_lit, avis_Exp)
      )
  },
  
  maneuverability_resolved = \(data) {
    data |>
      dplyr::mutate(
        wingspan = dplyr::coalesce(ws_wingspan_reported_mid_all,
                                    ws_wingspan_predicted_global),
        bird_wingload_ellipsetriangle = 
          1/3*ws_wing_length_cm_AVONET*ws_Secondary1_AVONET*(pi+1) + # inner segment
          (wingspan-2*ws_wing_length_cm_AVONET)*ws_Secondary1_AVONET, # outer segment
        bird_wingload_ellipse = 
          1/2*pi*ws_wing_length_cm_AVONET*ws_Secondary1_AVONET*(pi+1) + # inner segment
          (wingspan-2*ws_wing_length_cm_AVONET)*ws_Secondary1_AVONET, # outer segment
        maneuverability_resolved = dplyr::coalesce(
          bird_wingload_ellipsetriangle,
          bat_wingload_lit
        )
      )
  },
  
  lake_effect_resolved = \(data) {
    data |>
      dplyr::mutate(
        lake_effect_resolved = dplyr::case_when(
          forage_behav_lit == "high" | ws_Primary.Lifestyle_AVONET == "Aquatic" ~ "High",
          forage_behav_lit == "moderate" |
            stringr::str_detect(iucn_habitat_major_list, 
                                stringr::fixed("Wetlands (inland)")) ~ "Moderate",
          TRUE ~ "Low",
        )
      )
  },
  
  activity_period_resolved = \(data) {
    data |>
      dplyr::mutate(
        activity_period_resolved = dplyr::case_when(
          activity_lit == "nocturnal" | eltb_Nocturnal == 1 | 
            eltm_Activity.Nocturnal == 1 |
            amp_Noc == 1 | rep_ActiveTime == "Nocturnal" ~ "nocturnal",
          activity_lit == "diurnal" | eltb_Nocturnal == 0 | 
            eltm_Activity.Diurnal == 1 |
            amp_Diu == 1 | rep_ActiveTime == "Diurnal" ~ "diurnal",
          activity_lit == "crepuscular" | eltb_Nocturnal == 0 | 
            eltm_Activity.Crepuscular == 1 |
            amp_Crepu == 1 | rep_ActiveTime == "Cathemeral" ~ "crepuscular",
        )
      )
  },
  
  matrix_tolerance_resolved = \(data) {
    data |>
      dplyr::mutate(
        breedfeedsum = aub_FeedingHabitatAgriculturalLandscapes9 +
          aub_FeedingHabitatUrbanLandscapes9 +
          aub_BreedingHabitatAgriculturalLands9 +
          aub_BreedingHabitatUrban9,
        breedsum = aub_BreedingHabitatAgriculturalLands9 +
          aub_BreedingHabitatUrban9,
        feedsum = aub_FeedingHabitatAgriculturalLandscapes9 +
          aub_FeedingHabitatUrbanLandscapes9,
        bird_ubag_tolerance = dplyr::case_when(
          breedfeedsum == 0 ~ "Low",
          breedsum > 0 ~ "Moderate",
          breedsum > 0 & feedsum > 0 ~ "High"
          ),
        matrix_tolerance_resolved = dplyr::coalesce(
          ubag_tolerance_lit,
          bird_ubag_tolerance,
        )
      )
  },
  
  # Bird & mammal plant diet proportion tiers (50|80%); Reptiles: Herbivore, Omnivore, Carnivore
  plant_diet_prop = \(data) {
    data |>
      dplyr::mutate(
        plant_diet_prop = dplyr::coalesce(
          eltb_Diet.Nect + eltb_Diet.PlantO + eltb_Diet.Fruit + eltb_Diet.Seed,
          eltm_Diet.Nect + eltm_Diet.PlantO + eltm_Diet.Fruit + eltm_Diet.Seed,
        ),
        plant_diet_prop_resolved = dplyr::case_when(
          !is.na(plant_diet_prop_lit) ~ stringr::str_to_title(plant_diet_prop_lit),
          plant_diet_prop >= 80 ~ "High",
          plant_diet_prop >= 50 & plant_diet_prop < 80 ~ "Medium",
          plant_diet_prop > 0 & plant_diet_prop < 50 ~ "Low",
          rep_Diet == "Herbivorous" ~ "High",
          rep_Diet == "Omnivorous" ~ "Medium",
          rep_Diet == "Carnivorous" ~ "Low",
        )
      )
  },
  
  trophic_level_resolved = \(data) {
    data |>
      dplyr::mutate(
        trophic_level_resolved = dplyr::case_when(
          pan_TrophicLevel == 3 | ws_Trophic.Level_AVONET == "Carnivore" |
            rep_Diet == "Carnivorous" | trophic_lit == "carnivore" ~ "Lv4",
          pan_TrophicLevel == 2 | ws_Trophic.Level_AVONET == "Omnivore" |
            rep_Diet == "Omnivorous" | trophic_lit == "omnivore" ~ "Lv3", 
          pan_TrophicLevel == 1 | ws_Trophic.Level_AVONET == "Herbivore" |
            rep_Diet == "Herbivorous" | trophic_lit == "herbivore" ~ "Lv2", 
          trophic_lit == "apex" ~ "Lv5"
            
        ) 
      )
  }
  
)

## ---- driver ------------------------------------------------------------------

#' Runs all animal trait resolver, adding their output columns to the bound table
resolve_all_animal_traits <- \(data, resolvers = animal_trait_resolver) {
  purrr::reduce(resolvers, \(d, fn) fn(d), .init = data)
}