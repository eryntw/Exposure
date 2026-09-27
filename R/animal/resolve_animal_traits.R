#' Registry of animal trait resolver functions — DATABASE-DERIVED ONLY.
#' No manual/"_lit" columns are referenced; each function takes animaldb
#' directly (no join to animal_mtable at this stage) and returns it with ONE
#' new column added. NA results in these outputs are exactly the rows that
#' need manual coding — see flag_missing_traits().
animal_trait_resolvers <- list(
  
  habitat_count = \(data) {
    data |>
      dplyr::mutate(
        habitat_count = iucn_habitat_count_mj,
        hb_lit = NA
      )
  },
  
  # was manual-only ("_lit" passthrough) — no database column exists for
  # this trait, so it's a pure NA placeholder awaiting manual coding
  dispersal = \(data) {
    data |>
      dplyr::mutate(
        dispersal_lit = NA
      )
  },
  
  breed_freq_resolved = \(data) {
    data |>
      dplyr::mutate(
        breed_freq_resolved = dplyr::coalesce(
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
          stringr::str_detect(bb_NestType, "CV") | aub_NestLocationHollow12 == 1 ~ 1,
          !is.na(bb_NestType) | !is.na(aub_NestLocationHollow12) ~ 0,
        )
      )
  },
  
  # manual-only — no database fallback for this trait
  oldgrowth_lit = \(data) {
    data |>
      dplyr::mutate(
        oldgrowth_lit = NA
      )
  },
  
  connectivity_resolved = \(data) {
    data |>
      dplyr::mutate(
        connectivity_resolved = bb_Mig
      )
  },
  
  burrow_resolved = \(data) {
    data |>
      dplyr::mutate(
        burrow_resolved = dplyr::case_when(
          stringr::str_detect(bb_NestType, "BU") ~ 1,
          !is.na(bb_NestType) ~ 0,
        )
      )
  },
  
  # source_var only — never had a manual override
  arid_specialist = \(data) {
    data |>
      dplyr::mutate(
        arid_specialist = clim_desert_prop
      )
  },
  
  nest_sbs_resolved = \(data) {
    data |>
      dplyr::mutate(
        has_st = stringr::str_detect(bb_NestSbs, "T|S"),
        only_st = stringr::str_detect(bb_NestSbs, "^[TS,]+$"),
        nest_sbs_resolved = dplyr::case_when(
          eltm_ForStrat.Value == "Ar" | only_st ~ "High",
          has_st & !only_st ~ "Moderate",
          !has_st ~ "Low"
        )
      )
  },
  
  ground_nest_resolved = \(data) {
    data |>
      dplyr::mutate(
        ground_nest_resolved = aub_NestLocationGroundLevel12
      )
  },
  
  ground_forage_resolved = \(data) {
    data |>
      dplyr::mutate(
        forage_ground = dplyr::case_when(
          eltm_ForStrat.Value %in% c("G", "S") | amp_Ter == 1 ~ 100,
          eltm_ForStrat.Value == "Ar" ~ 30,
          !is.na(eltm_ForStrat.Value) | !is.na(amp_Ter) ~ 0,
        ),
        ground_forage_resolved = dplyr::coalesce(eltb_ForStrat.ground, forage_ground)
      )
  },
  
  is_mammal = \(data) {
    data |>
      dplyr::mutate(
        is_mammal = stringr::str_to_lower(ala_class) == "mammalia"
      )
  },
  
  is_flying = \(data) {
    data |>
      dplyr::mutate(
        is_flying = stringr::str_to_lower(ala_class) == "aves" |
          stringr::str_to_lower(ala_order) == "chiroptera"
      )
  },
  
  bodymass_resolved = \(data) {
    data |>
      dplyr::mutate(
        bodymass_resolved = dplyr::coalesce(
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
        flight_height_resolved = dplyr::case_when(
          !is_flying ~ "No exposure",
          is_flying ~ avis_Exp,
        )
      )
  },
  
  wingload_resolved = \(data) {
    data |>
      dplyr::mutate(
        wingspan = dplyr::coalesce(ws_wingspan_reported_mid_all, ws_wingspan_predicted_global),
        # ellipse-triangle model
        bird_wingload_resolved =
          1 / 3 * ws_wing_length_cm_AVONET * ws_Secondary1_AVONET * (pi + 1) + # inner segment
          (wingspan - 2 * ws_wing_length_cm_AVONET) * ws_Secondary1_AVONET, # outer segment
        wingload_resolved = dplyr::case_when(
          !is_flying ~ 0,
          is_flying ~ bird_wingload_resolved,
        )
      )
  },
  
  lake_effect_resolved = \(data) {
    data |>
      dplyr::mutate(
        lake_effect_resolved = dplyr::case_when(
          !is_flying ~ "Low",
          ws_Primary.Lifestyle_AVONET == "Aquatic" ~ "High",
          stringr::str_detect(iucn_habitat_major_list, stringr::fixed("Wetlands (inland)")) ~ "Moderate",
          !is.na(ws_Primary.Lifestyle_AVONET) | !is.na(iucn_habitat_major_list) ~ "Low",
        )
      )
  },
  
  activity_resolved = \(data) {
    data |>
      dplyr::mutate(
        activity_resolved = dplyr::case_when(
          eltb_Nocturnal == 1 | eltm_Activity.Nocturnal == 1 |
            amp_Noc == 1 | rep_ActiveTime == "Nocturnal" ~ "nocturnal",
          eltb_Nocturnal == 0 | eltm_Activity.Diurnal == 1 |
            amp_Diu == 1 | rep_ActiveTime == "Diurnal" ~ "diurnal",
          eltm_Activity.Crepuscular == 1 |
            amp_Crepu == 1 | rep_ActiveTime == "Cathemeral" ~ "crepuscular"
        )
      )
  },
  
  ubag_tolerance_resolved = \(data) {
    data |>
      dplyr::mutate(
        breedfeed_sum = aub_FeedingHabitatAgriculturalLandscapes9 +
          aub_FeedingHabitatUrbanLandscapes9 +
          aub_BreedingHabitatAgriculturalLands9 +
          aub_BreedingHabitatUrban9,
        breedsum = aub_BreedingHabitatAgriculturalLands9 + aub_BreedingHabitatUrban9,
        feedsum = aub_FeedingHabitatAgriculturalLandscapes9 + aub_FeedingHabitatUrbanLandscapes9,
        ubag_tolerance_resolved = dplyr::case_when(
          breedfeed_sum == 0 ~ "Low",
          breedsum > 0 & feedsum > 0 ~ "High",
          breedsum == 0 & feedsum > 0 ~ "Moderate",
          breedsum > 0 & feedsum == 0 ~ "Moderate",
        )
      )
  },
  
  # Bird & mammal plant diet proportion tiers (50|80%); Reptiles: Herbivore, Omnivore, Carnivore
  plant_diet_resolved = \(data) {
    data |>
      dplyr::mutate(
        plant_diet_prop = dplyr::coalesce(
          eltb_Diet.Nect + eltb_Diet.PlantO + eltb_Diet.Fruit + eltb_Diet.Seed,
          eltm_Diet.Nect + eltm_Diet.PlantO + eltm_Diet.Fruit + eltm_Diet.Seed,
        ),
        plant_diet_resolved = dplyr::case_when(
          plant_diet_prop >= 80 ~ "High",
          plant_diet_prop >= 50 & plant_diet_prop < 80 ~ "Medium",
          plant_diet_prop > 0 & plant_diet_prop < 50 ~ "Low",
          rep_Diet == "Herbivorous" ~ "High",
          rep_Diet == "Omnivorous" ~ "Medium",
          rep_Diet == "Carnivorous" ~ "Low",
        )
      )
  },
  
  freshwatersp_resolved = \(data) {
    data |>
      dplyr::mutate(
        has_freshwater_habitat = stringr::str_detect(
          iucn_habitat_major_list,
          "Wetlands \\(inland\\)|Artificial/Aquatic",
        ),
        
        freshwatersp_resolved = dplyr::case_when(
          # amphibians
          ala_class == "Amphibia" ~ TRUE,
          
          # fish and reptiles: only freshwater-associated members
          ala_class %in% c("Actinopterygii", "Reptilia") & has_freshwater_habitat ~ TRUE,
          ala_class %in% c("Actinopterygii", "Reptilia") & !has_freshwater_habitat ~ FALSE,
          
          # aquatic mammals
          is_mammal & stringr::str_detect(iucn_habitat_major_list, "Aquatic") ~ TRUE,
          
          # everything else
          TRUE ~ FALSE,
        )
      )
  },
  
  # NOTE: the "Lv5"/"apex" tier had NO database column backing it —
  # trophic_lit == "apex" was its only condition.
  trophic_resolved = \(data) {
    data |>
      dplyr::mutate(
        trophic_resolved = dplyr::case_when(
          pan_TrophicLevel == 3 | ws_Trophic.Level_AVONET == "Carnivore" | 
            rep_Diet == "Carnivorous" ~ "Lv4",
          pan_TrophicLevel == 2 | ws_Trophic.Level_AVONET == "Omnivore" | 
            rep_Diet == "Omnivorous" ~ "Lv3",
          pan_TrophicLevel == 1 | ws_Trophic.Level_AVONET == "Herbivore" | 
            rep_Diet == "Herbivorous" ~ "Lv2",
        )
      )
  }
  
)

## ---- driver ------------------------------------------------------------------

#' Runs all animal trait resolvers, adding their output columns to animaldb
resolve_animal_traits <- \(data, resolvers = animal_trait_resolvers) {
  purrr::reduce(resolvers, \(d, fn) fn(d), .init = data)
}