#' Registry of animal trait resolver functions — DATABASE-DERIVED ONLY.
#' No manual/"_lit" columns are referenced; each function takes animaldb
#' directly (no join to animal_mtable at this stage) and returns it with ONE
#' new column added. NA results in these outputs are exactly the rows that
#' need manual coding — see flag_missing_traits().
animal_trait_resolvers <- list(
  
  hb_lit = \(data) {
    data |>
      dplyr::mutate(
        hb_lit = iucn_habitat_major_list
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
  
  dispersal_resolved = \(data) {
    data |>
      dplyr::mutate(
        dispersal_resolved = dplyr::case_when(
          # migratory birds: regular long-distance movement
          !is.na(bb_Mig) ~ "High",
          
          # otherwise follow urban/agricultural tolerance
          ubag_tolerance_resolved == "High" ~ "High",
          ubag_tolerance_resolved == "Moderate" ~ "Moderate",
          
          # Low only imputed for birds
          ubag_tolerance_resolved == "Low" & ala_class == "Aves" ~ "Low",
          
          TRUE ~ NA_character_
        )
      )
  },
  
  breed_cond_resolved = \(data) {
    data |>
      dplyr::mutate(
        # numeric frequency from the first source that has a value
        breed_freq_n = dplyr::coalesce(
          bb_Brd2,
          pan_LittersPerYear,
          rep_NumberOfLittersOrClutchesProducedPerYear |> as.numeric()
        ),
        breed_cond_resolved = dplyr::case_when(
          # at most one litter/clutch per year
          breed_freq_n <= 1 ~ "once a year",
          
          # more than once per year, and none of these sources mention
          # condition dependency, which is exactly the "continuous" definition.
          # "highly dependent on conditions" cannot be assigned from counts
          # alone, so it has to come from manual coding.
          breed_freq_n > 1 ~ "continuous",
          
          TRUE ~ NA_character_
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
  
  oldgrowth_lit = \(data) {
    data |>
      dplyr::mutate(
        oldgrowth_lit = dplyr::case_when(
          # urban/agricultural tolerant (not "Low") -> 0
          ubag_tolerance_resolved %in% c("Moderate", "High") ~ 0,
          
          # artificial or marine habitat -> 0
          stringr::str_detect(iucn_habitat_major_list, "Artificial|Marine") ~ 0,
          
          # habitat list exists but has no "Forest" -> 0
          !is.na(iucn_habitat_major_list) &
            !stringr::str_detect(iucn_habitat_major_list, "Forest") ~ 0,
          
          # everything else stays NA for imputation
          TRUE ~ NA_real_
        )
      )
  },
  
  connectivity_resolved = \(data) {
    data |>
      dplyr::mutate(
        connectivity_resolved = case_when(
          !is.na(bb_Mig) ~ bb_Mig,
          is.na(bb_Mig) & ala_class == "Aves" ~ 0,
          TRUE ~ NA_real_
        )
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
        arid_specialist = dplyr::case_when(
          clim_desert_prop > 0.7 ~ 1,
          TRUE ~ 0
        )
      )
  },
  
  nest_sbs_resolved = \(data) {
    lvls <- c("Low", "Moderate", "High")
    
    data |>
      dplyr::mutate(
        has_st  = stringr::str_detect(bb_NestSbs, "T|S"),
        only_st = stringr::str_detect(bb_NestSbs, "^[TS,]+$"),
        nest_rule = dplyr::case_when(
          eltm_ForStrat.Value == "Ar" | only_st ~ "High",
          has_st & !only_st ~ "Moderate",
          !has_st ~ "Low"
        ),
        
        has_forest = stringr::str_detect(iucn_habitat_major_list, 
                                         "Forest") %in% TRUE,
        has_open   = stringr::str_detect(iucn_habitat_major_list, 
                                         "Shrubland|Savanna") %in% TRUE,
        habitat_rule = dplyr::case_when(
          has_forest ~ "High",
          has_open   ~ "Moderate",
          .default   = "Low"
        ),
        
        nest_sbs_resolved = lvls[pmax(
          match(nest_rule, lvls),
          match(habitat_rule, lvls),
          na.rm = TRUE
        )]
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
        ) |> 
          round(2)
      )
  },
  
  flight_height_resolved = \(data) {
    data |>
      dplyr::mutate(
        flight_height_resolved = dplyr::case_when(
          !is_flying ~ "No exposure",
          # bat
          ala_order %in% "Chiroptera" ~ bat_proxy(species, "flight_height"),
          # bird
          is_flying ~ avis_Exp
        ),
        # standardise vocabulary: trim/lowercase, then map to the canonical labels
        .fh_clean = stringr::str_to_lower(stringr::str_squish(flight_height_resolved)),
        flight_height_resolved = dplyr::case_match(
          .fh_clean,
          c("no", "no exposure") ~ "No exposure",
          "low"                  ~ "Low",
          c("mod", "moderate")   ~ "Moderate",
          "high"                 ~ "High",
          .default = flight_height_resolved   # unrecognised values pass through unchanged
        ),
        .fh_clean = NULL
      )
  },
  
  wingload_resolved = \(data) {
    data |>
      dplyr::mutate(
        wingspan = dplyr::coalesce(ws_wingspan_reported_mid_all,
                                   ws_wingspan_predicted_global), # cm
        winglength = ws_wing_length_cm_AVONET, # cm
        s1 = ws_Secondary1_AVONET / 10, # cm
        
        # ellipse-triangle model
        bird_wingarea =
          1 / 3 * winglength * s1 * (pi + 1) + # inner segment
          (wingspan - 2 * winglength) * s1, # outer segment
        
        wingload = dplyr::case_when(
          # non-flying: 0. `%in%` is used so an NA is_flying is never read as FALSE
          is_flying %in% FALSE ~ 0,
          # flying: only compute when both inputs exist
          is_flying %in% TRUE & !is.na(bird_wingarea) & !is.na(bodymass_resolved) ~
            round((bodymass_resolved / 1000 * 9.81) / (bird_wingarea / 10000), 2),
          # flying but missing inputs, or is_flying unknown: stays NA for imputation
          TRUE ~ NA_real_
        ),
        wingload_resolved = dplyr:: case_when(
          !is_flying ~ "No exposure",
          # bat
          ala_order %in% "Chiroptera" ~ bat_proxy(species, "wing_loading"),
          # bird
          is_flying %in% FALSE | wingload <= 25 | wingload > 90 ~ "Low",
          wingload > 25 & wingload <= 40 ~ "Moderate",
          wingload > 40 & wingload <= 90 ~ "High"
        )
      )
  },
  
  aspect_ratio_resolved <- \(data) {
    data |> dplyr::mutate(
      aspect_ratio  = round(wingspan^2/bird_wingarea, 2),
      aspect_ratio_resolved = dplyr::case_when(
        !is_flying ~ "No exposure",
        # bat
        ala_order %in% "Chiroptera" ~ bat_proxy(species, "aspect_ratio"),
        # bird
        is_flying %in% FALSE | aspect_ratio <= 6 ~ "Low",
        aspect_ratio > 6 & aspect_ratio <= 8 ~ "Moderate",
        aspect_ratio > 8 ~ "High"
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
          plant_diet_prop >= 50 & plant_diet_prop < 80 ~ "Moderate",
          plant_diet_prop > 0 & plant_diet_prop < 50 ~ "Low",
          rep_Diet == "Herbivorous" ~ "High",
          rep_Diet == "Omnivorous" ~ "Moderate",
          rep_Diet == "Carnivorous" | trophic_resolved == "Lv4" ~ "Low",
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
          ala_class == "Amphibia" ~ "High",
          ala_class == "Actinopterygii" & has_freshwater_habitat ~ "High",

          has_freshwater_habitat ~ "Moderate",
          
          # Everything else: check
          TRUE ~ NA_character_,
        )
      )
  }
)

## ---- driver ------------------------------------------------------------------

#' Runs all animal trait resolvers, adding their output columns to animaldb
resolve_animal_traits <- \(data, resolvers = animal_trait_resolvers) {
  purrr::reduce(resolvers, \(d, fn) fn(d), .init = data)
}