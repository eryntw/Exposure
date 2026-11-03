## classify_invasive_threat.R ##
## Classify the invasive species threat (IUCN 8_1_1 / 8_1_2) on plants as
## competition with invasive plants or grazing/browsing pressure.
## Split in two so the network call is its own (cacheable) target:
##   1. lookup_ias_kingdom()       - galah lookup, one row per named invasive species
##   2. classify_invasive_threat() - pure classification

## Look up the kingdom of named invasive species with galah ##
## `ias_names` is a character vector of scientific names (threat_ias).
## "Unspecified" is removed and case normalised before searching (see below).
## Names galah cannot match get NA, so they stay visible in the output.
lookup_ias_kingdom <- function(ias_names, atlas = "Australia") {
  ias_names <- unique(stats::na.omit(ias_names))
  
  if (length(ias_names) == 0) {
    return(dplyr::tibble(threat_ias = character(), ias_kingdom = character()))
  }
  
  # set the config on every call so it also works on a fresh session or a targets worker
  galah::galah_config(atlas = atlas)
  
  # drop "unspecified" and normalise case so galah can match genus/family-level
  # names (e.g. "Unspecified POACEAE" -> "Poaceae"); names left empty are not searched
  search_names <- ias_names |>
    stringr::str_remove(stringr::regex("unspecified", ignore_case = TRUE)) |>
    stringr::str_squish() |>
    stringr::str_to_sentence()
  search_names[search_names == ""] <- NA_character_
  to_search <- unique(stats::na.omit(search_names))
  
  kingdom <- rep(NA_character_, length(ias_names))
  if (length(to_search) > 0) {
    res <- galah::search_taxa(to_search)
    kingdom <- res$kingdom[match(search_names, res$search_term)]
  }
  
  # threat_ias keeps the original text (the join key); search_name is what galah saw
  dplyr::tibble(
    threat_ias = ias_names,
    search_name = search_names,
    ias_kingdom = kingdom
  )
}

## Classify invasive threats on plant species ##
## Rules (a row can meet both):
##   compete: threat_text contains "weed", or threat_ias is a plant
##   graze:   threat_text contains "browse" or "animal", or threat_ias is an animal
## invasive_threat: compete_invasive_plant | graze_pressure | both | unclassified
## Species are matched on iucn_data$`iucn_id_col` (scientific_name) against
## kingdom_lookup$`lookup_id_col` (taxa); `ias_kingdom` is the output of
## lookup_ias_kingdom().
classify_invasive_threat <- function(
    iucn_data,
    kingdom_lookup,
    ias_kingdom,
    iucn_id_col = "scientific_name",
    lookup_id_col = "taxa",
    kingdom_col = "kingdom",
    threat_codes = c("8_1_1", "8_1_2")
) {
  # a wrong column name or kingdom spelling would otherwise silently return 0 rows
  stopifnot(
    "lookup_id_col/kingdom_col not found in kingdom_lookup" =
      all(c(lookup_id_col, kingdom_col) %in% names(kingdom_lookup)),
    "iucn_id_col not found in iucn_data" = iucn_id_col %in% names(iucn_data),
    "no 'Plantae' values in kingdom_lookup" =
      any(kingdom_lookup[[kingdom_col]] == "Plantae", na.rm = TRUE)
  )
  
  plants <- kingdom_lookup[[lookup_id_col]][kingdom_lookup[[kingdom_col]] == "Plantae"]
  
  iucn_data |>
    dplyr::filter(
      .data[[iucn_id_col]] %in% plants,
      threat_code %in% threat_codes
    ) |>
    dplyr::left_join(ias_kingdom, by = "threat_ias", relationship = "many-to-one") |>
    dplyr::mutate(
      # coalesce: a missing threat_text counts as "no match", not NA
      compete = dplyr::coalesce(
        stringr::str_detect(threat_text, stringr::regex("weed", ignore_case = TRUE)),
        FALSE
      ) | ias_kingdom %in% "Plantae",
      graze = dplyr::coalesce(
        stringr::str_detect(threat_text, stringr::regex("browse|animal", ignore_case = TRUE)),
        FALSE
      ) | ias_kingdom %in% "Animalia",
      invasive_threat = dplyr::case_when(
        compete & graze ~ "both",
        compete ~ "compete_invasive_plant",
        graze ~ "graze_pressure",
        TRUE ~ "unclassified"
      )
    )
}

## Targets (add to _targets.R) ##
## tar_target(
##   ias_kingdom,
##   iucn_data$iucn_data |>
##     dplyr::filter(threat_code %in% c("8_1_1", "8_1_2")) |>
##     dplyr::pull(threat_ias) |>
##     lookup_ias_kingdom()
## ),
## tar_target(
##   plant_invasive_threat,
##   classify_invasive_threat(iucn_data$iucn_data, kingdom_lookup, ias_kingdom)
## )