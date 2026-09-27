#' Get IUCN habitat data and combine elements
#'
#' @param api IUCN API object
#' @param genus Character, genus name
#' @param species Character, species name
#' @return A list with combined dfs: `main` and `syms`
#'
get_iucn_habitat <- function(api, genus, species) {
  
  ## look up the assessment(s) for this genus/species; catch API errors as empty df ##
  assess <- tryCatch(
    {
      iucnredlist::assessments_by_name(api, genus, species)
    },
    error = function(e) {
      data.frame()
    }
  )
  
  ## bail out early if no assessment exists for this taxon ##
  if (nrow(assess) == 0) {
    message("No IUCN assessment found for ", genus, " ", species)
    return(list(main = NULL, syms = NULL))
  }
  
  ## take the first matching assessment_id and pull its full data payload ##
  assess_id <- assess$assessment_id[1]
  dat <- iucnredlist::assessment_data_many(api, assessment_ids = assess_id)
  
  ## helper: safely pull one "element" (sub-table) out of the assessment payload; ##
  ## returns empty df on error, or if required columns are missing ##
  safe_extract <- \(dat, element, required_cols = NULL) {
    df <- tryCatch(iucnredlist::extract_element(dat, element), error = \(e) data.frame())
    if (nrow(df) == 0) return(data.frame())
    if (!is.null(required_cols) && !all(required_cols %in% names(df))) return(data.frame())
    df
  }
  
  ## currently-accepted taxonomy (genus/species/etc.) for this assessment ##
  taxon <- safe_extract(dat, "taxon")
  if (nrow(taxon) > 0) taxon <- taxon[, c(2:4)]
  
  ## main common name (filter to the row flagged as the primary common name) ##
  common <- safe_extract(dat, "taxon_common_names", required_cols = c("main", "name"))
  if (nrow(common) > 0) {
    common <- common |>
      dplyr::filter(main == TRUE) |>
      dplyr::rename(common = name)
  }
  
  ## alternate/historical scientific names IUCN has on file for this taxon, ##
  ## trimmed to id + genus/species and de-duplicated ##
  synonyms <- safe_extract(dat, "taxon_synonyms")
  if (nrow(synonyms) > 0) {
    synonyms <- synonyms |>
      dplyr::select(dplyr::any_of(c("assessment_id", "genus_name", "species_name"))) |>
      dplyr::distinct()
  }
  
  ## current Red List category (e.g. threat status code + description) ##
  status <- safe_extract(dat, "red_list_category")
  if (nrow(status) > 0) status <- status[, c(2, 5)]
  
  ## full habitat list (one row per habitat type/code used by this assessment); ##
  ## also derive a one-row-per-assessment tally of how many habitat types are listed; ##
  ## count must be taken from the raw (pre-prefixed) table, before columns get renamed ##
  habitats <- safe_extract(dat, "habitats")
  
  habitat_count <- data.frame()
  if (nrow(habitats) > 0) {
    habitat_count <- habitats |>
      dplyr::group_by(assessment_id) |>
      dplyr::summarise(habitat_count = dplyr::n(), .groups = "drop")
    
    habitats <- habitats |>
      dplyr::rename_with(~ paste0("habitat_", .x), -assessment_id)
  }
  
  ## population trend info, prefixed so it doesn't clash with other elements on join ##
  poptrend <- safe_extract(dat, "population_trend")
  if (nrow(poptrend) > 0) {
    poptrend <- poptrend |>
      dplyr::rename_with(~ paste0("poptrend_", .x), -assessment_id)
  }
  
  ## existing conservation actions, prefixed for the same reason ##
  conservation <- safe_extract(dat, "conservation_actions_in_place")
  if (nrow(conservation) > 0) {
    conservation <- conservation |>
      dplyr::rename_with(~ paste0("conservation_", .x), -assessment_id)
  }
  
  ## drop any empty elements, then left_join everything that's left on assessment_id ##
  ## note: habitats is many-rows-per-assessment, so combined will fan out accordingly ##
  dfs <- list(taxon, common, status, habitats, habitat_count, poptrend, conservation)
  dfs_nonempty <- dfs[vapply(dfs, \(x) nrow(x) > 0, logical(1))]
  
  combined <- if (length(dfs_nonempty) == 0) {
    NULL
  } else if (length(dfs_nonempty) == 1) {
    dfs_nonempty[[1]]
  } else {
    purrr::reduce(dfs_nonempty, dplyr::left_join, by = "assessment_id")
  }
  
  ## build a separate taxonomic reference table: accepted name + common name + synonyms, ##
  ## useful for reconciling species lists that may use an outdated/alternate name ##
  syms <- list(taxon, common, synonyms)
  syms_nonempty <- syms[vapply(syms, \(x) nrow(x) > 0, logical(1))]
  
  syms_combined <- if (length(syms_nonempty) == 0) {
    NULL
  } else if (length(syms_nonempty) == 1) {
    syms_nonempty[[1]]
  } else {
    purrr::reduce(syms_nonempty, dplyr::left_join, by = "assessment_id")
  }
  
  return(list(main = combined, syms = syms_combined))
}