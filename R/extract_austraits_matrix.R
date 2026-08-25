#' Extract a species-by-trait matrix from AusTraits
#'
#' Filters an AusTraits `traits` table to a given set of taxa and traits,
#' collapses multiple records for the same taxon-trait combination into a
#' single delimited string, and returns both the resulting matrix and a
#' missing-data summary (per trait and per species).
#'
#' @param austraits_traits A data frame equivalent to `austraits$traits`
#'   (must contain `taxon_name`, `trait_name`, `value` columns).
#' @param taxa Character vector of taxon names to extract (matched exactly
#'   against `taxon_name`).
#' @param traits Character vector of trait names to extract (matched exactly
#'   against `trait_name`).
#' @param collapse_sep String used to collapse multiple values for the same
#'   taxon-trait cell. Default `"; "`.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{matrix}{A tibble, one row per taxon, one column per trait
#'       (wide format), multi-value cells joined by `collapse_sep`.}
#'     \item{missing_summary}{A named list of two tibbles:
#'       `by_trait` (trait_name, n_total, n_missing, pct_missing) and
#'       `by_species` (taxon_name, n_total, n_missing, pct_missing).}
#'   }
#'
#' @examples
#' \dontrun{
#' austraits <- austraits::load_austraits(version = "5.0.0", path = "data/austraits")
#' out <- extract_austraits_matrix(
#'   austraits_traits = austraits$traits,
#'   taxa   = c("Eucalyptus obliqua", "Acacia dealbata"),
#'   traits = c("plant_height", "leaf_area", "seed_mass")
#' )
#' out$matrix
#' out$missing_summary$by_trait
#' out$missing_summary$by_species
#' }
extract_austraits_matrix <- function(austraits_traits,
                                     taxa,
                                     traits,
                                     collapse_sep = "; ") {
  
  stopifnot(
    all(c("taxon_name", "trait_name", "value") %in% names(austraits_traits)),
    is.character(taxa), length(taxa) > 0,
    is.character(traits), length(traits) > 0
  )
  
  taxa   <- unique(taxa)
  traits <- unique(traits)
  
  # --- sanity check: flag requested taxa/traits with zero matches at all ---
  present_taxa   <- intersect(taxa, unique(austraits_traits$taxon_name))
  present_traits <- intersect(traits, unique(austraits_traits$trait_name))
  
  taxa_not_found   <- setdiff(taxa, present_taxa)
  traits_not_found <- setdiff(traits, present_traits)
  
  if (length(present_taxa) < length(taxa)) {
    missing_taxa <- setdiff(taxa, present_taxa)
    warning(
      "No records at all for ", length(missing_taxa), " requested taxa: ",
      paste(missing_taxa, collapse = ", ")
    )
  }
  if (length(present_traits) < length(traits)) {
    missing_traits <- setdiff(traits, present_traits)
    warning(
      "No records at all for ", length(missing_traits), " requested traits: ",
      paste(missing_traits, collapse = ", ")
    )
  }
  
  # full taxon x trait grid so absent combinations are retained as explicit NA
  full_grid <- tidyr::expand_grid(taxon_name = taxa, trait_name = traits)
  
  filtered <- austraits_traits |>
    dplyr::filter(
      .data$taxon_name %in% taxa,
      .data$trait_name %in% traits
    ) |>
    dplyr::select(taxon_name, trait_name, value)
  
  collapsed <- filtered |>
    dplyr::group_by(taxon_name, trait_name) |>
    dplyr::summarise(
      value = paste(sort(unique(as.character(value))), collapse = collapse_sep),
      .groups = "drop"
    )
  
  long_full <- full_grid |>
    dplyr::left_join(collapsed, by = c("taxon_name", "trait_name"))
  
  stopifnot(nrow(long_full) == length(taxa) * length(traits))
  
  species_trait_matrix <- long_full |>
    tidyr::pivot_wider(names_from = trait_name, values_from = value) |>
    dplyr::arrange(taxon_name)
  
  # --- missing-data summaries ---
  by_trait <- long_full |>
    dplyr::group_by(trait_name) |>
    dplyr::summarise(
      n_total     = dplyr::n(),
      n_missing   = sum(is.na(value)),
      pct_missing = round(100 * n_missing / n_total, 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(pct_missing))
  
  by_species <- long_full |>
    dplyr::group_by(taxon_name) |>
    dplyr::summarise(
      n_total     = dplyr::n(),
      n_missing   = sum(is.na(value)),
      pct_missing = round(100 * n_missing / n_total, 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(pct_missing))
  
  list(
    matrix = species_trait_matrix,
    missing_summary = list(by_trait = by_trait, by_species = by_species),
    diagnostics = list(
      taxa_not_found   = taxa_not_found,
      traits_not_found = traits_not_found
    )
  )
}