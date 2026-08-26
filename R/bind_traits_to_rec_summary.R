#' Bind selected rec_summary_pilot columns onto the AusTraits plant matrix
#'
#' Joins selected environmental summary columns (e.g. simpson diversity,
#' proportion columns) from rec_summary_pilot onto the trait matrix extracted
#' by extract_austraits_matrix(), matching on taxon name. Only taxa present
#' in the trait matrix are retained — this is a left join with the trait
#' matrix as the base table, so any taxa in rec_summary_pilot not present in
#' plant_trait_raw$matrix are dropped, and any trait-matrix taxa with no
#' match in rec_summary_pilot get NA for the joined columns.
#'
#' @param plant_trait_raw List output from extract_austraits_matrix(),
#'   containing a `matrix` element keyed by `taxon_name`
#' @param rec_summary_pilot Tibble of environmental summaries keyed by `taxa`
#' @param select_cols Character vector of column names in rec_summary_pilot
#'   to bind onto the trait matrix (e.g. c("simpson", "prop"))
#'
#' @return A tibble: plant_trait_raw$matrix with select_cols from
#'   rec_summary_pilot joined on, retaining only taxa in the trait matrix
bind_traits_to_rec_summary <- \(plant_trait_raw, rec_summary_pilot, select_cols) {
  
  # step 1: pull out the trait matrix — this is the base table, since we
  # only want to keep taxa that exist in the trait matrix
  trait_matrix <- plant_trait_raw$matrix
  
  # step 2: guard — confirm requested columns actually exist in
  # rec_summary_pilot before attempting the select, so a typo fails loudly
  # rather than silently dropping to zero joined columns
  missing_cols <- setdiff(select_cols, names(rec_summary_pilot))
  if (length(missing_cols) > 0) {
    stop(
      "select_cols not found in rec_summary_pilot: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # step 3: narrow rec_summary_pilot down to the join key (taxa) plus only
  # the requested columns, renaming taxa -> taxon_name to match the join key
  # used in trait_matrix
  rec_summary_selected <- rec_summary_pilot |>
    dplyr::select(taxon_name = taxa, dplyr::all_of(select_cols))
  
  # step 4: left join keeps every row of trait_matrix (the base/left table)
  # and only pulls in matching rows from rec_summary_selected — taxa present
  # only in rec_summary_pilot are dropped, per the "keep only trait matrix
  # taxa" requirement
  result <- trait_matrix |>
    dplyr::left_join(rec_summary_selected, by = "taxon_name")
  
  # step 5: row-count guard — a left join should never change row count
  # relative to trait_matrix; this catches unexpected many-to-one matches
  # (e.g. duplicate taxon_name entries in rec_summary_pilot)
  stopifnot(nrow(result) == nrow(trait_matrix))
  
  result
}