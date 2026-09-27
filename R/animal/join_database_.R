#' Multi-stage fuzzy taxonomic matching
#'
#' Performs a multi-round matching workflow between two taxonomic tables using:
#' (1) strict fuzzy matching on Genus + Species only;
#' (2) relaxed matching requiring exact Genus OR Species with fuzzy common name;
#' (3) synonym-based matching using ITIS-derived synonym tables.
#'
#' @param A Data frame containing columns `Genus`, `Species`, and optionally `common`
#' @param B Data frame containing columns `Genus`, `Species`, and optionally `common`
#' @param prefix Character. Prefix added to non-taxonomic columns in B
#' @param syn_db Character. Path to synonym CSV file
#'
#' @return A list with elements `match` and `unmatch`
#'
#' @author eryntw
#' @export

join_database_ <- function(A,
                           B,
                           prefix,
                           syn_db) {
  
  ## ---- 0. Ensure UTF-8 (defensive) ----
  A <- dplyr::mutate(A, dplyr::across(dplyr::where(is.character), stringi::stri_enc_toutf8))
  B <- dplyr::mutate(B, dplyr::across(dplyr::where(is.character), stringi::stri_enc_toutf8))
  
  ## ---- 0.1 Detect common availability ----
  use_common <- "common" %in% names(A) && "common" %in% names(B)
  
  ## ---- 0.2 Prefix B columns ----
  B <- B %>%
    dplyr::rename_with(
      ~ paste0(prefix, .x),
      .cols = -base::intersect(c("Genus", "Species", "common"), names(B))
    ) %>%
    dplyr::rename_with(
      ~ paste0("B_", .x),
      .cols = base::intersect(c("Genus", "Species", "common"), names(B))
    )
  
  ## ---- 1. Round 1: strict (Genus + Species ONLY) ----
  message("Starting Round 1...")
  match1 <- dplyr::inner_join(
    A,
    B,
    by = c("Genus"   = "B_Genus",
           "Species" = "B_Species")) %>%
    dplyr::mutate(match = "r1")
  
  ## ---- 1-1. Unmatched after round 1 ----
  unmatch1 <- dplyr::anti_join(
    A,
    match1,
    by = c("Genus", "Species")
  ) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE) %>%
    dplyr::mutate(match = "un_r1")
  
  ## ---- 2. Relaxed match (ONLY if common exists) ----
  message("Round 1 done. Starting Round 2...")
  if (use_common) {
    
    match2 <- tidyr::crossing(unmatch1, B) %>%
      dplyr::mutate(
        species_dist = mapply(function(a, b) adist(a, b), Species, B_Species),
        common_dist  = mapply(function(a, b) adist(a, b), common, B_common)
      ) %>% 
      dplyr::filter(Genus == B_Genus | species_dist <= 2) %>%
      dplyr::filter(common_dist <= 2) %>%
      dplyr::mutate(match = "r2") %>% 
      dplyr::select(-common_dist, -species_dist)
    
    ## ---- 2-1. Unmatched after round 2 ---- 
    unmatch2 <- dplyr::anti_join(
      unmatch1,
      match2,
      by = c("Genus", "Species")
    ) %>%
      dplyr::mutate(
        taxa  = base::paste(Genus, Species),
        match = "un_r2"
      )
    
  } else {
    
    match2 <- match1[0, ]  # empty, correct structure
    
    unmatch2 <- unmatch1 %>%
      dplyr::mutate(
        taxa  = base::paste(Genus, Species),
        match = "un_r2"
      )
  }
  
  ## ---- 3. Synonym matching ----
  message("Round 2 done. Starting Round 3...")
  syn_matches <- unmatch2 %>%
    dplyr::inner_join(syn_db, by = c("taxa" = "id")) %>%
    tidyr::separate(
      name_bi,
      into = c("S_Genus", "S_Species"),
      sep = " ",
      remove = FALSE
    ) %>%
    tidyr::crossing(B) %>%
    dplyr::mutate(
      genus_dist   = mapply(function(a, b) adist(a, b), S_Genus,   B_Genus),
      species_dist = mapply(function(a, b) adist(a, b), S_Species, B_Species)
    ) %>%
    dplyr::filter(genus_dist <= 2, species_dist <= 2) %>%
    dplyr::mutate(
      non_na = base::rowSums(!is.na(dplyr::across(dplyr::everything()))),
      match  = "r3"
    ) %>%
    dplyr::group_by(taxa) %>%
    dplyr::slice_max(non_na, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-non_na, -name, -name_bi, -genus_dist, -species_dist)
  
  ## ---- 3-1. Unmatched after round 3 ----
  message("Round 3 done.")
  unmatch3 <- dplyr::anti_join(
    unmatch2,
    syn_matches,
    by = "taxa"
  ) %>%
    dplyr::mutate(match = "un_r3")
  
  ## ---- Combine ----
  unwanted_cols <- c("match", "taxa", "name_type", "S_Genus", "S_Species",
                     "B_Genus", "B_Species", "B_common", "common_dist")
  
  combined <- dplyr::bind_rows(match1, match2, syn_matches) %>% 
    bind_rows(unmatch3) %>% 
    dplyr::select(-any_of(unwanted_cols))
  
  return(combined)
}
