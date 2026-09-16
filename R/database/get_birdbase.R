#' Prepare and optionally subset birdbase dataset
#'
#' Cleans birdbase, calculates migration score and dietary diversity
#'
#' @param birdbase Data frame containing bird traits
#'
#' @return A processed tibble
#' @export

get_birdbase <- function(birdbase) {
  
  hb_cols <- c("F", "Bm", "Wd", "Sh", "Sv", "G", "Pl", "R",
               "D", "A", "C", "Rv", "W", "Se", "O")
  
  db_cols <- birdbase %>%
    dplyr::select(dplyr::contains("Wt"), -SumWt) %>%
    names()
  
  # ---- Step 1: ALWAYS process data ----
  birdbase_proc <- birdbase %>%
    
    # ---- Fix T ----
  dplyr::mutate(
    dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., "T"))
  ) %>%
    readr::type_convert() %>%
    
    # ---- Fix NA in habitat and diet cols ----
  dplyr::mutate(
    dplyr::across(dplyr::all_of(c(hb_cols, db_cols)), ~ tidyr::replace_na(., 0))
  ) %>%
    
    # ---- character/numeric/integer ----
  readr::type_convert() %>% 
    
    # ---- force numeric ----
  mutate(across(contains("bb_Norm"), readr::parse_number))
  
  return(birdbase_proc)
}
