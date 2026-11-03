## recode_scores_plant.R ##
## Score plant traits one column at a time, like plant_trait_resolvers.
##   plant_trait_scorers  one small function per scored_var: takes the column,
##                        returns its numeric score. Edit one trait at a time.
##   recode_scores_plant() uses Rule_plant to find which column (processed_var)
##                        feeds which scorer (scored_var) and runs them all.
## Direction is NOT applied here (set it in the COINr iMeta). Values with no
## score become NA and are reported, never silently defaulted.

## helpers ##
lc      <- \(x) tolower(stringr::str_squish(as.character(x)))   # TRUE -> "true", 1 -> "1"
num     <- \(x) suppressWarnings(as.numeric(x))
na_keep <- \(x, y) dplyr::if_else(is.na(x), NA_real_, as.numeric(y))  # a 1/0 flag keeps NA

plant_trait_scorers <- list(
  
  ## Vegetation clearance: Simpson diversity (0-1) is the score; Direction -1 ##
  soil      = num,
  climate   = num,
  bioregion = num,
  
  ## Habitat conversion ##
  seedbank = \(x) dplyr::case_match(lc(x),
                                    "strong"   ~ 3,
                                    "moderate" ~ 2,
                                    "weak"     ~ 1
  ),
  
  resprout = \(x) na_keep(x, !lc(x) == "none"), # none 0; others 1
  
  seedmass = num, # log10 left to COINr
  
  maturity = \(x) dplyr::case_match(lc(x),
                                    "very short" ~ 1,   # <=2 y
                                    "short"      ~ 2,   # >2
                                    "moderate"   ~ 3,   # >5
                                    "long"       ~ 4    # >10
  ),
  
  dispersal = \(x) dplyr::case_match(lc(x),
                                     "low"           ~ 1,
                                     "low_moderate"  ~ 2,
                                     "moderate"      ~ 3,
                                     "moderate_high" ~ 4,
                                     "high"          ~ 5
  ),
  
  growthform = \(x) dplyr::case_match(lc(x),
                                      "tree"  ~ 3,
                                      "shrub" ~ 2,
                                      "herb"  ~ 1
  ),
  
  ## Invasive plants ##
  lma = \(x) findInterval(num(x), c(70, 150)) + 1, # <70, 70-150, >150 g/m2 -> 1-3
  
  recruit_special = \(x) na_keep(x, lc(x) %in% c("fire", "flood", "flooding", "mycorrhiza", "major rainfall")),
  
  herb = num,   # already 1/0
  
  ## Fragmentation ##
  pollination = \(x) na_keep(x, lc(x) == "biotic"), # biotic 1; abiotic, both 0
  
  ## Concentrated herbivory pressure ##
  defence = \(x) na_keep(x, !lc(x) == "absent"), # no effective defence 0; any 1
  
  tolerance = \(x) dplyr::case_match(lc(x),
                                          "low"      ~ 3,   # <10% land use
                                          "moderate" ~ 2,   # <50%
                                          "high"     ~ 1    # >50%
  )
)

#' @param data Trait table (e.g. plant_trait_final), one row per taxon.
#' @param rules Rule_plant (needs processed_var and scored_var).
#' @param scorers Registry of scorers, names = scored_var.
#' @param id_col Taxon column, kept in the output.
#'
#' @return list(data = id + one scored column per scored_var,
#'   diagnostics = list(unscored_values, skipped_vars))
recode_scores_plant <- function(
    data,
    rules,
    scorers = plant_trait_scorers,
    id_col = "taxon_name"
) {
  
  ## which column feeds which scorer (cross_reference rows have no processed_var) ##
  map <- rules |>
    dplyr::mutate(
      processed_var = stringr::str_remove_all(processed_var, "[\"'\\s]"),   # paste artefacts
      scored_var    = stringr::str_squish(scored_var)
    ) |>
    dplyr::filter(!is.na(processed_var), processed_var != "NA") |>
    dplyr::distinct(processed_var, scored_var)
  
  skipped <- setdiff(map$processed_var, names(data))
  if (length(skipped) > 0) {
    warning("processed_var not in data, not scored: ", paste(skipped, collapse = ", "))
    map <- dplyr::filter(map, processed_var %in% names(data))
  }
  
  no_scorer <- setdiff(map$scored_var, names(scorers))
  if (length(no_scorer) > 0) {
    stop("No scorer in `scorers` for: ", paste(no_scorer, collapse = ", "))
  }
  
  ## score one column at a time ##
  out <- data[id_col]
  unscored <- list()
  
  for (i in seq_len(nrow(map))) {
    var <- map$scored_var[i]
    x   <- data[[map$processed_var[i]]]
    
    out[[var]] <- scorers[[var]](x)
    
    ## a value that was present but got no score ##
    bad <- !is.na(x) & is.na(out[[var]])
    if (any(bad)) {
      unscored[[var]] <- tibble::tibble(scored_var = var, value = as.character(x[bad])) |>
        dplyr::count(scored_var, value)
    }
  }
  
  list(
    data = out,
    diagnostics = list(
      unscored_values = purrr::list_rbind(unscored),   # want 0 rows
      skipped_vars    = skipped
    )
  )
}

## Example ##
# scored <- recode_scores_plant(plant_trait_final, Rule_plant)
# scored$diagnostics$unscored_values   # values to add to a scorer
# plant_trait_scored <- scored$data
#
# ## change or add one trait: edit its entry, or pass your own registry ##
# my_scorers <- modifyList(plant_trait_scorers, list(
#   pollination = \(x) na_keep(x, lc(x) %in% c("biotic", "animal", "insect"))
# ))
# recode_scores_plant(plant_trait_final, Rule_plant, scorers = my_scorers)
#
# ## targets ##
# tar_target(plant_trait_scored, recode_scores_plant(plant_trait_final, Rule_plant)$data)