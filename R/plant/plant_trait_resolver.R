## plant_trait_resolvers.R ##
## One resolver per processed_var in Rule_plant.
## Each resolver takes the plant trait data frame (`x`) and returns a vector
## of length nrow(x) that becomes the "<trait>_resolved" column.
## Fill in the merging logic trait by trait; the TODO blocks are placeholders.
## NOTE: adjust the resolver signature / return type to match animal_trait_resolvers
## if it differs (e.g. if those return the whole data frame rather than a vector).


## Individual resolvers ##

# seedbank_longevity_class: no resolver required

# bud_bank_location: no resolver required

# seed_dry_mass: no resolver required. Need to be log transformed in scored.

resolve_reproductive_maturity <- \(x, source_col = "reproductive_maturity") {
  years <- x[[source_col]]
  as.character(
    cut(
      years,
      breaks = c(-Inf, 2, 5, 10, Inf),
      labels = c("very short", "short", "moderate", "long"),
      right = TRUE
    )
  )
}

resolve_dispersal_syndrome <- \(x) {
  # TODO: used by more than one rule - keep a single resolved column
}


## Plant growth form ##
## Raw values look like "shrub; shrub tree; tree":
##   - ";" separates entries (one per source record)
##   - spaces within an entry separate tokens, i.e. that record says the plant is
##     BOTH forms (e.g. "shrub tree")
## Each token is mapped to tree / shrub / herb, each entry casts one vote per
## distinct group it contains, and the group with the most votes wins.

growth_form_group_map <- c(
  tree = "tree",
  mallee = "tree",
  shrub = "shrub",
  hummock = "shrub",
  subshrub = "shrub",
  tussock = "shrub",
  herb = "herb",
  forb = "herb",
  graminoid = "herb",
  graminoid_not_tussock = "herb"
)
# Tokens not in the map (e.g. fern, climber, palmoid, basal_large) cast no vote;
# a row whose tokens are all unmapped resolves to NA.

resolve_growth_form <- \(
  x,
  source_col = "plant_growth_form",
  tie_order = c("tree", "shrub", "herb") # tie-break direction
) {
  purrr::map_chr(x[[source_col]], \(s) {
    if (is.na(s)) {
      return(NA_character_)
    }
    
    # one set of distinct groups per ";" entry, so "graminoid herb" is one herb vote
    votes <- strsplit(s, ";", fixed = TRUE)[[1]] |>
      purrr::map(\(e) unique(growth_form_group_map[strsplit(trimws(e), "\\s+")[[1]]])) |>
      unlist() |>
      unname()
    votes <- votes[!is.na(votes)]
    
    if (length(votes) == 0) {
      return(NA_character_)
    }
    
    # most votes wins; ties go to the first group in tie_order
    counts <- table(factor(votes, levels = tie_order))
    tie_order[as.vector(counts) == max(counts)][1]
  })
}

resolve_leaf_mass_per_area <- \(x, source_col = "leaf_mass_per_area") {
  lma <- x[[source_col]]
  
  dplyr::case_when(
    lma < 70 ~ "Low",
    lma <= 150 ~ "Moderate",
    lma > 150 ~ "High"
  )
}

resolve_recruit_special <- \(x, source_col = "seedling_establishment_conditions") {
  value <- x[[source_col]]
  
  dplyr::case_when(
    value == "establish_post_fire" ~ "fire",
    value %in% c("fire", "flood", "mycorrhiza", "major rainfall") ~ value,
    value == establish_anytime ~ "none"
  )
}

is_herb <- \(x) {
  gf <- x[[growth_form]]
  
  dplyr::case_when(
    gf == "herb" ~ 1,
    TRUE ~ 0)
}

# pollination_syndrome: no resolver required

resolve_defence <- \(x) {
  d <- x[[plant_physical_defence_structures]]
  
  dplyr::case_when(
    d %in% c("sharp_pointed_defence", "stinging_hair") ~ "physical",
    d %in% c("tannin", "silica", "alkaloid") ~ "chemical",
    TRUE ~ "none")
}

## rec_summary_pilot resolvers ##

resolve_ubag_prop <- \(x, source_col = "landuse_prop_modified") {
  prop <- x[[source_col]]
  
  dplyr::case_when(
    prop < 0.1 ~ "Low",
    prop < 0.5 ~ "Moderate",
    prop >= 0.5 ~ "High"
  )
}

## Registry ##
## Names are the output column names (the "_resolved" columns referenced by Rule_plant).

plant_trait_resolvers <- list(
  reproductive_maturity_resolved = resolve_reproductive_maturity,
  dispersal_syndrome_resolved = resolve_dispersal_syndrome,
  growth_form_resolved = resolve_growth_form,
  leaf_mass_per_area_resolved = resolve_leaf_mass_per_area,
  recruit_special_resolved = resolve_recruit_special,
  herb_resolved = is_herb,
  defence_resolved = resolve_defence,
  ubag_prop_resolved = resolve_ubag_prop
)


## Applier ##
## Runs every resolver and appends its column, with a row-identity guard.
## `resolvers` can be subset to test one trait at a time.

apply_plant_trait_resolvers <- function(x, resolvers = plant_trait_resolvers) {
  purrr::iwalk(resolvers, \(fn, nm) {
    out <- fn(x)
    stopifnot(
      "resolver must return one value per row" = length(out) == nrow(x)
    )
    x[[nm]] <<- out
  })
  x
}