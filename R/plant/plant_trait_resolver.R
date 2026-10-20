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
  rep(NA_character_, nrow(x))
}

resolve_plant_growth_form <- \(x) {
  # TODO: used by several rules - keep a single resolved column
  rep(NA_character_, nrow(x))
}

resolve_leaf_mass_per_area <- \(x) {
  # TODO: numeric (g/m2) - check units across sources
  rep(NA_real_, nrow(x))
}

resolve_recruitment_special_event <- \(x) {
  # TODO
  rep(NA_character_, nrow(x))
}

resolve_recruitment_peak_invasion <- \(x) {
  # TODO
  rep(NA_character_, nrow(x))
}

resolve_pollination <- \(x) {
  # TODO
  rep(NA_character_, nrow(x))
}

resolve_plant_physical_defence_structures <- \(x) {
  # TODO
  rep(NA_character_, nrow(x))
}


## Registry ##
## Names are the output column names (the "_resolved" columns referenced by Rule_plant).

plant_trait_resolvers <- list(
  seedbank_persistence_resolved = resolve_seedbank_persistence,
  bud_bank_location_resolved = resolve_bud_bank_location,
  seed_dry_mass_resolved = resolve_seed_dry_mass,
  reproductive_maturity_resolved = resolve_reproductive_maturity,
  dispersal_syndrome_resolved = resolve_dispersal_syndrome,
  plant_growth_form_resolved = resolve_plant_growth_form,
  leaf_mass_per_area_resolved = resolve_leaf_mass_per_area,
  recruitment_special_event_resolved = resolve_recruitment_special_event,
  recruitment_peak_invasion_resolved = resolve_recruitment_peak_invasion,
  pollination_resolved = resolve_pollination,
  plant_physical_defence_structures_resolved = resolve_plant_physical_defence_structures
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