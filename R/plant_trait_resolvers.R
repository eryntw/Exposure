#' Registry of trait resolver functions.
#' Each function takes the bound table and returns it with ONE new column
#' added — the fully-resolved value for that TraitValue, ready to be
#' scored by a single coding_type with no further merging logic needed.
plan_trait_resolvers <- list(
  
  seedbank_persistence = \(data) {
    data |>
      dplyr::mutate(
        seedbank_persistence_resolved = dplyr::coalesce(
          seedbank,
          dplyr::case_when(
            seedbank_longevity_class %in% c("long_persistent", "widely_dispersed") ~ TRUE,
            seedbank_longevity >= 5 ~ TRUE,
            is.na(seedbank_longevity_class) & is.na(seedbank_longevity) ~ NA,
            TRUE ~ FALSE
          )
        )
      )
  },
  
  pollination_dependence = \(data) {
    data |>
      dplyr::mutate(
        pollination_resolved = dplyr::coalesce(pollination, 
                                               pollination_system, 
                                               pollination_syndrome)
      )
  },
  
  recruitment_special_event = \(data) {
    data |>
      dplyr::mutate(
        recruitment_special_event_resolved = dplyr::coalesce(
          recruit_special,
          stringr::str_detect(seedling_establishment_conditions, 
                              "establish_post_fire|
                              establish_intermediate_to_mature_vegetation") 
        )
      )
  },
  
  recruitment_peak_invasion = \(data) {
    data |>
      dplyr::mutate(
        recruitment_peak_invasion_resolved = dplyr::coalesce(
          recruit_peak_invasion,
          as.numeric(stringr::str_detect(recruitment_time, "^N{4}[NY]{6}N{2}$"))
        )
      )
  }
  
)

#' Run all trait resolvers, adding their output columns to the bound table
resolve_all_traits <- \(data, resolvers = trait_resolvers) {
  purrr::reduce(resolvers, \(d, fn) fn(d), .init = data)
}