## plant_overrides.R ##
## Replace wrongly imputed plant trait values with manually corrected ones.
## Override file columns (";"-delimited): taxon_name; trait; corrected_value; source

## Apply overrides to plant_trait_imputed ##
## Every column is converted to character first (same approach as the animal
## pipeline), so any corrected value fits any column. Convert back afterwards
## with readr::type_convert() before steps that need numbers (e.g. the resolvers).
## `trait` in the override file must match a full trait column name in the matrix
## (not a *_raw column). The *_raw columns are left untouched apart from being
## converted to character, so the original database value stays visible.
## Accepts either the list from join_plant_imputed() (with `$matrix`) or the
## matrix tibble itself. The list is returned with `$matrix` corrected and an
## `$overrides` log; the tibble is returned with the log as attr(, "overrides").
## Log columns: row, taxon_found, trait_found, trait_added, applied, old_value.
## add_missing_traits: FALSE (default) leaves traits that are not in the matrix
## unapplied (trait_found = FALSE); TRUE adds each as a new column (NA except for
## the overridden taxa). Taxa that are not in the matrix are never added as rows.
apply_plant_overrides <- function(trait_imputed, overrides, add_missing_traits = FALSE) {
  is_list <- !is.data.frame(trait_imputed)
  m <- if (is_list) trait_imputed$matrix else trait_imputed
  
  stopifnot(
    "no matrix with a taxon_name column found in trait_imputed" =
      "taxon_name" %in% names(m),
    "duplicated taxon_name/trait in overrides" =
      !anyDuplicated(overrides[c("taxon_name", "trait")])
  )
  
  ## everything to character ##
  m <- dplyr::mutate(m, dplyr::across(dplyr::everything(), as.character))
  
  ## optionally add traits that are in the overrides but not in the matrix ##
  new_traits <- if (add_missing_traits) setdiff(unique(overrides$trait), names(m)) else character()
  if (length(new_traits) > 0) {
    m[new_traits] <- NA_character_
  }
  
  ## locate each override and record the value it replaces ##
  log <- overrides |>
    dplyr::mutate(
      row = match(taxon_name, m$taxon_name),
      taxon_found = !is.na(row),
      trait_found = trait %in% names(m),
      trait_added = trait %in% new_traits,
      applied = taxon_found & trait_found,
      old_value = purrr::pmap_chr(
        list(trait, row, applied),
        \(t, r, a) if (a) m[[t]][r] else NA_character_
      )
    )
  
  ## replace values ##
  to_apply <- dplyr::filter(log, applied)
  
  m <- purrr::reduce(
    seq_len(nrow(to_apply)),
    \(m, i) {
      m[[to_apply$trait[i]]][to_apply$row[i]] <- to_apply$corrected_value[i]
      m
    },
    .init = m
  )
  
  if (!is_list) {
    attr(m, "overrides") <- log
    return(m)
  }
  
  trait_imputed$matrix <- m
  trait_imputed$overrides <- log
  trait_imputed
}

## Target (add to _targets.R) ##
## tar_file_read() tracks the csv and reads it with the expression in the 3rd
## argument (!!.x is the path). type_convert() restores numeric columns before
## the resolvers run.
##
## tarchetypes::tar_file_read(
##   plant_trait_final,
##   "data/plant_overrides.csv",
##   readr::read_delim(!!.x, delim = ";", col_types = readr::cols(.default = "c"), trim_ws = TRUE) |>
##     apply_plant_overrides(trait_imputed = plant_trait_imputed) |>
##     readr::type_convert() |>
##     apply_plant_trait_resolvers()
## )