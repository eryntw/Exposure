#' Creates or updates the animal manual-coding table (wide format) directly
#' from animal_traits_resolved. Only columns that currently have at least one
#' NA are included (no point asking someone to manually code a trait the
#' database already resolved for every species). Safe to re-run:
#'   - existing manually-entered values are NEVER overwritten
#'   - new species are appended
#'   - new trait columns (newly-NA somewhere) are appended, blank
#'   - trait columns that later become fully-resolved are still RETAINED
#'     (not dropped), so any manual work already done on them isn't lost
#'   - species that drop out of animal_traits_resolved are kept as
#'     historical rows, not deleted
#'
#' @param animal_traits_resolved The database-derived resolved-traits table,
#'   one row per species.
#' @param path Output CSV path — this IS the manually-edited file.
#' @param id_col Species identifier column.
#' @param context_cols Metadata columns refreshed from animal_traits_resolved
#'   on every run (never manually edited, so safe to overwrite).
create_or_update_mtable_animal <- function(
    animal_traits_resolved,
    path,
    id_col = "species",
    context_cols = c("ala_class", "ala_order", "ala_vernacular_name", "epbc")
) {
  
  trait_cols <- animal_traits_resolved |>
    dplyr::select(dplyr::matches("_resolved$|_lit$")) |>
    names()
  
  cols_with_na <- trait_cols[
    purrr::map_lgl(trait_cols, \(col) anyNA(animal_traits_resolved[[col]]))
  ]
  
  current <- animal_traits_resolved |>
    dplyr::select(dplyr::all_of(c(id_col, context_cols, cols_with_na))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cols_with_na), as.character))
  
  if (!fs::file_exists(path)) {
    readr::write_csv(current, path)
    message("Created new animal_mtable: ", nrow(current), " species x ", length(cols_with_na), " trait column(s)")
    return(path)
  }
  
  existing <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  
  metadata_cols <- c(id_col, context_cols)
  existing_trait_cols <- setdiff(names(existing), metadata_cols)
  all_trait_cols <- union(existing_trait_cols, cols_with_na)
  new_trait_cols <- setdiff(all_trait_cols, existing_trait_cols)
  
  # pad existing with any brand-new trait columns before the merge, so every
  # row has every column to coalesce against
  existing_padded <- new_trait_cols |>
    purrr::reduce(\(tbl, col) dplyr::mutate(tbl, !!col := NA_character_), .init = existing) |>
    dplyr::select(dplyr::all_of(c(id_col, all_trait_cols)))
  
  current_padded <- setdiff(all_trait_cols, cols_with_na) |>
    purrr::reduce(\(tbl, col) dplyr::mutate(tbl, !!col := NA_character_), .init = current) |>
    dplyr::select(dplyr::all_of(c(id_col, context_cols, all_trait_cols)))
  
  # species present in BOTH: refresh metadata, and per trait column,
  # BACKFILL only where the existing cell is NA — never overwrite a
  # manually-entered value, even if the database now disagrees with it
  matched <- current_padded |>
    dplyr::select(dplyr::all_of(c(id_col, context_cols))) |>
    dplyr::inner_join(
      existing_padded,
      by = id_col,
      suffix = c("", ".existing"),
    )
  
  backfilled <- all_trait_cols |>
    purrr::reduce(
      \(tbl, col) {
        new_vals <- current_padded[[col]][match(tbl[[id_col]], current_padded[[id_col]])]
        dplyr::mutate(tbl, !!col := dplyr::coalesce(.data[[col]], new_vals))
      },
      .init = matched
    )
  
  # brand-new species: not in existing at all — take database values as-is
  # (nothing to backfill against yet)
  new_species <- setdiff(current_padded[[id_col]], existing_padded[[id_col]])
  new_rows <- current_padded |>
    dplyr::filter(.data[[id_col]] %in% new_species)
  
  # species no longer in animal_traits_resolved — retained as historical
  # rows exactly as last recorded, nothing to backfill from
  dropped_rows <- existing_padded |>
    dplyr::filter(!(.data[[id_col]] %in% current_padded[[id_col]])) |>
    dplyr::left_join(existing |> dplyr::select(dplyr::all_of(metadata_cols)), by = id_col) |>
    dplyr::select(dplyr::all_of(c(id_col, context_cols, all_trait_cols)))
  
  final_tbl <- dplyr::bind_rows(backfilled, new_rows, dropped_rows)
  
  readr::write_csv(final_tbl, path)
  
  message(
    length(new_species), " new species added; ",
    length(new_trait_cols), " new trait column(s) added",
    if (nrow(dropped_rows) > 0) paste0("; ", nrow(dropped_rows), " species retained as no-longer-present") else ""
  )
  
  path
}