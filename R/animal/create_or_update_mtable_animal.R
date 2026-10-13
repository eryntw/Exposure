#' Creates or updates the animal manual-coding table (wide format) directly
#' from animal_traits_resolved. Only columns that currently have at least one
#' NA are included on first creation (no point asking someone to manually code
#' a trait the database already resolved for every species). Safe to re-run:
#'   - existing manually-entered values are NEVER overwritten (except
#'     placeholders you list in `fill_placeholders`, see below)
#'   - NA / blank cells ARE backfilled from the database whenever the
#'     database now has a value, for EVERY trait column in the mtable,
#'     including columns that have since become fully resolved
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
#' @param fill_placeholders Character vector of manual values that mean "not
#'   really coded yet" (e.g. "unknown"). A cell holding one of these is
#'   replaced when the database has a non-NA value; if the database is also
#'   NA the placeholder is kept. Default is none, so only NA/blank cells are
#'   backfilled.
create_or_update_mtable_animal <- function(
    animal_traits_resolved,
    path,
    id_col = "species",
    context_cols = c("ala_class", "ala_order", "ala_vernacular_name", "epbc"),
    fill_placeholders = character(0)
) {
  
  ## coerce database table to character ##
  # The manual CSV is always read back as all-character, so the database
  # table is coerced to match. Otherwise coalesce()/bind_rows() can fail on
  # logical (e.g. epbc) or numeric trait columns vs character.
  db <- animal_traits_resolved |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  trait_cols <- db |>
    dplyr::select(dplyr::matches("_resolved$|_lit$")) |>
    names()
  
  cols_with_na <- trait_cols[
    purrr::map_lgl(trait_cols, \(col) anyNA(db[[col]]))
  ]
  
  ## first run: create the file ##
  if (!fs::file_exists(path)) {
    current <- db |>
      dplyr::select(dplyr::all_of(c(id_col, context_cols, cols_with_na)))
    readr::write_csv(current, path)
    message("Created new animal_mtable: ", nrow(current), " species x ", length(cols_with_na), " trait column(s)")
    return(path)
  }
  
  ## read existing manual table ##
  # "" and "NA" both read as NA, so blank cells and typed "NA" are treated alike
  existing <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA")
  )
  
  metadata_cols <- c(id_col, context_cols)
  existing_trait_cols <- setdiff(names(existing), metadata_cols)
  all_trait_cols <- union(existing_trait_cols, cols_with_na)
  new_trait_cols <- setdiff(all_trait_cols, existing_trait_cols)
  
  ## database values for EVERY trait column in the mtable ##
  # Previously only cols_with_na were pulled from the database, so a column
  # that had become fully resolved never received its values for species
  # whose mtable cell was still NA. Columns missing from the database
  # entirely (e.g. manual-only columns) are padded with NA.
  db_trait_cols <- intersect(all_trait_cols, names(db))
  
  current_padded <- setdiff(all_trait_cols, db_trait_cols) |>
    purrr::reduce(
      \(tbl, col) dplyr::mutate(tbl, !!col := NA_character_),
      .init = dplyr::select(db, dplyr::all_of(c(id_col, context_cols, db_trait_cols)))
    ) |>
    dplyr::select(dplyr::all_of(c(id_col, context_cols, all_trait_cols)))
  
  # pad existing with any brand-new trait columns before the merge, so every
  # row has every column to fill against
  existing_padded <- new_trait_cols |>
    purrr::reduce(\(tbl, col) dplyr::mutate(tbl, !!col := NA_character_), .init = existing) |>
    dplyr::select(dplyr::all_of(c(id_col, all_trait_cols)))
  
  ## species present in BOTH: refresh metadata, backfill NA cells ##
  matched <- current_padded |>
    dplyr::select(dplyr::all_of(c(id_col, context_cols))) |>
    dplyr::inner_join(existing_padded, by = id_col)
  
  # per trait column: take the database value where the existing cell is NA
  # (or a listed placeholder AND the database has a real value). A real
  # manually-entered value is never overwritten, even if the database now
  # disagrees with it.
  backfilled <- purrr::reduce(
    all_trait_cols,
    \(tbl, col) {
      db_vals <- current_padded[[col]][match(tbl[[id_col]], current_padded[[id_col]])]
      dplyr::mutate(
        tbl,
        !!col := dplyr::if_else(
          is.na(.data[[col]]) | (.data[[col]] %in% fill_placeholders & !is.na(db_vals)),
          db_vals,
          .data[[col]]
        )
      )
    },
    .init = matched
  )
  
  # count cells that changed, for the run message
  n_filled <- purrr::map_int(
    all_trait_cols,
    \(col) {
      old <- matched[[col]]
      new <- backfilled[[col]]
      sum(is.na(old) & !is.na(new)) + sum(!is.na(old) & !is.na(new) & old != new)
    }
  ) |>
    sum()
  
  ## brand-new species ##
  # not in existing at all — take database values as-is
  new_species <- setdiff(current_padded[[id_col]], existing_padded[[id_col]])
  new_rows <- current_padded |>
    dplyr::filter(.data[[id_col]] %in% new_species)
  
  ## species no longer in animal_traits_resolved ##
  # retained as historical rows exactly as last recorded
  dropped_rows <- existing_padded |>
    dplyr::filter(!(.data[[id_col]] %in% current_padded[[id_col]])) |>
    dplyr::left_join(existing |> dplyr::select(dplyr::all_of(metadata_cols)), by = id_col) |>
    dplyr::select(dplyr::all_of(c(id_col, context_cols, all_trait_cols)))
  
  final_tbl <- dplyr::bind_rows(backfilled, new_rows, dropped_rows)
  
  readr::write_csv(final_tbl, path)
  
  message(
    n_filled, " cell(s) backfilled from database; ",
    length(new_species), " new species added; ",
    length(new_trait_cols), " new trait column(s) added",
    if (nrow(dropped_rows) > 0) paste0("; ", nrow(dropped_rows), " species retained as no-longer-present") else ""
  )
  
  path
}