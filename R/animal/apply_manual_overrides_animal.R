#' Coalesces manually-coded values onto database-derived resolved traits.
#' Both animal_traits_resolved and animal_mtable are character at this
#' point (stringify_all_cols() applied upstream), so the coalesce() itself
#' needs no type handling. Real types are restored ONCE, at the very end,
#' via readr::type_convert() — after the override is applied, not before.
apply_manual_overrides_animal <- 
  function(animal_traits_resolved, 
           animal_mtable, 
           id_col = "species",
           context_cols = c("taxa",
                            "ala_class", 
                            "ala_order", 
                            "ala_vernacular_name", 
                            "epbc"))
  {
    
    # Step 1: Identify which resolved/lit trait columns actually exist in
    # BOTH tables — animal_mtable only contains columns that had at least
    # one NA when it was built, so this is the working override set.
    trait_cols <- intersect(
      names(animal_mtable),
      animal_traits_resolved |> 
        dplyr::select(dplyr::matches("_resolved$|_lit$")) |> 
        names()
    )
    
    # Step 2: Align animal_mtable to animal_traits_resolved's row order via
    # a left_join on species, rather than assuming row order (bind_cols()
    # would silently break if the two tables were ever in different order).
    mtable_j <- animal_traits_resolved |>
      dplyr::select(dplyr::all_of(c(id_col, context_cols))) |>
      dplyr::left_join(animal_mtable, 
                       by = id_col
      )
    
    # Step 3: For each trait column, the manual value wins wherever it's
    # non-NA; otherwise the database-resolved value is kept. Character-vs-
    # character throughout, so this coalesce() can't hit a type mismatch.
    overridden <- trait_cols |>
      purrr::reduce(
        \(data, col) dplyr::mutate(data, "{col}" := dplyr::coalesce(mtable_j[[col]],
                                                                    .data[[col]])),
        .init = animal_traits_resolved
      )
    
    # Step 4: Restore real types (double/logical/character) across the whole
    # table in one pass, now that the override merge is complete — this is
    # the only point at which type inference happens in this function.
    overridden |>
      readr::type_convert()
    
    # Step 5: Keep only the 
  }