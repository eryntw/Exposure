#' Combine a named list of per-variable summary tibbles into one wide table
#'
#' Each element of `lst` (as returned by `summarise_env_by_species()`)
#' shares `id_cols` (species identifier columns) but has its own set of
#' value columns. This function prefixes each element's value columns
#' with the list element's name, then joins all elements together on
#' `id_cols` so the identifier columns appear only once in the output.
#'
#' @param lst A named list of data frames, each containing `id_cols` plus
#'   variable-specific value columns (e.g. `rec_summary` from
#'   `summarise_env_by_species()`)
#' @param id_cols Character vector of shared identifier columns to exclude
#'   from prefixing and use as the join key. Default c("taxa", "common").
#'
#' @return A single wide tibble with `id_cols` plus all value columns,
#'   each prefixed with their source list element's name
#'
#' @importFrom purrr imap reduce
#' @importFrom dplyr rename_with inner_join all_of
#' @export
combine_named_list_prefixed <- function(lst, id_cols = c("taxa", "common")) {
  lst |>
    purrr::imap(\(df, nm) {
      dplyr::rename_with(df, \(x) paste0(nm, "_", x), .cols = -dplyr::all_of(id_cols))
    }) |>
    purrr::reduce(\(x, y) dplyr::inner_join(x, y, by = id_cols))
}
