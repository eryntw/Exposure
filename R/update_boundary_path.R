#' Update the boundary index in an envPIA path
#'
#' Systematically replaces the numeric boundary identifier embedded in
#' `pia__boundary__<n>__0__50000` path segments, leaving all other
#' path components (including other numeric tokens like P1Y, P50Y) untouched.
#'
#' @param path character vector of paths to update
#' @param new_boundary integer or character, the new boundary index
#'
#' @return character vector, paths with boundary index replaced
#' @export
update_boundary_path <- function(path, new_boundary) {
  stringr::str_replace(
    path,
    "(pia__boundary__)\\d+(__0__50000)",
    paste0("\\1", new_boundary, "\\2")
  )
}