#' Summarise major IUCN habitat categories per species
#'
#' Filters out habitats with unknown suitability, extracts the major
#' habitat code (digits before the underscore in `habitat_code`) and the
#' major habitat name (text before " - " in `habitat_description`), then
#' compiles one row per species with a count and a semicolon-separated list
#' of unique major habitat names.
#'
#' @param iucn_data Data frame of IUCN habitat records, e.g.
#'   `iucn_habitat[["iucn_data"]]`. Must include `scientific_name`,
#'   `habitat_suitability`, `habitat_code`, `habitat_description`.
#'
#' @return A tibble with one row per `scientific_name`, containing
#'   `habitat_count_mj` (number of unique major habitat categories) and
#'   `habitat_major_list` (semicolon-separated, alphabetised major habitat names)
#' @export
#'
#' @examples
#' summarise_iucn_habitat(iucn_habitat[["iucn_data"]])
#'
summarise_iucn_habitat <- function(iucn_data) {
  
  iucn_data |>
    dplyr::filter(habitat_suitability != "Unknown") |>
    dplyr::mutate(
      habitat_major      = stringr::str_extract(habitat_code, "^[0-9]+"),
      habitat_major_name = stringr::str_extract(habitat_description, "^[^-]+") |> stringr::str_trim()
    ) |>
    dplyr::distinct(scientific_name, habitat_major, habitat_major_name) |>
    dplyr::summarise(
      habitat_count_mj    = dplyr::n(),
      habitat_major_list  = paste(sort(unique(habitat_major_name)), collapse = "; "),
      .by = scientific_name
    )
}