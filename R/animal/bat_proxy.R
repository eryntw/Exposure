#' Bat genus proxy category for a bird-style trait.
#' Reads the Harris & Turbill (2026) Table 1 sheet, finds the header row
#' ("Family"), stops at the first blank genus (before "Summary Statistics"),
#' and returns Low/Moderate/High for each species by genus.
#'
#' Scores are ordinal vulnerability scores, so categories are relative among
#' bats, not comparable to the measured bird cut points.
#'
#' @param species Character vector of binomials (use the `species` column).
#' @param trait "aspect_ratio" and "wing_loading" use Wing Morphology;
#'   "flight_height" uses Foraging Guild.
#' @return Character vector the same length as `species`; NA where the genus
#'   isn't in the table (including all non-bats).
bat_proxy <- function(
    species,
    trait = c("aspect_ratio", "wing_loading", "flight_height"),
    path  = "/mnt/envshare/data/traits/raw/mammal/bat_vulnerability_table.xlsx",
    sheet = "Bat Vulnerability Table 1"
) {
  trait <- match.arg(trait)
  
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")
  
  # header row = the row whose first cell is "Family"; skips the title rows
  header_row <- which(trimws(raw[[1]]) %in% "Family")[1]
  if (is.na(header_row)) stop("Couldn't find the 'Family' header row in sheet '", sheet, "'")
  
  body <- raw[-seq_len(header_row), ]
  names(body) <- trimws(as.character(unlist(raw[header_row, ])))
  
  # table ends at the first blank genus (before "Summary Statistics")
  end <- which(is.na(body[["Genus"]]))[1]
  if (!is.na(end)) body <- body[seq_len(end - 1), ]
  
  score_col <- if (trait == "flight_height") "Foraging Guild" else "Wing Morphology"
  score <- readr::parse_number(body[[score_col]])   # handles "2. " and "2.5"
  
  category <- dplyr::case_when(
    score <= 1.5 ~ "Low",
    score <= 2.5 ~ "Moderate",
    score >  2.5 ~ "High"
  )
  
  lookup <- stats::setNames(category, trimws(body[["Genus"]]))
  unname(lookup[stringr::word(species, 1)])
}