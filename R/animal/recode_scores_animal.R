## recode_scores.R ##
## Convert Low / Moderate / High (plus extra words that mean the same) to 1 / 2 / 3.
## Trophic codes: Lv2 = 1, Lv3 = 2, Lv4 = 3, Lv5 = 4.
## Matching is exact on the whole value (case-insensitive, whitespace ignored),
## so "Low" never matches part of a longer string.
## Anything that does not fit (e.g. hb_lit, IDs, numeric columns) is left for other treatment.

#' @param data Data frame / tibble.
#' @param low,moderate,high Extra words scored 1 / 2 / 3, added to the built-in
#'   "Low" / "Moderate" / "High". Defaults come from the Excel scoring sheet.
#' @param very_high Words scored 4 (trophic code Lv5). Set NULL to turn off.
#' @param zero Words scored 0 (set NULL to turn off).
#' @param cols <tidyselect> columns to consider. Default: all character columns.
#' @param exclude Columns never touched.
#' @param rules Optional rule table (e.g. Rule_animal) with `processed_var` and `iCode`
#'   columns. If given, columns are renamed processed_var -> iCode. If NULL, the
#'   "_lit" / "_resolved" suffix is simply removed.
#' @return list(data, diagnostics = list(recoded, partial, unmapped_values))
recode_scores_animal <- function(
    data,
    low      = c("irruptive", "once a year", "diurnal", "Lv2"),
    moderate = c("dependent", "crepuscular", "cathemeral", "Lv3"),
    high     = c("continuous", "nocturnal","Lv4"),
    very_high = "Lv5",
    zero     = "No exposure",
    cols     = tidyselect::where(is.character),
    exclude  = c("taxa", "species", "hb_lit"),
    rules    = Rule_animal
) {
  
  #### Preparation before string replacement ####
  
  stopifnot(all(c("hb_lit", "activity_resolved", "ala_class") %in% names(data)))
  
  ## Habitat list to habitat count
  data$hb_count <- purrr::map_int(
    stringr::str_split(data$hb_lit, ";"),
    \(x) {
      x <- tolower(stringr::str_squish(x))   # ignore case and stray spaces
      x <- x[!is.na(x) & x != ""]            # drop empties (trailing ";" or "")
      if (length(x) == 0) NA_integer_ else dplyr::n_distinct(x)
    }
  )
  
  ## Translate activity into roadkill risk
  data$activity_resolved <- dplyr::if_else(
    !is.na(data$ala_class) & 
      !(data$ala_class %in% c("Aves", "Mammalia", "Reptilia", "Amphibia")),
    "No exposure",
    as.character(data$activity_resolved)
  )
  
  ## Recode values
  
  key <- \(v) tolower(stringr::str_squish(as.character(v)))
  
  ## build the string -> score map ##
  words <- list(
    `0` = zero,
    `1` = c("Low", low),
    `2` = c("Moderate", moderate),
    `3` = c("High", high),
    `4` = very_high
  )
  ## names = the words, values = their score (use.names = FALSE avoids unlist()
  ## prefixing names with the list name, e.g. "1.Low", which never matched) ##
  score_map <- stats::setNames(
    rep(as.numeric(names(words)), lengths(words)),
    unlist(words, use.names = FALSE)
  )
  stopifnot(!anyDuplicated(key(names(score_map))))  # a word can only have one score
  
  col_names <- names(tidyselect::eval_select(rlang::enquo(cols), data))
  col_names <- setdiff(col_names, exclude)
  if (length(col_names) == 0) {
    warning("No columns selected for recoding (check `cols` and `exclude`).")
  }
  
  ## TRUE/FALSE columns to 1/0 (NA stays NA) ##
  lgl_cols <- setdiff(names(data)[purrr::map_lgl(data, is.logical)], exclude)
  if (length(lgl_cols) > 0) {
    data[lgl_cols] <- purrr::map(data[lgl_cols], as.numeric)
  }
  
  recoded  <- lgl_cols
  partial  <- character()
  unmapped <- list()
  
  for (col in col_names) {
    x   <- data[[col]]
    hit <- match(key(x), key(names(score_map)))
    
    if (all(is.na(hit[!is.na(x)]))) next        # nothing matches: not a score column
    
    bad <- unique(x[!is.na(x) & is.na(hit)])
    if (length(bad) > 0) {                       # partly matched: do not half-convert
      partial <- c(partial, col)
      unmapped[[col]] <- tibble::tibble(column = col, value = bad)
      next
    }
    
    data[[col]] <- unname(score_map)[hit]        # NA stays NA
    recoded <- c(recoded, col)
  }
  
  list(
    data = data,
    diagnostics = list(
      recoded         = recoded,
      partial         = partial,
      unmapped_values = purrr::list_rbind(unmapped)
    )
  )
  
  ## Rename columns to the iCode used in Rule_animal ##
  
  # hb_lit text is no longer needed once hb_count exists
  data <- dplyr::select(data, -dplyr::any_of("hb_lit"))
  
  if (!is.null(rules)) {
    icode_map <- rules |>
      dplyr::mutate(processed_var = stringr::str_squish(processed_var)) |>
      dplyr::distinct(processed_var, iCode) |>
      dplyr::filter(!is.na(processed_var), !is.na(iCode))
    old <- icode_map$processed_var
    new <- icode_map$iCode
  } else {
    old <- grep("_(lit|count|resolved)$", names(data), value = TRUE)
    new <- stringr::str_remove(old, "_(lit|count|resolved)$")
  }
  
  ## hb's iCode belongs to the numeric count column, not the hb_lit text ##
  old[old == "hb_lit"] <- "hb_count"
  
  not_found <- setdiff(old, names(data))
  if (length(not_found) > 0) {
    warning("Rule_animal processed_var not found in data, not renamed: ",
            paste(not_found, collapse = ", "))
  }
  
  data <- dplyr::rename(data, dplyr::any_of(stats::setNames(old, new)))
}

## Example ##
# res <- recode_scores(animal_traits_scored)
# res$diagnostics$partial          # want character(0)
# res$diagnostics$unmapped_values  # strings still needing a score (e.g. an Lv1 code)
# animal_traits_numeric <- res$data
#
# ## add more words only when needed (this REPLACES the default list, so repeat the defaults) ##
# res <- recode_scores(
#   animal_traits_scored,
#   high = c("continuous", "nocturnal", "Lv4", "my_new_word")
# )