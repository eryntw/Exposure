#' Clean and standardise a taxonomic data frame
#'
#' Ensures consistent Genus–Species structure, optional common names,
#' removes duplicates, and fills missing common names with "NoName".
#' Optionally standardises column names first, applies a custom
#' post-processing function, and/or selects a subset of columns.
#'
#' @param df A data frame containing taxonomic information
#' @param taxacol Optional. Column containing binomial names "Genus Species" (unquoted). Default = NULL
#' @param commoncol Optional. Column containing common names (unquoted). Default = NULL
#' @param clean_names Logical. If TRUE, runs `janitor::clean_names()` on `df`
#'   before any other processing. Default = FALSE
#' @param clean_names_case Character. Case convention passed to
#'   `janitor::clean_names(case = ...)`. Only used if `clean_names = TRUE`.
#'   Default = "snake"
#' @param post_fn Optional. A function to apply after taxa/common-name cleaning
#'   (e.g. `get_birdbase`). Must accept the cleaned data frame as its first
#'   argument. Default = NULL
#' @param post_args Optional. A named list of additional arguments passed to
#'   `post_fn`. Default = list()
#' @param select Optional. Additional columns to keep, using any
#'   `dplyr::select()` syntax (unquoted). Combine multiple selectors with
#'   `c()`, e.g. `select = c(Family:Order, dplyr::contains("Wt"))`.
#'   `Genus`, `Species`, and `common` (if present) are always kept regardless
#'   of what's specified here. Default = NULL
#' @param require_genus_species Logical. If TRUE (default), errors if `Genus`
#'   and `Species` are not present in `df` by the time taxa-splitting/dedup
#'   would run — either supply `taxacol`, or ensure `df` already has `Genus`/
#'   `Species` columns. Set FALSE to allow column-selection-only use without
#'   Genus/Species.
#'
#' @return A cleaned data frame with columns `Genus`, `Species`,
#' `common` (if provided), and any additional columns selected via `select`
#' @export
#'
clean_exp_df <- function(df,
                         taxacol = NULL,
                         commoncol = NULL,
                         clean_names = FALSE,
                         clean_names_case = "snake",
                         post_fn = NULL,
                         post_args = list(),
                         select = NULL,
                         require_genus_species = TRUE) {
  
  out <- df
  commoncol <- dplyr::enquo(commoncol)
  taxacol   <- dplyr::enquo(taxacol)
  select    <- rlang::enquo(select)
  
  ## ---- Optionally standardise column names first ----
  ## must run before taxacol/commoncol/select are used downstream, since
  ## those quosures are evaluated against post-clean_names column names
  if (clean_names) {
    out <- out |> janitor::clean_names(case = clean_names_case)
  }
  
  ## ---- Split taxa column if provided ----
  if (!rlang::quo_is_null(taxacol)) {
    out <- out %>%
      tidyr::separate(
        !!taxacol,
        into = c("Genus", "Species"),
        sep = " ",
        remove = TRUE,
        fill = "right"
      ) %>%
      dplyr::filter(!is.na(Species))
  }
  
  ## ---- Check Genus/Species exist before arrange/dedup steps that need them ----
  has_genus_species <- all(c("Genus", "Species") %in% names(out))
  if (require_genus_species && !has_genus_species) {
    stop(
      "clean_exp_df: `Genus` and `Species` columns are required but not found. ",
      "Either supply `taxacol` to split a binomial name column, ensure `df` ",
      "already has `Genus`/`Species`, or set `require_genus_species = FALSE` ",
      "if this call is only intended for column selection.",
      call. = FALSE
    )
  }
  
  ## ---- Handle common name column if provided ----
  if (!rlang::quo_is_null(commoncol)) {
    out <- out %>%
      dplyr::rename(common = !!commoncol) %>%
      dplyr::mutate(common = dplyr::coalesce(common, "NoName")) %>%
      dplyr::arrange(Genus, Species, is.na(common)) %>%
      dplyr::distinct(Genus, Species, common, .keep_all = TRUE)
  } else if (has_genus_species) {
    out <- out %>%
      dplyr::arrange(Genus, Species) %>%
      dplyr::distinct(Genus, Species, .keep_all = TRUE)
  }
  
  ## ---- Optionally apply a custom post-processing function ----
  if (!is.null(post_fn)) {
    out <- do.call(post_fn, c(list(out), post_args))
  }
  
  ## ---- Optionally select a subset of columns, always keeping ----
  ## Genus/Species/common; any_of() used so this doesn't error if they
  ## don't exist (e.g. require_genus_species = FALSE was used)
  if (!rlang::quo_is_null(select)) {
    out <- out |>
      dplyr::select(
        dplyr::any_of(c("Genus", "Species", "common")),
        !!select
      )
  }
  
  return(out)
}