#' Look up a coding_type's definition and required columns
#'
#' @param type Character, a coding_type name (e.g. "categorical_tier").
#'   If omitted, prints all coding_types.
#' @param schema_path Path to rule_coding_types.yaml
#'
#' @return Invisibly returns the schema list for `type`; prints a
#'   readable summary as a side effect
#' @export
coding_type_info <- \(type = NULL, schema_path = "settings/rule_coding_types.yaml") {
  
  schema <- yaml::read_yaml(schema_path)$coding_types
  
  if (is.null(type)) {
    cat("Available coding_types:\n")
    cat(paste0("  - ", names(schema), collapse = "\n"), "\n")
    cat("\nCall coding_type_info(\"<name>\") for details.\n")
    return(invisible(schema))
  }
  
  if (!type %in% names(schema)) {
    stop("Unknown coding_type: ", type, "\nAvailable: ", paste(names(schema), collapse = ", "))
  }
  
  spec <- schema[[type]]
  
  cat("coding_type:", type, "\n\n")
  cat(trimws(spec$description), "\n\n")
  cat("Required columns:", paste(spec$required_cols, collapse = ", "), "\n")
  if (!is.null(spec$optional_cols) && length(spec$optional_cols) > 0) {
    cat("Optional columns:", paste(spec$optional_cols, collapse = ", "), "\n")
  }
  
  invisible(spec)
}