# flags any line with `,,` or a comma immediately before a closing bracket
fs::dir_ls("R", regexp = "\\.R$", recurse = TRUE) |>
  purrr::walk(\(f) {
    lines <- readr::read_lines(f)
    hits <- stringr::str_which(lines, ",\\s*,|,\\s*\\)|,\\s*\\]")
    if (length(hits) > 0) {
      cat(f, "— lines:", paste(hits, collapse = ", "), "\n")
    }
  })