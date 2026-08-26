library(dplyr)
library(targets)

# packages --------
envFunc::check_packages(yaml::read_yaml("settings/packages.yaml") |> 
                          unlist() |> 
                          unname() |> 
                          unique(), 
                        update_env = TRUE
)

# tars --------

## local ------
store_base <- envFunc::get_env_dir() |>
  fs::path_rel() |>
  fs::path(if(grepl("\\/prod\\/", here::here())) "prod" else "dev"
           , "out"
  )

tars_local <- envTargets::make_tars(settings = envFunc::extract_scale("Exposure"),
                                    save_yaml = FALSE,
                                    store_base = store_base)

## Other projects ------
tars_rec <- envTargets::make_tars(settings = envFunc::extract_scale("RecExtract"),
                                  project_base = fs::path("..", "RecExtract"),
                                  local = FALSE,
                                  list_names = "store")

tars_pia <- envTargets::make_tars(settings = envFunc::extract_scale("envPIA_b"),
                                   project_base = fs::path("..", "envPIA_b"),
                                   local = FALSE,
                                   list_names = c("extent", "grain", "aoi"))

tars_status <- envTargets::make_tars(settings = envFunc::extract_scale("Status"),
                                     project_base = fs::path("..", "Status"),
                                     local = FALSE)

tars = c(tars_local, tars_rec, tars_pia, tars_status)

envTargets::write_tars(tars)

# run everything ----------
# in _targets.yaml
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
)

# prune everything ----------
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_prune(script = x, store = y)
)
