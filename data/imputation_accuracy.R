
# Final imputation method used
tar_load(pick_method_exp, 
         store = "/mnt/envshare/dev/out/Trait_impute/_targets/method_choice")

# Accuracy test - categorical
tar_load(impute_combined_cat_exp, 
         store = "/mnt/envshare/dev/out/Trait_impute/_targets/impute_exp")

# Accuracy test - numerical
tar_load(impute_combined_num_exp, 
         store = "/mnt/envshare/dev/out/Trait_impute/_targets/impute_exp")

## Matching trait accuracy by closest missing rate
# NRMSE above 1 means the imputation is worse than just using the trait mean. Kappa near 0 means agreement no better than chance.
# kappa < 0.4: fair; 0.4-0.6:moderate; > 0.6-0.8 confidential; > 0.8 almost perfect
# nrmse threshold: 0.75

library(dplyr)

summarise_matched <- function(pick, val, metric) {
  # Only keep traits that were scored in this validation set
  pick <- pick |>
    filter(trait %in% unique(val$trait)) |>
    rename(target_miss = miss_rate)
  
  # Simulated miss-rate grid per trait x method x phylo_model
  grid <- val |> distinct(trait, method, phylo_model, miss_rate)
  
  # Closest simulated miss_rate for each picked method
  matched <- pick |>
    left_join(grid,
              by = c("trait", "method", "phylo_model"),
              na_matches = "na",              # so NA phylo_model matches NA
              relationship = "many-to-many") |>
    group_by(trait, method, phylo_model, target_miss) |>
    slice_min(abs(miss_rate - target_miss), n = 1, with_ties = FALSE) |>
    ungroup() |>
    rename(matched_miss = miss_rate)
  
  # Pull the replicate-level scores at that matched rate and summarise
  matched |>
    left_join(val,
              by = c("trait", "method", "phylo_model",
                     "matched_miss" = "miss_rate"),
              na_matches = "na") |>
    group_by(trait, method, phylo_model, target_miss, matched_miss) |>
    summarise(
      n_rep = sum(!is.na(.data[[metric]])),
      mean  = mean(.data[[metric]], na.rm = TRUE),
      sd    = sd(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(gap = abs(matched_miss - target_miss)) |>
    rename_with(~ paste0(metric, "_", .x), c(mean, sd))
}

acc_num <- summarise_matched(pick_method_exp, impute_combined_num_exp, "nrmse")
acc_cat <- summarise_matched(pick_method_exp, impute_combined_cat_exp, "kappa")

# One row per trait
accuracy_by_trait <- pick_method_exp |>
  select(trait, method, phylo_model, miss_rate) |>
  left_join(select(acc_num, trait, matched_miss_num = matched_miss,
                   nrmse_mean, nrmse_sd), by = "trait") |>
  left_join(select(acc_cat, trait, matched_miss_cat = matched_miss,
                   kappa_mean, kappa_sd), by = "trait")

accuracy_class <- accuracy_by_trait |>
  mutate(
    kappa_class = case_when(
      is.na(kappa_mean)  ~ NA_character_,
      kappa_mean < 0.4   ~ "fair",
      kappa_mean < 0.6   ~ "moderate",
      kappa_mean <= 0.8  ~ "substantial",
      TRUE               ~ "almost perfect"
    ),
    kappa_class = factor(kappa_class,
                         levels = c("fair", "moderate", "substantial", "almost perfect")),
    
    nrmse_class = case_when(
      is.na(nrmse_mean)  ~ NA_character_,
      nrmse_mean <= 0.75 ~ "acceptable",
      TRUE               ~ "poor"
    ),
    
    # single column for reporting
    accuracy = coalesce(as.character(kappa_class), nrmse_class),
    metric   = case_when(!is.na(kappa_mean) ~ "kappa",
                         !is.na(nrmse_mean) ~ "nrmse")
  )

accuracy_class |>
  select(trait, method, phylo_model, miss_rate,
         metric, kappa_mean, nrmse_mean, accuracy) |> View()
