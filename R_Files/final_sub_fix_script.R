library(dplyr)

# ── Configuration ────────────────────────────────────────────────────────────
# Paths are now relative to your getwd()
template_path <- "march-machine-learning-mania-2026/SampleSubmissionStage2.csv"
output_dir    <- "submission_csv_files"

# Ensure the output directory exists in your local repo
if (!dir.exists(output_dir)) dir.create(output_dir)

# Load the official template once
sample_sub <- read.csv(template_path)

# ── Function ──────────────────────────────────────────────────────────────────
create_submission <- function(input_rel_path, output_filename) {
  
  if (!file.exists(input_rel_path)) {
    warning("Skipping: File not found at ", input_rel_path)
    return(NULL)
  }
  
  # Read individual model predictions
  my_preds <- read.csv(input_rel_path)
  
  # Join with template to ensure correct ID order and length
  # Uses 0.5 as a neutral fallback for missing matchups
  final_sub <- sample_sub %>%
    select(ID) %>%
    left_join(my_preds, by = "ID") %>%
    mutate(Pred = ifelse(is.na(Pred), 0.5, Pred))
  
  # Save to the local submission folder
  write.csv(final_sub, file.path(output_dir, paste0(output_filename, ".csv")), row.names = FALSE)
  
  message("Successfully generated: ", output_filename)
}

# ── Execution ─────────────────────────────────────────────────────────────────

# Define your input files (Relative to your project root)
models <- list(
  c(path = "submission_csv_files/xgboost_overrides_2026.csv",             name = "xgboost_final_sub"),
  c(path = "submission_csv_files/DO_Model_submission_overrides_2026.csv", name = "DO_final_sub"),
  c(path = "submission_csv_files/Linear_Reg_Monte_Carlo_overrides_2026.csv", name = "LIn_Reg_final_sub")
)

# Run the loop
invisible(lapply(models, function(m) create_submission(m["path"], m["name"])))
