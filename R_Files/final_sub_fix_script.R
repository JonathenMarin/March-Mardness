template_path <- "C:/Users/jonathenmarin/Documents/March-Mardness/march-machine-learning-mania-2026/SampleSubmissionStage2.csv"
sample_sub <- read.csv(template_path)

create_submission <- function(input_path, output_filename) {
  
  my_preds <- read.csv(input_path)
  
  
  final_sub <- merge(sample_sub["ID"], my_preds, by = "ID", all.x = TRUE)
  final_sub$Pred[is.na(final_sub$Pred)] <- 0.5
  
  
  output_dir <- "C:/Users/jonathenmarin/Documents/March-Mardness/submission_csv_files/"
  full_save_path <- paste0(output_dir, output_filename, ".csv")
  
  write.csv(final_sub, full_save_path, row.names = FALSE)
  cat("Saved:", output_filename, ".csv\n")
}


xgb_path <- "C:/Users/jonathenmarin/Documents/March-Mardness/submission_csv_files/xgboost_overries_2026.csv"
do_path  <- "C:/Users/jonathenmarin/Documents/March-Mardness/submission_csv_files/DO_Model_submission_overrides_2026.csv"
lin_path <- "C:/Users/jonathenmarin/Documents/March-Mardness/submission_csv_files/Linear_Reg_Monte_Carlo_overrides_2026.csv"


create_submission(xgb_path, "xgboost_final_sub")
create_submission(do_path, "DO_final_sub")
create_submission(lin_path, "LIn_Reg_final_sub")
