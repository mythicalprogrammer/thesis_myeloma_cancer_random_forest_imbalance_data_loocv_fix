dir_path <- "./intermediate_data"
results_path <- list.files(path = dir_path, full.names = TRUE)

for (i in results_path) {
  pred_df <- read.csv(file = i,
                      header = TRUE,
                      sep = ",")
  tmp_str <- str_sub(i, 58)
  cur_max_tree <- substr(tmp_str, 1, nchar(tmp_str) - 4)
  create_confusion_matrix(pred_df$predictions, myeloma_data$state, cur_max_tree)
}
