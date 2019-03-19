myeloma_data <- read.table("start_data/GDS531_after_anova.csv",
                            sep = ",",
                            header = TRUE)



rand_seed <- 1030
max_tree <- 1000
minority_nrow <- nrow(myeloma_data[myeloma_data$state == "WO",])
nr <- nrow(myeloma_data)
for (j in 117:max_tree) {
  set.seed(rand_seed)
  random_forest_predictions <- list()
  for (i in  1:nr) {
    # remove subject to validate
    train_data <- myeloma_data[-c(i), ]
    train_minority <- train_data[train_data$state == "WO",]
    train_majority <- train_data[train_data$state == "W",]

    # get subsample to balance dataset
    indices <- sample(nrow(train_majority), minority_nrow)
    train_majority <- train_majority[indices, ]
    train_data <- rbind(train_minority, train_majority)

    test <- myeloma_data[i,]
    rf.fit <- ranger(state ~ ., data = train_data, num.trees = j, seed = rand_seed)
    pred <- predict(rf.fit, test)
    random_forest_predictions[[i]] <- pred$predictions
   }

  pred_df <- data.frame(unlist(random_forest_predictions))
  names(pred_df) <- "predictions"
  # back up the result since this took awhile to run
  file_path  <- str_c('intermediate_data/LOOCV_random_forest_results_num_tree_',
                      j, '.csv')
  write.csv(pred_df,
             file = file_path,
             row.names = FALSE)
}
