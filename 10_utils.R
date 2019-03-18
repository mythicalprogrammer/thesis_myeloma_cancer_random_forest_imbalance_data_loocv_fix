sinkall <- function() {
  i <- sink.number()
  if (i > 0) {
    for (i in 1:i) {
      sink()
    }
  }
}

create_confusion_matrix <-
  function(
    pred,
    actual,
    num_tree) {
  file_path  <- str_c('confusion_matrices/random_forest_confusion_matrix_num_tree_',
                      num_tree,
                      '.txt')
  sink(file_path)
  print(confusionMatrix(pred, actual))
  sinkall()
}
