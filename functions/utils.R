
list2Vector <- function(list) {
  vector <- c()
  for (i in seq_along(list)) {
    vector[list[[i]][[2]]] <- list[[i]][[1]]
  }
  return(vector)
}
