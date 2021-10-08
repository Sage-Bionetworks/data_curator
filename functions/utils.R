
list2Vector <- function(list) {
  vector <- c()
  for (i in seq_along(list)) {
    vector[list[[i]][[2]]] <- list[[i]][[1]]
  }
  return(vector)
}

# convert long string from "x1, x2, x3, x4, x5" into "x1, x2 ... x5" 
TruncateEllipsis <- function(string, max, split_pattern = NULL) {
  
  if (!is.null(split_pattern)) {
    string <- str_split(string, split_pattern)
  }
  
  sapply(string,function(i) {
    n <- length(i)
    if (n > max) {
      firstMaX <- str_c(i[1:(max-1)], collapse = ", ")
      str_c(c(firstMaX, "...", i[n]), collapse = " ")
    } else {
      str_c(i, collapse = ", ")
    }
  })
}
