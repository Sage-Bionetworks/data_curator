
list2Vector <- function(list) {
  vector <- c()
  for (i in seq_along(list)) {
    vector[list[[i]][[2]]] <- list[[i]][[1]]
  }
  return(vector)
}

# convert long string from "x1, x2, x3, x4, x5" into "x1, x2 ... x5"
truncate_ellipsis <- function(string, max, pattern = NULL) {
  if (!is.null(pattern)) {
    string <- str_split(string, pattern)
  }

  sapply(string, function(i) {
    n <- length(i)
    if (n > max) {
      firstMaX <- str_c(i[1:(max - 1)], collapse = ", ")
      concatenated_str <- str_c(c(firstMaX, "...", i[n]), collapse = " ")
    } else {
      concatenated_str <- str_c(i, collapse = ", ")
    }
    return(paste0("[", concatenated_str, "]"))
  })
}