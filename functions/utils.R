
# convert [list] parent_list: child_list1, child_list2
# to [vector]: child_list1 with name as child_list2
list2Vector <- function(list) {
  child <- sapply(list, `[[`, 1)
  names(child) <- sapply(list, `[[`, 2)

  return(child)
}

runTime <- function(expr) {
  t1 <- Sys.time()
  expr
  t2 <- Sys.time() - t1
  return(t2)
}

# convert long string from "x1, x2, x3, x4, x5" into "x1, x2 ... x5"
TruncateEllipsis <- function(string, max, split_pattern = NULL) {
  if (!is.null(split_pattern)) {
    string <- str_split(string, split_pattern)
  }

  sapply(string, function(i) {
    n <- length(i)
    if (n > max) {
      firstMaX <- str_c(i[1:(max - 1)], collapse = ", ")
      str_c(c(firstMaX, "...", i[n]), collapse = " ")
    } else {
      str_c(i, collapse = ", ")
    }
  })
}
