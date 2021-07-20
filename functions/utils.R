
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
