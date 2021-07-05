
# convert [list] parent_list: child_list1, child_list2 
# to [vector]: child_list1 with name as child_list2 
list2Vector <- function(list) {
  
  child <- sapply(list, `[[`, 1)
  names(child) <- sapply(list, `[[`, 2)

  return(child)
}

# process the output from schematic: getProjectManifests()
extractManifests <- function(list) {

  list <- list[lengths(list) != 0]
  df <- data.frame()

  if (length(list) != 0) {
    df <- data.frame(
      synID = sapply(list, `[[`, c(1, 1)),
      schema = sapply(list, `[[`, c(1, 2)),
      create = sapply(list, `[[`, c(1, 3)),
      modify = sapply(list, `[[`, c(1, 4))
    ) %>% 
      filter(schema != "" & schema != "NaN") %>% 
      distinct(schema, .keep_all = TRUE)
    # display_list <- names(template_namedList)[match(all_req, template_namedList)]
    # names(display_list) <- names(template_namedList)[match(names(all_req), template_namedList)]
  }

  return(df)
}

runTime <- function(expr) {
  t1 <- Sys.time()
  expr
  t2 <- Sys.time() - t1
  return(t2)
}