
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

# add tooltip; only support its position on the top for now
addTooltip <- function(.data, message, position = c("top")) {
  position <- match.arg(position, c("top"))
  tooltip_class <- paste0("dc-tooltip ", position)
  .data %>%
    tagAppendAttributes(`aria-label` = message) %>%
    tagAppendAttributes(class = tooltip_class)
}