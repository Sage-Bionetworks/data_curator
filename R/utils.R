#' @title parse environment variables for configuration
#' @param x string
#' @param el_delim delimeter of list elements
#' @param kv_delim delimeter of key-value pairs
#' @export
parse_env_var <- function(x, el_delim=",", kv_delim=":"){
  if (!grepl(kv_delim, x)) stop(sprintf("%s delimiter not in %s", kv_delim, x))
  # assume string of key-value pairs
  elements <- stringr::str_split(x, el_delim, simplify = TRUE)
  unlist(lapply(elements, function(y){
    kv <- stringr::str_split(y, kv_delim, n=2)
    setNames(kv[[1]][[2]], kv[[1]][[1]])
  }))
}