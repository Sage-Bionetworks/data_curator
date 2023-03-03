
# tranform the list output from schematic to named vectore
# convert [list] parent_list: child_list1, child_list2
# to [vector]: child_list1 with name as child_list2
list2Vector <- function(list) {
  child <- sapply(list, `[[`, 1)
  names(child) <- sapply(list, `[[`, 2)

  return(child)
}

# truncate long string with ellipsis, e.g. "x1, x2, x3, x4, x5" into "x1, x2 ... x5"
# TODO: consider replacing by stringr::str_trunc
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

# parse environment variables for configuration
parse_env_var <- function(x, el_delim=",", kv_delim=":"){
  if (!grepl(kv_delim, x)) stop(sprintf("%s delimiter not in %s", kv_delim, x))
  # assume string of key-value pairs
  elements <- stringr::str_split(x, el_delim, simplify = TRUE)
  unlist(lapply(elements, function(y){
    kv <- stringr::str_split(y, kv_delim, n=2)
    setNames(kv[[1]][[2]], kv[[1]][[1]])
  }))
}

# Map logo information for each synapse ID.
update_logo <- function(project = "sage") {
  
  img <- switch(project,
                syn20446927 = list(href = "https://humantumoratlas.org/",
                                   img_src = "img/HTAN_text_logo.png"),
                syn27210848 = list(href = "https://cancercomplexity.synapse.org/",
                                   img_src = "img/cckp_logo.png"),
                syn30109515 = list(href = "https://https://includedcc.org/",
                                   img_src = "img/INCLUDE DCC Logo-01.png"),
                list(href = "https://synapse.org",
                     img_src = "img/Logo_Sage_Logomark.png")
  )
  
  tags$li(
    class = "dropdown", id = "logo",
    tags$a(
      href = img$href,
      target = "_blank",
      tags$img(
        height = "40px", alt = "LOGO",
        src = img$img_src
      )
    )
  )
}