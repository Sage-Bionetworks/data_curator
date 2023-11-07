#' @export
format_edge_type <- function(edge_types) {
  et <- dplyr::bind_rows(lapply(edge_types, function(x) data.frame(value=x[[2]], schema_name=x[[1]])))
  et |> dplyr::filter(value %in% c("Component", "Filename")) |> 
    dplyr::group_by(schema_name) |> 
    dplyr::summarise(file_based = "Filename" %in% value)
}

#' @export
get_display_names <- function(qlist) {
  if (!"schema_url" %in% names(qlist)) stop("qlist needs element named `schema_url`")
  if (!"node_list" %in% names(qlist)) stop("qlist needs at least one element named `node_list`")
  httr::GET(url = "https://schematic-dev.api.sagebionetworks.org/v1/schemas/get_nodes_display_names",
            query = qlist
  )
}

#' @export
create_template_config <- function(data_model) {
  edges <- graph_by_edge_type(schema_url = data_model)
  schema_names <- format_edge_type(edges)
  nl <- setNames(as.list(schema_names$schema_name), rep("node_list", length(schema_names$schema_name)))
  dnames <- get_display_names(c(schema_url = data_model, nl)) |> httr::content()
  data.frame(display_name = unlist(dnames), schema_name = unlist(nl)) |>
    dplyr::left_join(schema_names, by = "schema_name") |>
    dplyr::mutate(type = ifelse(file_based, "file", "record")) |>
    dplyr::select(-file_based)
}

#' @export
create_dca_template_config <- function(data_model) {
  df <- create_template_config(data_model)
  schematic_version <- httr::GET("https://schematic-dev.api.sagebionetworks.org/v1/version") |>
    httr::content()
  list(
    manifest_schemas = df,
    service_version = schematic_version,
    schema_version = ""
  )
}

#' @export
#' @description Create a DCA-specific template generation function
write_dca_template_config <- function(data_model, file) {
  df <- create_dca_template_config(data_model)
  jsonlite::write_json(df, file, pretty = TRUE, auto_unbox = TRUE)
}
