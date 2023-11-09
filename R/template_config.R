#' @export
format_edge_type <- function(edge_types) {
  et <- dplyr::bind_rows(lapply(edge_types, function(x) data.frame(value = x[[2]], schema_name = x[[1]])))
  components <- et |>
    dplyr::filter(tolower(value) == "component") |>
    dplyr::pull(schema_name)
  et |>
    dplyr::filter(value %in% c("Component", "Filename")) |>
    dplyr::group_by(schema_name) |>
    dplyr::summarise(file_based = "Filename" %in% value) |>
    dplyr::filter(schema_name %in% components)
}

#' @export
get_display_names <- function(qlist) {
  if (!"schema_url" %in% names(qlist)) stop("qlist needs element named `schema_url`")
  if (!"node_list" %in% names(qlist)) stop("qlist needs at least one element named `node_list`")
  httr::GET(
    url = "https://schematic-dev.api.sagebionetworks.org/v1/schemas/get_nodes_display_names",
    query = qlist
  )
}

#' @export
create_template_config <- function(data_model, include_schemas = NULL, exclude_schemas = NULL) {
  if (!is.null(include_schemas) && !is.null(exclude_schemas)) stop("include_schemas and exclude_schemas cannot both have values")
  edges <- graph_by_edge_type(schema_url = data_model)
  schema_names <- format_edge_type(edges)
  nl <- setNames(as.list(schema_names$schema_name), rep("node_list", length(schema_names$schema_name)))
  dnames <- get_display_names(c(schema_url = data_model, nl)) |> httr::content()
  config <- data.frame(display_name = unlist(dnames), schema_name = unlist(nl)) |>
    dplyr::left_join(schema_names, by = "schema_name") |>
    dplyr::mutate(type = ifelse(file_based, "file", "record")) |>
    dplyr::select(-file_based)
  if (!is.null(include_schemas)) {
    if (any(length(x <- setdiff(include_schemas, config$schema_name)))) stop(sprintf("%s is not a schema name in the data model", x))
    config <- dplyr::filter(config, schema_name %in% include_schemas)
  }
  if (!is.null(exclude_schemas)) {
    if (any(length(y <- setdiff(exclude_schemas, config$schema_name)))) stop(sprintf("%s is not a schema name in the data model", y))
    config <- dplyr::filter(config, !schema_name %in% exclude_schemas)
  }
  config
}

#' @export
create_dca_template_config <- function(data_model, include_schemas = NULL, exclude_schemas = NULL) {
  df <- create_template_config(data_model, include_schemas, exclude_schemas)
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
write_dca_template_config <- function(data_model, file, include_schemas = NULL, exclude_schemas = NULL) {
  df <- create_dca_template_config(data_model, include_schemas, exclude_schemas)
  jsonlite::write_json(df, file, pretty = TRUE, auto_unbox = TRUE)
}
