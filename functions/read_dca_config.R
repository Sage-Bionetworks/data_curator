#' @title Read the DCA config file and report issues
#' @param config URL or filepath to a DCA JSON config file
read_dca_config <- function(config) {
  conf <- jsonlite::fromJSON(config)
  
  name_check <- function(req, prov) {
    if (!all(req %in% prov)) {
      which_miss <- req[which(!req %in% prov)]
      stop(sprintf("DCA config missing %s", which_miss))
    }
  }
  
  lvl_1_props_req <- list(
    "dcc" = list(),
    "dca" = list(),
    "schematic" = list()
  )
  lvl_1_props_ops <- list() # Placeholder for optional properties
  lvl_1_props_conf <- names(conf)
  name_check(names(lvl_1_props_req), lvl_1_props_conf)
  
  dca_props_req <- list() # Placeholder for required DCA properties
  dca_props_ops <- list(
    "use_compliance_dashboard" = FALSE,
    "primary_col" = "#2a668d",
    "secondary_col" = "#184e71",
    "sidebar_col" = "#191919"
  )
  dca_props_conf <- names(conf$dca)
  name_check(names(dca_props_req), dca_props_conf)
  
  if (!"use_compliance_dashboard" %in% dca_props_conf) {
    conf$dca$use_compliance_dashboard <- FALSE
  }
  if (!"primary_col" %in% dca_props_conf) {
    conf$dca$primary_col <- "#2a668d"
  }
  if (!"secondary_col" %in% dca_props_conf) {
    conf$dca$secondary_col <- "#184e71"
  }
  if (!"primary_col" %in% dca_props_conf) {
    conf$dca$sidebar_col <- "#191919"
  }
  
  # required elements should not have a default. Should error if not provided.
  # WIP, confirm required and move others to ops with defaults
  schematic_props_req <- list(
    "manifest_generate" = list(),
    "model_validate" = list(),
    "model_submit" = list()
  )
  schematic_props_ops <- list(
    "global" = list()
  )
  schematic_props_conf <- names(conf$schematic)
  name_check(names(schematic_props_req), schematic_props_conf)
  
  if (!"global" %in% schematic_props_conf) {
    conf$schematic$global <- list()
  }
  
  global_ops <- list(
    "data_model_labels" = "class_label"
  )
  global_conf <- names(conf$schematic$global)
  if (!"data_model_labels" %in% global_conf) {
    conf$schematic$global$data_model_labels <- "class_label"
  }
  
  # required elements should not have a default. Should error if not provided.
  # WIP, confirm required and move others to ops with defaults
  mg_props_req <- list(
    "output_format" = "excel",
    "use_annotations" = TRUE
  )
  mg_props_ops <- list()
  mg_props_conf <- names(conf$schematic$manifest_generate)
  name_check(names(mg_props_req), mg_props_conf)
  
  # required elements should not have a default. Should error if not provided.
  # WIP, confirm required and move others to ops with defaults
  mv_props_req <- list(
    "restrict_rules" = FALSE
  )
  mv_props_ops <- list(
    "cross_manifest_validation" = FALSE
  )
  mv_props_conf <- names(conf$schematic$model_validate)
  name_check(names(mv_props_req), mv_props_conf)
  
  if (!"cross_manifest_validation" %in% mv_props_confg) {
    conf$schematic$model_validate$cross_manifest_validation <- FALSE
  }
  
  # required elements should not have a default. Should error if not provided.
  # WIP, confirm required and move others to ops with defaults
  ms_props_req <- list(
    "table_manipulation" = "replace",
    "manifest_record_type" = "file_only",
    "hide_blanks" = FALSE
  )
  ms_props_ops <- list()
  ms_props_conf <- names(conf$schematic$model_submit)
  
  conf
  
}
