get_override_status_from_synapse_user_id <- function(permission_table, user_teams){
  # The following code chunk ensures that only members of specific teams can override validation in the app.

  # the teams that user belongs t
  all_teams <- purrr::map_chr(user_teams, function(x) x$id)
  
  # the teams with override access
  dashboard_teams <- syn_tableQuery(glue::glue("select * from {permission_table}"))$asDataFrame()
  allowed_teams <- sapply(dashboard_teams$TeamID, jsonlite::fromJSON)
  
  #final allowed agencies
  allowed_teams <- all_teams[all_teams %in% allowed_teams]

  if(length(allowed_teams)>0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
