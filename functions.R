### if synapser used 
# detach("package:synapser", unload=TRUE)
# unloadNamespace("synapser")
# install.packages("reticulate") ## if libpython = NA sometimes reinstalling reticulate works
# library(reticulate)

# use_condaenv('py3.5', required = TRUE ) ### use path to python 3.5 env
# py_discover_config()

# reticulate::import("sys")
# reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")

# source_python("synStore_Session.py")

### functions to get filename, foldername

### get folder_synID 
get_folder_synID <- function(synStore_obj, project_synID, selected_folder) {
  folder_list <- get_folder_list(synStore_obj, project_synID)
  folders_namedList <- c()
  for (i in seq_along(folder_list)) {
    folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
  }
  
  folder_synID <- folders_namedList[[selected_folder]]
  return(folder_synID)
}


### get file list
get_filename_list <- function(synStore_obj, folder_synID) {
  file_list <- get_file_list(synStore_obj, folder_synID)
  
  file_namedList <- c()
  for (i in seq_along(file_list)) {
    file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
  }
  filename_list <- names(file_namedList)
  return(filename_list)
}

######============= dashboard dataframe 
### logs in and gets list of projects they have access to
get_proj_folder_manifest_cells_df <- function(synStore_obj, projects_namedList, sessionToken){ ## gets user into 

  ### for each component (biosamp, assay, clinical) right now only assay 

  ### project to folder dataframe generation
  # get fileview of folders and files
  syn <- syn_login(sessionToken = sessionToken)
  tab <- syn_tableQuery("select * from syn20446927", resultsAs='csv')
  tab <- read.csv(tab$filepath)
  # get just folders and match parent ID to project name
  projects_df <- stack(projects_namedList)
  colnames(projects_df) <- c("parentId", "project_name")
  proj_folder_df <- inner_join(tab %>% filter(type == "folder"), projects_df)

  all_folders_list <- as.character(proj_folder_df$id)

  ### get manifest paths per folder
  folders_manifest_df <- data.frame()
  for (i in seq_along(all_folders_list)) {
    # folders_manifest_df[]
    print(i)
    print(all_folders_list[i])
    folders_manifest_df[i,1] <- all_folders_list[i]
    path <- get_storage_manifest_path(sessionToken , all_folders_list[i])
    if ( is.null(path) ) { ## if no manifest is uploaded 
      folders_manifest_df[i,2] <- NA  
    } else {
    folders_manifest_df[i,2] <- path
    }
  }

  colnames(folders_manifest_df) <- c("id", "manifest_path")

  proj_folder_manifest <- inner_join(folders_manifest_df, proj_folder_df, by = "id")

  ### get the manifest synID
  # entity <- syn_tableQuery(  ("select id, name from syn20446927 where name = 'synapse_storage_manifest.csv' " ), resultsAs='csv'  )
  # entity <- read.csv(entity$filepath)
  # 
  # manifest_path_df <- data.frame()
  # for (i in seq_along(entity$id)) {
  #   manifest_path_df[i,1] <- entity$id[i] ##id of manifest
  #   syn_login(sessionToken ="10b3258a-1486-4663-9018-822b034dd84c")
  #   fh <- syn_get(entity$id[i])
  #   manifest_path_df[i,2] <- fh$path
  # }
  # colnames(manifest_path_df) <- c("id", "manifest_path")
  # 
  # inner_join(manifest_path_df, tab)

  ### end faster way?

  ### get metrics for each manifest
  manifest_cells <- data.frame()
  for (i in seq_along(proj_folder_manifest$id)) {
    print(i)
    manifest_path <- proj_folder_manifest$manifest_path[i]
    manifest_cells[i, 1] <- proj_folder_manifest$id[i]
    manifest_cells[i, 2] <- manifest_path
    
    if ( !is.na(manifest_path)) {
      manifest_df <- read.csv(manifest_path)
      
      ### subset cols by component type ASSAY
      assay_cols <- manifest_df[, c("Cancer.Type", "Library.Construction.Method")]
      
      # how to count empty cells vs all cells
      dim_val <- dim(assay_cols)
      total_cells <- dim_val[1] * dim_val[2]
      empty_cells <- table(is.na(assay_cols))
      per_filled <- (empty_cells[1] / total_cells )* 100
      
      manifest_cells[i,3] <- "Assay"
      manifest_cells[i,4] <- total_cells
      manifest_cells[i,5] <- empty_cells[1] ## filled, empty= F
      manifest_cells[i,6] <- per_filled
      
      
      HTAN_cols <- manifest_df[, c("HTAN.Participant.ID", "HTAN.Sample.ID")]
      # how to count empty cells vs all cells
      dim_val <- dim(HTAN_cols)
      total_cells <- dim_val[1] * dim_val[2]
      empty_cells <- table(is.na(HTAN_cols))
      per_filled <- (empty_cells[1] / total_cells )* 100
      
      manifest_cells[i,7] <- "HTAN_IDs"
      manifest_cells[i,8] <- total_cells
      manifest_cells[i,9] <- empty_cells[1] ## filled, empty= F
      manifest_cells[i,10] <- per_filled
    } else {
      # print(proj_folder_manifest$id[i])
      manifest_cells[i,3] <- "Assay"
      manifest_cells[i,4] <- 0
      manifest_cells[i,5] <- 0
      manifest_cells[i,6] <- 0

      manifest_cells[i,7] <- "HTAN_IDs"
      manifest_cells[i,8] <- 0
      manifest_cells[i,9] <- 0 ## filled, empty= F
      manifest_cells[i,10] <- 0
      
    }
  }

  ### NAs to 0 but not for manifest path
  manifest_cells[,-c(2)][is.na((manifest_cells[,-c(2)]))] <- 0

  colnames(manifest_cells) <- c("id", "manifest_path", "component", "total_cells","filled_cells", "percent_filled", "component2", "total_cells2","filled_cells2", "percent_filled2")

  proj_folder_manifest_cells <- right_join(manifest_cells, proj_folder_manifest )
  return(proj_folder_manifest_cells)
}

### do in server
# minimal <- proj_folder_manifest_cells[, c("project_name", "name", "component", "total_cells","filled_cells", "percent_filled", "component2", "total_cells2","filled_cells2", "percent_filled2")]

# colnames(minimal)[7:10] <- c("component", "total_cells","filled_cells", "percent_filled")

# minimal_long <- rbind(minimal[,1:6], minimal[,c(1:2,7:10) ])

# ### get the wrapped project names to fit
# minimal_long <- mutate(minimal_long, project = str_wrap(project_name, width = 10))

# plot_percentFilled <- ggplot(minimal_long, aes(x= name, y = percent_filled, fill = component)) + 
#   geom_histogram(stat= "identity", position = "dodge")  + 
#   coord_flip() +
#   facet_grid(project ~ ., 
#              scales = "free_y", space = "free_y",
#              drop = TRUE)

create_synapse_links <- function(
  df, link_keys
) {
  base_url <- "https://www.synapse.org/#!Synapse:"
  link_template <- glue::glue(
    "<a href='{base}{{id}}' target='_blank'>{{target}}</a>",
    base = base_url
  )
  link_keys %>%
    walk2(names(.), function(id_key, target_key) {
      target_col <- as.name(target_key)
      id_col <- as.name(id_key)
      df <<- df %>%
        mutate(
          UQ(target_col) :=
            ifelse(!is.na(UQ(target_col)),
                   glue::glue(
                     link_template,
                     id = UQ(id_col),
                     target = UQ(target_col)
                   ),
                   UQ(target_col))
        )
    })
  df
}
### do in server
# proj_folder_manifest_cells <- proj_folder_manifest_cells %>% 
#   mutate('Synapse Project Folder' = id) %>% 
# create_synapse_links( list('Synapse Project Folder' = "id") )
