# This is the module to create one/two buttons to switch to previous/next tab
tagID <- c("Next", "Prev") # Do not change order, to ensure server works properly

switchTabUI <- function(id, direction = c("left", "right", "both")) {

  # namespace
  ns <- NS(id)
  tags$head()
  # if we put buttons in server, buttons will change after observing tabs' change  delay
  # which cause some add-remove tranisition in delay
  # now, put buttons in UI
  btn_prev <- actionButton(ns(tagID[2]), class = "switch-tab-prev", lapply(1:3, function(i) tags$i(class = "fa fa-angle-left")))
  btn_next <- actionButton(ns(tagID[1]), class = "switch-tab-next", lapply(1:3, function(i) tags$i(class = "fa fa-angle-right")))
  fluidRow(
    if (direction == "right") {
      column(1, offset = 5, btn_next)
    } else if (direction == "left") {
      column(1, offset = 1, btn_prev)
    } else {
      div(column(1, offset = 1, btn_prev), column(1, offset = 8, btn_next))
    }
  )
}


switchTabServer <- function(id, tabId, tab, tabList, parent) {
  moduleServer(
    id,
    function(input, output, session) {
      lapply(c(-1, 1), function(i) {
        tagName <- tagID[i]
        observeEvent(input[[tagName]], {
          current_tab <- which(tabList == tab)
          # need to use parent session to update tab
          # TODO: figure out how to call parent inputs in module to minimize args
          updateTabItems(parent, tabId, selected = tabList[current_tab + i])
        })
      })
    }
  )
}
