progressBarUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("progress"))
}

progressBarServer <- function(id, value, title = NULL, subtitle = NULL, display_pct = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      
      new_value = 100 - value

      output$progress <- renderUI({
        tagList(
          div(class = "progress-group",
            tagList(
              if (!is.null(title)) span(title, class = "progress-title", style = "font-weight: 600;"),
              if (!is.null(subtitle)) span(subtitle, class = "progress-subtitle", style = "font-style: italic;")
            ),
            div(class = "progress",
              style = "
                position: relative;
                height: 20px;
                margin-top: 8px;
                overflow: hidden;
                background-image: linear-gradient(
                to right,
                #E53935,
                #ff9698,
                #ad6fbc,
                #6162a5
              );
              border-radius: 25px;
              border: #000 1px solid;
              ",
              if (display_pct)
                span(paste0(value, "%"), class = "progress-pct",
                  style = paste0("
                    position: absolute;
                    top: -1px;
                    width: ", value, "%;
                    font-size: 12px;
                    line-height: 20px;
                    color: #fff;
                    text-align: center;
                  ")
                ),
              div(class = "progress-value",
                style = paste0("
                  float: right;
                  width:", new_value, "%;
                  height: 100%;
                  background-color: #f5f5f5;
                  box-shadow: inset 0 -1px 0 rgb(0 0 0 / 15%);
                  transition: width .6s ease;
                ")
              )
            )
          )
        )
      })
    }
  )
}