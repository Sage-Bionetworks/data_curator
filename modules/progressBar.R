progressBarUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("progress"))
}

progressBar <- function(id, value = 100, title = NULL, subtitle = NULL, 
                        display_pct = TRUE, width = NULL, height = NULL, r = 80,
                        circular = FALSE, color = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      if (is.null(width)) width <- ifelse(circular, "160px", "100%")
      if (is.null(height)) height <- ifelse(circular, "160px", "20px")
      if (circular) {
        r <- as.numeric(r)
        C <- 2 * pi * (r - 10) # svg circle r
      }

      ns <- session$ns

      # set values
      value <- round(as.numeric(value), 0)
      value <- case_when(
        value < 0 ~ 0,
        value > 100 ~ 100,
        TRUE ~ value
      )
      progress_value <- ifelse(circular, C * (1 - value / 100), 100 - value)

      # set delay time in ms
      dt <- 20

      # set color
      if(is.null(color)) color <- c("#e91e63", "#673ab7")

      # progress_id
      pb_id <- sample(1:10000, 1)
      # set id name for animation
      pct_id_name <- ifelse(circular, paste0("circular-pct-", pb_id), paste0("linear-pct-", pb_id))
      unique_var_name <- gsub("-", "_", pct_id_name)

      output$progress <- renderUI({
        tagList(
          if (circular) {
            tags$script(HTML(paste0(
              '
              var ', unique_var_name, '_Num = document.getElementById("', pct_id_name, '");
              var ', unique_var_name, '_Counter = 0;
              ', unique_var_name, '_Num.innerHTML = "0%";
              setInterval(() => {
                if (', unique_var_name, '_Counter == ', value, ') {
                  clearInterval();
                } else {
                  ', unique_var_name, '_Counter += 1;
                  ', unique_var_name, '_Num.innerHTML = ', unique_var_name, '_Counter + "%";
                }
              }, ', dt, ');
              '
            )))
          },
          # TODO: add animation for linear progress bar
          if (circular) {
            tags$style(paste0(
              '
              @keyframes circularSlide-', pb_id, ' {
                100% {
                  stroke-dashoffset: ',  progress_value, ';
                }
              };
              '
            ))
          },
          div(class="progress-group",
            tagList(
              if (!circular) {
                tagList(
                  div(class="progress-title-container",
                    tagList(
                      if (!is.null(title)) span(title, class = "progress-title"),
                      if (!is.null(subtitle)) span(subtitle, class = "progress-subtitle")
                    )
                  ),
                  div(class = "progress-linear",
                    style = paste0(
                      '
                      height: ', height, ';
                      width: ', width, ';
                      background-image: linear-gradient(
                        to right, ', color[1], ', ', tail(color, 1), ');
                      '
                    ),
                    div(class = "progress-linear-value",
                      style = paste0("
                        width:", progress_value, "%;
                      ")
                    ),
                    if(display_pct) {
                      div(id=pct_id_name, class="progress-pct linear")
                    }
                  )
              )
              } else {
                div(class="progress-circular",
                  tagList(
                    div(class="progress-title-container",
                      tagList(
                        if (!is.null(title)) span(title, class = "progress-title"),
                        if (!is.null(subtitle)) span(subtitle, class = "progress-subtitle")
                      )
                    ),
                    div(class="progress-circular-outer",
                      style = paste0(
                        '
                        height: ', 2 * r, 'px;
                        width: ', 2 * r, 'px;
                        '
                      ),
                      tagList(
                        div(class="progress-circular-inner",
                          style = paste0(
                            '
                            height: ', 2 * r - 40  , 'px;
                            width: ', 2 * r - 40, 'px;
                            '
                          ),
                          if(display_pct) div(id=pct_id_name, class="progress-pct circular")
                        ),
                        div(HTML(paste0(
                          '
                          <svg xmlns="http://www.w3.org/2000/svg" version="1.1" id = "progress-svg-', pb_id, '"
                            width=', 2 * r, ' height=', 2 * r, '>
                            <defs>
                                <linearGradient id="GradientColor-', pb_id, '">
                                  <stop offset="0%" stop-color=', color[1], ' />
                                  <stop offset="100%" stop-color=', tail(color, 1), ' />
                                </linearGradient>
                            </defs>
                            <circle cx="', r , '" 
                                    cy="', r, '" 
                                    r="', r - 10, '" 
                                    stroke="url(#GradientColor-', pb_id, ')" 
                                    stroke-linecap="round" 
                                    stroke-dasharray="', C , '"
                                    stroke-dashoffset="', C, '"
                                    style="animation: circularSlide-', pb_id, ' ', value * dt, 'ms linear forwards;" />
                          </svg>
                          '
                          ))
                        )
                      )
                    )
                  )  
                )
              }   
            )
          )
        )
      })
    }
  )
}