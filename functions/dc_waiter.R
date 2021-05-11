# This is script to wrap up the waiter screen for data curator app
# TODO: maybe we could split into UI and server if we need

dc_waiter <- function(stage = c("show", "update", "hide"),
                      isLogin = FALSE, isPass = TRUE, usrName = NULL,
                      sleep = 2, msg = NULL) {
  # validate arguments
  if (!is.logical(isLogin)) stop("isLogin must be a boolean")
  if (!is.logical(isPass)) stop("isPass must be a boolean")
  if (!is.numeric(sleep)) stop("sleep must be a numeric")
  if (!stage %in% c("show", "update", "hide")) {
    stop("Please provide a value for stage: 'show', 'update' or 'hide'.")
  }

  # if "hide", proceed hiding process immediately and exit function
  if (stage == "hide") {
    Sys.sleep(sleep)
    return(waiter_hide())
  }

  # log in screen
  if (isLogin) {
    # The message on initial loading page are not customizable
    if (!is.null(msg)) message("message for log in screen can be changed in dc_waiter.R")

    if (stage == "show") {
      waiter_show_on_load(
        html = tagList(
          img(src = "loading.gif"),
          h4("Retrieving Synapse information...")
        ),
        color = "#424874"
      )
    } else if (isPass) {
      waiter_update(html = tagList(
        img(src = "synapse_logo.png", height = "120px"),
        h3(sprintf("Welcome, %s!", usrName))
      ))
      Sys.sleep(sleep)
      waiter_hide()
    } else {
      # ensure the synapse logo image is stored in www/
      waiter_update(html = tagList(
        img(src = "synapse_logo.png", height = "120px"),
        h3("Looks like you're not logged in!"), span(
          "Please ", a("login",
            href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"
          ),
          " to Synapse, then refresh this page."
        )
      ))
    }
  } else {
    # other loading screens
    if (is.null(msg)) msg <- "Loading ..."

    if (stage == "show") {
      waiter_show(
        html = tagList(spin_plus(), br(), h3(msg)),
        color = "rgba(66, 72, 116, .9)"
      )
    } else {
      Sys.sleep(2) # has to put at least 2s before to make update work
      waiter_update(html = tagList(spin_loaders(32), br(), h3(msg)))
      Sys.sleep(sleep)
      waiter_hide()
    }
  }
}
