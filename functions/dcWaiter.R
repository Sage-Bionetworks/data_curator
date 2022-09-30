# This is script to wrap up the waiter screen for data curator app
# TODO: maybe we could split into UI and server if we need

dcWaiter <- function(stage = c("show", "update", "hide"), landing = FALSE, userName = NULL,
                     isLogin = TRUE, isCertified = TRUE, isPermission = TRUE,
                     sleep = 2, msg = NULL, spin = NULL) {
  # validate arguments
  if (!is.logical(landing)) stop("landing must be a boolean")
  if (!is.logical(isLogin)) stop("isLogin must be a boolean")
  if (!is.logical(isCertified)) stop("isCertified must be a boolean")
  if (!is.logical(isPermission)) stop("isPermission must be a boolean")
  if (!is.numeric(sleep)) stop("sleep must be a numeric")
  if (!stage %in% c("show", "update", "hide")) {
    stop("Please provide a value for stage: 'show', 'update' or 'hide'.")
  }
  if (is.null(msg)) msg <- "Loading ..."
  if (is.null(spin)) spin <- spin_plus()

  # if "hide", proceed hiding process immediately and exit function
  if (stage == "hide") {
    Sys.sleep(sleep)
    return(waiter_hide())
  }

  # first loading screen of app
  if (landing) {
    if (stage == "show") {
      waiter_show_on_load(
        html = tagList(
          img(src = "img/loading.gif"),
          h4("Retrieving Synapse information...")
        ),
        color = "#424874"
      )
    } else if (!isCertified) {
      # when user is not certified synapse user
      waiter_update(html = tagList(
        img(src = "img/synapse_logo.png", height = "120px"),
        h3("Looks like you're not a synapse certified user!"),
        span(
          "Please follow the ",
          a("instruction",
            href = "https://help.synapse.org/docs/User-Account-Tiers.2007072795.html#UserAccountTiers-CertifiedUsers",
            target = "_blank"
          ),
          " to become a certified user, then refresh this page."
        )
      ))
    } else if (!isPermission) {
      # when user is not certified synapse user
      waiter_update(html = tagList(
        img(src = "img/synapse_logo.png", height = "120px"),
        h3("Fileview/Project Access Denied!"),
        span("You may not have sufficient permissions for curation.
         Please contact your team and project administrators.")
      ))
    } else {
      # success loading page; userName needed to provide
      waiter_update(html = tagList(
        img(src = "img/synapse_logo.png", height = "120px"),
        h3(sprintf("Welcome, %s!", userName))
      ))
      Sys.sleep(sleep)
      waiter_hide()
    }
  } else {

    # other loading screens
    if (stage == "show") {
      waiter_show(
        html = tagList(spin, br(), h3(msg)),
        color = "rgba(66, 72, 116, .9)"
      )
    } else {
      Sys.sleep(2) # has to put at least 2s before to make update work
      waiter_update(html = tagList(spin, br(), h3(msg)))
      Sys.sleep(sleep)
      waiter_hide()
    }
  }
}