# Convert hex color with no alpha to hex with alpha
col2rgba <- function(x, alpha=255) {
  y <- col2rgb(x)
  rgb(y[1], y[2], y[3], alpha = alpha, maxColorValue = 255)
}

# This is script to wrap up the waiter screen for data curator app
dcWaiter <- function(stage = c("show", "update", "hide"),
                     id = NULL, landing = FALSE, userName = NULL,
                     isLogin = TRUE, isCertified = TRUE, isPermission = TRUE,
                     sleep = 0, msg = NULL, style = NULL,
                     spin = NULL, custom_spinner = FALSE, url = "",
                     color = col2rgba("#424874", 255*0.9)) {
  # validate arguments
  if (!is.logical(landing)) stop("landing must be a boolean")
  if (!is.logical(isLogin)) stop("isLogin must be a boolean")
  if (!is.logical(isCertified)) stop("isCertified must be a boolean")
  if (!is.logical(isPermission)) stop("isPermission must be a boolean")
  if (!is.numeric(sleep)) stop("sleep must be a numeric")
  if (is.null(msg)) msg <- "Loading ..."
  if (is.null(spin)) spin <- spin_plus()
  match.arg(stage, c("show", "update", "hide"))

  # if "hide", proceed hiding process immediately and exit function
  if (stage == "hide") {
    Sys.sleep(sleep)
    return(waiter_hide(id = id))
  }

  # first loading screen of app
  if (landing) {
    if (stage == "show") {
      waiter_show_on_load(
        html = tagList(
          img(src = "img/sage_logo_mark_only.png"),
          h4("Logging into Data Curator App")
        ),
        color = col2rgba("#2a668d", 255*0.9)
      )
    } else if (!isCertified) {
      # when user is not certified synapse user
      waiter_update(html = tagList(
        img(src = "img/sage_logo_mark_only.png", height = "120px"),
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
        img(src = "img/sage_logo_mark_only.png", height = "120px"),
        h3("Fileview/Project Access Denied!"),
        span("You may not have sufficient permissions for curation.
         Please contact your team and project administrators.")
      ))
    } else {
      # success loading page; userName needed to provide
      waiter_update(html = tagList(
        img(src = "img/sage_logo_mark_only.png", height = "120px"),
        h3(sprintf("Welcome, %s!", userName))
      ))
      Sys.sleep(sleep)
      waiter_hide()
    }
  } else {

    # loading tags
    if (custom_spinner) {
      # encode the custom image
      img_type <- tools::file_ext(basename(url))
      if (img_type == "svg") img_type <- "svg+xml"
      b64 <- base64enc::dataURI(file = url, mime = paste0("image/", img_type))
      spinner <-
        tagList(
          img(src = b64, class = "image-spin"),
          h4(msg, style = style)
        )
    } else {
      spinner <- tagList(spin, br(), h4(msg, style = style))
    }

    # other loading screens
    if (stage == "show") {
      waiter_show(
        id = id,
        html = spinner,
        color = col2rgba(color, 255*0.9)
      )
    } else {
      Sys.sleep(2) # wait at least 2s to update
      waiter_update(id = id, html = spinner)
      Sys.sleep(sleep)
      waiter_hide(id = id)
    }
  }
}
