boxEffect <- function(zoom = FALSE, float = FALSE) {
  # this is cleaner way, one caveat: need to change zoom scale in scss file
  toggleClass(selector = ".box", class = "box-zoom", condition = zoom)
  toggleClass(selector = ".box", class = "box-float", condition = float)
}
