#' RStudio Addin: Containerize and Render R Markdown Document with Docker
#'
#' RStudio addin for containerizing and rendering the current document.
#'
#' @importFrom rstudioapi getActiveDocumentContext
#'
#' @keywords internal
docker_run = function() {
  docker_run()
}


#' RStudio Addin: Stop container
#'
#' RStudio addin for stopping containers
#'
#' @importFrom rstudioapi getActiveDocumentContext
#'
#' @keywords internal
docker_stop = function() {
  docker_stop()
}
