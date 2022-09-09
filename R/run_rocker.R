#...............................................................................
#                                                                              .
#  rocker image specific functions and wrappers                                .
#                                                                              .
#...............................................................................

#' Pull Rocker Images
#'
#' @param name Rocker image name from the [data_rocker_table]
#' @param tag Version tag of rocker image. Will default to `latest`
#'
#' @export
rocker_pull <- function(name, tag = NULL){

  # Check for image name
  data_rocker_table <- ContainR::data_rocker_table
  if(!(name %in% data_rocker_table$name)){
    cli::cli_warn("{.arg name} must be one of: {.emph {data_rocker_table$name}}")
    print(data_rocker_table)
  }

  # Set tag
  if(is.null(tag)){
    tag <- "latest"
  } else {
    tag <- tag
  }

  exec_sys_cmd <- switch(name,
                         rstudio = paste0("docker pull rocker/rstudio:", tag),
                         tidyerse = paste0("docker pull rocker/tidyverse:", tag),
                         verse = paste0("docker pull rocker/verse:", tag),
                         geospatial = paste0("docker pull rocker/geospatial:", tag),
                         binder = paste0("docker pull rocker/binder:", tag))

  system(exec_sys_cmd)
}

#' Rocker run
#'
#' Launch the active R project into a Rocker Rstudio container
#' and opens a browser to the session.
#'
#' @param image Repository Name of rocker image from [data_rocker_table]. Defaults to
#' the `rstudio` stack `rocker/rstudio:latest`. For more information about each stack
#' visit \url{https://rocker-project.org/images/}. If you ran `docker_build()` then supply
#' this argument with the string of the *name* of the build. Built container images can be
#' view by running the `docker_images()` function.
#'
#' @param tag Version number of stack. Defaults to the `:latest` tag if none is supplied.
#'
#' @param DISABLE_AUTH Bypass authentication and show the R session. Defaults to TRUE and will
#' login when the session is launced in a browser.
#'
#' @param use_local Whether to clone the local `.config`, `.Renviron` and `.Rprofile` to container session or
#' use the default settings. Defaults to `TRUE` and makes these config files available. Handy if the user has a
#' particular panel and theme layout and want to access private repos for install and testing.
#'
#' @export
rocker_run <- function(image = "testing",
                       tag = NULL,
                       DISABLE_AUTH = TRUE,
                       use_local = TRUE){


  project_name = tolower(basename(rstudioapi::getActiveProject()))

  data_rocker_table <- ContainR::data_rocker_table

  # Set tag
  if(is.null(tag)){
    tag <- "latest"
  } else {
    tag <- tolower(tag)
  }

  image <- check_images(image = image, tag = tag)

  processed_args <- rocker_args(DISABLE_AUTH, use_local, image)

  docker_run(cmd = "docker", proc_args = processed_args, name = project_name)

  # dockered <- processx::process$new(command = "docker",
  #                                   args = proc_args,
  #                                   pty = TRUE) # currently only supported on Unix systems
  if(isTRUE(dockered$is_alive())){
    cli::cli_h1("Running: {.val {image}} as container: {.val {project_name}}")
    cli::cli_alert_info("Launching {.path {image}} in you browser...")
    utils::browseURL("http://localhost:8787")
  }

}

# TODO set a healthcheck on the container to check its running













