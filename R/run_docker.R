#' Check Docker Services
#'
#' Performs a quick system check for installation
#' of docker and if not found gives URL to page for install instructions.
#'
#'
#' @export
docker_check <- function(){

  if(Sys.which("docker") == "") {

    cli::cli_alert_warning("Docker was not found on your system")

    if(Sys.info()[["sysname"]] == "Darwin"){
      cli::cli_alert_info("Visit: {.url https://docs.docker.com/desktop/install/mac-install/}")
    }

    if(Sys.info()[["sysname"]] == "Windows"){
      cli::cli_alert_info("Visit: {.url https://docs.docker.com/desktop/install/windows-install/}")
    }

    if(Sys.info()[["sysname"]] == "Linux"){
      cli::cli_alert_info("Visit: {.url https://docs.docker.com/engine/install/}")
    }

  } else {
    dock_vers <- sys::exec_internal('docker', '-v')
    dock_vers_char <- rawToChar(dock_vers$stdout)
    if (grepl('Docker version', dock_vers_char)) cli::cli_alert_success(dock_vers_char)

  }

}

#' Stop docker session
#' @export
docker_stop <- function(){

  project_name <- basename(rstudioapi::getActiveProject())

  cli::cli_h1("Stopping {.path {project_name}} container")

  cmd_execute <- paste0("docker stop ", project_name)

  system(cmd_execute)

  cli::cli_alert_success("Done")
}

#' Create Config File
#'
#' Creates a temp .Rprofile and writes a setHook to launch
#' activate current project in the Rstudio docker container.
#'
#'
#' @importFrom yaml as.yaml
#'
#' @param project_name Given name of project. Defaults to repo directory.
#'
#' @export
create_config_file <- function(project_name){

  rprofile <-  paste0(tempdir(), "/.Rprofile")

  project_name <- basename(rstudioapi::getActiveProject())

  work_directory <- paste0("setwd(\"/home/rstudio/", project_name, "\")")

  writeLines(
    c(".First <- function() {",
      work_directory,
      "setHook(\"rstudio.sessionInit\", function(newSession) {
    if (newSession && is.null(rstudioapi::getActiveProject()))
      rstudioapi::openProject(rstudioapi::initializeProject())
  }, action = \"append\")

  invisible(TRUE)

}"),
con = rprofile,
sep = "\n")

  # check to ensure the temp profile was created
  if(isTRUE(file.exists(rprofile))) cli::cli_alert_success(".Rprofile Created")

  return(rprofile)

}

#' Docker build
#'
#' Builds a docker image from a file location
#'
#' @param dockerfile Character path to supplied Dockerfile. Save to `inst/dockerfiles`
#' if one doesnt exists then a Dockerfile can be generated from the `docker_file_create()` function.
#'
#' @param name Name and or name and tag of container name such as username/image_name
#'
#' @export
docker_build_dockerfile <- function(dockerfile = NULL, name = NULL){

  if(isFALSE(file.exists(dockerfile = "inst/dockerfiles/Dockerfile"))){

    cli::cli_abort("{.path No Dockerfile could be found. Please check your path}")

  }

  if(is.null(name)) {
    cli::cli_abort("Please supply a name for container")
  } else {
    name <- name
  }

  if(is.null(dockerfile)) {
    cli::cli_abort("Error: No dockerfile supplied")
  } else {

    docker_check()

    build_command <- paste0("docker build -t ", name ," -f ", dockerfile, " .")
    cli::cli_alert_info("Building  {.path {dockerfile}} as {.emph {name}}")
    system(build_command)

    cli::cli_alert_success("Success!")

    system("docker image list")

  }

}

#' Docker Search
#'
#' Perform a search for docker images and returns a table of
#' results.
#'
#' @importFrom tidyr separate
#'
#' @param search_string Character string for search term
#'
#' @export
docker_search <- function(search_string = "rstudio"){

  search_command <- paste0('docker search --format "table {{.Name}}\t{{.Description}}\t{{.StarCount}}" ', search_string)
  returned_results <- system(search_command, intern = TRUE)

  search_results <- dplyr::tibble(returned_results)
  search_results <- search_results[2:nrow(search_results),]

  cleaned_results <- tidyr::separate(
    data = search_results,
    col = returned_results,
    into = c("Name", "Description", "Stars"),
    sep = "(\\s+)(\\s+)",
    remove = TRUE,
    convert = TRUE,
    fill = "right") |>
    dplyr::mutate(Stars = dplyr::case_when(is.na(Stars) ~ Description,
                                           TRUE ~ paste0(Stars))) |>
    dplyr::mutate(Description = dplyr::case_when(Description != Stars ~ Description,
                                                 Description == Stars ~ as.character(NA)))

  return(cleaned_results)


}

#' Docker Login
#'
#' Log into docker hub using a `DOCKER_PAT` that should be setup
#' through \url{https://hub.docker.com} and assigned to `.Renviron`.
#'
#' @param username Character string of dockerhub user login
#'
#' @export
docker_login <- function(username){

  cli::cli_h1("Docker Login")

  url_helper = "https://hub.docker.com/settings/general"

  # Set username
  if(is.null(username)){
    cli::cli_abort("No {.emph username} provided")
  } else {
    USER = username
  }

  # set docker hub token
  if(Sys.getenv("DOCKER_PAT") == "") {
    cli::cli_abort("No DOCKER_PAT token can be found in {.emph .Renviron}
                         Visit: {.url {url_helper}}")
  } else {
    PWD <- Sys.getenv("DOCKER_PAT")
  }

  exec_cmd <- paste0("echo ",
                     PWD,
                     " | docker login -u ",
                     USER,
                     " --password-stdin")

  system(exec_cmd, intern = TRUE)

}

#' Docker Logout
#'
#' Log out of docker
#'
#' @export
docker_logout <- function(){

  cli::cli_h1("Docker Logout")

  sys_cmd <- sys::exec_internal("docker", "logout")
  sys_to_console(sys_cmd)

}

#' Pull Rocker Image
#'
#' @param rocker_verse Rocker image name from the [data_rocker_table]
#' @param tag Version tag of rocker image. Will default to local
#' r version using getRversion.
#'
#' @export
docker_pull <- function(rocker_verse, tag = NULL){

  # Check for image name
  data_rocker_table <- data_rocker_table
  if(!(rocker_verse %in% data_rocker_table$name)){
    cli::cli_warn("{.arg rocker_verse} must be one of: {.emph {data_rocker_table$name}}")
    print(data_rocker_table)
  }

  # Set tag
  if(is.null(tag)){
    tag <- getRversion()
  } else {
    tag <- tag
  }

  exec_sys_cmd <- switch(rocker_verse,
                         rstudio = paste0("docker pull rocker/rstudio:", tag),
                         tidyerse = paste0("docker pull rocker/tidyverse:", tag),
                         verse = paste0("docker pull rocker/verse:", tag),
                         geospatial = paste0("docker pull rocker/geospatial:", tag),
                         binder = paste0("docker pull rocker/binder:", tag))

  system(exec_sys_cmd)
}


#' List Docker Images
#'
#'
#' @export
docker_list_images <- function(){

  cli::cli_h1("Container Images")
  sys_cmd <- sys::exec_internal("docker", c("image",  "ls", "-a"))
  sys_to_console(sys_cmd)
}

docker_list_containers <- function(){

  cli::cli_h1("Container Images")
  sys_cmd <- sys::exec_internal("docker", c("container",  "ls", "-a"))
  sys_to_console(sys_cmd)
}


#' Docker run
#'
#' Launches current active R project into a rocker container
#' and opens a browser.
#'
#' @param image Name of rocker image from [data_rocker_table]. Defaults to
#' the `rstudio` stack `rocker/rstudio:latest`. For more information about each stack
#' visit \url{https://rocker-project.org/images/}. If you ran `docker_build()` then supply
#' this argument with the string of the name of the build.
#'
#' @param tag Version number of stack. Defaults to the `:latest` tag if none is supplied.
#'
#' @export
docker_run <- function(image = "rstudio", tag = NULL){

  cli::cli_alert_info("Checking Docker Install...")
  docker_check()

  # Checker for image name
  data_rocker_table <- ContainR::data_rocker_table

  # Set tag
  if(is.null(tag)){
    tag <- "latest"
  } else {
    tag <- tag
  }


  if((image %in% data_rocker_table$name)){
    # Switch for changing rocker image based on image name
    docker_image <- switch(image,
                           rstudio = paste0("rocker/rstudio:", tag),
                           tidyerse = paste0("rocker/tidyverse:", tag),
                           verse = paste0("rocker/verse:", tag),
                           geospatial = paste0("rocker/geospatial:", tag),
                           binder = paste0("rocker/binder:", tag))

  } else {
    # If the name isnt one of rocker name then assume custom image
    docker_image <- paste0(image, ":", tag)

  }
  # TODO check if image is in docker repo
  cli::cli_h1("Launching Docker Project {.path {docker_image}}")

  # Setup names and projec paths
  project_name <-  basename(rstudioapi::getActiveProject())
  cli::cli_alert_info("Setting Project to: {.path {project_name}}")

  # get working directory from local and set to repo directory
  project_dir <- getwd()
  cli::cli_alert_info("Working Directory Set to: {.path {project_dir}}")

  #Copying .Renviron
  local_r_env <- paste0(Sys.getenv("HOME"), "/.Renviron")
  cli::cli_alert_info("Cloning .Renviron : {.path {local_r_env}}")

  #Copy preferences stored in /inst/prefs
  rstudio_prefs <- paste0(Sys.getenv("HOME"), "/.config/rstudio")
  cli::cli_alert_info("R Settings Cloned from: {.path {rstudio_prefs}}")

  # set rprofile inside container to active project
  rprof_file <- create_config_file(project_name = project_name)
  #rprof_file <- "/Users/awwillc/Repos/replicats/inst/docker/.Rprofile"
  cli::cli_alert_info("Cloning temp: {.path {rprof_file}}")

  # # Docker launch command
  # --rm remove the container automatically after it exits
  # TODO allow for different run commands
  AUTH = TRUE
  exec_sys_cmd <- paste0(c(
    paste0("docker run -d --rm -ti"),
    paste0(" -e DISABLE_AUTH=", ifelse(isTRUE(AUTH), AUTH, "FALSE"), sep = ""),
    paste0(" -p 127.0.0.1:8787:8787"),
    paste0(" -v ", project_dir, ":/home/rstudio/", project_name, sep = ""),
    paste0(" -v ", rstudio_prefs, ":/home/rstudio/.config/rstudio", sep = ""),
    paste0(" -v ", local_r_env, ":/home/rstudio/.Renviron", sep = ""),
    paste0(" -v ", rprof_file, ":/home/rstudio/.Rprofile", sep = ""),
    paste0(" --name ", project_name),
    paste0(" ", docker_image)
  ), collapse = "")

  cli::cli_alert_info("{.emph Running Command: } {exec_sys_cmd}",
                      class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))

  system(exec_sys_cmd)

  cli::cli_alert_info("Launching {.path {project_name}} in you browser...")
  utils::browseURL("http://localhost:8787")

}

#' Create a Dockerfile
#'
#' This function creates a dockerfile based off the rocker version image stacks and reads
#' from session information to included `loaded `or `installed` R libraries.
#'
#' @param dockerfile Default location is in the `inst/dockerfiles/` folder. Build will save the final `Dockerfile`
#' to this location. You can also add youre own Dockerfile, however, this package was primarily designed to launch
#' an active project into a Rstudio container from rocker version.
#'
#' @param which_pkgs Provide a string of either `loaded` or `installed`. `Loaded` will included packages that are currently
#' loaded in your active project session. The `installed` string will included everything inside you local default R library.
#'
#' @param rocker_name The Rocker name of the stack that you want to build off. To see the current stack options
#' load the [data_rocker_table]. A character string from the `name` will load the `FROM` command with the associated `image`.
#'
#' @param tag A character string of the require version. If no tag is supplied then the function will get the local
#' version of R from `getRversion()`,
#'
#' @param include_python Flag to install python using the rocker scripts \url{https://github.com/rocker-org/rocker-versioned2} which have had minor modifications. Future updates
#' will see this streamlined.
#'
#'
#' @export
docker_file_create <- function(dockerfile = "inst/dockerfiles/Dockerfile",
                          which_pkgs = c("loaded", "installed"),
                          rocker_name = "rstudio",
                          tag = NULL,
                          include_python = FALSE){

  cli::cli_h1("Creating Dockerfile")

  # Check for image name
  data_rocker_table <- data_rocker_table

  if(!(rocker_name %in% data_rocker_table$name)){
    cli::cli_warn("{.arg rocker_name} must be one of: {.emph {data_rocker_table$name}}")
    print(data_rocker_table)
  }

  # Set tag
  if(is.null(tag)){
    tag <- getRversion()
  } else {
    tag <- tag
  }

  rocker_image <- switch(rocker_name,
                         rstudio = paste0("rocker/rstudio:", tag),
                         tidyerse = paste0("rocker/tidyverse:", tag),
                         verse = paste0("rocker/verse:", tag),
                         geospatial = paste0("rocker/geospatial:", tag),
                         binder = paste0("rocker/binder:", tag))

  cli::cli_alert_info("Using Rocker Image: {.path {rocker_image}}")

  cli::cli_alert_info("Creating library list for image...")
  packages <- sessioninfo::package_info(pkgs = which_pkgs)

  packs <- dplyr::tibble(Package = packages$package,
                         Version = packages$ondiskversion,
                         Source = packages$source)

  cli::cli_alert_info("{.path {nrow(packs)}} lib{?s} from {.var {which_pkgs}}")
  cli::cli_alert_info("Including: {.val {packs$Package}}")


  # TODO check for local and CRAN
  # TODO get version

  # get first lot of packages minus last
  packs$Package[0 -length(packs$Package)]
  # get last package name
  packs$Package[length(packs$Package) - 0]

  packs_list <- paste0("\t", packs$Package[0 -length(packs$Package)], " \\")

  last_list <- paste0("\t", packs$Package[length(packs$Package) - 0], "")

  bash_file = "inst/rocker_scripts/install_libs_local.sh"

  installR_script <- paste(c(
    paste("#!/bin/bash"),
    paste(""),
    paste("set -e"),
    paste(""),
    paste("NCPUS=${NCPUS:--1}"),
    paste(""),
    paste("CRAN=${1:-${CRAN:-\"https://cran.r-project.org\"}}"),
    paste(""),
    paste("ARCH=$(uname -m)"),
    paste(""),
    paste("export DEBIAN_FRONTEND=noninteractive"),
    paste(""),
    paste("UBUNTU_VERSION=$(lsb_release -sc)"),
    paste("CRAN_SOURCE=${CRAN/\"__linux__/$UBUNTU_VERSION/\"/\"\"}"),
    paste(""),
    paste("if [ \"$ARCH\" = \"aarch64\" ]; then"),
    paste("\t", "CRAN=$CRAN_SOURCE"),
    paste("fi"),
    paste(""),
    paste("install2.r --error --skipinstalled -n \"$NCPUS\" \\ "),
    paste(packs_list),
    paste(last_list)
  ),
  collapse = "\n")

  writeLines(installR_script, con = bash_file, sep = "")

  if(isTRUE(file.exists(bash_file))){
    cli::cli_alert_success("Additional libraries updated to file: {.path {bash_file}}")
  }

  body <- paste(c(
    paste("FROM", rocker_image),
    # TODO paste("LABEL", "label_name"),
    paste(""),
    if(isTRUE(include_python)) {
      c(
        paste("COPY", "./rocker_scripts/install_python.sh /tmp/"),
        paste("COPY", "./rocker_scripts/install_pyenv.sh /tmp/"),
        paste(""),
        paste("RUN", "chmod +x /tmp/install_python.sh"),
        paste(""),
        paste("RUN", "/tmp/install_python.sh"),
        paste("RUN", "R -e \"reticulate::py_config()\"")
      )
    },
    paste(""),
    paste("COPY", "./rocker_scripts/install_additional.sh /tmp/"),
    paste(""),
    paste("RUN", "/tmp/install_additional.sh"),
    paste(""),
    paste("RUN", "R -e \"sessioninfo::platform_info()\"")
  ),
  collapse = "\n")

  writeLines(body, dockerfile, sep = "")

  if(isTRUE(file.exists(dockerfile))){
    cli::cli_alert_success("Dockerfile saved to: {.path {docker_file}}")
  }
}

# Console alert helper
sys_to_console <- function(x){

  if(x$status == 1){
    cli::cli_alert_danger("Error")
  } else {
    cap_out <- rawToChar(x$stdout)
    cli::cli_alert_info(cap_out)

  }

}
