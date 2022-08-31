#' Check Docker Services
#'
#' Performs a quick system check for installation
#' of Docker and if not found, gives URL to page for install instructions.
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
#'
#' @export
rocker_stop <- function(){

  project_name <- basename(rstudioapi::getActiveProject())

  cli::cli_h1("Stopping {.path {project_name}} container")

  cmd_execute <- paste0("docker stop ", project_name)

  system(cmd_execute)

  cli::cli_alert_success("Done")
}

#' Docker build
#'
#' Builds a docker container from a `Dockerfile`. The function will look in the `inst/dockerfile/` directory
#' for an existing Dockerfile.
#'
#' @param dockerfile Character path to supplied Dockerfile. Save to `inst/dockerfiles` or
#' if one doesnt exists then a Dockerfile can be generated from the `docker_file()` function.
#'
#' @param name Name and or name and tag of container name such as your Dockerhub username: `username/image_name`. If no name
#' is supplied then it will use the active Rstudio project name.
#'
#' @export
docker_build <- function(dockerfile = "inst/dockerfiles/Dockerfile", name = NULL){

  docker_check()

  if(isFALSE(file.exists(dockerfile = "inst/dockerfiles/Dockerfile"))){
    cli::cli_abort("No Dockerfile could be found. Please check your path or run:
                  {.fun ContainR::docker_file}")
  }

  if(is.null(name)) {
    name <- tolower(basename(rstudioapi::getActiveProject()))
  } else {
    name <- tolower(name)
  }

  build_command <- paste0("docker build --no-cache=true -t ", name ," -f ", dockerfile, " .")
  cli::cli_alert_info("Building {.path {dockerfile}} as {.emph {name}}...")
  system(build_command)

  # executed_cmd <- processx::run(command = "docker",
  #                               args = c("build",
  #                                        "--no-cache=true",
  #                                        "-t",
  #                                        name,
  #                                        "-f",
  #                                        dockerfile,
  #                                        "."),
  #                               echo_cmd = TRUE,
  #                               echo = TRUE,
  #                               spinner = TRUE)

  docker_images()

  cli::cli_alert_success("Success!")


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


#' List Docker Images
#'
#' A basic system wrapper for equiv terminal command
#'
#' @export
docker_images <- function(){

  cli::cli_h1("Container Images")
  sys_cmd <- sys::exec_internal("docker", c("image",  "ls", "-a"))
  sys_to_console(sys_cmd)
}

#' List Docker Images
#'
#' A basic system wrapper for equiv terminal command
#'
#' @export
docker_list <- function(){

  cli::cli_h1("Container Images")
  sys_cmd <- sys::exec_internal("docker", c("container",  "ls", "-a"))
  sys_to_console(sys_cmd)
}


#' Rocker run
#'
#' Launch the active R project into a Rocker Rstudio container
#' and opens a browser to the session. Mounts local config and .Renviron from local
#' session.
#'
#' @param image Repository Name of rocker image from [data_rocker_table]. Defaults to
#' the `rstudio` stack `rocker/rstudio:latest`. For more information about each stack
#' visit \url{https://rocker-project.org/images/}. If you ran `docker_build()` then supply
#' this argument with the string of the *name* of the build. Built container images can be
#' view by running the `docker_list()` function.
#'
#' @param tag Version number of stack. Defaults to the `:latest` tag if none is supplied.
#'
#' @export
rocker_run <- function(image = "rstudio", tag = NULL){

  cli::cli_alert_info("Checking Docker Install...")
  docker_check()

  # Checker for image name
  data_rocker_table <- ContainR::data_rocker_table

  # Set tag
  if(is.null(tag)){
    tag <- "latest"
  } else {
    tag <- tolower(tag)
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
  cli::cli_h1("Launching Container {.path {docker_image}}")

  # Setup names and projec paths
  project_name <- basename(rstudioapi::getActiveProject())
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

  cli::cli_alert_info("Launching {.path {docker_image}} in you browser...")
  utils::browseURL("http://localhost:8787")

}

#' Create a Dockerfile
#'
#' This function creates a dockerfile based off the rocker version image stacks and reads
#' from session information to install `loaded `, `installed` \(or `none`\) R libraries. The `install2.r` handles
#' additional package installs and will skip already installed packages on the rocker stack image. The final `Dockerfile`
#' can be used to build a container image with your development environment.
#'
#' @param dockerfile Default location is in the `inst/dockerfiles/` folder. Build will save the final `Dockerfile`
#' to this location. You can also add you're own Dockerfile, however, this package was primarily designed to launch
#' an active project into a Rstudio container from one of the Rocker images: \url{https://rocker-project.org/images/}.
#'
#' @param which_pkgs Provide a string of either `loaded`, `installed` or `none`. `Loaded` will included packages that are currently
#' loaded in your active project session. The `installed` string will included everything inside you local default R library. Already
#' installed packages are skipped when the [docker_build] command is run. If you have a large package library it is
#' recommended to only install `loaded` as you develop your workflow.
#'
#' @param name The Rocker name of the stack that you want to build. To see the current stack options
#' load the [data_rocker_table]. For more detailed information visit \url{https://github.com/rocker-org/rocker-versioned2}.
#'
#' @param tag A character string of the require version. If no tag is supplied then the function will default to `latest`.
#'
#' @param include_python Flag to install python using the rocker scripts \url{https://github.com/rocker-org/rocker-versioned2} which have had minor modifications. Future updates
#' will see this streamlined. `Pandas` and `numpy` modules are also installed if this flag is set to `TRUE`.
#'
#'
#' @export
docker_file <- function(dockerfile = "inst/dockerfiles/Dockerfile",
                          which_pkgs = c("loaded", "installed", "none"),
                          name = c("rstudio", "tidyerse", "verse", "geospatial", "binder"),
                          tag = NULL,
                          include_python = FALSE){

  cli::cli_h1("Creating Dockerfile")

  # Check for image name
  data_rocker_table <- ContainR::data_rocker_table

  # File overwrite warning
  if(file.exists(dockerfile)) {
    cli::cli_alert_warning("{.emph dockerfile} will be overwritten.",
                           class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))

  } else {
    cli::cli_alert_info("No Dockerfile found. Creating new {.path inst/dockerfiles/Dockerfile}")
  }

  # which_pckgs argument check
  if(!(which_pkgs %in% c("loaded", "installed", "none"))) {

    cli::cli_alert_warning("{.emph which_pkgs} must be of type {.emph loaded} or {.emph installed} or {.emph none}",
                           class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))

    }

  # rocker name match check
  if(!(name %in% data_rocker_table$name)){
    cli::cli_warn("{.arg name} must be one of: {.emph {data_rocker_table$name}}")
    print(data_rocker_table)
  }

  # Set tag
  if(is.null(tag)){
    tag <- "latest"
  } else {
    tag <- tolower(tag)
  }

  # Flag for python inclusion
  if(isTRUE(include_python)) {
    cli::cli_alert_info("Install Python: {.val {include_python}}")
  } else if(isFALSE(include_python)) {
    cli::cli_alert_info("Install Python: {.val {include_python}}")
  }

  rocker_image <- switch(name,
                         rstudio = paste0("rocker/rstudio:", tag),
                         tidyerse = paste0("rocker/tidyverse:", tag),
                         verse = paste0("rocker/verse:", tag),
                         geospatial = paste0("rocker/geospatial:", tag),
                         binder = paste0("rocker/binder:", tag))

  cli::cli_alert_info("Using Rocker Stack: {.path {rocker_image}}")

  packages <- sessioninfo::package_info(pkgs = which_pkgs)

  if(which_pkgs != "none") {
    write_install_bash_file(x = packages)
  }

  # command layout for dockerfile
  docker_cmds <- paste(c(
    paste("FROM", rocker_image),
    # TODO paste("LABEL", "label_name"),
    paste(""),
    if(isTRUE(include_python)) {
      c(
        paste("COPY", "/inst/dockerfiles/rocker_scripts/install_python.sh /tmp/"),
        paste(""),
        paste("COPY", "/inst/dockerfiles/rocker_scripts/install_pyenv.sh /tmp/"),
        paste(""),
        paste("RUN", "chmod +x /tmp/install_python.sh"),
        paste(""),
        paste("RUN", "/tmp/install_python.sh"),
        paste(""),
        paste("RUN", "R -e \"reticulate::py_config()\""),
        paste("")
      )
    },
    paste("COPY /inst/dockerfiles/rocker_scripts/install_libs_local.sh /tmp/"),
    paste(""),
    paste("COPY /inst/dockerfiles/rocker_scripts/install_additional.sh /tmp/"),
    paste(""),
    paste("RUN chmod +x /tmp/install_libs_local.sh"),
    paste(""),
    paste("RUN chmod +x /tmp/install_additional.sh"),
    paste(""),
    paste("RUN", "/tmp/install_additional.sh")
  ),
  collapse = "\n")

  writeLines(docker_cmds, dockerfile, sep = "")

  if(isTRUE(file.exists(dockerfile))){
    cli::cli_alert_success("Dockerfile saved to: {.path {dockerfile}}")
  }

  # Tip to run function next
  #cli::cli_alert_info("Run: {.emph docker_build_dockerfile(dockerfile = \"{dockerfile}\", name = \"name\")} ")

}

#' Docker Push Image
#'
#' Push container image to \url{https://hub.docker.com/}.
#'
#' @param name The user/name of the docker image
#' @param tag Version or tag of image. Defaults to latest.
#'
#' @export
docker_push <- function(name, tag){

  cli::cli_h1("Docker Push Image")
  #docker_login()

  sys_cmd <- paste0("docker push ", name, ":", tag)

  cli::cli_alert_info("I know. You were expecting this function to work.
                      Im sorry to dissapoint {.emph buuuut} we are working on it!!",
                         class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))


  # cli::cli_alert_warning("{.emph {sys_cmd}}",
  #                        class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))
  #
  # print(sys_cmd)

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

# Write the install bash file
write_install_bash_file <- function(x){

  packages <- x
  Source <- NULL

  packs <- dplyr::tibble(Package = packages$package,
                         Version = packages$ondiskversion,
                         Source = packages$source)

  # hacky work around
  CRAN_packs <- packs |>
    dplyr::filter(Source == stringr::str_match_all(Source,
                                                   pattern = "CRAN \\(R \\d.\\d.\\d\\)"))

  cli::cli_alert_info("{.path {nrow(CRAN_packs)}} lib{?s} from {.var {which_pkgs}}")
  cli::cli_alert_info("Including: {.val {CRAN_packs$Package}}")

  cran_packs_first <- paste0("\t", CRAN_packs$Package[0 -length(CRAN_packs$Package)], " \\")
  cran_packs_last <- paste0("\t", CRAN_packs$Package[length(CRAN_packs$Package) - 0], "")

  GITHUB_packs <- packs |>
    dplyr::filter(Source != stringr::str_match_all(Source,
                                                   pattern = "CRAN \\(R \\d.\\d.\\d\\)")) |>
    dplyr::filter(Source != "local") |>
    dplyr::filter(Source == stringr::str_match_all(Source,
                                                   pattern = "Github \\(\\w+/\\w+@[0-9A-Za-z]+\\)"))

  github_installs <- GITHUB_packs |>
    dplyr::mutate(Source = stringr::str_extract_all(Source,
                                                    pattern = "\\w+/\\w+",
                                                    simplify = TRUE)) |>
    dplyr::select(Source) |>
    purrr::pluck("Source")

  cli::cli_alert_info("Including {.path {nrow(GITHUB_packs)}} GitHub Package{?s}")
  cli::cli_alert_info("Including: {.val {GITHUB_packs$Package}}")

  bash_file = "inst/dockerfiles/rocker_scripts/install_libs_local.sh"

  # Command layout for bash script
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
    if(length(github_installs) > 0) {
      c(paste("Rscript -e", paste("\"remotes::install_github(\'", github_installs , "\')\" ", sep = "") ," "))
    },
    paste(""),
    paste("install2.r --error --skipinstalled -n \"$NCPUS\" \\"),
    paste(cran_packs_first),
    paste(cran_packs_last)
  ),
  collapse = "\n")

  writeLines(installR_script, con = bash_file, sep = "")

  if(isTRUE(file.exists(bash_file))){
    cli::cli_alert_success("Bash file updated: {.path {bash_file}}")
  }


}

# Config file
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
