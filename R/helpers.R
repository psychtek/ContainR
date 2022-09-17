

check_images <- function(image, tag){

  repo_images <- docker_images() |> dplyr::filter(Repository %in% image)

  data_rocker_table <- ContainR::data_rocker_table

  if(image %in% repo_images$Repository) {

    image <- image

  } else if(image %in% data_rocker_table$name) {
    # Switch for changing rocker image based on image name
    image <- switch(image,
                    rstudio = paste0("rocker/rstudio:", tag),
                    tidyerse = paste0("rocker/tidyverse:", tag),
                    verse = paste0("rocker/verse:", tag),
                    geospatial = paste0("rocker/geospatial:", tag),
                    binder = paste0("rocker/binder:", tag))

    cli::cli_alert_info("Using: {.var {image}}")

  } else {
    cli::cli_abort("{.var {image}} not a Rocker image or in Docker Register: {.fun docker_images}")
  }
  return(image)
}

rocker_args <- function(DISABLE_AUTH, use_local, image){

  proj_name <- basename(rstudioapi::getActiveProject())
  # set volume mounts
  r_dir <- paste0(fs::path_wd(), ":/home/rstudio/", basename(fs::path_wd()))
  r_config <- paste0(fs::path_home_r(), "/.config/rstudio")
  r_env <- paste0(fs::path_home_r(), "/.Renviron")
  r_prof <- create_config_file(project_name = proj_name)

  r_config <- paste0(r_config, ":/home/rstudio/.config/rstudio", collapse = "")
  r_env <- paste0(r_env, ":/home/rstudio/.Renviron", collapse = "")
  r_prof <- paste0(r_prof, ":/home/rstudio/.Rprofile", collapse = "")

  if(isTRUE({{use_local}})){
    vols <- c(r_dir, r_config, r_env, r_prof)
    volumes <- unlist(purrr::map(vols, function(x) c("-v", x)))
  } else {
    volumes <- c("-v", paste0(r_dir))
  }

  # set ports
  ports <- c("-p", "127.0.0.1:8787:8787")

  # set options
  # removed -it dur to "the input device is not a TTY"
  # more here: https://stackoverflow.com/questions/43099116/error-the-input-device-is-not-a-tty
  docker_opts <- c("run", "--rm")
  set_env <- c("-e", paste0("DISABLE_AUTH=", ifelse(isTRUE({{DISABLE_AUTH}}), {{DISABLE_AUTH}}, "FALSE")))
  set_name <- c("--name", proj_name)
  image_name <- c(image)

  # Argument string for docker
  proc_args <- c(docker_opts, set_env, ports, volumes, set_name, image_name)

  return(proc_args)

}



# Write the install_libs_local.sh so this can be dynamically
# updated.
write_install_bash_file <- function(x){

  bash_file <- "docker/scripts/install_libs_local.sh"

  if(isTRUE(file.exists(bash_file))){
    cli::cli_alert_info("Updating: {.path {bash_file}}")
  }

  packages <- x
  Source <- NULL

  packs <- dplyr::tibble(Package = packages$package,
                         Version = packages$ondiskversion,
                         Source = packages$source)

  # hacky work around
  CRAN_packs <- packs |>
    dplyr::filter(Source == stringr::str_match_all(Source,
                                                   pattern = "CRAN \\(R \\d.\\d.\\d\\)"))

  cli::cli_alert_info("{.path {nrow(CRAN_packs)}} lib{?s}")
  cli::cli_alert_info("Including: {.val {CRAN_packs$Package}}")

  # trailing slash in bash file so hack fix
  cran_packs_first <- paste0("\t", CRAN_packs$Package[0 -length(CRAN_packs$Package)], " \\")
  cran_packs_last <- paste0("\t", CRAN_packs$Package[length(CRAN_packs$Package) - 0], "")

  # Filter to GIThub repos and remove local installs
  GITHUB_packs <- packs |>
    dplyr::filter(Source != stringr::str_match_all(Source,
                                                   pattern = "CRAN \\(R \\d.\\d.\\d\\)")) |>
    dplyr::filter(Source != "local")


  if(nrow(GITHUB_packs) > 0){
    github_installs <- GITHUB_packs |>
      dplyr::mutate(Source = stringr::str_extract_all(Source,
                                                      pattern = "(.*?)@",
                                                      simplify = TRUE)) |>
      dplyr::mutate(Source = stringr::str_extract_all(Source,
                                                      pattern = "\\(([\\s\\S]*)$",
                                                      simplify = TRUE)) |>
      dplyr::mutate(Source = stringr::str_remove_all(Source,
                                                     pattern = "\\((.*?)")) |>
      dplyr::mutate(Source = stringr::str_remove_all(Source,
                                                     pattern = "@")) |>
      dplyr::select(Source) |>
      purrr::pluck("Source")
  } else {
    github_installs <- 0
  }


  if(nrow(GITHUB_packs) > 0){
    cli::cli_alert_info("Including {.path {nrow(GITHUB_packs)}} GitHub Package{?s}")
    cli::cli_alert_info("{.val {GITHUB_packs$Package}}")
  }

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
    paste(cran_packs_last),
    paste("")
  ),
  collapse = "\n")

  writeLines(installR_script, con = bash_file, sep = "")

  if(isTRUE(file.exists(bash_file))){
    cli::cli_alert_success("Bash file updated: {.path {bash_file}}")
  }
  return(bash_file)
}

# Config file that will overwrite the Rstudio one
# and then when session is launch it will activate working dir
create_config_file <- function(project_name){

  rprofile <-  paste0(tempdir(), "/.Rprofile")

  if(is.null(project_name)){
    project_name <- basename(fs::path_wd())
  } else {
    project_name <- project_name
  }


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
  #if(isTRUE(file.exists(rprofile))) cli::cli_alert_success(".Rprofile Created")

  return(rprofile)

}

#' Setup Docker Files
#'
#' Setups up a dockerfile and scripts folders in root directory
#' of the active R project or directory.
#'
#' @param print_tree Flag to display the directory tree
#'
#' @export
docker_folder_setup <- function(print_tree = FALSE){

  if(!fs::dir_exists("docker")) {
    fs::dir_create("docker")
  }

  if(!fs::dir_exists("docker/scripts")) {
    fs::dir_create("docker/scripts")
  }

  if(!fs::file_exists("docker/scripts/install_libs_local.sh")) {
    fs::file_create("docker/scripts/install_libs_local.sh")
  }

  if(!fs::file_exists("docker/Dockerfile")) {
    fs::file_create("docker/Dockerfile")
  }

  if(!fs::file_exists("docker/scripts/install_python.sh")) {
    get_extdata("install_python.sh")
  }

  if(!fs::file_exists("docker/scripts/install_pyenv.sh")) {
    get_extdata("install_pyenv.sh")
  }

  if(!fs::file_exists("docker/scripts/install_additional.sh")) {
    get_extdata("install_additional.sh")
  }

  if(isTRUE(print_tree)){
    fs::dir_tree("docker")
  }

}

# Grabs the additional rocker scripts in extdata
get_extdata <- function(file){

  if(fs::dir_exists("docker/scripts")){
    x <- system.file("extdata",
                     file,
                     package="ContainR",
                     mustWork = TRUE)
    fs::file_copy(x,
                  "docker/scripts",
                  overwrite = TRUE)

  } else {
    warning("docker/scripts doesnt exist")
  }
}



