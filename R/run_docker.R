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
    sys_cmd <- processx::run(command = "docker",
                             args = c("-v"),
                             error_on_status = FALSE)

    cli::cli_alert_success('{gsub("[\r\n]", "", sys_cmd$stdout)}')

  }

}

#' Docker build
#'
#' Builds a docker container from a `Dockerfile`. The function will look in the `inst/dockerfile/` directory
#' for an existing Dockerfile.
#'
#' @param dockerfile Character path to supplied Dockerfile. Save to `docker/` or
#' if one doesnt exists then a Dockerfile can be generated from the `docker_file()` function.
#'
#' @param name Name and or name and tag of container name such as your Dockerhub username: `username/image_name`. If no name
#' is supplied then it will use the active Rstudio project name `rstudioapi::getActiveProject()`.
#'
#' @export
docker_build <- function(dockerfile = "docker/Dockerfile", name = NULL){

  docker_check()

  if(isFALSE(fs::file_exists(dockerfile))){
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

  columns <- c("Name", "Description", "StarCount", "IsOfficial", "IsAutomated")
  format <- paste(paste0("{{.", columns, "}}"), collapse = "\t")
  stdout <- processx::run("docker", c("search", paste0("--format=", format), search_string), echo = FALSE)$stdout

  if (stdout != "") {
    df <- utils::read.delim(text = stdout, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
    colnames(df) <- columns
    cont_list <- dplyr::as_tibble(df)
    return(cont_list)
  } else {
    purrr::map(columns, ~ character(0)) |>
      purrr::set_names(columns) |>
      dplyr::as_tibble()

  }

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
  #TODO this needs to be update for secure login
  # also to return state to check if already logged in processx::new
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
  #
  # p <- processx::process$new(
  #   "docker",
  #   c("login", "-u", "gaborcsardi", "--password-stdin"),
  #   stdin = "|", stdout = "|", stderr = "|")

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




#' View Docker Images
#'
#' A basic system wrapper for equiv terminal command to view the Docker register
#'
#' @importFrom utils read.delim
#'
#' @export
docker_images <- function(){
  # https://github.com/dynverse/babelwhale/blob/0f8f9bab5e35150f05d60aba45eec77946f21f50/R/list_docker_images.R
  # defaultImageTableFormat
  columns <- c("Repository", "Tag", "Digest", "ID", "CreatedSince", "Size")
  format <- paste(paste0("{{.", columns, "}}"), collapse = "\t")
  stdout <- processx::run("docker", c("image", "ls", paste0("--format=", format)), echo = FALSE)$stdout

  if (stdout != "") {
    df <- utils::read.delim(text = stdout, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
    colnames(df) <- columns
    cont_list <- dplyr::as_tibble(df)
    return(cont_list)
  } else {
    purrr::map(columns, ~ character(0)) |>
      purrr::set_names(columns) |>
      dplyr::as_tibble()
  }
}


#' Create a Dockerfile
#'
#' This function creates a dockerfile based off the rocker version image stacks and reads
#' from session information to install `loaded `, `installed` \(or `none`\) R libraries. The `install2.r` handles
#' additional package installs and will skip already installed packages on the rocker stack image. The final `Dockerfile`
#' can be used to build a container image with your development environment.
#'
#' @param dockerfile Default location is in the `docker/` folder. Build will save the final `Dockerfile`
#' to this location at the root project directory. You can also add you're own Dockerfile, however, this package was primarily designed to launch
#' an active project into a **Rstudio** container from one of the Rocker images: \url{https://rocker-project.org/images/}.
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
docker_file <- function(dockerfile = "docker/Dockerfile",
                          which_pkgs = c("loaded", "installed", "none"),
                          name = c("rstudio", "tidyverse", "verse", "geospatial", "binder"),
                          tag = NULL,
                          include_python = FALSE){

  cli::cli_h1("Creating Dockerfile")
  name <- match.arg(name)
  # Check for image name
  data_rocker_table <- ContainR::data_rocker_table

  # which_pkgs argument check
  if(!(which_pkgs %in% c("loaded", "installed", "none"))) {

    cli::cli_abort("{.emph which_pkgs} must be of type {.val loaded}, {.val installed} or {.val none}",
                   class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))

  }

  # rocker name match check
  if(!(name %in% data_rocker_table$name)){
    cli::cli_abort("{.arg name} must be one of: {.emph {data_rocker_table$name}}")
    print(data_rocker_table)
  }

  # Flag for python inclusion
  if(isTRUE(include_python)) {
    cli::cli_alert_info("Install Python: {.val {include_python}}")
  } else if(isFALSE(include_python)) {
    cli::cli_alert_info("Install Python: {.val {include_python}}")
  }

  # Set tag
  if(is.null(tag)){
    tag <- "latest"
  } else {
    tag <- tolower(tag)
  }

  # File overwrite warning
  if(fs::file_exists(dockerfile)) {
    cli::cli_alert_warning("{.emph dockerfile} will be overwritten.",
                           class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))

  } else {
    cli::cli_alert_info("Setting up directory and files...")
    docker_folder_setup()
  }

  rocker_image <- switch(name,
                         rstudio = paste0("rocker/rstudio:", tag),
                         tidyverse = paste0("rocker/tidyverse:", tag),
                         verse = paste0("rocker/verse:", tag),
                         geospatial = paste0("rocker/geospatial:", tag),
                         binder = paste0("rocker/binder:", tag))

  cli::cli_alert_info("Using Rocker Stack: {.path {rocker_image}}")

  packages <- sessioninfo::package_info(pkgs = which_pkgs)

  if(which_pkgs != "none") {
   write_install_bash_file(packages)
  }

  # command layout for dockerfile
  docker_cmds <- paste(c(
    paste("FROM", rocker_image),
    # TODO paste("LABEL", "label_name"),
    paste(""),
    if(isTRUE(include_python)) {
      c(
        paste("COPY /docker/scripts/install_python.sh /tmp/"),
        paste(""),
        paste("COPY /docker/scripts/install_pyenv.sh /tmp/"),
        paste(""),
        paste("RUN", "chmod +x /tmp/install_python.sh"),
        paste(""),
        paste("RUN", "/tmp/install_python.sh"),
        paste(""),
        paste("RUN", "R -e \"reticulate::py_config()\""),
        paste("")
      )
    },
    paste("COPY /docker/scripts/install_libs_local.sh /tmp/"),
    paste(""),
    paste("COPY /docker/scripts/install_additional.sh /tmp/"),
    paste(""),
    paste("RUN chmod +x /tmp/install_libs_local.sh"),
    paste(""),
    paste("RUN chmod +x /tmp/install_additional.sh"),
    paste(""),
    paste("RUN", "/tmp/install_additional.sh")
  ),
  collapse = "\n")

  writeLines(docker_cmds, con = dockerfile, sep = "")

  if(isTRUE(file.exists(dockerfile))){
    cli::cli_alert_success("Dockerfile saved to: {.path {dockerfile}}")
  }

  # Tip to run function next
  #cli::cli_alert_info("Run: {.emph docker_build_dockerfile(dockerfile = \"{dockerfile}\", name = \"name\")} ")

}

#' Docker Push Image
#'
#' Push container image to \url{https://hub.docker.com/}. Naming Convention in Docker Hub:
#' Very popularly known software are given their own name as Docker image. For individuals and
#' corporates, docker gives a username. And all the images can be pushed to a particular username.
#' And can be pulled using the image name with a username.
#'
#'
#' @param name The user/name of the docker image
#' @param tag Version or tag of image. Defaults to latest.
#'
#' @export
docker_push <- function(name = NULL, tag = NULL){

  cli::cli_h1("Docker Push {.val {name}}:{.val {tag}}")
  #docker_login()

  if(is.null(tag)){
    tag <- "latest"
  } else {
    tag <- tag
  }

  sys_cmd <- paste0("docker push ", name, ":", tag)

  cli::cli_alert_info("{.emph Have you ever heard the tragedy of Darth Plagueis, the wise?}",
                         class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))

  system(sys_cmd)
  cli::cli_alert_success("Done")

}

#' View Running Containers
#'
#' View current running containers.
#'
#' @export
docker_container <- function(){

  # TODO add name filter in case other containers are running

  # defaultContainerTableFormat
  columns <- c("ID", "Image", "Command", "CreatedAt", "Status", "Names", "Labels", "Ports")
  format <- paste(paste0("{{.", columns, "}}"), collapse = "\t")
  stdout <- processx::run("docker", c("ps", paste0("--format=", format)), echo = FALSE)$stdout

  if (stdout != "") {
    df <- utils::read.delim(text = stdout, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
    colnames(df) <- columns
    cont_list <- dplyr::as_tibble(df)
    return(cont_list)
  } else {
    purrr::map(columns, ~ character(0)) |>
      purrr::set_names(columns) |>
      dplyr::as_tibble()
  }
}


#' @title Define the connection to the docker container
#'
#' @details This object defines the connection to the docker image when it can accept
#' external inputs
#'
#' @param name the name of the docker image

#' @return dockefile object
#' @exportClass docker_connection docker_container
#' @export
docker_connection <- function(name){

  if(isFALSE(dockered$is_alive())){
    stop("No Containers are currently running")
  }

  # defaultContainerTableFormat
  columns <- c("ID", "Image", "Command", "CreatedAt", "Status", "Names", "Labels", "Ports")
  format <- paste(paste0("{{.", columns, "}}"), collapse = "\t")
  stdout <- processx::run("docker", c("ps", paste0("--format=", format)), echo = FALSE)$stdout

  df <- utils::read.delim(text = stdout,
                          sep = "\t",
                          header = FALSE,
                          stringsAsFactors = FALSE)

  colnames(df) <- columns
  cont_list <- dplyr::as_tibble(df)

  if(name != cont_list$Names){
    cli::cli_abort("{.var {name}} container not found.")
  } else {
    docker_info <- cont_list |>
      dplyr::filter(Names == name)
  }

  name = docker_info$Names
  internal_port <- strsplit(docker_info$Ports,"->")[[1]][1]
  docker_port <- strsplit(docker_info$Ports,"->")[[1]][2]

  docker_status <- strsplit(docker_info$Status," ")[[1]][1]

  structure(
    .Data        = name,
    .name        = name,
    .image       = docker_info$Image,
    .port        = internal_port,
    .docker_port = docker_port,
    .docker_id   = docker_info$ID,
    .status = docker_info$Status,
    .pid = dockered$get_pid(),
    class = c("docker_connection","docker_container")
  )
}

#' Print method
#'
#' @param x Name of image
#' @param ... Additional params
#'
#' @export
print.docker_connection <- function(x,...){

  container <- docker_connection(attr(x,".name"))

  cat(paste("<", cli::style_bold("Docker Container:"), attr(x,".name"),
            "|", cli::style_bold("Port:"), attr(x,".port"),
            "|", cli::style_bold("Pid:"), attr(x,".pid"),
            "|", cli::style_bold("Status:"), cli::col_green(attr(x,".status")),
            "|", cli::style_bold("image:"), attr(x,".port"),
            ">"))

}

#' Run a Docker Container
#'
#' Currently only `docker` is available but plans are to implement `podman` in the future.
#'
#' @param cmd System command. `docker` as default until other container as are supported.
#' @param proc_args A character string of arguments to the `cmd`.
#' @param name Character string of the docker image name.
#'
#' @export
docker_run <- function(cmd, proc_args, name){


  if(isTRUE(dockered$is_alive())){
    cli::cli_alert_info(dockered$format())
  } else {
    # global assign for intrim testing
    dockered <<- processx::process$new(command = cmd,
                                       args = proc_args,
                                       pty = TRUE) #only supported on Unix sys
  }

  # dockered$is_alive()
  # dockered$get_pid()
  # dockered$get_status()
  # dockered$get_name()
  # dockered$get_cmdline()
  # dockered$get_wd()
  # dockered$get_username()

}


#' Stop the active docker container
#'
#' @param x either the name of the docker container or a docker_connection
#'
#' @return NULL
#'
#' @export
docker_stop <- function(x) {
  UseMethod("docker_stop", x)
}

#' @export
docker_stop.character <- function(x) {
  conn <- docker_connection(x)
  docker_stop.docker_connection(conn)
}

#' @export
docker_stop.docker_connection <- function(x) {

  id <- attr(x,".docker_id")

  project_name <- basename(rstudioapi::getActiveProject())

  cli::cli_h1("Stopping {.val {id}} container")

  cmd_stop <- processx::run(command = "docker",
                            args = c("stop",
                                     project_name),
                            echo_cmd = FALSE,
                            echo = FALSE,
                            spinner = TRUE)

  dockered$kill()

  if(is.null(attr(result,".status"))){
    cli::cli_alert_success('{.path {gsub("[\r\n]", "", cmd_stop$stdout)}} Successfully stopped.')
  }

}
