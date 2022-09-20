# R6 Class for Docker -----------------------------------------------------

docker <- R6::R6Class(
  classname = "docker",
  cloneable = FALSE,

  # Public List -------------------------------------------------------------


  public = list(

    #' Docker System Wrapper WPI
    #'
    #' @description
    #' Work in progress: Eventually will be part of the containr class.
    #'
    #' @param process The system process. Plan is to add `podman` support but currently only
    #' is setup for `docker`.
    #'
    #' @param commands The main docker commands. Currently support commands
    #' are `images`, `container` and `search`.
    #'
    #' @param options the `command` option arguments.
    initialize = function(process = NA, commands = NA, options = NA) {

      if (!is.character(process)) stop("process must be character")
      if (!is.character(commands)) stop("commands must be character")
      if (!is.character(options)) stop("options must be character")
      private$process = process
      private$commands = commands
      private$options = options
      private$process_check(process)
      private$commands_check(commands)
      private$options_check(options)
      private$docker_command_run(process, commands, options)
    },

    print = function() {
      cat(paste("|", cli::style_bold("Command:"),
        paste(private$process, private$commands, private$options, collapse = " ")), "\n"
      )
      invisible(self)
    },

    #' @description
    #' Displays returned output in `tibble` format.
    show_output = function(){
      dplyr::as_tibble(private$output)
    },

    #' @description
    #' Displays a `tibble` of local images.
    docker_images = function(){
      private$docker_command_run(
        process = "docker",
        commands = "images",
        options = "ls")$self$show_output()
    },

    #' @description
    #' Displays a `tibble` of running containers.
    docker_containers = function(){
      private$docker_command_run(
        process = "docker",
        commands = "container",
        options = "lst")$self$show_output()
    }

  ),

  # Private List ------------------------------------------------------------


  private = list(

    process = NULL,
    commands = NULL,
    options = NULL,
    output = NULL,
    columns = NULL,

    docker_command_run = function(process, commands, options){
      format <- paste(paste0("{{.", private$columns, "}}"), collapse = "\t")
      stdout <- processx::run(command = private$process,
        args = c(private$commands, private$options, paste("--format=", format)),
        echo = FALSE)$stdout

      if (stdout != "") {
        df <- utils::read.delim(text = stdout, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
        colnames(df) <- private$columns
        private$output <- df
        return(private$output)
      } else {
        purrr::map(private$columns, ~ character(0)) |>
          purrr::set_names(private$columns)
      }
    },

    process_check = function(process){
      private$process <- match.arg(process, c("docker", "podman"))
      return(private$process)
    },

    commands_check = function(commands){
      private$commands <- match.arg(commands, c("image", "container", "search", "build", "history"))
    },

    options_check = function(options){
      if(private$commands %in% "image"){
        private$options <- match.arg(options, c("build", "history", "inspect", "load", "ls", "prune", "pull", "rm", "save"))
        private$columns <- c("Repository", "Tag", "ID", "CreatedSince", "Size")
      }

      if(private$commands %in% "container"){
        private$options <- match.arg(options, c("attach", "start", "inspect", "stop", "ls", "kill"))
        private$columns <- c("ID", "Image", "Command", "CreatedAt", "Status", "Names", "Labels", "Ports")
      }

      if(private$commands %in% "search"){
        private$columns <- c("Name", "Description", "StarCount", "IsOfficial", "IsAutomated")
      }
      return(private$options)
    }
  )
)


# Docker functions  -----------------------------------------------------

#' Docker Images
#' @export
docker_images <- function(){

  docker$new(process = "docker",
    commands = "image",
    options = "ls")$show_output()

}

#' Docker Containers
#'
#' @export
docker_containers <- function(){

  docker$new(process = "docker",
    commands = "container",
    options = "ls")$show_output()

}

#' Docker Search
#'
#' @param search A character string of image to search on DockerHub. Returns it in `tibble`
#' format.
#'
#' @export
docker_search <- function(search = "rstudio"){
  docker$new(process = "docker",
    commands = "search",
    options = search)$show_output()
}



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

  #sys_cmd <- paste0("docker push ", name, ":", tag)

  cli::cli_alert_info("{.emph Have you ever heard the tragedy of Darth Plagueis, the wise?}",
    class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))

  #system(sys_cmd)
  #cli::cli_alert_success("Done")

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




