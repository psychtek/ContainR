

# R6 Class for Docker -----------------------------------------------------

docker <- R6::R6Class(
  classname = "docker",
  cloneable = FALSE,

  public = list(

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
    },

    show_output = function(){
      dplyr::as_tibble(private$output)
    },


    # docker_images = function(){
    #   private$docker_command_run(
    #     process = "docker",
    #     commands = "images",
    #     options = "ls")
    # },
    #
    docker_containers = function(){
      private$docker_command_run(
        process = "docker",
        commands = "container",
        options = "lst")$self$show_output()
    }

    # docker_search = function(){
    #   private$docker_command_run(
    #     process = "docker",
    #     commands = "search",
    #     options = "rstudio")
    # },

    # docker_images = function(){
    #   #image options
    #   columns <- c("Repository", "Tag", "Digest", "ID", "CreatedSince", "Size")
    #   format <- paste(paste0("{{.", columns, "}}"), collapse = "\t")
    #   stdout <- processx::run(command = self$process,
    #     args = c(self$commands, self$options, paste0("--format=", format)),
    #     echo = FALSE)$stdout
    #
    #   df <- utils::read.delim(text = stdout, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
    #   colnames(df) <- columns
    #   cont_list <- dplyr::as_tibble(df)
    #   return(cont_list)
    # },


  ),

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

      df <- utils::read.delim(text = stdout, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
      colnames(df) <- private$columns
      private$output <- df
      return(private$output)
    },

    process_check = function(process){
      private$process <- match.arg(process, c("docker", "podman"))
      return(private$process)
    },

    commands_check = function(commands){
      private$commands <- match.arg(commands, c("image", "container", "search"))
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
      return(private$options)
    }
  )
)

# Constructors ------------------------------------------------------------
command <- function(process, commands, options, ...){

  if (!is.character(process)) stop("X must be character")
  if (!is.character(commands)) stop("X must be character")
  if (!is.character(options)) stop("X must be character")
  # TODO set missing() for options check

  # cmd must be a command type of docker or podman
  proc <- match.arg(process, c("docker", "podman"))
  comm <- match.arg(commands, c("run", "image", "container",
                                "search", "build", "stop"))
  opts <- options

  value <- list(process = proc,
                commands = comm,
                options = opts)

  structure(.Data = value,
            class = c("command"))

}

# containr <- function(image = NULL, name = NULL, DISABLE_AUTH = TRUE, use_local = TRUE, ...){
#
#   # Check and Set image
#   if(!is.character(image)) stop("image must be character")
#   image <- check_images(image = image)
#   image_id <- docker_images() |> dplyr::filter(Repository %in% image) |> purrr::pluck("ID")
#
#   # Check and Set name
#   if(!is.null(name) && !is.character(name)) stop("name must be character")
#
#   if(is.null(name)) {
#     name <- basename(rstudioapi::getActiveProject())
#   } else {
#     name <- name
#   }
#
#   # Check and Set logical
#   if(!is.logical(DISABLE_AUTH)) stop("AUTH must be TRUE or FALSE")
#   if(!is.logical(use_local)) stop("AUTH must be TRUE or FALSE")
#
#
#   commands <- rocker_args(DISABLE_AUTH = TRUE, use_local = TRUE, image = "testing")
#
#   value <- list(process = "docker", # set to docker only for now
#                 commands = commands,
#                 image = image,
#                 name = name,
#                 AUTH = DISABLE_AUTH,
#                 local = use_local)
#
#   structure(.Data = value,
#             .ID = "",
#             class = c("containr"))
# }

# Methods -----------------------------------------------------------------

docker <- function(x) {
  UseMethod("docker")
}

docker.command <- function(x, ...) {

  # TODO update to use switch()
  if(x$commands == "image"){
    #image options
    columns <- c("Repository", "Tag", "Digest", "ID", "CreatedSince", "Size")
  }

  if(x$commands == "container"){
    #container options
    columns <- c("ID", "Image", "Command", "CreatedAt", "Status", "Names", "Labels", "Ports")
  }

  if(x$commands == "search"){
    #search options
    columns <- c("Name", "Description", "StarCount", "IsOfficial", "IsAutomated")
    format <- paste(paste0("{{.", columns, "}}"), collapse = "\t")
    stdout <- processx::run(command = x$process,
                            args = c(x$commands, paste0("--format=", format), x$options),
                            echo = FALSE)$stdout
  } else {
    format <- paste(paste0("{{.", columns, "}}"), collapse = "\t")
    stdout <- processx::run(command = x$process,
                            args = c(x$commands, x$options, paste0("--format=", format)),
                            echo = FALSE)$stdout
  }


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
#
# run <- function(x){
#   UseMethod("run")
# }
#
# run.default <- function(x, ...){
#   cat("This is the run.default function\n")
# }
#
# run.containr <- function(x, ...){
#
#   dockered <- processx::process$new(command = x$process,
#                                     args = x$commands,
#                                     pty = TRUE) #only supported on Unix sys
#
#
#   Sys.sleep(3)
#   columns <- c("ID", "Image", "Command", "CreatedAt", "Status", "Names", "Labels", "Ports")
#   format <- paste(paste0("{{.", columns, "}}"), collapse = "\t")
#   stdout <- processx::run("docker", c("ps", paste0("--format=", format)), echo = FALSE)$stdout
#
#   df <- utils::read.delim(text = stdout,
#                           sep = "\t",
#                           header = FALSE,
#                           stringsAsFactors = FALSE)
#
#   colnames(df) <- columns
#   docker_info <- dplyr::as_tibble(df) |> dplyr::filter(Names == x$name)
#
#   name = docker_info$Names
#   internal_port <- strsplit(docker_info$Ports,"->")[[1]][1]
#   docker_port <- strsplit(docker_info$Ports,"->")[[1]][2]
#   docker_status <- strsplit(docker_info$Status," ")[[1]][1]
#
#   # get the env set object
#   docker_id <- docker_info$ID
#   port <- internal_port
#   docker_port <- docker_port
#   pid <-  dockered$get_pid()
#   status <-  dockered$get_status()
#   image <- docker_info$Image
#
#   cat(paste("<", cli::style_bold("Process:"), x$process,
#             "|", cli::style_bold("ID:"), docker_id,
#             "|", cli::style_bold("Port:"), port,
#             "|", cli::style_bold("Docker Port:"), docker_port,
#             "|", cli::style_bold("Pid:"), pid,
#             "|", cli::style_bold("Status:"), cli::col_green(status), # ifelse red then green
#             "|", cli::style_bold("Image:"), image,
#             ">"))
#
# }
#

# Print Methods -----------------------------------------------------------

print.command <- function(x){
  cat(paste(cli::style_italic(x$process),
            cli::style_italic(x$commands),
            cli::style_italic(x$options),
            collapse = " "))
}

# print.containr <- function(x){
#   cat(paste(" |", cli::style_bold("Process:"),
#             if(length(Sys.which("docker")) > 0) {
#               cli::col_green(x$process)
#             } else {
#               cli::col_red(x$process, " Not Running")
#             }, "\n",
#             "|", cli::style_bold("Image:"), x$image, "\n",
#             "|", cli::style_bold("Project:"), x$name, "\n",
#             "|", cli::style_bold("Auth:"),
#             if(isTRUE(x$AUTH)) {
#               cli::col_blue(x$AUTH)
#             } else {
#               cli::col_yellow(x$AUTH)
#             }, "\n",
#             "|", cli::style_bold("Local:"),
#             if(isTRUE(x$local)) {
#               cli::col_blue(x$local)
#             } else {
#               cli::col_yellow(x$local)
#             }, "\n"),
#       c("|", cli::style_bold("Commands:"), cli::style_italic(x$commands)))
# }
#
# print.run <- function(x, ...){
# message("this is the print.run ")
# }


# Containr functions  -----------------------------------------------------

docker_images <- function(){

  obj_check <- command(process = "docker",
                       commands = "image",
                       options = "list")
  docker(obj_check)

}

docker_containers <- function(){

  obj_check <- command(process = "docker",
                       commands = "container",
                       options = "list")
  docker(obj_check)

}

docker_search <- function(search = "rstudio"){

  obj_check <- command(process = "docker",
                       commands = "search",
                       options = search)
  docker(obj_check)

}





