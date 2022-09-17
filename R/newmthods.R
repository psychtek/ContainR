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

cont <- docker$new(process = "docker", command = "image", options = "ls")

cont$show_output()
cont$docker_images()
cont$docker_containers()




docker_containers <- function(){

  docker$new(process = "docker",
    commands = "container",
    options = "ls")$show_output()
  #docker$docker_return(obj_check)

}

docker_search <- function(search = "rstudio"){

  obj_check <- command(process = "docker",
    commands = "search",
    options = search)
  docker(obj_check)

}
