containr <- R6::R6Class("containr",
  cloneable = FALSE,
  public = list(

    #' Start a ContainR
    #'
    #' @description
    #' Setup and start a new `containr` in the background.
    #'
    #' @param image Repository Name of rocker image from [data_rocker_table]. Defaults to
    #' the `rstudio` stack `rocker/rstudio:latest`. For more information about each stack
    #' visit \url{https://rocker-project.org/images/}. If you ran `docker_build()` then supply
    #' this argument with the string of the *name* of the build. Built container images can be
    #' view by running the `docker_images()` function.
    #'
    #' @param name The name of the containr for the docker process. If none is applied then
    #' it will default to the active Rstudio project name.
    #'
    #' @param DISABLE_AUTH Bypass authentication and show the R session.
    #' Defaults to TRUE and will login when the `launch()` function is invoked.
    #'
    #' @param use_local If you want to port your local Rstudio session into the containr.
    #' Defaults to `TRUE`.

    name = NULL,

    initialize = function(image = NULL, name = NULL, DISABLE_AUTH = TRUE,
      use_local = TRUE){
      if(!is.character(image)) stop("image must be character")
      if(!is.character(name)) stop("name must be character")
      if(!is.logical(DISABLE_AUTH)) stop("TRUE/FALSE only")
      if(!is.logical(use_local)) stop("TRUE/FALSE only")

      self$name <- name

      private$DISABLE_AUTH <- DISABLE_AUTH
      private$use_local <- use_local
      private$check_image(image)

    },

    #' Check docker
    #'
    #' @description
    #' Performs a quick system check for installation
    #' of Docker and if not found, gives URL to page for install instructions.
    docker_check = function(){
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
    },

    # Start containr object
    start = function(){
      private$containr_start()
    },

    # Stop containr object
    stop = function(){
      private$containr_stop()
    },

    # Get the status of the containr
    status = function(){
      status = private$containr_status()
      return(status)
    },

    info = function(){
      cat(paste("<", cli::style_bold("Process:"), "docker",
        "|", cli::style_bold("ID:"), gsub("[^[:alnum:] ]", "", private$containr_name),
        "|", cli::style_bold("Pid:"), gsub("[^[:alnum:] ]", "", private$containr_pid),
        "|", cli::style_bold("Status:"),
        if(isTRUE(self$status() == "Running")) {
          cli::col_green("Running")
        } else if(isFALSE(self$status() == "Not Running")){
          cli::col_red("Not Running")
        } else {
          cli::col_yellow("Not Started")
        },
        "|", cli::style_bold("Image:"), gsub("[^[:alnum:] ]", "", private$containr_image),
        ">"))
    },

    launch = function(){
      cli::cli_alert_info("Launching ContainR in your browser...")
      utils::browseURL("http://localhost:8787")
    },

    #' @description
    #' `print(containr)` or `containr$print()` shows some information about the
    #' process on the screen, whether it is running and it's process id, etc.
    print = function(){
      cat(paste(" |", cli::style_bold("Status:"),
        if(isTRUE(self$status() == "Running")) {
          cli::col_green("Running")
        } else if(isFALSE(self$status() == "Not Running")){
          cli::col_red("Not Running")
        } else {
          cli::col_yellow("Not Started")
        }, "\n",
        "|", cli::style_bold("Image:"), private$image, "\n",
        "|", cli::style_bold("Project:"), self$name, "\n",
        "|", cli::style_bold("Auth:"),
        if(isTRUE(private$DISABLE_AUTH)) {
          cli::col_blue(private$DISABLE_AUTH)
        } else {
          cli::col_yellow(private$DISABLE_AUTH)
        }, "\n",
        "|", cli::style_bold("Local:"),
        if(isTRUE(private$use_local)) {
          cli::col_blue(private$use_local)
        } else {
          cli::col_yellow(private$use_local)
        }, "\n"),
        c("|", cli::style_bold("Commands:"), "\n",
          cli::style_italic(private$setup_command())), "\n")
      invisible(self)
    }
  ),

  private = list(

    image = NULL,
    DISABLE_AUTH = TRUE,
    use_local = TRUE,
    connected = FALSE,
    process = NULL,
    containr_pid = NULL,
    containr_name = NULL,
    containr_image = NULL,
    # status = NULL,
    # proc_name = NULL,

    setup_config = function(){
      rprofile <-  paste0(tempdir(), "/.Rprofile")
      return(rprofile)
    },

    # Image check in register or rocker values
    check_image = function(image){

      # Tag set internal for now
      tag = NULL
      if(is.null(tag)){
        tag <- "latest"
      } else {
        tag <- tag
      }

      repo_images <- docker_images() |> dplyr::filter(Repository %in% image)

      data_rocker_table <- ContainR::data_rocker_table

      if(image %in% repo_images$Repository) {

        private$image <- image

      } else if(image %in% data_rocker_table$name) {
        # Switch for changing rocker image based on image name
        private$image <- switch(image,
          rstudio = paste0("rocker/rstudio:", tag),
          tidyerse = paste0("rocker/tidyverse:", tag),
          verse = paste0("rocker/verse:", tag),
          geospatial = paste0("rocker/geospatial:", tag),
          binder = paste0("rocker/binder:", tag))
      } else {
        cli::cli_abort("{.var {image}} not a Rocker image or in Docker Register. Run {.fun docker_build}")
      }
      return(private$image)
    },

    # Setup the docker files and folders
    setup_dockerfiles = function(){
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
        private$get_extdata("install_python.sh")
      }

      if(!fs::file_exists("docker/scripts/install_pyenv.sh")) {
        private$get_extdata("install_pyenv.sh")
      }

      if(!fs::file_exists("docker/scripts/install_additional.sh")) {
        private$get_extdata("install_additional.sh")
      }
      fs::dir_tree("docker")
    },

    # Grab the bash scripts from extdata
    get_extdata = function(file){
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
    },

    # Writelines to the install_libs_local.sh file
    setup_installs = function(file){
      bash_file <- "docker/scripts/install_libs_local.sh"
      return(bash_file)
    },

    # Setup the docker command string
    setup_command = function(DISABLE_AUTH = TRUE, use_local = TRUE, image = NULL){
      testing_cmd <- rocker_args(DISABLE_AUTH = TRUE, use_local = TRUE, image = "testing")
      return(testing_cmd)
    },

    containr_start = function(){
      cmd <- private$setup_command()

      private$process <- processx::process$new(
        command = "docker",
        args = cmd,
        supervise = TRUE,
        stdout = tempfile("containr-stdout-", fileext = ".log"),
        stderr = tempfile("containr-stderr-", fileext = ".log")
      )

      Sys.sleep(3)
      assign("process", private$process, envir = parent.frame())

      if(private$process$is_alive()) {
        # private$process$get_error_file()
        # private$process$get_output_file()
        # private$process$get_status()
        # private$process$format()
        # private$process$get_name()
        # Reset State
        private$connected <- TRUE
        private$get_containr_data()
      }
    },

    containr_stop = function(){
      if(isFALSE(private$connected)) cli::cli_abort("Nothing to stop")
      stopifnot(inherits(private$process, "process"))
      processx::run(command = "docker",
        args = c("stop",
          "ContainR"),
        error_on_status = TRUE,
        echo = FALSE)
      Sys.sleep(0.5)
      on.exit({
        try(private$process$kill(),
          silent = TRUE)
      },
        add = TRUE)
      # Reset State
      private$connected <- FALSE
      private$containr_pid <- NULL
      private$containr_name <- NULL
      private$containr_image <- NULL
    },

    containr_status = function(){
      if(isTRUE(private$connected)){
        status <- "Running"
      }
      if(isFALSE(private$connected)){
        status <- "Not Running"
      }
      return(status)
    },

    get_containr_data = function(){
      private$containr_name <- processx::run(command = "docker",
        args = c("inspect",
          "--format",
          "'{{.Name}}'",
          "ContainR"),
        error_on_status = TRUE)$stdout

      private$containr_pid <- processx::run(command = "docker",
        args = c("inspect",
          "--format",
          "'{{.State.Pid}}'",
          "ContainR"),
        error_on_status = TRUE)$stdout

      private$containr_image <- processx::run(command = "docker",
        args = c("inspect",
          "--format",
          "'{{.Config.Image}}'",
          "ContainR"),
        error_on_status = TRUE)$stdout
    }
  )

)