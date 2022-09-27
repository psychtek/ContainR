#' @include docker.R dockerfile.R
#' @title ContainR Class Object
#'
#' @description
#' A class object of `containr` that sets up a object to launch a defined rocker
#' containr in a browser.
#'
#' @section Rockered:
#' Creates a new `containr` object based on the \url{https://rocker-project.org/images/}.
#'
#' @export
containr <- R6::R6Class("containr",
  cloneable = FALSE,

  # Public Functions --------------------------------------------------------

  public = list(

    #' @field image Name of the image from either the [data_rocker_table] or in docker register
    image = NULL,

    #' @field name Name of the containr when launched. Defaults to the active Rstudio Project.
    name = NULL,

    #' @field tag Tag of image defaults to `latest` if nothing set.
    tag = NULL,

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
    #' @param tag A character string of the require version. If no tag is supplied then the function will default to `latest`.
    #'
    #' @param DISABLE_AUTH Bypass authentication and show the R session.
    #' Defaults to TRUE and will login when the `launch()` function is invoked.
    #'
    #' @param use_local If you want to port your local Rstudio session into the containr.
    #' Defaults to `TRUE`.
    initialize = function(image = NULL, name = NULL, tag = NULL, DISABLE_AUTH = TRUE, use_local = TRUE){

      self$set_login(DISABLE_AUTH)
      self$set_local(use_local)
      self$set_name(name)
      self$set_tag(tag)
      self$set_image(image)

    },

    #' @description
    #' Set or change the containr image name. Also checks the docker register for existing images.
    #' @param image Set image name that was set from the `build_image()` setup with the `set_image_name`.
    set_image = function(image){
      if(image %in% data_rocker_table$name) {
        self$image <- switch(image,
          rstudio = paste0("rocker/rstudio:", self$tag),
          tidyverse = paste0("rocker/tidyverse:", self$tag),
          verse = paste0("rocker/verse:", self$tag),
          geospatial = paste0("rocker/geospatial:", self$tag),
          binder = paste0("rocker/binder:", self$tag))
      } else {
        self$image <- image
      }
      private$containr_image <- self$image
      return(private$containr_image)
    },

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project.
    #' @param name Change containr name
    set_name = function(name){
      if(is.null(name)){
        self$name <- basename(getwd())
      } else {
        self$name <- name
      }
      private$containr_name <- self$name
      return(private$containr_name)
    },

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project.
    #' @param tag Change tag name. Defaults to `latest`
    set_tag = function(tag){
      if(isTRUE(is.null(tag))){
        self$tag <- "latest"
      } else {
        self$tag <- tolower(tag)
      }
      return(self$tag)
    },

    #' @description
    #' Rstudio login
    #' @param DISABLE_AUTH Set to enable the Rstudio Login. Set to `TRUE` until secure
    #' env is setup.
    set_login = function(DISABLE_AUTH){
      private$DISABLE_AUTH <-  DISABLE_AUTH
    },

    #' @description
    #' Port local config and theme settings
    #' @param use_local Flag `TRUE` to port local settings or `FALSE` for a clean session.
    set_local = function(use_local){
      private$use_local <-  use_local
    },

    #' @description
    #' Start the predefined containr as set by the `image` argument.
    start = function(){
      private$containr_start()
    },

    #' @description
    #' Stop containr object
    stop = function(){
      private$containr_stop()
    },

    #' @description
    #' Get the status of the containr
    status = function(){
      status = private$containr_status()
      return(status)
    },

    #' @description
    #' Opens the active containr in a browser.
    launch = function(){
      if(is.null(private$containr_image)){
        cli::cli_alert_warning("No ContainR Running. Have you tried turning it off and back on again?")
        cli::cli_alert_info("Tip: Try running the {.fun {containr:start}}")
      } else {
        cli::cli_alert_info("Launching ContainR in your browser...")
        utils::browseURL("http://localhost:8787")
      }
    },

    #' @description
    #' Displays the process information of the active containr
    proc = function(){
      cli::cli_h1("Process")
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
        "|", cli::style_bold("Image:"), ifelse(is.null(private$containr_image_run_mode),
          cli::col_yellow("Not Started"),
          cli::col_green(gsub("[^[:alnum:] ]", "", private$containr_image_run_mode))),
        ">"))
    },

    #' @description
    #' `info(containr)` or `containr$info()` shows some information about the
    #' process on the screen, whether it is running and it's process id, etc.
    info = function(){
      cli::cli_h1("ContainR")
      cat(paste(" |", cli::style_bold("Status:"),
        if(isTRUE(self$status() == "Running")) {
          cli::col_green("Running")
        } else if(isFALSE(self$status() == "Not Running")){
          cli::col_red("Not Running")
        } else {
          cli::col_yellow("Not Started")
        }, "\n",
        "|", cli::style_bold("Image:"), private$containr_image, "\n",
        "|", cli::style_bold("Project:"), private$containr_name, "\n",
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

  # Private Functions -------------------------------------------------------


  private = list(

    proj_name = NULL,
    proj_image = NULL,
    DISABLE_AUTH = TRUE,
    use_local = TRUE,
    connected = FALSE,
    process = NULL,
    containr_pid = NULL,
    containr_name = NULL,
    containr_image = NULL,
    containr_config = NULL,
    containr_image_run_mode = NULL,

    setup_config = function(){
      proj_name <- private$containr_name
      private$containr_config <-  create_config_file(proj_name)
    },

    # Setup the docker command string
    setup_command = function(){

      # proj_image <- private$containr_image
      # proj_name <- private$containr_name
      private$setup_config()
      # set volume mounts
      r_dir <- paste0(fs::path_wd(), ":/home/rstudio/", basename(fs::path_wd()))
      r_config <- paste0(fs::path_home_r(), "/.config/rstudio")
      r_env <- paste0(fs::path_home_r(), "/.Renviron")
      r_prof <- private$containr_config

      r_config <- paste0(r_config, ":/home/rstudio/.config/rstudio", collapse = "")
      r_env <- paste0(r_env, ":/home/rstudio/.Renviron", collapse = "")
      r_prof <- paste0(r_prof, ":/home/rstudio/.Rprofile", collapse = "")

      if(isTRUE({{private$use_local}})){
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
      set_env <- c("-e", paste0("DISABLE_AUTH=", ifelse(isTRUE({{private$DISABLE_AUTH}}), {{private$DISABLE_AUTH}}, "FALSE")))
      set_name <- c("--name", private$containr_name)
      image_name <- c(private$containr_image)

      # Argument string for docker
      proc_args <- c(docker_opts, set_env, ports, volumes, set_name, image_name)

      return(proc_args)
    },

    containr_start = function(){
      cmd <- private$setup_command()

      private$process <- processx::process$new(
        command = "docker",
        args = cmd,
        supervise = TRUE,
        stdout = tempfile("containr-stdout-", fileext = ".log"),
        stderr = tempfile("containr-stderr-", fileext = ".log")
      ) # TODO this still flags as running when docker fails

      Sys.sleep(3)

      if(private$process$is_alive()) {
        # private$process$get_error_file()
        # private$process$get_output_file()
        # private$process$get_status()
        # private$process$format()
        # private$process$get_name()
        # Reset State
        private$connected <- TRUE
        assign("process", private$process, envir = parent.frame())
        private$get_containr_data()
      }
    },

    containr_stop = function(){
      if(isFALSE(private$connected)) cli::cli_abort("Nothing to stop")
      stopifnot(inherits(private$process, "process"))

      processx::run(command = "docker",
        args = c("stop",
          private$containr_name),
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
      private$containr_name <- self$name
      private$containr_image <- self$image
      private$containr_image_run_mode <- NULL
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
      private$containr_image_run_mode <- processx::run(command = "docker",
        args = c("inspect",
          "--format",
          "'{{.Name}}'",
          private$containr_name),
        error_on_status = TRUE)$stdout

      private$containr_pid <- processx::run(command = "docker",
        args = c("inspect",
          "--format",
          "'{{.State.Pid}}'",
          private$containr_name),
        error_on_status = TRUE)$stdout

      private$containr_image <- processx::run(command = "docker",
        args = c("inspect",
          "--format",
          "'{{.Config.Image}}'",
          private$containr_name),
        error_on_status = TRUE)$stdout

      # additional outputs
      # processx::run(command = "docker",
      #   args = c("inspect",
      #     "--format",
      #     "'{{.State.Running}}'",
      #     "ContainR"),
      #   error_on_status = TRUE)$stdout
      #
      # processx::run(command = "docker",
      #   args = c("inspect",
      #     "--format",
      #     "'{{.State.Paused}}'",
      #     "ContainR"),
      #   error_on_status = TRUE)$stdout
      #
      # processx::run(command = "docker",
      #   args = c("inspect",
      #     "--format",
      #     "'{{.State.StartedAt}}'",
      #     "ContainR"),
      #   error_on_status = TRUE)$stdout
      #
      # processx::run(command = "docker",
      #   args = c("inspect",
      #     "--format",
      #     "'{{.Name}}'",
      #     "ContainR"),
      #   error_on_status = TRUE)$stdout
    }
  )

)



# Helpers -----------------------------------------------------------------

# Config file that will overwrite the Rstudio one
# and then when session is launch it will activate working dir
# Works but could be a better way.
create_config_file <- function(proj_name){

  rprofile <-  paste0(tempdir(), "/.Rprofile")

  work_directory <- paste0("setwd(\"/home/rstudio/", proj_name, "\")")

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

  return(rprofile)

}




