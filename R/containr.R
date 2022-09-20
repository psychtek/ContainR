#' R6 Class for a Rocker ContainR
#'
#' @description
#' A class object of `containr` that sets up a object to launch a defined rocker
#' containr in a browser.
#'
#' @details
#' Creates a new `containr` object based on the \url{https://rocker-project.org/images/}.

containr <- R6::R6Class("containr",
  cloneable = FALSE,

  # Public Functions --------------------------------------------------------

  public = list(

    #' @field image Name of the image from either the [data_rocker_table] or in docker register
    image = NULL,

    #' @field name Name of the containr when launched. Defaults to the active Rstudio Project.
    name = NULL,

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
    #' @param DISABLE_AUTH Bypass authentication and show the R session.
    #' Defaults to TRUE and will login when the `launch()` function is invoked.
    #'
    #' @param use_local If you want to port your local Rstudio session into the containr.
    #' Defaults to `TRUE`.
    initialize = function(image = NULL, name = NULL, DISABLE_AUTH = TRUE, use_local = TRUE){

      self$set_image(image)
      self$set_name(name)

      private$DISABLE_AUTH = DISABLE_AUTH
      private$use_local = use_local

    },

    #' @description
    #' Set or change the containr image name. Also checks the docker register for existing images.
    #' @param name
    set_image = function(image){
      if(is.null(image)){
        self$image <- "rstudio"
      } else if(image %in% data_rocker_table$name) {
        # Switch for changing rocker image based on image name
        self$image <- switch(image,
          rstudio = paste0("rocker/rstudio:", self$tag),
          tidyerse = paste0("rocker/tidyverse:", self$tag),
          verse = paste0("rocker/verse:", self$tag),
          geospatial = paste0("rocker/geospatial:", self$tag),
          binder = paste0("rocker/binder:", self$tag))
      } else {
        private$proj_image <- self$image
        return(private$proj_image)
      }

    },

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project.
    #' @param name Change containr name
    set_name = function(name){
      if(is.null(name)){
        self$name <- basename(rstudioapi::getActiveProject())
      } else {
        self$name <- name
      }
      private$proj_name <- self$name
      return(private$proj_name)
    },

    #' @description
    #' Start the predefined containr. If none are defined then it will default to
    #' the `rstudio` image and port local settings.
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
        "|", cli::style_bold("Image:"), ifelse(is.null(private$containr_image),
          cli::col_yellow("Not Started"),
          gsub("[^[:alnum:] ]", "", private$containr_image)),
        ">"))
    },

    #' @description
    #' `print(containr)` or `containr$print()` shows some information about the
    #' process on the screen, whether it is running and it's process id, etc.
    proc = function(){
      cat(paste(" |", cli::style_bold("Status:"),
        if(isTRUE(self$status() == "Running")) {
          cli::col_green("Running")
        } else if(isFALSE(self$status() == "Not Running")){
          cli::col_red("Not Running")
        } else {
          cli::col_yellow("Not Started")
        }, "\n",
        "|", cli::style_bold("Image:"), private$proj_image, "\n",
        "|", cli::style_bold("Project:"), private$proj_name, "\n",
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

    setup_config = function(){
      proj_name <- private$proj_name
      private$containr_config <-  create_config_file(proj_name)
    },

    # Image check in register or rocker values
    # check_image = function(proj_image){
    #
    #   proj_image <- proj_image
    #   # Tag set internal for now
    #   tag = NULL
    #   if(is.null(tag)){
    #     tag <- "latest"
    #   } else {
    #     tag <- tag
    #   }
    #
    #   repo_images <- docker_images() |> dplyr::filter(Repository %in% proj_image)
    #
    #   data_rocker_table <- data_rocker_table
    #
    #   if(proj_image %in% repo_images$Repository) {
    #
    #     private$proj_image <- proj_image
    #
    #   } else if(proj_image %in% data_rocker_table$name) {
    #     # Switch for changing rocker image based on image name
    #     private$proj_image <- switch(proj_image,
    #       rstudio = paste0("rocker/rstudio:", tag),
    #       tidyerse = paste0("rocker/tidyverse:", tag),
    #       verse = paste0("rocker/verse:", tag),
    #       geospatial = paste0("rocker/geospatial:", tag),
    #       binder = paste0("rocker/binder:", tag))
    #   } else {
    #     cli::cli_abort("{.var {image}} not a Rocker image or in Docker Register. Run {.fun docker_build}")
    #   }
    #   return(private$proj_image)
    # },

    # Setup the docker command string
    setup_command = function(proj_image = NULL, proj_name = NULL, DISABLE_AUTH = NULL, use_local = NULL){

      proj_image <- private$proj_image
      proj_name <- private$proj_name
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
      set_name <- c("--name", proj_name)
      image_name <- c(proj_image)

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
      ) #TODO this still flags as running when docker fails

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


# Helpers -----------------------------------------------------------------

# Config file that will overwrite the Rstudio one
# and then when session is launch it will activate working dir
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


# Dockerfile Class --------------------------------------------------------

dockerfile <- R6::R6Class(
  classname = "dockerfile",
  inherit = containr,

  # Public List -------------------------------------------------------------

  public = list(

    image_name = NULL,
    dockerfile = NULL,
    rocker_image = NULL,
    packages = NULL,
    include_python = NULL,
    build = NULL,
    tag = NULL,

    initialize = function(image_name = NA, rocker_image = NA, tag = NULL, dockerfile = NA,
      packages = NA, include_python = FALSE, build = FALSE){

      # These are initialized in order from top to bottom
      self$set_image_name(image_name)
      self$set_tag(tag)
      self$set_rocker_image(rocker_image)
      self$set_dockerfile(dockerfile)
      self$set_packages(packages)
      self$set_python(include_python)
      private$create_directories()
      private$setup_packages()
      private$create_dockerfile()
      self$build = build
      super$initialize(image = private$containr_image_name)
    },

    set_image_name = function(image_name){
      if(is.null(image_name)) {
        self$image_name <- tolower(basename(rstudioapi::getActiveProject()))
      } else {
        self$image_name <- tolower(image_name)
      }
      private$containr_image_name <- self$image_name
      return(private$containr_image_name)
    },

    set_tag = function(tag){
      if(is.null(tag)){
        self$tag <- "latest"
      } else {
        self$tag <- tolower(tag)
      }
      super$tag <- self$tag
      return(self$tag)
    },

    set_rocker_image = function(rocker_image){

      rocker_image <- match.arg(rocker_image, c("rstudio", "tidyverse", "verse", "geospatial", "binder"))

      self$rocker_image <- switch(rocker_image,
        rstudio = paste0("rocker/rstudio:", self$tag),
        tidyverse = paste0("rocker/tidyverse:", self$tag),
        verse = paste0("rocker/verse:", self$tag),
        geospatial = paste0("rocker/geospatial:", self$tag),
        binder = paste0("rocker/binder:", self$tag))

      private$containr_rocker_image <- self$rocker_image
      return(private$containr_rocker_image)
    },

    set_dockerfile = function(dockerfile){
      if(!is.character(dockerfile)) stop("dockerfile must be a character")
      self$dockerfile <- dockerfile
      private$containr_dockerfile <- self$dockerfile
      return(private$containr_dockerfile)
    },

    set_packages = function(packages){
      self$packages <- match.arg(packages, c("loaded", "none", "installed"))
      private$containr_packages <- self$packages
      return(private$containr_packages)
    },

    set_python = function(include_python){
      self$include_python <- include_python
      private$containr_include_python <- self$include_python
      return(private$containr_include_python)
    },

    build_image = function(){

      if(isFALSE(fs::file_exists(private$containr_dockerfile))){
        cli::cli_abort("No Dockerfile could be found")
      }

      if(isTRUE(self$build)){
        processx::run(command = "docker",
          args = c("build",
            "--no-cache=true",
            "-t",
            private$containr_image_name,
            "-f",
            private$containr_dockerfile,
            "."),
          echo_cmd = TRUE,
          echo = TRUE,
          spinner = TRUE)

        cli::cli_alert_success("Success!")
      }
    },

    print = function(){
      cat(
        paste(
          " |",cli::col_green("Image Name:"), private$containr_image_name, "\n",
          "|", cli::col_green("Dockfile:"), private$containr_dockerfile, "\n",
          "|", cli::col_green("Rocker Image:"), private$containr_rocker_image, "\n",
          "|", cli::col_green("Self Image:"), self$rocker_image, "\n",
          "|", cli::col_green("Self tag:"), self$tag, "\n",
          "|", cli::col_green("Packages:"), private$containr_packages, "\n",
          "|", cli::col_green("Python:"), private$containr_include_python, "\n",
          "|", cli::col_green("Built:"), self$build, "\n",
          "\n")
      )
      invisible(self)
    }
  ),


  # Private List ------------------------------------------------------------


  private = list(

    containr_image_name = NULL,
    containr_dockerfile = NULL,
    containr_rocker_image = NULL,
    containr_packages = NULL,
    containr_include_python = FALSE,
    python_file = NULL,
    python_env = NULL,
    additional = NULL,
    #bash_file  = NULL,
    file = NULL,

    create_dockerfile = function(){

      cli::cli_h1("Writing Dockerfile")
      # File overwrite warning
      if(fs::file_exists(private$containr_dockerfile)) {
        cli::cli_alert_warning("{.emph dockerfile} will be overwritten.",
          class = cli::cli_div(theme = list(span.emph = list(color = "orange"))))
      }

      # Flag for python inclusion
      # if(isTRUE(private$containr_include_python)) {
      #   cli::cli_alert_info("Install Python: {.val {private$containr_include_python}}")
      # } else if(isFALSE(private$containr_include_python)) {
      #   cli::cli_alert_info("Install Python: {.val {private$containr_include_python}}")
      # }

      # command layout for dockerfile
      docker_cmds <- paste(c(
        paste("FROM", private$containr_rocker_image),
        # TODO paste("LABEL", "label_name"),
        paste(""),
        if(isTRUE(private$containr_include_python)) {
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

      writeLines(docker_cmds, con = private$containr_dockerfile, sep = "")

      if(isTRUE(file.exists(private$containr_dockerfile))){
        cli::cli_alert_success("Dockerfile saved to: {.path {private$containr_dockerfile}}")
      }
    },

    setup_packages = function(){
      packages <- private$containr_packages
      bash_file <- "docker/scripts/install_libs_local.sh"

      if(packages %in% "none"){
        private$reset_dir()
        private$create_directories()
        #return(bash_file)
      } else if(packages %in% c("loaded", "installed")){

        cli::cli_h1("Setting Package Preferences")
        private$create_directories()
        package_df <- sessioninfo::package_info(pkgs = packages)

        packs <- dplyr::tibble(Package = package_df$package,
          Version = package_df$ondiskversion,
          Source = package_df$source)

        # CRAN Packages

        CRAN <- packs |>
          dplyr::filter(Source == stringr::str_match_all(Source,
            pattern = "CRAN \\(R \\d.\\d.\\d\\)"))

        # trailing slash in bash file so hack fix
        cran_packs_first <- paste0("\t", CRAN$Package[0 -length(CRAN$Package)], " \\")
        cran_packs_last <- paste0("\t", CRAN$Package[length(CRAN$Package) - 0], "")


        # GitHub Packages
        Other <- packs |>
          dplyr::filter(Source != stringr::str_match_all(Source,
            pattern = "CRAN \\(R \\d.\\d.\\d\\)")) |>
          dplyr::filter(Source != "local") |>
          dplyr::mutate(Source = stringr::str_extract_all(Source,
            pattern = "(.*?)@",
            simplify = TRUE)) |>
          dplyr::mutate(Source = stringr::str_extract_all(Source,
            pattern = "\\(([\\s\\S]*)$",
            simplify = TRUE)) |>
          dplyr::mutate(Source = stringr::str_remove_all(Source,
            pattern = "\\((.*?)")) |>
          dplyr::mutate(Source = stringr::str_remove_all(Source,
            pattern = "@"))

        not_installed <- Other |>
          dplyr::filter(Source == "")

        Github <- Other |>
          dplyr::filter(Source != "") |>
          dplyr::select(Source) |>
          purrr::pluck("Source")

        if(nrow(not_installed) > 0){
          cli::cli_alert_warning("Not Installing { {nrow(not_installed)}} Package{?s}")
          cli::cli_alert_warning("{.val {not_installed$Package}}")
        }

        if(length(Github) > 0){
          cli::cli_alert_info("Including { {nrow(Github)}} GitHub Package{?s}")
          cli::cli_alert_info("{.val {Github}}")
        }

        if(nrow(CRAN) > 0){
          cli::cli_alert_info("Including { {nrow(CRAN)}} GitHub Package{?s}")
          cli::cli_alert_info("{.val {CRAN$Package}}")
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
          if(length(Github) > 0) {
            c(paste("Rscript -e", paste("\"remotes::install_github(\'", Github , "\')\" ", sep = "") ," "))
          },
          paste(""),
          paste("install2.r --error --skipinstalled -n \"$NCPUS\" \\"),
          paste(cran_packs_first),
          paste(cran_packs_last),
          paste("")
        ),
          collapse = "\n")

        writeLines(installR_script, con = bash_file, sep = "")

        if(length(readLines(bash_file)) > 0){
          fs::dir_tree("docker")
          cli::cli_alert_success("Package Installs file updated: {.path {bash_file}}")
        }
      }
    },

    # Setup the docker files and folders
    create_directories = function(){

      private$python_file = "install_python.sh"
      private$python_env = "install_pyenv.sh"
      private$additional = "install_additional.sh"

      #cli::cli_h2("Creating Docker Folders")

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
        private$get_extdata(private$python_file)
      }

      if(!fs::file_exists("docker/scripts/install_pyenv.sh")) {
        private$get_extdata(private$python_env)
      }

      if(!fs::file_exists("docker/scripts/install_additional.sh")) {
        private$get_extdata(private$additional)
      }

    },

    get_extdata = function(file = NA){
      if(fs::dir_exists("docker/scripts")){
        x <- system.file("extdata",
          file,
          package = "ContainR",
          mustWork = TRUE)
        fs::file_copy(x,
          "docker/scripts",
          overwrite = TRUE)

      }
    },

    reset_dir = function(){
      cli::cli_h2("Resetting Docker Folder")
      if(fs::dir_exists("docker")) {
        fs::dir_delete("docker")
      }
    }

  )
)




