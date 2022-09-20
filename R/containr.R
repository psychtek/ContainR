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
    #' @param name
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
        self$name <- basename(rstudioapi::getActiveProject())
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
    # super$docker_images()
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
    }
  )

)



# Dockerfile Class --------------------------------------------------------

dockerfile <- R6::R6Class(
  classname = "dockerfile",
  inherit = containr,

  # Public List -------------------------------------------------------------

  public = list(

    #' @field dockerfile Location of the dockerfile. Is set to `docker/Dockerfile/` directory.
    dockerfile = NULL,

    #' @field rocker_image
    rocker_image = NULL,

    #' @field packages
    packages = NULL,

    #' @field include_pythong
    include_python = NULL,

    #' @field build
    build = NULL,

    #' Docker build
    #'
    #' This function creates a dockerfile based off the rocker version image stacks and reads
    #' from session information to install `loaded `, `installed` \(or `none`\) R libraries. The `install2.r` handles
    #' additional package installs and will skip already installed packages on the rocker stack image. The final `Dockerfile`
    #' can be used to build a container image with your development environment.
    #' Builds a docker container from a `Dockerfile`. The function will look in the `inst/dockerfile/` directory
    #' for an existing Dockerfile.
    #'
    #'
    #' @param name Name of the saved image. If no name is supplied then it will use the active Rstudio project name `rstudioapi::getActiveProject()`.
    #'
    #' @param rocker_image The base Rocker image: \url{https://rocker-project.org/images/} to build upon. Defau;t
    #' is to use the `rstudio` build into `packages` based on preferences. The list of current Rockered image
    #' stacks can be view in the data [data_rocker_table].
    #'
    #' @param tag A character string of the require version. If no tag is supplied then the function will default to `latest`.
    #'
    #' @param dockerfile Default location is in the `docker/` folder. Build will save the final `Dockerfile`
    #' to this location at the root project directory. You can also add you're own Dockerfile, however, this package was primarily designed to launch
    #' an active project into a **Rstudio** container from one of the Rocker images: \url{https://rocker-project.org/images/}.
    #'
    #' @param packages Provide a string of either `loaded`, `installed` or `none`. `Loaded` will included packages that are currently
    #' loaded in your active project session. The `installed` string will included everything inside you local default R library. Already
    #' installed packages are skipped when the [build_image] command is run. If you have a large package library it is
    #' recommended to only install `loaded` as you develop your workflow.
    #'
    #' @param include_python Flag to install python using the rocker scripts \url{https://github.com/rocker-org/rocker-versioned2} which have had minor modifications. Future updates
    #' will see this streamlined. `Pandas` and `numpy` modules are also installed if this flag is set to `TRUE`.
    #'
    #' @param build This is the flag set to `FALSE` for when the settings are in place for the Docker process to build. View the
    #' current settings with the `print()` method and then `build_image(TRUE)` to start the process. This can take some time so grab a coffee.
    initialize = function(name = NULL, rocker_image = NA, tag = NULL, dockerfile = NA,
      packages = NA, include_python = FALSE, build = FALSE){

      # These are initialized in order from top to bottom
      self$set_image_name(name)
      super$set_tag(tag)
      self$set_rocker_image(rocker_image)
      self$set_dockerfile(dockerfile)
      self$set_packages(packages)
      self$set_python(include_python)
      private$create_directories()
      private$setup_packages()
      private$create_dockerfile()
      self$build = build
      super$initialize(image = private$containr_image_name, tag = self$tag)
    },


    #' @description
    #' Set the name of the image.
    #' @param name Set or get the name of the image for build.
    set_image_name = function(name){
      private$containr_image_name <- super$set_name(name)
      return(private$containr_image_name)
    },

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project.
    #' @param rocker_image Change containr name
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

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project.
    #' @param dockerfile Change containr name
    set_dockerfile = function(dockerfile){
      if(!is.character(dockerfile)) stop("dockerfile must be a character")
      self$dockerfile <- dockerfile
      private$containr_dockerfile <- self$dockerfile
      return(private$containr_dockerfile)
    },

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project.
    #' @param packages Change containr name
    set_packages = function(packages){
      self$packages <- match.arg(packages, c("loaded", "none", "installed"))
      private$containr_packages <- self$packages
      return(private$containr_packages)
    },

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project.
    #' @param include_python Change containr name
    set_python = function(include_python){
      self$include_python <- include_python
      private$containr_include_python <- self$include_python
      return(private$containr_include_python)
    },

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project.
    #' @param build Change containr name
    build_image = function(build){

      if(isFALSE(fs::file_exists(private$containr_dockerfile))){
        cli::cli_abort("No Dockerfile could be found")
      }

      self$build <- build
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
      self$build <- TRUE
    },

    print = function(){
      cli::cli_h1("Dockerfile Build Settings")
      cat(
        paste(
          " |",cli::style_bold("Image Name:"), private$containr_image_name, "\n",
          "|", cli::style_bold("Dockfile:"), private$containr_dockerfile, "\n",
          "|", cli::style_bold("Using Image:"), private$containr_rocker_image, "\n",
          #"|", cli::style_bold("Self Image:"), self$rocker_image, "\n",
          #"|", cli::style_bold("Self tag:"), self$tag, "\n",
          "|", cli::style_bold("Packages:"), private$containr_packages, "\n",
          "|", cli::style_bold("Python:"), private$containr_include_python, "\n",
          "|", cli::style_bold("Built:"), ifelse(isTRUE(self$build),
            cli::col_green(self$build),
            cli::col_yellow(self$build)), "\n",
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




