#' @include docker.R
#' @title containr Class Object
#'
#' @description
#' A R6 class object of `containr` that sets up a object to launch a defined rocker
#' containr in a browser. This function creates a dockerfile based off the rocker version image stacks and reads
#' from session information to install `loaded `, `installed` \(or `none`\) R libraries. The internal `install2.r` handles
#' additional package installs and will skip already installed packages on the rocker stack image. The final `Dockerfile`
#' can be used to build a container image with your development environment.
#'
#' @section Rockered:
#' Creates a new `containr` object based on the \url{https://rocker-project.org/images/}.
#'
#' @section Setup:
#' To get started, load a project and load any packages used throughout your scripts.
#' ```R
#' cont <- containr$new(image = "rstudio", name = "testing", tag = "latest",
#'  packages = "loaded", include_python = FALSE, DISABLE_AUTH = TRUE,
#'  use_local = TRUE)
#' ```
#' Then use the `build_image(TRUE)` to flag the build process
#' ```R
#'  cont$build_image(TRUE)
#'  cont$print()
#' ```
#' Start then launch the container session in a new browser session
#' ```R
#' cont$start() # Start the container
#' cont$proc() # View the processes
#' cont$launch() # launch session
#' cont$stop() # stop the container
#' ```
#'
#'
#' @export
containr <- R6::R6Class("containr",
  cloneable = FALSE,
  lock_class = TRUE,

  # Public Functions --------------------------------------------------------

  public = list(

    #' @field image Name of the image from either the [data_rocker_table] or in docker register
    image = NULL,

    #' @field name Name of the containr when launched. Defaults to the active Rstudio Project.
    name = NULL,

    #' @field tag Tag of image defaults to `latest` if nothing set.
    tag = NULL,

    #' @field packages Provide a string of either `loaded`, `installed` or `none`.
    packages = NULL,

    #' @field dockerfile Location of the dockerfile. Is set to `docker/Dockerfile/` directory.
    dockerfile = NULL,

    #' @field include_python Flag to install python using the rocker scripts
    include_python = NULL,

    #' @field build Default is `FALSE`. Set to `TRUE` with the `build_image` fun.
    build = NULL,

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
    #' @param name The name of the containr image to build for the docker process to launch. If none is applied then
    #' it will default to the working Rstudio project directory
    #'
    #' @param tag A character string of the require version. If no tag is supplied then the function will default to `latest`.
    #'
    #' @param packages Provide a string of either `loaded`, `installed` or `none`. `Loaded` will included packages that are currently
    #' loaded in your active project session. The `installed` string will included everything inside you local default R library. Already
    #' installed packages are skipped when the `build_image` command is run. If you have a large package library it is
    #' recommended to only install `loaded` as you develop your workflow.
    #'
    #' @param dockerfile Default location is in the `docker/` folder. Build will save the final `Dockerfile`
    #' to this location at the root project directory. You can also add you're own Dockerfile, however, this package was primarily designed to launch
    #' an active project into a **Rstudio** container from one of the Rocker images: \url{https://rocker-project.org/images/}.
    #'
    #' @param include_python Flag to install python using the rocker scripts \url{https://github.com/rocker-org/rocker-versioned2} which have had minor modifications. Future updates
    #' will see this streamlined. `Pandas` and `numpy` modules are also installed if this flag is set to `TRUE`.
    #'
    #' @param DISABLE_AUTH Bypass authentication and show the R session.
    #' Defaults to TRUE and will login when the `launch()` function is invoked.
    #'
    #' @param use_local If you want to port your local Rstudio session into the containr.
    #' Defaults to `TRUE`.
    #'
    #' @param build This is the flag set to `FALSE` for when the settings are in place for the Docker process to build. View the
    #' current settings with the `print()` method and then `build_image(TRUE)` to start the process. This can take some time so grab a coffee.
    initialize = function(image = NULL, name = NULL, tag = NULL, packages = NULL, dockerfile = NULL, include_python = NULL,
      DISABLE_AUTH = TRUE, use_local = TRUE, build = FALSE){

      self$set_name(name)
      self$set_tag(tag)
      self$set_image(image)
      self$set_local(use_local)
      self$set_login(DISABLE_AUTH)
      self$set_python(include_python)
      self$set_dockerfile(dockerfile)
      self$set_packages(packages)
      private$setup_packages()
      private$create_dockerfile()
      self$build = build
    },

    #' @description
    #' Set or change the base image name to build off. Additional flags can be set to
    #' to include python with `include_python()` function/
    #'
    #' @param image Set base image name using `c("rstudio", "tidyverse", "verse", "geospatial", "binder")`.
    set_image = function(image){
      rocker_image <- match.arg(image, c("rstudio", "tidyverse", "verse", "geospatial", "binder"))

      self$image <- switch(rocker_image,
        rstudio = paste0("rocker/rstudio:", self$tag),
        tidyverse = paste0("rocker/tidyverse:", self$tag),
        verse = paste0("rocker/verse:", self$tag),
        geospatial = paste0("rocker/geospatial:", self$tag),
        binder = paste0("rocker/binder:", self$tag))

      private$containr_image <- self$image
      return(private$containr_image)
    },

    #' @description
    #' Change the containr name when launched. Defaults to active Rstudio project directory.
    #'
    #' @param name Character string of required name
    set_name = function(name){
      if(is.null(name)){
        self$name <- tolower(basename(getwd()))
      } else {
        self$name <- tolower(name)
      }
      private$containr_name <- self$name
      return(private$containr_name)
    },

    #' @description
    #' Set the tag of the image. This defaults to *latest* if left flagged as
    #' `NULL`.
    #'
    #' @param tag Character string to change tag name to specify the version of the base image. Setting `local_version` sets the
    #' tag to use the local R version in the session. Or supply a specific R version number `R (>= 4.0.0)`.
    set_tag = function(tag){
      if(isTRUE(is.null(tag))){
        self$tag <- "latest"
      } else if(tag == "local_version"){
        self$tag <- getRversion()
      } else {
        self$tag <- tolower(tag)
      }
      return(self$tag)
    },

    #' @description
    #' This is used to set the pointer to where the `Dockerfile` is located.
    #' @param dockerfile Character string to `Dockerfile`. Defaults to the `docker/Dockerfile`
    #' directory and recommened leaving as is.
    set_dockerfile = function(dockerfile){
      if(is.null(dockerfile)) {dockerfile <- "docker/Dockerfile"}
      self$dockerfile <- dockerfile
      private$containr_dockerfile <- self$dockerfile
      return(private$containr_dockerfile)
    },

    #' @description
    #' Setting for which packages to append to the build install script.
    #'
    #' @param packages A character string of `c("loaded", "none", "installed")`. Where `loaded` installs attached
    #' packages or what is in the `DESCRIPTION` file. The `installed` string will install *all the things* - this will
    #' need to be used with care if you have a large R library. The `none` character string doesnt install any packages.
    set_packages = function(packages){
      self$packages <- match.arg(packages, c("loaded", "none", "installed"))
      private$containr_packages <- self$packages
      return(private$containr_packages)
    },

    #' @description
    #' If to install python on the image. These make use of the rocker scripts
    #' by using the raw scripts from rocker-versioned2.
    #'
    #' @param include_python Flag `TRUE` or `FALSE` if to include python
    set_python = function(include_python){
      self$include_python <- include_python
      private$containr_include_python <- self$include_python
      return(private$containr_include_python)
    },

    #' @description
    #' Flag to set the Rstudio login. When the `launch()` fun is run the browser will opeen and
    #' this setting bypasses the need for a password.
    #'
    #' @param DISABLE_AUTH Set to enable the Rstudio Login. Set to `TRUE` until secure
    #' env is setup.
    set_login = function(DISABLE_AUTH){
      private$DISABLE_AUTH <-  DISABLE_AUTH
    },

    #' @description
    #' Port local config and theme settings. Option setting to port the local
    #' config and panel settings into the container session.
    #' @param use_local Flag `TRUE` to port local settings or `FALSE` for a clean session.
    set_local = function(use_local){
      private$use_local <-  use_local
    },

    #' @description
    #' Start the predefined containr as set by the `name` argument.
    start = function(){
      private$containr_start()
      self$proc()
    },

    #' @description
    #' Stop containr object
    stop = function(){
      private$containr_stop()
      self$proc()
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
    #' Starts the build process and updates the `print` status.
    #' @param build Set flag for build
    build_image = function(build){

      if(isFALSE(fs::file_exists(private$containr_dockerfile))){
        cli::cli_abort("No Dockerfile could be found")
      }
      # TODO set for false builds
      self$build <- build
      if(isTRUE(self$build)){
        processx::run(command = "docker",
          args = c("build",
            "--no-cache=true",
            "-t",
            private$containr_name,
            "-f",
            private$containr_dockerfile,
            "."),
          echo_cmd = TRUE,
          echo = TRUE,
          spinner = TRUE)

        cli::cli_alert_success("Success!")
      }
      self$build <- TRUE
      private$built_image <- private$containr_name
    },

    #' @description
    #' Displays the process information of the active containr.
    proc = function(){
      cli::cli_h1("<Process>")
      cat(paste(
        "<", cli::style_bold("ID:"),
        if(isTRUE(self$status() == "Running")) {
          docker_containers()$ID
        } else {
          cli::col_yellow("Not Started")
        },
        "|", cli::style_bold("Name:"),
        if(isTRUE(self$status() == "Running")) {
          docker_containers()$Names
        } else {
          cli::col_yellow("Not Started")
        },
        "|", cli::style_bold("Ports:"),
        if(isTRUE(self$status() == "Running")) {
          docker_containers()$Ports
        } else {
          cli::col_yellow("Not Started")
        },
        "|", cli::style_bold("Pid:"),
        if(isTRUE(self$status() == "Running")) {
          gsub("[^[:alnum:] ]", "", private$containr_pid)
        } else {
          cli::col_yellow("Not Started")
        },
        "|", cli::style_bold("Status:"),
        if(isTRUE(self$status() == "Running")) {
          cli::col_green(docker_containers()$Status)
        } else {
          cli::col_yellow("Not Started")
        },
        ">"))
    },

    #' @description
    #' `containr$info()` shows some information about the
    #' process on the screen, whether it is running and it's process id, etc.
    print = function(){
      cli::cli_h1("<ContainR>")

      cat(
        " |", cli::style_bold("Project:"), private$containr_name, "\n",
        paste("|", cli::style_bold("Status:"),
          if(isTRUE(self$status() == "Running")) {
            cli::col_green("Running")
          } else if(isFALSE(self$status() == "Not Running")){
            cli::col_red("Not Running")
          } else {
            cli::col_yellow("Not Started")
          }, "\n",
          "|", cli::style_bold("Base Image:"), private$containr_image, "\n",
          "|", cli::style_bold("Packages:"), private$containr_packages, "\n",
          "|", cli::style_bold("Python:"), private$containr_include_python, "\n",
          "|", cli::style_bold("Built:"),
          ifelse(isTRUE(self$build),
            cli::col_green(self$build),
            cli::col_yellow(self$build)), "\n",
          "|", cli::style_bold("Auth:"),
          if(isTRUE(private$DISABLE_AUTH)) {
            cli::col_green(private$DISABLE_AUTH)
          } else {
            cli::col_yellow(private$DISABLE_AUTH)
          }, "\n",
          "|", cli::style_bold("Local:"),
          if(isTRUE(private$use_local)) {
            cli::col_green(private$use_local)
          } else {
            cli::col_yellow(private$use_local)
          }, "\n"),
        "|", cli::style_bold("Dockerfile:"), cli::style_hyperlink("docker/Dockerfile", fs::path_real(private$containr_dockerfile)), "\n",
        c("|", cli::style_bold("Run Command:"), "\n",
          cli::style_italic(private$setup_command())), "\n")

      invisible(self)
    },

    #' @description
    #' Returns a character string of the run command.
    get_cmd = function(){
      cmd <- private$setup_command()
      return(cmd)
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
    containr_dockerfile = NULL,
    containr_packages = NULL,
    containr_include_python = FALSE,
    python_file = NULL,
    python_env = NULL,
    built_image = NULL,

    # Build Process Functions -------------------------------------------------

    create_dockerfile = function(){

      cli::cli_h2("Creating Dockerfile")

      # command layout for dockerfile
      docker_cmds <- paste(c(
        paste("FROM", private$containr_image),
        # TODO paste("LABEL", "label_name"),
        paste(""),
        if(isTRUE(private$containr_include_python)) {
          c(
            paste("COPY", private$python_file, "/tmp/"),
            paste(""),
            paste("COPY", private$python_env, "/tmp/"),
            paste(""),
            paste("RUN", "chmod +x /tmp/install_python.sh"),
            paste(""),
            paste("RUN", "chmod +x /tmp/install_pyenv.sh"),
            paste(""),
            paste("RUN", "/tmp/install_python.sh"),
            paste(""),
            paste("RUN", "/tmp/install_pyenv.sh"),
            paste(""),
            paste("RUN", "R -e \"reticulate::py_config()\""),
            paste("")
          )
        },
        paste(""),
        if(private$containr_packages %in% c("loaded", "installed")){
          c(paste("COPY /docker/scripts/install_additional.sh /tmp/"),
            paste(""),
            paste("RUN chmod +x /tmp/install_additional.sh"),
            paste(""),
            paste("RUN", "/tmp/install_additional.sh"))
        },
        paste("")
      ),
        collapse = "\n")

      writeLines(docker_cmds, con = private$containr_dockerfile, sep = "")

      if(isTRUE(file.exists(private$containr_dockerfile))){
        cli::cli_alert_success("Dockerfile saved to: {.path {private$containr_dockerfile}}")
      }
    },

    setup_packages = function(){
      packages <- private$containr_packages
      bash_file <- "docker/scripts/install_additional.sh"

      if(packages %in% "none"){
        private$reset_dir()
        private$create_directories()
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
          dplyr::filter(!Package %in% CRAN$Package) |>
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
          dplyr::filter(Source == "") |>
          dplyr::select(-Source)

        Github <- Other |>
          dplyr::filter(Source != "")
        #dplyr::select(Source)
        #purrr::pluck("Source")

        if(nrow(not_installed) > 0){
          cli::cli_alert_warning("{ {nrow(not_installed)}} Unknown Source Package{?s}")
          cli::cli_alert_warning("{.val {not_installed$Package}}")
        }

        if(nrow(Github) > 0){
          cli::cli_alert_info("Including { {nrow(Github)}} GitHub Package{?s}")
          cli::cli_alert_info("{.val {Github$Package}}")
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
          paste("function apt_install() {"),
          paste("\tif ! dpkg -s \"$@\" >/dev/null 2>&1; then"),
          paste("\t\tif [ \"$(find /var/lib/apt/lists/* | wc -l)\" = \"0\" ]; then"),
          paste("\t\t\tapt-get update"),
          paste("\t\tfi"),
          paste("\t\tapt-get install -y --no-install-recommends \"$@\""),
          paste("\tfi"),
          paste("}"),
          paste(""),
          paste("UBUNTU_VERSION=$(lsb_release -sc)"),
          paste("CRAN_SOURCE=${CRAN/\"__linux__/$UBUNTU_VERSION/\"/\"\"}"),
          paste(""),
          paste("if [ \"$ARCH\" = \"aarch64\" ]; then"),
          paste("\tCRAN=$CRAN_SOURCE"),
          paste("fi"),
          paste(""),
          if(length(Github) > 0) {
            c(paste("Rscript -e \"install.packages(c('remotes'), repos='${CRAN_SOURCE}')\""),
              paste("\n echo -e 'Installing GitHub Packages'\n "),
              paste("Rscript -e", paste("\"remotes::install_github(\'",  Github$Source , "\')\" ", sep = "") ," "))
          },
          paste(""),
          paste("install2.r --error --skipinstalled -n \"$NCPUS\" \\"),
          paste(cran_packs_first),
          paste(cran_packs_last),
          paste(""),
          paste("rm -rf /var/lib/apt/lists/*"),
          paste("rm -rf /tmp/downloaded_packages"),
          paste("strip /usr/local/lib/R/site-library/*/libs/*.so"),
          paste(""),
          paste("echo -e \"\nFinished intalling additional packages!\""),
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

    create_directories = function(){

      if(!fs::dir_exists("docker")) {
        fs::dir_create("docker")
      }

      if(private$containr_packages %in% c("loaded", "installed")){
        if(!fs::dir_exists("docker/scripts")) {
          fs::dir_create("docker/scripts")
        }

        if(!fs::file_exists("docker/scripts/install_additional.sh")) {
          fs::file_create("docker/scripts/install_additional.sh")
        }
      }

      if(!fs::file_exists("docker/Dockerfile")) {
        fs::file_create("docker/Dockerfile")
      }

      if(isTRUE(private$containr_include_python)) {
        fs::dir_create("docker/scripts")
        inst_python <- "/install_python.sh"
        tmp_dir <- tempdir()
        inst_python_dest <- paste0(tmp_dir, inst_python)
        curl::curl_download(url = "https://raw.githubusercontent.com/rocker-org/rocker-versioned2/master/scripts/install_python.sh",
          destfile = inst_python_dest,
          mode = "wb",
          quiet = FALSE)
        file.copy(inst_python_dest, "docker/scripts")
        private$python_file = "docker/scripts/install_python.sh"
      }

      if(isTRUE(private$containr_include_python)) {
        fs::dir_create("docker/scripts")
        inst_pyenv <- "/install_pyenv.sh"
        tmp_dir <- tempdir()
        inst_pyenv_dest <- paste0(tmp_dir, inst_pyenv)
        curl::curl_download(url = "https://raw.githubusercontent.com/rocker-org/rocker-versioned2/master/scripts/install_pyenv.sh",
          destfile = inst_pyenv_dest,
          mode = "wb",
          quiet = FALSE)
        file.copy(inst_pyenv_dest, "docker/scripts")
        private$python_env = "docker/scripts/install_pyenv.sh"
      }

    },

    reset_dir = function(){
      cli::cli_h2("Resetting Docker Folder")
      if(fs::dir_exists("docker")) {
        fs::dir_delete("docker")
      }
    },

    # Launch Process Functions ------------------------------------------------

    setup_config = function(){
      proj_name <- private$containr_name
      private$containr_config <-  create_config_file(proj_name)
    },

    # Setup the docker command string
    setup_command = function(){

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
      image_name <- c(private$built_image)

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

      # private$containr_image <- processx::run(command = "docker",
      #   args = c("inspect",
      #     "--format",
      #     "'{{.Config.Image}}'",
      #     private$containr_name),
      #   error_on_status = TRUE)$stdout

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




