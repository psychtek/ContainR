#' @title containr Class Object
#'
#' @description
#' A R6 class object of `containr`
#'
#' @details
#' The general concept of this package is to aid in containerization of a workflow.
#' A workflow could be a simple analysis script to a complex pipeline however, capturing
#' the system ecosystem with all the additional libraries and packages are often an afterthought.
#' `ContainR` package is an attempt to be an easier implementation of Rstudio containers ported
#' from the cool developers at \url{https://rocker-project.org/images/}. This package is both a wrapper
#' with Docker support to build an image and clone a workflow and packages into a Rstudio image.
#'
#' @section Rockered:
#' Creates a new `containr` object based on one of
#' the \url{https://rocker-project.org/images/}.
#' The `DISABLE_AUTH=TRUE`has been set as default so each session logs in direct without needing
#' a password.
#'
#'
#' @section Setup:
#' To get started, load a RStudio project and attached any libraries. The default base image *rstudio*
#' will load the `rocker/rstudio:latest` image from Dockerhub. Additional script preferences can be called
#' up by using the relevant `include_`. This allows for more flexible container setup depending on how you want
#' to containerize your project.
#' ```R
#' cont <- containr$new(image = "rstudio", name = "projectname", tag = "latest",
#'  packages = "loaded", include_py = TRUE)
#' ```
#' Then use the `build_image(TRUE)` to flag the build process. Depending on how many dependencies
#' are needed or packages to install, this may take some time.
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
#' @export
containr <- R6::R6Class("containr",
  cloneable = FALSE,
  lock_class = TRUE,
  lock_objects = FALSE,

  public = list(

    #' @description
    #' Initialise and build a new `containr` image from a RStudio Rocker image.
    #'
    #' @return R6 object with configured build settings as meta data and build image.
    #'
    #' @param image Repository Name of rocker image from [data_rocker_table]. Defaults to
    #' the `rstudio` stack `rocker/rstudio:latest`. For more information about each stack
    #' visit \url{https://rocker-project.org/images/}. If you ran `build_image(TRUE)` then
    #' this argument updates the string of the *name* of the build. Built container images can be
    #' view by running the `docker_images()` function.
    #'
    #' @param name The name of the containr image to build for the Docker process to launch. If none is applied then
    #' it will default to the working Rstudio project directory name.
    #'
    #' @param tag A character string of the require version. If no tag is supplied then the function will default to `latest`.
    #'
    #' @param packages Provide a string of either `loaded`, `installed` or `none`. `Loaded` will included packages that are currently
    #' loaded in your active project session. The `installed` string will included everything inside you local default R library.
    #' Note that already installed packages on the image are skipped when the `build_image` command is run. If you have a large package library it is
    #' recommended to only install `loaded` as you develop your workflow.
    #'
    #' @param dockerfile A Dockerfile recipe to create a Docker image. Default location is in the `containr/` folder. Build will save the final `Dockerfile`
    #' to this location at the root project directory. You can also add you're own Dockerfile, however, this package was primarily designed to launch
    #' an active project into a **Rstudio** container from one of the Rocker images: \url{https://rocker-project.org/images/}. Suggest
    #' leaving blank for defaults.
    #'
    #' @param copy This will run the CMD Check function and create a tar file and update the Dockerfile to
    #' unpack this into the directory `/home/rstudio/` on the Rstudio image.
    #'
    #' @param preview Preview the base image in a Rstudio session in a browser with cloned local settings like theme, config and environment.
    #'
    #' @param build Status of build
    #'
    #' @param build_image This is the flag set to `FALSE` for when the settings are in place for the Docker process to build. View the
    #' current settings with the `print()` method and then `build_image(TRUE)` to start the process. This can take some time so grab a coffee!
    #'
    #' @param include_py Install python
    #'
    #' @param include_pyenv Include python env
    #'
    #' @param include_tensor Install tensor `Untested`
    #'
    #' @param include_geo Install geo `Untested`
    #'
    #' @param include_quarto Install quarto
    #'
    #' @param include_tex Install text
    #'
    #' @param include_julia Install Julia `Untested`
    #'
    #' @param include_jupyter Add Juypter notebook support `Untested`
    #'
    #' @param include_tidy Install tidy
    #'
    #' @param include_verse Install verse
    #'
    #' @param include_pandoc Install pandoc
    #'
    #' @param include_shiny Install Shiny server`Untested`
    #'
    #' @export
    #'
    initialize = function(image = "rstudio", name = NULL, tag = NULL, packages = "none", dockerfile = NULL,
      copy = FALSE, preview = TRUE, build = FALSE, include_py = FALSE, include_pyenv = FALSE,
      include_tensor = FALSE, include_geo = FALSE, include_quarto = FALSE, include_tex = FALSE, include_julia = FALSE,
      include_jupyter = FALSE, include_tidy = FALSE, include_verse = FALSE, include_pandoc = FALSE, include_shiny = FALSE){

      private$setup_system()

      self$set_name(name)
      self$set_tag(tag)
      self$set_image(image)
      self$set_copy(copy)
      self$set_preview(preview)

      self$include_py = include_py
      self$include_pyenv = include_pyenv
      self$include_tensor = include_tensor
      self$include_geo = include_geo
      self$include_quarto = include_quarto
      self$include_tex = include_tex
      self$include_julia = include_julia
      self$include_jupyter = include_jupyter
      self$include_tidy = include_tidy
      self$include_verse = include_verse
      self$include_pandoc = include_pandoc
      self$include_shiny = include_shiny

      self$set_flags()
      self$set_dockerfile(dockerfile)
      self$set_packages(packages)
      private$setup_packages()
      private$create_dockerfile()
      if (build) self$build()

    },

    #' @description Sets the include flags for additional scripts to run
    set_flags = function(){
      private$inc_py     = self$include_py
      private$inc_pyenv  = self$include_pyenv
      private$inc_tensor = self$include_tensor
      private$inc_geo    = self$include_geo
      private$inc_quarto = self$include_quarto
      private$inc_tex    = self$include_tex
      private$inc_julia  = self$include_julia
      private$inc_jupyter = self$include_jupyter
      private$inc_tidy   = self$include_tidy
      private$inc_verse  = self$include_verse
      private$inc_pandoc = self$include_pandoc
      private$inc_shiny = self$include_shiny
      # Switch flags as the script already references other
      if(isTRUE(private$inc_jupyter)){
        private$inc_py <- FALSE
      }
      if(isTRUE(private$inc_shiny)){
        private$inc_pandoc <- FALSE
      }
    },

    #' @description
    #' Name of the built image and used as container name. Defaults to the active project directory name.
    #'
    #' @param name Character string of required name
    set_name = function(name){
      if(is.null(name)){
        self$name <- tolower(basename(fs::path_wd()))
      } else {
        self$name <- tolower(name)
      }
      private$containr_name <- self$name
      return(private$containr_name)
    },

    #' @description
    #' Set or change the base image name to build off. Additional flags can be set to
    #' to include python with the relevant `include_` functions.
    #'
    #' @param image Set base image name using `c("rstudio", "tidyverse", "verse", "geospatial", "binder", "shiny)`.
    set_image = function(image){
      rocker_image <- match.arg(image, c("rstudio", "tidyverse", "verse", "geospatial", "binder", "shiny"))

      self$image <- switch(rocker_image,
        rstudio = paste0("rocker/rstudio:", self$tag),
        tidyverse = paste0("rocker/tidyverse:", self$tag),
        verse = paste0("rocker/verse:", self$tag),
        geospatial = paste0("rocker/geospatial:", self$tag),
        binder = paste0("rocker/binder:", self$tag),
        shiny = paste0("rocker/shiny-verse:", self$tag))

      private$containr_image <- self$image
      return(private$containr_image)
    },

    #' @description
    #' Set the tag of the image. This defaults to *latest* if left flagged as
    #' `NULL`.
    #'
    #' @param tag Character string to change tag name to specify the version of the base image. Setting `local_version` sets the
    #' tag to use the local R version currently installed on the system. Or supply a specific R version number `R (>= 4.0.0)`.
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
    #'
    #' @param dockerfile Character string to `Dockerfile`. Defaults to the `containr/Dockerfile`
    #' directory and recommend leaving as is.
    set_dockerfile = function(dockerfile){
      if(is.null(dockerfile)) {dockerfile <- "containr/Dockerfile"}
      self$dockerfile <- dockerfile
      private$containr_dockerfile <- self$dockerfile
      return(private$containr_dockerfile)
    },

    #' @description
    #' Setting for which packages to append to the build install script.
    #'
    #' @param packages A character string of `c("loaded", "none", "installed")`. Where `loaded` installs attached
    #' packages. The `installed` string will install *all the things* - this will
    #' need to be used with care if you have a large R library. The `none` character string doesnt install any packages.
    set_packages = function(packages){
      self$packages <- match.arg(packages, c("loaded", "none", "installed"))
      private$containr_packages <- self$packages
      return(private$containr_packages)
    },

    #' @description
    #' Create a tar of the RStudio project to copy into an image.
    #'
    #' @param copy Set to `TRUE` will run the `CMD BUILD` and create a tar of the active package for unpacking
    #' in the Rstudio image.
    #'
    set_copy = function(copy){
      self$copy <- copy
      private$COPY <-  self$copy
      #private$LOCAL_DIR <- fs::path_wd()
      #private$LOCAL_DIR_NAME <- basename(fs::path_wd())
    },

    #' @description
    #' Port local config and theme settings. Optional setting to port the local
    #' config and panel settings into the container session. This is good for previewing an image
    #' before building your stack.
    #'
    #' @param preview Flag `TRUE` to port local settings or `FALSE` for a clean session.
    set_preview = function(preview){
      self$preview <- preview
      private$PREV <-  self$preview
    },

    #' @description
    #' Start the predefined containr as set by the `set_name` argument.
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
    #' Starts the process to build a Docker image.
    #'
    #' @param build Set flag for build
    build_image = function(build){

      if(isFALSE(fs::file_exists(private$containr_dockerfile))){
        cli::cli_abort("No Dockerfile could be found")
      }

      self$build <- build
      if(isTRUE(build)){
        tryCatch({
          result <- processx::run(command = "docker",
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
          if (result$status == 0) {
            cli::cli_alert_success("Success!")
            self$build <- TRUE
            private$containr_image <- private$containr_name
          } else {
            cli::cli_alert_error("build failed")
          }
        }, error = function(err) {
          cli::cli_alert_error(err)
        })
      }
    },

    #' @description
    #' Displays the process information of the active containr.
    proc = function(){
      cli::cli_h1("Process")
      cat(paste(
        "<", cli::style_bold("ID:"),
        if(isTRUE(self$status() == "Running")) {
          docker_containers()$ID
        } else {
          cli::col_yellow("Not Started")
        },
        "|", cli::style_bold("Name:"),
        if(isTRUE(self$status() == "Running")) {
          cli::col_green(docker_containers()$Names)
        } else {
          cli::col_yellow("Not Started")
        },
        "|", cli::style_bold("Ports:"),
        if(isTRUE(self$status() == "Running")) {
          cli::col_green(docker_containers()$Ports)
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
    #' `print()` displays information about the process on the console.
    #'
    print = function(){
      cli::cli_h1("ContainR")

      cli::cli_dl(c(
        "{.strong | Project}" = cli::col_green(private$containr_name),

        "{.strong | Status}" = if(isTRUE(self$status() == "Running")) {
          cli::col_green(self$status())
        } else if(isFALSE(self$status() == "Not Running")){
          cli::col_red(self$status())
        } else {
          cli::col_yellow("Not Started")
        },

        "{.strong | Base Image}" = private$containr_image,

        "{.strong | Packages}" =  private$containr_packages,

        "{.strong | Includes}" = if(length(private$included) > 0) {
          paste0(private$scripts, collapse = ", ")
        } else {
          "None"
        },

        "{.strong | Preview Mode}" = if(isTRUE(private$PREV)) {
          cli::col_green("On")
        } else {
          cli::col_yellow("Off - Using Built Image")
        },

        "{.strong | Copied}" = if(isTRUE(private$COPY)) {
          cli::col_green(private$COPY)
        } else {
          cli::col_yellow(private$COPY)
        },

        "{.strong | Build Status}" = ifelse(isTRUE(self$build),
          cli::col_green("Image Built: {.var {private$containr_created}}"),
          cli::col_yellow("Image Not Built")),

        "{.strong | Dockerfile}" = cli::style_hyperlink("{.file containr/Dockerfile}",
          fs::path_real(private$containr_dockerfile)),

        "{.strong Run Command}",

        "{.strong | -}" = private$setup_command(),

        "{.strong | Docker Layers}" = cli::style_italic(private$inst_dockerfile)
      ))

      invisible(self)
    },

    #' @description
    #' Returns a character string of the run command.
    get_cmd = function(){
      cmd <- private$setup_command()
      return(cmd)
    },

    #' @description
    #' WIP to store all settings and view as a list. Use `save_json()` to save the full list
    #' to `JSON` formatted file.
    view_meta = function(){
      meta <- list(
        containr = list(name = private$containr_name,
                        tag = self$tag),
        base_image = private$containr_image,
        image_created = private$containr_created,
        image_Id = private$containr_id,
        packages = list(unknown = list(Name = private$meta_unknown,
                                       Version = private$meta_unknown_vers),
                        github = list(Name = private$meta_github,
                                      Version = private$meta_github_vers),
                        cran = list(Name = private$meta_cran,
                                    Version = private$meta_cran_vers)),
        recipe = private$inst_dockerfile,
        tarfile = private$packaged_tar_info,
        log = private$out,
        system_info = list(System_Name = private$sys_name,
                   System = private$sys_machine,
                   System_Version = private$sys_version))
      return(meta)
    },

    #' @description
    #' This will take the `view_meta()` information and save it to the `containr` directory.
    save_json = function(){
      file_path <- paste0("containr/", basename(fs::path_wd()), ".json")
      jsonlite::toJSON(self$view_meta(), pretty = TRUE)
      #jsonlite::write_json(self$view_meta(), path = file_path)
    }

  ),

# Private  ----------------------------------------------------------------

  private = list(

    #proj_name = NULL,
    #proj_image = NULL,
    connected = FALSE,
    process = NULL,
    containr_pid = NULL,
    containr_name = NULL,
    containr_image = NULL,
    containr_config = NULL,
    containr_image_run_mode = NULL,
    containr_dockerfile = NULL,
    containr_packages = NULL,
    containr_created = NULL,
    containr_id = NULL,
    built_image = NULL,
    COPY = FALSE,
    PREV = TRUE,
    AUTH = TRUE,
    package = NULL,
    LOCAL_DIR = NULL,
    LOCAL_DIR_NAME = NULL,
    R_HOME_DIR = "/home/rstudio/",
    R_CONFIG_DIR = "/home/rstudio/.config/rstudio",
    R_ENV_DIR = "/home/rstudio/.Renviron",
    R_PROF = "/home/rstudio/.Rprofile",
    inst_dockerfile = NULL,
    inc_py = FALSE,
    inc_pyenv = FALSE,
    inc_tensor = FALSE,
    inc_geo = FALSE,
    inc_quarto = FALSE,
    inc_tex = FALSE,
    inc_julia = FALSE,
    inc_jupyter = FALSE,
    inc_tidy = FALSE,
    inc_verse = FALSE,
    inc_pandoc = FALSE,
    inc_shiny = FALSE,
    included = NULL,
    scripts = NULL,
    out = NULL,
    meta_unknown = NULL,
    meta_unknown_vers = NULL,
    meta_github = NULL,
    meta_github_vers = NULL,
    meta_cran = NULL,
    meta_cran_vers = NULL,
    packaged_tar_info = NULL,
    sys_name = NULL,
    sys_machine = NULL,
    sys_version = NULL,


# System Settings ---------------------------------------------------------

    setup_system = function(){
      private$sys_name <- Sys.info()[["sysname"]]
      private$sys_machine <- Sys.info()[["machine"]]
      private$sys_version <- Sys.info()[["version"]]
      private$sys_osname <- sessioninfo::os_name()
      private$LOCAL_DIR <- fs::path_wd()
      private$LOCAL_DIR_NAME <- basename(fs::path_wd()) # project directory
    },

    # Build Process Functions -------------------------------------------------

    create_dockerfile = function(){

      set_add_flags <- dplyr::tibble(
        include = c("python", "pyenv", "tensor", "geospatial", "quarto",
          "texlive", "julia", "jupyter", "tidyverse", "verse", "pandoc", "shiny"),
        flag = c(private$inc_py, private$inc_pyenv, private$inc_tensor, private$inc_geo,
          private$inc_quarto, private$inc_tex, private$inc_julia, private$inc_jupyter,
          private$inc_tidy, private$inc_verse, private$inc_pandoc, private$inc_shiny),
        string = c("install_python.sh", "install_pyenv.sh", "install_tensorflow.sh", "install_geospatial.sh",
          "install_quarto.sh", "install_texlive.sh", "install_julia.sh", "install_jupyter.sh",
          "install_tidyverse.sh", "install_verse.sh", "install_pandoc.sh", "install_shiny_server.sh"))

      flagged <- set_add_flags |>
        dplyr::filter(if_any("flag", ~ . == TRUE ))

      private$scripts <- flagged |>
        purrr::pluck("include")

      if(isTRUE(private$COPY)){
        private$PREV <- FALSE
        self$build <- FALSE # Reset
      }
      # command layout for dockerfile
      docker_cmds <- paste(c(
        paste("# Created by ContainR package with RockerVersioned2"),
        paste("FROM", private$containr_image),
        paste(""),
        if(isTRUE(any(flagged$flag))){
          private$included <- flagged |>
            purrr::pluck("string")
          glue::glue("RUN /rocker_scripts/{private$included}")
        },
        paste(""),
        if(private$containr_packages %in% c("loaded", "installed")){
          c(paste("COPY /containr/scripts/install_additional.sh /tmp/"),
            paste(""),
            paste("RUN chmod +x /tmp/install_additional.sh"),
            paste(""),
            paste("RUN", "/tmp/install_additional.sh"))
        },
        paste(""),
        if(isTRUE(private$COPY)){
          c(paste("RUN mkdir /home/rstudio/containr"),
            paste(""),
            paste("WORKDIR /home/rstudio"),
            paste(""),
            paste("COPY", paste0("./containr/", basename(private$package)), "."),
            paste(""),
            paste("RUN tar -xvzf", paste0("/home/rstudio/", basename(private$package))))
        },
        paste("")
      ),
        collapse = "\n")

      writeLines(docker_cmds,
        con = private$containr_dockerfile,
        sep = "")
      private$inst_dockerfile <- docker_cmds

      if(isTRUE(file.exists(private$containr_dockerfile))){
        cli::cli_alert_success("Dockerfile saved to: {.file {private$containr_dockerfile}}")
      }
    },

setup_packages = function(){
  packages <- private$containr_packages
  bash_file <- "containr/scripts/install_additional.sh"

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
    dplyr::mutate(Source = stringr::str_extract(Source, "(.*?)@")) |>
    dplyr::filter(!is.na(Source))

    if(nrow(not_installed) > 0){
      private$meta_unknown <- not_installed$Package
      private$meta_unknown_vers <- not_installed$Version
      cli::cli_alert_warning("{ {nrow(not_installed)}} Unknown Source Package{?s}")
      cli::cli_alert_warning("{.val {not_installed$Package}}")
    }

    if(nrow(Github) > 0){
      private$meta_github <- Github$Package
      private$meta_github_vers <- Github$Version
      cli::cli_alert_info("Including { {nrow(Github)}} GitHub Package{?s}")
      cli::cli_alert_info("{.val {Github$Package}}")
    }

    if(nrow(CRAN) > 0){
      private$meta_cran <- CRAN$Package
      private$meta_cran_vers <- CRAN$Version
      cli::cli_alert_info("Including { {nrow(CRAN)}} CRAN Package{?s}")
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
      paste("Rscript -e \"install.packages(c('remotes', 'pak'), repos='${CRAN_SOURCE}')\""),
      paste(""),
      paste("UBUNTU_RELEASE=$(lsb_release -sr)"),
      paste(""),
      paste("pkg_req=$(Rscript -e \"pak::pkg_system_requirements(",
            paste0("c(\'", paste0(CRAN$Package, collapse = "\', \'"), "\')"),
            ", ",
            "\'ubuntu', ",
            "\'${UBUNTU_RELEASE}') |>
            gsub('apt-get|-y|install', '', x = _) |>
            trimws(x = _, 'both') |>
            cat()\") ", sep = ""),
      paste(""),
      paste("echo -e \"Installing System Requirements... \" "),
      paste("apt_install $pkg_req"),
      paste(""),
      if(nrow(Github) > 0) {
        c(paste("\necho -e 'Installing GitHub Packages'\n "),
          paste("Rscript -e", paste("\"remotes::install_github(\'",  Github$Source , "\', ", "\'", Github$Version, "\'", ")\" ", sep = "") ," "))
      },
      paste(""),
      paste("\necho -e 'Installing CRAN Packages'\n "),
      paste("Rscript -e ", "\"remotes::install_version('",CRAN$Package,"', version = '",CRAN$Version, "', repos='${CRAN_SOURCE}')\"", sep = ""),
      paste(""),
      paste("rm -rf /var/lib/apt/lists/*"),
      paste("rm -rf /tmp/downloaded_packages"),
      paste("strip /usr/local/lib/R/site-library/*/libs/*.so"),
      paste("apt-get autoremove -y"),
      paste("echo -e \"Finished intalling additional packages!\""),
      paste("")
    ),
    collapse = "\n")

    writeLines(installR_script, con = bash_file, sep = "")

    if(length(readLines(bash_file)) > 0){
      fs::dir_tree("containr")
      cli::cli_alert_success("Package Installs Script Updated: {.path {bash_file}}")
    }
  }
},

    create_directories = function(){

      if(!fs::dir_exists("containr")) {
        fs::dir_create("containr")
      }

      if(isTRUE(private$COPY)){
        pack_dir <- fs::path_temp()
        private$package <- devtools::build(".",
          path = pack_dir, vignettes = FALSE)

        fs::file_copy(path = private$package,
          new_path = "containr", overwrite = TRUE)

        get_tar_file <- list.files("containr", pattern = ".tar.gz$", full.names = TRUE)
        if(isTRUE(fs::file_exists(get_tar_file))) {
          private$packaged_tar_info <- fs::file_info(get_tar_file)
          cli::cli_alert_success("Archived Project Saved: {.file {get_tar_file}}")
        }
      }

      if(private$containr_packages %in% c("loaded", "installed")){
        if(!fs::dir_exists("containr/scripts")) {
          fs::dir_create("containr/scripts")
        }

        if(!fs::file_exists("containr/scripts/install_additional.sh")) {
          fs::file_create("containr/scripts/install_additional.sh")
        }
      }

      if(!fs::file_exists("containr/Dockerfile")) {
        fs::file_create("containr/Dockerfile")
      }

    },

    reset_dir = function(){
      cli::cli_h2("Resetting Docker Folder")
      if(fs::dir_exists("containr")) {
        fs::dir_delete("containr")
      }
    },

    # Launch Process Functions ------------------------------------------------

    setup_config = function(){
      proj_name <- private$containr_name
      private$containr_config <-  create_config_file(proj_name)
    },

    # RStudio Server’s configuration files are saved to: ~/.config/rstudio/ directory
    # not recommended to bind-mount directory on the container (/home/rstudio)
    # RStudio Server opens the user’s home directory (/home/rstudio) by default
    # Use working directory is set up under /home/rstudio, e.g. /home/rstudio/workspace
    setup_command = function(){

      private$setup_config()
      AUTH <-  private$AUTH
      PREV <- private$PREV

      # Local options
      LOCAL_DIR <- private$LOCAL_DIR
      LOCAL_DIR_NAME <- private$LOCAL_DIR_NAME
      LOCAL_R_PROF <- private$containr_config
      LOCAL_R_HOME <- paste0(fs::path_home_r(), "/.config/rstudio")
      LOCAL_RENV <-  paste0(fs::path_home_r(), "/.Renviron")


      CMD_OPTS = c("run", "--rm")
      HOST = "127.0.0.1"
      PORTS = "8787:8787"

      CONTAINR_NAME = LOCAL_DIR_NAME
      IMG_NAME = shQuote(private$containr_image)
      # TODO setup to allow for different tag objects. One for image build and one for rocker versions

      vol_paths <- c(paste0(LOCAL_DIR, ":", private$R_HOME_DIR, LOCAL_DIR_NAME),
                     paste0(LOCAL_R_HOME, ":", private$R_CONFIG_DIR),
                     paste0(LOCAL_RENV, ":", private$R_ENV_DIR),
                     paste0(LOCAL_R_PROF, ":", private$R_PROF))

      VOLUMES <- unlist(purrr::map(vol_paths, function(x) c("-v", x)))

      # Argument string for docker
      proc_args <- c(CMD_OPTS,
        "-e", paste0("DISABLE_AUTH=", AUTH),
        "-p", paste0(HOST, ":", PORTS),
        if(isFALSE(PREV)){
          NULL
        } else {
          VOLUMES
        },
        "--name", private$containr_name, # The name of the container to run, can be anaything but set to proj name
        IMG_NAME) # Set per project

      return(proc_args)
    },

    containr_start = function(){

  if(isTRUE(private$connected)) cli::cli_abort("Containr Already Started")

  cmd <- private$setup_command()

  private$process <- processx::process$new(
    command = "docker",
    args = cmd,
    supervise = TRUE,
    stdout = tempfile("containr-stdout-", fileext = ".log"),
    stderr = tempfile("containr-stderr-", fileext = ".log")
  )

  #use tryCatch to catch any errors that may occur when attempting to start a container
  tryCatch({
    # wait for the container to start by checking its status using the "docker ps" command
    while(!processx::run(command = "docker", args = c("ps", "-q", "--filter", paste0("name=", private$containr_name)), echo = FALSE)$status == 0) {
      Sys.sleep(1)
    }
    private$out <-  private$process$get_output_file()
    # Reset State
    private$connected <- TRUE
    assign("process", private$process, envir = parent.frame())
    private$get_containr_data()
  }, error = function(e) {
    message(e)
    private$process$kill()
  }
  )
},

    containr_stop = function(){
    if(!private$connected) cli::cli_abort("Nothing to stop")
    stopifnot(inherits(private$process, "process"))

    tryCatch({
        processx::run(command = "docker",
          args = c("stop", private$containr_name),
          error_on_status = TRUE,
          echo = FALSE)
    }, error = function(e) {
        message("The container is already stopped.")
    })
    if(private$process$is_alive()){
        private$process$terminate()
        private$process$wait()
    }

    private$connected <- private$process$is_alive()
    private$containr_pid <- NULL
    private$containr_name <- self$name
    private$containr_image <- self$image
    private$containr_image_run_mode <- NULL
},

    containr_status = function(){
    if(!private$connected) {
        status <- "Not Running"
    }else{
        containr_status <- processx::run(command = "docker",
                              args = c("ps", "-f", "name=" ,private$containr_name),
                              error_on_status = TRUE,
                              echo = FALSE)
        if(containr_status$status == 0) {
            status <- "Running"
        }else {
            status <- "Not Running"
        }
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

      created <- processx::run(command = "docker",
                                        args = c("inspect",
                                                 "--format",
                                                 "'{{.Created}}'",
                                                 private$containr_name),
                                        error_on_status = TRUE)$stdout
      private$containr_created <- private$format_string(created) |> as.Date()

      meta_id <- processx::run(command = "docker",
                               args = c("inspect",
                                        "--format",
                                        "'{{.Id}}'",
                                        private$containr_name),
                               error_on_status = TRUE)$stdout
      private$containr_id <- private$format_string(meta_id)

    },

    format_string = function(string){
      gsub("\'|\\\n", "", string)
    },

    save_containr = function(path, meta){
      saveRDS(object = meta, file = path, version = 3L)
    },

    load_containr = function(path){
      readRDS(path)
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

