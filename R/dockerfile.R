
# Dockerfile Class --------------------------------------------------------
#' @include docker.R containr.R
#' @title dockerfile Class Object
#'
#' @description Setup to create a dockerfile based on a rockerimage
#'
#' @export
dockerfile <- R6::R6Class(
  classname = "dockerfile",
  inherit = containr,

  # Public List -------------------------------------------------------------

  public = list(

    #' @field dockerfile Location of the dockerfile. Is set to `docker/Dockerfile/` directory.
    dockerfile = NULL,

    #' @field rocker_image A `name` from the [data_rocker_table] to build off/
    rocker_image = NULL,

    #' @field packages Provide a string of either `loaded`, `installed` or `none`.
    packages = NULL,

    #' @field include_python Flag to install python using the rocker scripts
    include_python = NULL,

    #' @field build Default is `FALSE`. Set to `TRUE` with the `build_image` fun.
    build = NULL,

    #' @field tag Create a tag for the image. Defaults to `latest`.
    tag = NULL,

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
    #' installed packages are skipped when the `build_image` command is run. If you have a large package library it is
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
    #' If to install python on the image. These make use of the rocker scripts
    #' and are somewhat modified. But `pip` with `pandas` and `numpy` will be included.
    #'
    #' @param include_python Flag `TRUE` or `FALSE` if to include python
    set_python = function(include_python){
      self$include_python <- include_python
      private$containr_include_python <- self$include_python
      return(private$containr_include_python)
    },

    #' @description
    #' Starts the build process based on settings.
    #' @param build Set flag for build
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

    #' @description
    #' Prints a table of the argument settings.
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
