
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ContainR <img src="inst/figures/ContainR.png" align="right" width="120" />

<!-- badges: start -->
<!-- badges: end -->

A set of functions that I found handy during development of the
repliCATS pipeline. The goal of ContainR is to enable easier container
development for computational reproducibility. The package ports the
[rocker-versioned](https://github.com/rocker-org/rocker-versioned2)
stacks and basic [Docker](https://docs.docker.com/reference/) commands
to R.

It is currently a work in progess and welcome any issues and comments.

## Overview

At its core a user can run the `docker_run()` function which will launch
the `rocker/rstudio:latest` stack into a Docker container, port **local
config** and **environment** settings into the container and activate
the Rstudio project in a virtual session. Wrappers for the basic Docker
commands allow for ease of starting and stopping the container.

Included images of the
[rocker-versioned](https://github.com/rocker-org/rocker-versioned2)
stacks can be found by exploring the `data_rocker_table` inside the
package or visiting their
[repository](https://github.com/rocker-org/rocker-versioned2) to learn
more.

    #> # A tibble: 5 × 4
    #>   name       image             base_image        description                    
    #>   <chr>      <chr>             <chr>             <chr>                          
    #> 1 rstudio    rocker/rstudio    rocker/r-ver      Rstudio Server                 
    #> 2 tidyverse  rocker/tidyverse  rocker/rstudio    Adds tidyverse packages & devt…
    #> 3 verse      rocker/verse      rocker/tidyverse  Adds tex & publishing-related …
    #> 4 geospatial rocker/geospatial rocker/verse      Adds geospatial packages       
    #> 5 binder     rocker/binder     rocker/geospatial Adds requirements to run repos…

## Installation

You can install the development version of `ContainR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("psychtek/ContainR")
```

### Docker Check

Check if Docker is installed on the system or visit install instructions
for [OSX](https://docs.docker.com/desktop/install/mac-install/),
[Windows](https://docs.docker.com/desktop/install/windows-install/) or
[Linux](https://docs.docker.com/engine/install/):

``` r
docker_check()
```

## ContainR Addin

ContainR installs two basic functions into the `Addins` menu,
`Run Project in Rocker(Rstudio)` and `Stop rocker` which will launch the
`rocker/rsudio:latest` container and copy your config settings and
active rstudio project. This doesnt install any packages however, is
isolated from your local R library. Its a quick way to explore a Rocker
stack and build a container based around your workflow.

## Workflow

- Install `ContainR`.
- Open your current Rstudio project.
- Explore which base Rocker image to use `ContainR::data_rocker_table`
- Create a Dockerfile `docker_file()` **Future plan is to allow a choice
  to also direct copy the R project into the image**
- Build a container `docker_build()`
- Run the new Docker file (Launches browser) `docker_run()`

## Usage

### Create a Dockerfile

The benefit of this package is to allow a researcher ease of developing
a container without deep dive learning about Docker commands. The
`docker_file()` function is an attempt to make it easier to create a
`Dockerfile` by allowing the user to:

1)  choose the Rocker base image;

2)  choose to install either your local `loaded`, `installed` or `none`
    packages;

3)  choose which version (tag);

4)  choose to included python (pandas and numpy).

``` r
docker_file(dockerfile = "inst/dockerfiles/Dockerfile",
                        which_pkgs = c("loaded", "installed", "none"),
                        name = c("rstudio", "tidyerse", "verse", "geospatial", "binder"),
                        tag = NULL,
                        include_python = FALSE)
```

The install scripts also skip any packages already installed on the base
Rocker image.

### Build a Docker Container

The `docker_build()` function will then read the newly created
`Dockerfile` and build the image based on the previous user
requirements. This is saved to the local Docker registery and images can
be view with the `docker_images()` function.

``` r
docker_build(dockerfile =  "inst/dockerfiles/Dockerfile", name = "psychtek/imagename")
```

### Launch a Container

Finally,`docker_run()` is can then be used (with the user supplied
Docker image) to launch a Rstudio container in the browser, activating
the current working project. Packages provided in the
`which_pkgs = c("loaded", "installed", "none")` argument preference
should be available. Your custom theme and layout should also be ready
and a clone of the working Rstudio project should be available.

``` r
rocker_run(image = "psychtek/imagename", tag = "latest")
```

## Benefits and Uses

When a container is launched **none of your local Rstudio settings are
preserved** in the built image. Only the R packages and Rocker stack
that was chosen during the build are preserved. This allows the user the
choice to explore out a Rocker image or build into the image R packages
used with the current active Rstudio project. **Future plans is to allow
the user to copy the working directory into the container image**.

The Docker image can also be pushed to
[Dockerhub](https://hub.docker.com/) and shared with others for
collaboration on a project so that the team is using the same
environment for development.

If you have a Rmarkdown or [Quarto](https://quarto.org/) analysis you
can provide a `docker-compose` file to reference your Docker image and
link Zenodo DOIs etc.

Ease of exploring Docker containers within Rstudio.

------------------------------------------------------------------------

There are similar packages available such as
[dockr](https://github.com/smaakage85/dockr) or
[devindocker](https://github.com/ThinkR-open/devindocker) which provide
various levels of functionality. We also recommend checking these out to
see if these address your requirements.
