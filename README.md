
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ContainR *Work in progress*

<img src="inst/figures/ContainR.png" alt="Alt text" width="10%"/>
<!-- badges: start --> <!-- badges: end -->

A set of functions that I found handy during development of the
repliCATS pipeline. The goal of ContainR is to enable easier container
development for computational reproducibility. It is currently a work in
progess and welcome any issues and comments.

At its core a user can run a command `docker_run()` which will launch
the `rocker/rstudio:latest` stack into a Docker container, port **local
config** and **environment** settings into the container and activate
the Rstudio project. Wrappers for the basic Docker commands allow for
ease of starting and stopping the container.

Included images of the
[rocker-versioned](https://github.com/rocker-org/rocker-versioned2)
stacks can be found by exploring the `data_rocker_table` inside the
package.

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

Check if Docker is installed on the system or installed for
[OSX](https://docs.docker.com/desktop/install/mac-install/),
[Windows](https://docs.docker.com/desktop/install/windows-install/) or
[Linux](https://docs.docker.com/engine/install/).

``` r
docker_check()
```

## ContainR Addin

ContainR installs two basic functions into the `Addins` menu,
`Run Project in Rocker(Rstudio)` and `Stop rocker` which will launch the
`rocker/rsudio:latest` container and copy your config settings and
active rstudio project. This doesnt install any packages but is isolated
from your local R library. Its a quick way to explore the Rocker image
and test your script and then build a container based around your
workflow.

## Workflow

- Install `ContainR`
- Open your Rstudio project
- Load your scripts as usual
- Explore which base Rocker image to use
- Create a Dockerfile with `loaded` packages argument
- Build a container
- Run the new Docker file (Launches browser)

## Benefits

\_ The user now has a container and a form of meta data for
distribution. \_ Container environment is replicated. \_ Choose to
included data assets or separate functions from data. \_ Collaboration
with others.

## Create a Dockerfile

The benefit of this package was to allow a researcher ease of developing
a container without deep dive learning about Docker commands. The
`docker_file()` function is an attempt to make it easier to create a
`Dockerfile` by allowing the user to:

1)  choose the rocker base image,

2)  choose to install either the local `loaded`, `installed` or `none`
    packages,

3)  choose to included python.

``` r
docker_file(dockerfile = "inst/dockerfiles/Dockerfile",
                   which_pkgs = "none",
                   name = "rstudio",
                   tag = "latest",
                   include_python = TRUE)
```

The benefit of this functionality gives the user the choice to build a
container image and only include packages that are loaded at the time of
development. The install scripts also skip any packages already
installed on the base Rocker image.

The `docker_build()` function will then read the newly created
`Dockerfile` and build the image based on the previous user
requirements.

``` r
docker_build(dockerfile =  "inst/dockerfiles/Dockerfile", name = "projectname")
```

Finally,`docker_run()` is can then be used (with the user supplied
Docker image) to launch a Rstudio container in the browser activating
the current working project.

``` r
rocker_run()
```

There are similar packages available such as
[dockr](https://github.com/smaakage85/dockr) or
[devindocker](https://github.com/ThinkR-open/devindocker) which provide
various levels of functionality. We also recommend checking these out to
see if these address your requirements.
