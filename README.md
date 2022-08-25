
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ContainR

**Work in progress** <!-- badges: start --> <!-- badges: end -->

The goal of ContainR is to enable easier container development for
computational reproducibility. At its core a user can run a command
`docker_run()` which will launch the `rocker/rstudio` stack into a
docker container, port local config and environment settings into the
container and activate the active Rstudio project. Wrappers for the
basic docker commands allow for ease of starting and stopping the
container.

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

## Note

The default design of this package allows the user to launch one of the
rocker images, and activate the users current working Rstudio project
directory. Local R packages are not copied but allow the user to explore
a fresh container image.

The `docker_create()` function is an attempt to make it easier to create
a `Dockerfile` by allowing the user to:

1)  choose the rocker base image,

2)  choose to install either the local `loaded` or `installed` packages,

3)  choose to included python.

``` r
docker_create(dockerfile = "inst/dockerfiles/Dockerfile", 
    which_pkgs = "loaded", 
    rocker_name = "verse",
    include_python = TRUE) 
```

The benefit of this functionality gives the user the choice to build a
container image and only include packages that are loaded at the time of
development. The install scripts also skip any packages already
installed.

The `docker_build()` function will then read the newly created
`Dockerfile` and build the image based on the previous user
requirements.

``` r

docker_build(dockerfile = "inst/dockerfiles/Dockerfile", 
                name = "username/custom_image")
```

Finally, `docker_run()` is then run with the user supplied Docker image
to launch a Rstudio container in the browser activating the current
working project.

``` r
docker_run()
```

The created `Dockerfile` found in the `inst/dockerfiles/` directory
provides a form of metadata for environmental computational
reproducibility and, can easily be relaunch with the `docker_run()`
command. Although the users local `.config` and `.Renviron` settings are
copied when the container is launched, this arent included in the actual
docker image. This allows teams to work on the scripts, update the
library packages as needed but keep their custom working Rstudio
configuration settings private.

There are similar packages available such as
[dockr](https://github.com/smaakage85/dockr) or
[devindocker](https://github.com/ThinkR-open/devindocker) which provide
various levels of functionality. We also recommend checking these out to
see if these suit your requirements.
