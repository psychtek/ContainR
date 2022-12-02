
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ContainR <img src="inst/figures/ContainR.png" align="right" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/psychtek/ContainR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psychtek/ContainR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The key problems to irreproducibility of data in research is missing
*documentation*, *version control* and *containerization*. `ContainR`
seeks to address these issues by making it easier to create the
environment for a R project.

The package ports the pre-built
[rocker-versioned](https://github.com/rocker-org/rocker-versioned2)
Rstudio stacks and basic [Docker](https://docs.docker.com/reference/)
commands to R. Based on the active working Rstudio project, it allows
the user to create a Dockerfile and build a Docker image with Rstudio.
Additional settings to include CRAN packages, Github packages, include
Python, Julia, jupyter, Pandoc etc. and launch Rstudio container in a
browser.

**Outputs**

Once the image has baked any additional packages into the base Rocker
stack, the Dockerfile can be saved for distribution and the image pushed
to [DockerHub](https://hub.docker.com/). When a **ContainR session** is
launched, your local config settings can also *cloned* into the session
for previewing before hard coding the package directory into the Docker
image.

The completed `containr/` folder is created with a `Dockerfile`, a tar
of the package, a `scripts/install_additional.sh` file and an optional
JSON file containing the Docker recipe to build the image. The image can
be pushed to Dockerhub and the `containr/` file can be version
controlled and/or shared with collaborators.

    #> containr
    #> ├── ContainR.json
    #> ├── Dockerfile
    #> ├── containr_0.1.5.9000.tar.gz
    #> └── scripts
    #>     └── install_additional.sh

It is currently a work in progress and welcome any [issues and
comments](https://github.com/psychtek/ContainR/issues).

[Read more about the Rocker Project](https://rocker-project.org/)

### System Requirements

[Docker](https://docker-docs.netlify.app/install/) is required for the
use of this package currently support only `R (>= 4.0.0)` base images.

### Installation

You can install the development version of `ContainR` from
[GitHub](https://github.com/psychtek/ContainR) with:

``` r
# install.packages("devtools")
devtools::install_github("psychtek/ContainR")
```

#### Docker Check

Check if Docker is installed on the system or visit install instructions
for [OSX](https://docs.docker.com/desktop/install/mac-install/),
[Windows](https://docs.docker.com/desktop/install/windows-install/) or
[Linux](https://docs.docker.com/engine/install/):

``` r
containr::docker_check()
```

### Rocker-Versioned Images

Supported
[rocker-versioned](https://github.com/rocker-org/rocker-versioned2)
stacks can be found by exploring the `data_rocker_table` inside the
package or visiting their
[repository](https://github.com/rocker-org/rocker-versioned2) to learn
more.

``` r
library(containr)

data_rocker_table |> dplyr::select(-base_image)
#> # A tibble: 5 × 3
#>   name       image             description                                   
#>   <chr>      <chr>             <chr>                                         
#> 1 rstudio    rocker/rstudio    Rstudio Server                                
#> 2 tidyverse  rocker/tidyverse  Adds tidyverse packages & devtools            
#> 3 verse      rocker/verse      Adds tex & publishing-related package         
#> 4 geospatial rocker/geospatial Adds geospatial packages                      
#> 5 binder     rocker/binder     Adds requirements to run repos on mybinder.org
```

## Workflow Basics

- Open your current working Rstudio project.
- Explore which base [Rocker image](https://rocker-project.org/) to use
  `containr::data_rocker_table`.
- **Create** a Dockerfile `containr$new()`.
- **Build** a container `build_image(TRUE)`.
- **Run** the container `start()`.
- **Launch** the session in a browser `launch()`.
- **Stop** the container `stop()`

#### Create a Dockerfile

For example, to create a Dockerfile based on the latest Rocker Rstudio
and install packages that are loaded into the local session, add python
support and install the active project from `CMD CHECK` archive:

``` r
containr <- containr$new(image = "rstudio", name = "project_name", 
packages = "loaded", copy = TRUE, include_python = TRUE)
```

The return process from this function results in the creation of a
`docker/Dockerfile` build recipe with additional scripts in the
`docker/scripts/` directory. If any packages require install from
Github, the function takes note of these and attempts to add them to the
install file as well. Any packages already installed on the base Rocker
image will be skipped.

<img src="inst/figures/initialize.png" align="centre"/>

#### View Preferences

At any point you can check and change what was initialized. This will
clear the `docker` folder and re-write the folder and scripts according
to your preferences. Calling the `print()` will give you sanity checks
for your preferences and the **Status** will update automatically.

<img src="inst/figures/print_settings_command.png" align="centre"/>

The `proc()` will display the Docker background process:

<img src="inst/figures/check_process.png" align="centre"/>

#### Build a ContainR

Setting the `build_image(TRUE)` flag will then read the newly created
`Dockerfile` and build the image based on the previous user
requirements. This is saved to the local Docker register and images can
be view with the `docker_images()` function.

<img src="inst/figures/image_building.png" align="centre"/>

> Note that this can take some time depending on your install
> preferences.

#### Start

When this finished building the image is now ready to be started. The
`start()` fun will launch the container in a background process and if
we run the `proc()` command we see that it is up and running.

<img src="inst/figures/process_active.png" align="centre"/>

#### Launch

Running `launch()` will open a the session in a new browser window.

#### Stop

Running `stop()` will stop the container session completely leaving the
image built intact.

## Docker Functions

Basic Docker functions are also included and additional functionality
for the Docker CLI will be added as development continues. These return
tibbles to the console instead of the terminal. Currently included are
four functions:

- `docker_check()`: Check if Docker is installed and running.

``` r
containr::docker_check()
#> ✔ Docker version 20.10.21, build baeda1f
```

- `docker_images()`: returns a tibble of built images.

``` r
containr::docker_images()
#> # A tibble: 4 × 5
#>   Repository      Tag    ID           CreatedSince   Size  
#>   <chr>           <chr>  <chr>        <chr>          <chr> 
#> 1 rstudio_pandoc  latest a2feb74b885b 44 minutes ago 2GB   
#> 2 rstudio_py      latest ed6c79987814 2 hours ago    2.14GB
#> 3 testing_rstudio latest adab64defd77 3 hours ago    1.79GB
#> 4 rocker/rstudio  latest d110bab4d154 4 weeks ago    1.79GB
```

- `docker_containers()`: Returns a tibble of active containers.

``` r
containr::docker_containers() |> dplyr::select(Command, CreatedAt, Image, Names, Ports, Status)
#> # A tibble: 1 × 6
#>   Command CreatedAt                      Image          Names       Ports Status
#>   <chr>   <chr>                          <chr>          <chr>       <chr> <chr> 
#> 1 /init   2022-12-02 13:42:07 +1100 AEDT rstudio_pandoc rstudio_pa… 127.… Up 30…
```

- `docker_search("rstudio")`: Returns a tibble of search results.

``` r
containr::docker_search("rocker")
#> # A tibble: 25 × 5
#>    Name               Description                        StarC…¹ IsOff…² IsAut…³
#>    <chr>              <chr>                                <int> <lgl>   <chr>  
#>  1 rocker/rstudio     "RStudio Server image"                 402 NA      [OK]   
#>  2 rocker/shiny       ""                                     172 NA      [OK]   
#>  3 rocker/tidyverse   "Version-stable build of R, rstud…     158 NA      [OK]   
#>  4 rocker/r-base      "Basic R for Rocker And Official …      81 NA      [OK]   
#>  5 rocker/verse       "Adds tex & related publishing pa…      64 NA      [OK]   
#>  6 rocker/r-ver       "Reproducible builds to fixed ver…      50 NA      [OK]   
#>  7 rocker/shiny-verse "Rocker Shiny image + Tidyverse R…      45 NA      [OK]   
#>  8 rocker/geospatial  "Docker-based Geospatial toolkit …      44 NA      [OK]   
#>  9 rocker/r-devel     ""                                      22 NA      [OK]   
#> 10 rocker/ropensci    ""                                      22 NA      [OK]   
#> # … with 15 more rows, and abbreviated variable names ¹​StarCount, ²​IsOfficial,
#> #   ³​IsAutomated
```

------------------------------------------------------------------------

There are similar packages available such as:
[dockr](https://github.com/smaakage85/dockr),
[devindocker](https://github.com/ThinkR-open/devindocker),
[harbor](https://github.com/wch/harbor)
[containerit](https://github.com/o2r-project/containerit) and
[stevedore](https://github.com/richfitz/stevedore) which provide various
levels of functionality. We also recommend checking these out to see if
these address your requirements.

### References

- The team at
  [rocker-versioned](https://github.com/rocker-org/rocker-versioned2)
- [repliCATS Data Management
  Team](https://replicats.research.unimelb.edu.au/)
