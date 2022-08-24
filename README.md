
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

<div id="zehwcckpij" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#zehwcckpij .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: smaller;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 100%;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #000000;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zehwcckpij .gt_heading {
  background-color: #C6DBEF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zehwcckpij .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zehwcckpij .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zehwcckpij .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zehwcckpij .gt_col_headings {
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zehwcckpij .gt_col_heading {
  color: #333333;
  background-color: #EDF8FB;
  font-size: smaller;
  font-weight: bold;
  text-transform: uppercase;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zehwcckpij .gt_column_spanner_outer {
  color: #333333;
  background-color: #EDF8FB;
  font-size: smaller;
  font-weight: bold;
  text-transform: uppercase;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zehwcckpij .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zehwcckpij .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zehwcckpij .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zehwcckpij .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFCC;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#zehwcckpij .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFCC;
  font-size: 80%;
  font-weight: bolder;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zehwcckpij .gt_from_md > :first-child {
  margin-top: 0;
}

#zehwcckpij .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zehwcckpij .gt_row {
  padding-top: 1.5px;
  padding-bottom: 1.5px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zehwcckpij .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zehwcckpij .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zehwcckpij .gt_row_group_first td {
  border-top-width: 2px;
}

#zehwcckpij .gt_summary_row {
  color: #333333;
  background-color: #EDF8FB;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zehwcckpij .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zehwcckpij .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zehwcckpij .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zehwcckpij .gt_grand_summary_row {
  color: #FFFFFF;
  background-color: #66A3A3;
  text-transform: uppercase;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zehwcckpij .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zehwcckpij .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zehwcckpij .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zehwcckpij .gt_footnotes {
  color: #333333;
  background-color: #EDF8FB;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zehwcckpij .gt_footnote {
  margin: 0px;
  font-size: 9px;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zehwcckpij .gt_sourcenotes {
  color: #333333;
  background-color: #F6EFF7;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zehwcckpij .gt_sourcenote {
  font-size: 8px;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zehwcckpij .gt_left {
  text-align: left;
}

#zehwcckpij .gt_center {
  text-align: center;
}

#zehwcckpij .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zehwcckpij .gt_font_normal {
  font-weight: normal;
}

#zehwcckpij .gt_font_bold {
  font-weight: bold;
}

#zehwcckpij .gt_font_italic {
  font-style: italic;
}

#zehwcckpij .gt_super {
  font-size: 65%;
}

#zehwcckpij .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#zehwcckpij .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zehwcckpij .gt_indent_1 {
  text-indent: 5px;
}

#zehwcckpij .gt_indent_2 {
  text-indent: 10px;
}

#zehwcckpij .gt_indent_3 {
  text-indent: 15px;
}

#zehwcckpij .gt_indent_4 {
  text-indent: 20px;
}

#zehwcckpij .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: #000000; font-size: 12px; text-transform: uppercase;" scope="col">name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: #000000; font-size: 12px; text-transform: uppercase;" scope="col">image</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: #000000; font-size: 12px; text-transform: uppercase;" scope="col">base_image</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: #000000; font-size: 12px; text-transform: uppercase;" scope="col">description</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">rstudio</td>
<td class="gt_row gt_left">rocker/rstudio</td>
<td class="gt_row gt_left">rocker/r-ver</td>
<td class="gt_row gt_left">Rstudio Server</td></tr>
    <tr><td class="gt_row gt_left gt_striped">tidyverse</td>
<td class="gt_row gt_left gt_striped">rocker/tidyverse</td>
<td class="gt_row gt_left gt_striped">rocker/rstudio</td>
<td class="gt_row gt_left gt_striped">Adds tidyverse packages &amp; devtools</td></tr>
    <tr><td class="gt_row gt_left">verse</td>
<td class="gt_row gt_left">rocker/verse</td>
<td class="gt_row gt_left">rocker/tidyverse</td>
<td class="gt_row gt_left">Adds tex &amp; publishing-related package</td></tr>
    <tr><td class="gt_row gt_left gt_striped">geospatial</td>
<td class="gt_row gt_left gt_striped">rocker/geospatial</td>
<td class="gt_row gt_left gt_striped">rocker/verse</td>
<td class="gt_row gt_left gt_striped">Adds geospatial packages</td></tr>
    <tr><td class="gt_row gt_left">binder</td>
<td class="gt_row gt_left">rocker/binder</td>
<td class="gt_row gt_left">rocker/geospatial</td>
<td class="gt_row gt_left">Adds requirements to run repos on mybinder.org</td></tr>
  </tbody>
  
  
</table>
</div>

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
