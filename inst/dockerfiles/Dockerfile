FROM rocker/rstudio:4.2.1

COPY /inst/dockerfiles/rocker_scripts/install_python.sh /tmp/

COPY /inst/dockerfiles/rocker_scripts/install_pyenv.sh /tmp/

RUN chmod +x /tmp/install_python.sh

RUN /tmp/install_python.sh

RUN R -e "reticulate::py_config()"

COPY /inst/dockerfiles/rocker_scripts/install_libs_local.sh /tmp/

COPY /inst/dockerfiles/rocker_scripts/install_additional.sh /tmp/

RUN chmod +x /tmp/install_libs_local.sh

RUN chmod +x /tmp/install_additional.sh

RUN /tmp/install_additional.sh

RUN R -e "sessioninfo::platform_info()"
