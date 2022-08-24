FROM rocker/rstudio:4.2.1

COPY ./rocker_scripts/install_python.sh /tmp/
COPY ./rocker_scripts/install_pyenv.sh /tmp/

RUN chmod +x /tmp/install_python.sh

RUN /tmp/install_python.sh
RUN R -e "reticulate::py_config()"

COPY ./rocker_scripts/install_additional.sh /tmp/

RUN /tmp/install_additional.sh

RUN R -e "sessioninfo::platform_info()"