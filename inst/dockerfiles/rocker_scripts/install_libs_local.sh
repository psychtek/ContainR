#!/bin/bash

set -e

NCPUS=${NCPUS:--1}

CRAN=${1:-${CRAN:-"https://cran.r-project.org"}}

ARCH=$(uname -m)

export DEBIAN_FRONTEND=noninteractive

UBUNTU_VERSION=$(lsb_release -sc)
CRAN_SOURCE=${CRAN/"__linux__/$UBUNTU_VERSION/"/""}

if [ "$ARCH" = "aarch64" ]; then
	 CRAN=$CRAN_SOURCE
fi

install2.r --error --skipinstalled -n "$NCPUS" \
    assertthat \
	  cli \
	  DBI \
	  digest \
	  dplyr \
	  evaluate \
	  fansi \
	  fastmap \
	  generics \
	  glue \
	  here \
	  htmltools \
	  knitr \
	  lifecycle \
	  magrittr \
	  pillar \
	  pkgconfig \
	  purrr \
	  R6 \
	  rlang \
	  rmarkdown \
	  rprojroot \
	  rstudioapi \
	  sessioninfo \
	  sys \
	  tibble \
	  tidyr \
	  tidyselect \
	  utf8 \
	  vctrs \
	  xfun \
	  yaml
