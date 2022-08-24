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
	backports \
	broom \
	cellranger \
	cli \
	colorspace \
	crayon \
	DBI \
	dbplyr \
	digest \
	dplyr \
	ellipsis \
	evaluate \
	fansi \
	fastmap \
	forcats \
	fs \
	gargle \
	generics \
	ggplot2 \
	glue \
	googledrive \
	googlesheets4 \
	gtable \
	haven \
	here \
	hms \
	htmltools \
	httr \
	jsonlite \
	knitr \
	lattice \
	lifecycle \
	lubridate \
	magrittr \
	Matrix \
	modelr \
	munsell \
	pillar \
	pkgconfig \
	png \
	purrr \
	R6 \
	Rcpp \
	readr \
	readxl \
	reprex \
	reticulate \
	rlang \
	rmarkdown \
	rprojroot \
	rstudioapi \
	rvest \
	scales \
	sessioninfo \
	stringi \
	stringr \
	tibble \
	tidyr \
	tidyselect \
	tidyverse \
	tzdb \
	utf8 \
	vctrs \
	withr \
	xfun \
	xml2 \
	yaml