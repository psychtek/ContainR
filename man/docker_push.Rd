% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docker.R
\name{docker_push}
\alias{docker_push}
\title{Docker Push Image}
\usage{
docker_push(name = NULL, tag = NULL)
}
\arguments{
\item{name}{The user/name of the docker image}

\item{tag}{Version or tag of image. Defaults to latest.}
}
\description{
Push container image to \url{https://hub.docker.com/}. Naming Convention in Docker Hub:
Very popularly known software are given their own name as Docker image. For individuals and
corporates, docker gives a username. And all the images can be pushed to a particular username.
And can be pulled using the image name with a username.
}
