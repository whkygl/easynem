% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nem_plot.R
\name{beta}
\alias{beta}
\alias{nem_plot,beta-method}
\title{nem_plot}
\usage{
\S4method{nem_plot}{beta}(object, level = 0.6, type = 1, ...)
}
\arguments{
\item{object}{easynem or other types data.}

\item{level}{Used to adjust the size of the confidence ellipse.}

\item{type}{Method used to adjust the display of scatter area.}

\item{...}{Other parameters for ggplot2::stat_ellipse and ggalt::geom_encircle functions.}
}
\value{
An ggplot object.
}
\description{
For visualization of nematode community data.
}
