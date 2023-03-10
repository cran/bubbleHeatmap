% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{widthDetails.bubbleHeatmap}
\alias{widthDetails.bubbleHeatmap}
\alias{widthDetails.bubbleLegend}
\alias{widthDetails.colorLegend}
\alias{heightDetails.bubbleHeatmap}
\alias{heightDetails.bubbleLegend}
\alias{heightDetails.colorLegend}
\title{Methods for determining the width/height of bubbleHeatmap plots and legends.}
\usage{
\method{widthDetails}{bubbleHeatmap}(x)

\method{widthDetails}{bubbleLegend}(x)

\method{widthDetails}{colorLegend}(x)

\method{heightDetails}{bubbleHeatmap}(x)

\method{heightDetails}{bubbleLegend}(x)

\method{heightDetails}{colorLegend}(x)
}
\arguments{
\item{x}{A bubbleHeatmap, bubbleLegend, or colorLegend object.}
}
\value{
Grid \code{\link[grid]{unit}} object representing object width or height.
}
\description{
Methods for determining the width/height of bubbleHeatmap plots and legends.
}
\section{Functions}{
\itemize{
\item \code{widthDetails(bubbleHeatmap)}: Get width of bubbleHeatmap

\item \code{widthDetails(bubbleLegend)}: Get width of bubbleLegend

\item \code{widthDetails(colorLegend)}: Get width of colorLegend

\item \code{heightDetails(bubbleHeatmap)}: Get height of bubbleHeatmap

\item \code{heightDetails(bubbleLegend)}: Get height of bubbleLegend

\item \code{heightDetails(colorLegend)}: Get height of colorLegend

}}
