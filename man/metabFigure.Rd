% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metabFigure.R
\name{metabFigure}
\alias{metabFigure}
\title{Create Nightingale Figure From Template}
\usage{
metabFigure(
  data,
  colorValue = "estimate",
  sizeValue = "negLog10P",
  legendHeight = 8,
  context = gpar(cex = 0.8),
  unitBase = unit(0.5, "cm"),
  colorLim = c(NA, NA),
  sizeLim = c(NA, NA)
)
}
\arguments{
\item{data}{Nightingale results data merged with the ntngale225 or ntngale249
template.}

\item{colorValue}{String naming the variable of \code{data} containing the
values to be plotted on the color scale.}

\item{sizeValue}{String naming the variable of \code{data} containing the
values to scale bubble sizes.}

\item{legendHeight}{Numeric, preferred height of legends in multiples of
0.5cm. This will be supplied to pretty function.}

\item{context}{\code{\link[grid]{gpar}} object to provide drawing context.
Will be applied to the parent viewport in the childrenvp slot of the gTree.}

\item{unitBase}{\code{\link[grid]{unit}} object indicating the size of one
plot grid box.}

\item{colorLim, sizeLim}{Numeric vectors indicating limits (min/max) outside
which the values of color/size matrices should be truncated. NOTE! These
data ranges are separate from the scale ranges, which are taken from the
min/max values of \code{colorBreaks} and \code{sizeBreaks}.}
}
\value{
Produces a complete metabolomics figure consisting of 14
bubbleHeatmap blocks in a pre-set layout. Output is not drawn but returned as
a \code{\link[grid]{gTree}} object which can be edited or drawn using
the \code{\link[grid]{grid.draw}} function. NOTE: A cairo output device with
gradient support is necessary to correctly render the color legend.
}
\description{
Produce a single \code{\link[grid]{gTree}} object representing plots arranged
according to the provided Nightingale metabolomics figure template.
}
\details{
Quick function to produce a final Nightingale figure from results data merged
with one of the supplied template datasets. This function wraps the functions
\code{\link{formatData}}, \code{\link{multiPlotInput}},
\code{\link{bubbleHeatmapList}} and \code{\link{metabFigurePlot}} applying
default settings.
}
\examples{
myData <- merge_template(cetp, "ckb_id")
metabTree <- metabFigure(myData)
grid.draw(metabTree)

}
