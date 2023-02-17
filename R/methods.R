#### Documentation: width/heightDetails ####
#'
#' Methods for determining the width/height of bubbleHeatmap plots and legends.
#'
#' @param x A bubbleHeatmap, bubbleLegend, or colorLegend object.
#'
#' @return Grid \code{\link[grid]{unit}} object representing object width or height.
#' @describeIn widthDetails.bubbleHeatmap Get width of bubbleHeatmap
#' @export
widthDetails.bubbleHeatmap <- function(x) {
  minX0 <- min(do.call(unit.c, lapply(x$children, grobX, theta = 180)))
  maxX1 <- max(do.call(unit.c, lapply(x$children, grobX, theta = 0)))
  return(grobWidth(segmentsGrob(x0 = minX0, x1 = maxX1)))
}
#' @describeIn widthDetails.bubbleHeatmap Get width of bubbleLegend
#' @export
widthDetails.bubbleLegend <- function(x) {
  minX0 <- min(do.call(unit.c, lapply(x$children, grobX, theta = 180)))
  maxX1 <- max(do.call(unit.c, lapply(x$children, grobX, theta = 0)))
  return(grobWidth(segmentsGrob(x0 = minX0, x1 = maxX1)))
}
#' @describeIn widthDetails.bubbleHeatmap Get width of colorLegend
#' @export
widthDetails.colorLegend <- function(x) {
  minX0 <- min(do.call(unit.c, lapply(x$children, grobX, theta = 180)))
  maxX1 <- max(do.call(unit.c, lapply(x$children, grobX, theta = 0)))
  return(grobWidth(segmentsGrob(x0 = minX0, x1 = maxX1)))
}
#' @describeIn widthDetails.bubbleHeatmap Get height of bubbleHeatmap
#' @export
heightDetails.bubbleHeatmap <- function(x) {
  minX0 <- min(do.call(unit.c, lapply(x$children, grobY, theta = 270)))
  maxX1 <- max(do.call(unit.c, lapply(x$children, grobY, theta = 90)))
  return(grobWidth(segmentsGrob(x0 = minX0, x1 = maxX1)))
}
#' @describeIn widthDetails.bubbleHeatmap Get height of bubbleLegend
#' @export
heightDetails.bubbleLegend <- function(x) {
  minX0 <- min(do.call(unit.c, lapply(x$children, grobY, theta = 270)))
  maxX1 <- max(do.call(unit.c, lapply(x$children, grobY, theta = 90)))
  return(grobWidth(segmentsGrob(x0 = minX0, x1 = maxX1)))
}
#' @describeIn widthDetails.bubbleHeatmap Get height of colorLegend
#' @export
heightDetails.colorLegend <- function(x) {
  minX0 <- min(do.call(unit.c, lapply(x$children, grobY, theta = 270)))
  maxX1 <- max(do.call(unit.c, lapply(x$children, grobY, theta = 90)))
  return(grobWidth(segmentsGrob(x0 = minX0, x1 = maxX1)))
}
