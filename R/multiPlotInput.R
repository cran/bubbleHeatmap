#' Arrange bubbleHeatmap Arguments in Lists for Building Multiple Plots.
#'
#' Build bubbleHeatmap arguments into a list of lists to facilitate building of
#' multiple plots in a single call. It is useful for creating sets of plots with
#' the same data scales, similar plots with minor differences in settings, or
#' the supplied Nightingale plot template.
#'
#' @section Nightingale:
#' For more information about using the Nightingale style template, see the
#' vignette. Requesting Nightingale style values overrides any argument for
#' plotTitles, xTitles, yTitles, showTopLabels, showLeftLabels,
#' leftLabelsTitles, showRowBracket, rowTitles, showColBracket, colTitles,
#' showBubbleLegend or showColorLegend.
#'
#' @param colorList,sizeList List of matrices, each item representing one plot,
#' containing values to be plotted as the bubble color and size respectively. If
#' \code{sizeList} is not supplied then color values will also be used for size.
#' @param nightingale Logical, apply Nightingale style template?
#' @param treeNames Vector of names for gTrees output by bubbleHeatmap, if not
#' supplied then \code{names(colorList)} will be used.
#' @param context \code{\link[grid]{gpar}} object to provide drawing context.
#' Will be applied to the parent viewport in the childrenvp slot of gTrees
#' output by bubbleHeatmap.
#' @param colorSeq Vector of colors to distribute over color scale gradient.
#' @param unitBase \code{\link[grid]{unit}} object indicating the size of one
#' plot grid square.
#' @param diameter Maximum diameter of bubbles as multiple of unitBase.
#' @param colorLim,sizeLim Numeric vectors indicating limits (min/max) beyond
#' which the values of color/size matrices should be truncated.
#' @param showTopLabels Logical vector, should matrix column names be printed
#' above each plot? Should have \code{length == length(colorList)} or
#' \code{length == 1} to use same value for all plots.
#' @param showLeftLabels Logical vector, should matrix row names be printed
#' above each plot? Should have \code{length == length(colorList)} or
#' \code{length == 1} to use same value for all plots.
#' @param leftLabelsTitles Vector of headings to go over the row names of each
#' plot. Character strings or FALSE for no title. A single FALSE value can also
#' be used if no left label titles are required.
#' @param showRowBracket Logical vector, should vertical bracket be printed to
#' the right of each plot (to label a row of plots in a final figure layout)?
#' Should have \code{length == length(colorList)} or \code{length == 1} to use
#' same value for all plots.
#' @param rowTitles Vector of labels to be printed to the right of each plot,
#' outside row bracket if present (to label a row of plots in a final figure
#' layout). Character strings or FALSE for no title. A single FALSE value
#' can also be used if no row titles are required.
#' @param showColBracket Logical vector, should horizontal bracket be printed
#' below each plot (to label a column of plots in a final figure layout)?
#' Should have \code{length == length(colorList)} or \code{length == 1} to use
#' same value for all plots.
#' @param colTitles Vector of labels to be printed to the right of each plot,
#' outside col bracket if present (to label a column of plots in a final figure
#' layout). Character strings or FALSE for no title. A single FALSE value can
#' also be used if no column titles are required.
#' @param plotTitles Vector of plot titles to be centered above each plot.
#' Character strings or FALSE for no title. A single FALSE value can also be
#' used if no plot titles are required.
#' @param xTitles Vector of x axis titles to be printed left of
#' \code{LeftLabels}. Character strings or FALSE for no title. A single FALSE
#' value can also be used if no x axis titles are required.
#' @param yTitles Vector of y axis titles to be printed above \code{TopLabels}.
#' Character strings or FALSE for no title. A single FALSE value can also be
#' used if no y axis titles are required.
#' @param showColorLegend,showBubbleLegend Logicals, should a legend be produced
#' for color scale/bubble size? If vector with \code{length == length(colorList)}
#' then values will be input for each plot and any legends produced will be an
#' element of a plot tree. A single TRUE value will build the legends outside
#' the plots as separate trees.
#' @param colorBreaks,sizeBreaks Character vectors of legend tick labels. If not
#' supplied then these will be generated using pretty to a range outside data
#' values (after values truncated to \code{colorLim}\\\code{sizeLim} if
#' applicable).
#' @param legendHeight Numeric, preferred height of legends as multiple of
#' \code{unitBase}. This will be supplied to pretty function. If {sizeBreaks} is
#' supplied then this will be ignored and legend height will be
#' \code{length(sizeBreaks)}.
#' @param legendTitles Character vector of legend titles. First string should be
#' the size legend title, second for the color legend.
#'
#' @return
#' List with two elements:
#' \describe{
#' \item{trees}{List of lists, arguments for \code{bubbleHeatmap}}
#' \item{legend}{List of arguments for \code{bubbleHMLegends}}}
#' The list returned by this plot can be passed directly to
#' \code{\link{bubbleHeatmapList}} to produce a list of plot trees.
#'
#' @family build functions
#' @import grid
#' @importFrom utils tail
#' @export

multiPlotInput <- function(colorList,
                           sizeList = colorList,
                           nightingale = TRUE,
                           treeNames = names(colorList),
                           context = gpar(cex = 0.8),
                           colorSeq = c("#053061", "#2166AC", "#4393C3",
                                        "#92C5DE", "#D1E5F0", "#FDDBC7",
                                        "#F4A582", "#D6604D", "#B2182B",
                                        "#67001F"),
                           unitBase = unit(0.5, "cm"),
                           diameter = 0.8,
                           colorLim = c(NA, NA),
                           sizeLim = c(NA, NA),
                           showTopLabels = TRUE,
                           showLeftLabels = TRUE,
                           leftLabelsTitles = FALSE,
                           showRowBracket = FALSE,
                           rowTitles = FALSE,
                           showColBracket = FALSE,
                           colTitles = FALSE,
                           plotTitles = FALSE,
                           xTitles = FALSE,
                           yTitles = FALSE,
                           showColorLegend = TRUE,
                           showBubbleLegend = TRUE,
                           colorBreaks = NULL,
                           sizeBreaks = NULL,
                           legendHeight = nrow(colorList[[1]]),
                           legendTitles = c(expression("-log"[10]*"P"), "Estimate (SD)")) {
  #### Prepare Data Matrices ####
  # If min or max limits are preset for color or size then#
  #edit any values outside limits#
  if (!all(is.na(c(colorLim, sizeLim)))) {
    applyLim <- function(x, min, max) {
      if (!is.na(min)) x[x < min] <- min
      if (!is.na(max)) x[x > max] <- max
      return(x)
    }
    colorList <- lapply(colorList, applyLim, colorLim[1], colorLim[2])
    sizeList <- lapply(sizeList, applyLim, sizeLim[1], sizeLim[2])
    for(i in 1:length(colorList)){
      storage.mode(colorList[[i]]) <= "numeric"
      storage.mode(sizeList[[i]]) <= "numeric"
    }}
  #If limits not preset then set range using min/max across all plots#
  if(any(is.na(c(colorLim, sizeLim)))) {
    if(is.na(c(colorLim[1]))){colorLim[1] <- min(do.call(c, colorList)[is.finite(as.numeric(do.call(c, colorList)))])}
    if(is.na(c(colorLim[2]))){colorLim[2] <- max(do.call(c, colorList)[is.finite(as.numeric(do.call(c, colorList)))])}
    if(is.na(c(sizeLim[1]))){sizeLim[1] <- min(do.call(c, sizeList)[is.finite(as.numeric(do.call(c, sizeList)))])}
    if(is.na(c(sizeLim[2]))){sizeLim[2] <- max(do.call(c, sizeList)[is.finite(as.numeric(do.call(c, sizeList)))])}}
  #### Set Legend Breakpoints####
  #If bubble legend labels are not supplied and#
  #bubble legend is required then set size breakpoints#
  if (is.null(sizeBreaks) & showBubbleLegend != FALSE) {
    sizeBreaks <- pretty(c(sizeLim[1], sizeLim[2]), legendHeight)
    if (sizeBreaks[1] > sizeLim[1]) sizeBreaks <-
        c(sizeBreaks[1] - (sizeBreaks[2] - sizeBreaks[1]), sizeBreaks)
    if (tail(sizeBreaks, 1) < sizeLim[2]) sizeBreaks <-
        c(sizeBreaks, tail(sizeBreaks, 1) + (sizeBreaks[2] - sizeBreaks[1]))
    sizeLim <- c(sizeBreaks[1], tail(sizeBreaks, 1))
  }
  #If color legend labels are not supplied and#
  #color legend is required then set color breakpoints#
  #(Attempt to match legend sizes)#
  if (is.null(colorBreaks) & showColorLegend != FALSE) {
    if (!(is.null(sizeBreaks))) legendHeight <- length(sizeBreaks)
    colorBreaks <- pretty(c(colorLim[1], colorLim[2]), legendHeight)
    if (colorBreaks[1] > colorLim[1]) colorBreaks <-
      c(colorBreaks[1] - (colorBreaks[2] - colorBreaks[1]), colorBreaks)
    if (tail(colorBreaks, 1) < colorLim[2]) colorBreaks <-
      c(colorBreaks, tail(sizeBreaks, 1) + (colorBreaks[2] - colorBreaks[1]))
    colorLim <- c(colorBreaks[1], tail(colorBreaks, 1))
  }
  #### Set color palette####
  colorSeq <- grDevices::colorRampPalette(colorSeq)(200)
  #### Set Variable Vectors For Each Plot####
  if (nightingale == TRUE) {
    plotTitles <- c(rep.int(FALSE, length(colorList)))
    xTitles <- rep.int(FALSE, length(colorList))
    yTitles <- rep.int(FALSE, length(colorList))
    showTopLabels <- c(TRUE, FALSE, TRUE, TRUE, rep.int(FALSE, length(colorList) - 4))
    showLeftLabels <- c(TRUE, TRUE, FALSE, rep.int(TRUE, length(colorList) - 3))
    leftLabelsTitles <- names(colorList)
    leftLabelsTitles[3] <- FALSE
    showRowBracket <- rep.int(FALSE, length(colorList))
    rowTitles <- rep.int(FALSE, length(colorList))
    showColBracket <- rep.int(FALSE, length(colorList))
    colTitles <- rep.int(FALSE, length(colorList))
    showBubbleLegend <- TRUE
    showColorLegend <- TRUE
  } else {
    varList <- c("plotTitles", "xTitles", "yTitles", "showTopLabels",
                 "showLeftLabels", "leftLabelsTitles", "showRowBracket",
                 "rowTitles", "showColBracket", "colTitles",
                 "showBubbleLegend", "showColorLegend")
    for (x in varList) {
      if (length(get(x)) != (0 || 1 || length(colorList))) {
        stop(paste("length", x, "must equal 0, 1 or length(colorList)"))
      }
    }
  }
  if (length(showColorLegend) == 1 & showColorLegend == TRUE) {
    sCLOverall <- TRUE
    showColorLegend <- FALSE
  }
  if (length(showBubbleLegend) == 1 & showBubbleLegend == TRUE) {
    sBLOverall <- TRUE
    showBubbleLegend <- FALSE
  }
  vectorList <- list(showTopLabels = showTopLabels,
                     showLeftLabels = showLeftLabels,
                     showRowBracket = showRowBracket,
                     showColBracket = showColBracket,
                     rowTitle = rowTitles,
                     colTitle = colTitles,
                     xTitle = xTitles,
                     yTitle = yTitles,
                     leftLabelsTitle = leftLabelsTitles,
                     plotTitle = plotTitles,
                     showBubbleLegend = showBubbleLegend,
                     showColorLegend = showColorLegend)
  #### Combine variables to list for output ####

  inputA <- c(list(f = list, colorMat = colorList,
                   sizeMat = sizeList, treeName = treeNames), vectorList)
  inputA <- do.call(Map, inputA)
  inputB <- c(list(colorSeq = colorSeq, unitBase = unitBase,
                   colorLim = colorLim, sizeLim = sizeLim,
                   colorBreaks = colorBreaks, sizeBreaks = sizeBreaks,
                   legendTitles = legendTitles, legendHeight = legendHeight,
                   context = context,
                   diameter = diameter))
  if (sCLOverall == TRUE || sBLOverall == TRUE) {
    legend <- c(list(colorSeq = colorSeq, unitBase = unitBase,
                     diameter = diameter, context = context,
                     colorBreaks = colorBreaks, sizeBreaks = sizeBreaks,
                     legendTitles = legendTitles, legendHeight = legendHeight,
                     showColorLegend = sCLOverall,
                     showBubbleLegend = sBLOverall))
  }
  return(list(trees = lapply(inputA, function(a, b) {
    c(a, b)
  }, b = inputB), legends = legend))
}
