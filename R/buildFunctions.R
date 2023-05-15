####Documentation: bubbleHeatmap####
#' Generate bubbleHeatmap Plot Object
#'
#' This function creates a gTree for a bubbleHeatmap plot. These plots have a
#' grid of "bubbles" with the color and size each scaled according to the values
#' of a different variable. They can be used as an alternative to forest plots
#' for visualizing estimates and errors that can be organised in a grid format,
#' correlograms, and any other two variable matrices.
#'
#' @param colorMat,sizeMat Numeric matrices containing values to be mapped to
#' bubble color and size respectively. The dimensions, rownames and colnames of
#' these matrices must match.
#' @param treeName Identifying character string for \code{\link[grid]{gTree}}.
#' @param context \code{\link[grid]{gpar}} object to provide drawing context.
#' Will be applied to the parent viewport in the childrenvp slot of the gTree.
#' @param colorSeq Character vector of colors to be used in creating color
#' scale.
#' @param unitBase \code{\link[grid]{unit}} object indicating the size of one
#' plot grid box.
#' @param diameter Maximum diameter of bubbles as multiple of unitBase.
#' @param colorLim,sizeLim Numeric vectors indicating limits (min/max) outside
#' which the values of color/size matrices should be truncated. NOTE! These
#' data ranges are separate from the scale ranges, which are taken from the
#' min/max values of \code{colorBreaks} and \code{sizeBreaks}.
#' @param showTopLabels Logical, should matrix column names be printed above
#' plot?
#' @param showLeftLabels Logical, should matrix row names be printed above
#' plot?
#' @param leftLabelsTitle Heading to print over the column of row names.
#' @param showRowBracket Logical vector, should vertical bracket be printed to
#' the right of the plot (to label a row of plots in a final figure layout)?
#' @param rowTitle Label to be printed to the right of the plot, outside row
#' bracket if present, to label a row of plots in a final figure layout.
#' @param showColBracket Logical, should horizontal bracket be printed
#' below the plot (to label a column of plots in a final figure layout)?
#' @param colTitle Label to be printed below plot, outside col bracket if
#' present, to label a column of plots in a final figure layout.
#' @param plotTitle Plot title string, to be centered above plot.
#' @param xTitle X-axis title string, displayed left of \code{LeftLabels}.
#' @param yTitle Y-axis title string, displayed above \code{TopLabels}.
#' @param showColorLegend,showBubbleLegend Should plot include legends for color
#' scale/bubble size?
#' @param colorBreaks,sizeBreaks Character vectors of legend tick labels. The
#' min/max values will represent the range of color/size scales. If not supplied
#' then these will be generated using \code{\link[base]{pretty}}, to a range
#' outside data values (after values truncated to \code{colorLim}\\\code{sizeLim}
#' if applicable).
#' @param legendHeight Numeric, preferred height of legends in multiples of
#' \code{unitBase}. This will be supplied to  \code{\link[base]{pretty}}
#' function. If {sizeBreaks} vector is supplied then this will be ignored and
#' legend height will be \code{length(sizeBreaks)}.
#' @param legendTitles Character vector of legend titles. First string should be
#' the size legend title, second for the color legend.
#'
#' @return
#' This function creates a bubbleHeatmap using the \code{grid} graphics package.
#' It does not draw any output but produces a \code{\link[grid]{gTree}} object
#' which can be drawn using the \code{\link[grid]{grid.draw}} function. Returned
#' trees always include a \code{\link[grid]{viewport}} object with a 7x6 layout
#' and a given plot element is always positioned in the same cell, to assist
#' with aligning elements when combining multiple plots in a single figure (see
#' vignette, \code{\link{matchLayoutHeights}}, \code{\link{matchLayoutWidths}}).
#' NOTE: A cairo output device with gradient support is necessary to correctly
#' render the color legend.
#'
#' @examples
#' # Example 1 - Available plot features#
#' names <- list(paste0("leftLabels", 1:6), paste0("topLabels", 1:10))
#' colorMat <- matrix(rnorm(60), nrow = 6, ncol = 10, dimnames = names)
#' sizeMat <- matrix(abs(rnorm(60)), nrow = 6, ncol = 10, dimnames = names)
#' tree <- bubbleHeatmap(colorMat, sizeMat,
#'   treeName = "example",
#'   leftLabelsTitle = "leftLabelsTitle", showRowBracket = TRUE,
#'   rowTitle = "rowTitle", showColBracket = TRUE, colTitle = "colTitle",
#'   plotTitle = "plotTitle", xTitle = "xTitle", yTitle = "yTitle",
#'   legendTitles = c("legendTitles[1]", "legendTitles[2]")
#' )
#' grid.draw(tree)
#'
#' @family build functions
#' @import grid
#' @importFrom grDevices dev.off colorRampPalette
#' @export

####BubbleHeatmap Function####
bubbleHeatmap <- function(colorMat,
                          sizeMat = colorMat,
                          treeName = "Plot",
                          context = gpar(cex = 0.8),
                          colorSeq = c("#053061", "#2166AC", "#4393C3", "#92C5DE",
                                       "#D1E5F0", "#FDDBC7", "#F4A582", "#D6604D",
                                       "#B2182B", "#67001F"),
                          unitBase = unit(0.5, "cm"),
                          diameter = 0.8,
                          colorLim = c(NA, NA),
                          sizeLim = c(NA, NA),
                          showTopLabels = TRUE,
                          showLeftLabels = TRUE,
                          leftLabelsTitle = FALSE,
                          showRowBracket = FALSE,
                          rowTitle = FALSE,
                          showColBracket = FALSE,
                          colTitle = FALSE,
                          plotTitle = FALSE,
                          xTitle = FALSE,
                          yTitle = FALSE,
                          showColorLegend = TRUE,
                          showBubbleLegend = TRUE,
                          colorBreaks = NULL,
                          sizeBreaks = NULL,
                          legendHeight = 8,
                          legendTitles = c(expression("-log"[10]*"P"), "Estimate (SD)")) {
  #### Check input####
  # Check colorMat/sizeMat are numerical matrices#
  if (missing(colorMat)) stop("colorMat argument is required")
  if (is.null(sizeMat)) sizeMat <- colorMat
  if (!all(is.numeric(colorMat), is.matrix(colorMat), is.numeric(sizeMat), is.matrix(sizeMat))) {
    stop("Color/Size lists must contain numeric matrices")
  }
  # Check dimensions and names match for color/size matrix pairs#
  checkMatch <- function(x, y) {
    t1 <- dim(x) == dim(y)
    t2 <- rownames(x) == rownames(y)
    t3 <- colnames(x) == colnames(y)
    all(c(t1, t2, t3))
  }
  if (!(all(checkMatch(colorMat, sizeMat)))) {
    stop("Dimensions and row/column names must match for color/size matrices")
  }
  #### Prep Data ####
  # If min or max limits are preset for color or size then edit any values outside limits#
  sizeMat <- signif(sizeMat, 4)
  if (!all(is.na(c(colorLim, sizeLim)))) {
    applyLim <- function(x, min, max) {
      if (!is.na(min)) x[x < min] <- min
      if (!is.na(max)) x[x > max] <- max
      return(x)
    }
    colorMat <- applyLim(colorMat, colorLim[1], colorLim[2])
    sizeMat <- applyLim(sizeMat, sizeLim[1], sizeLim[2])}
  #If data limits not preset then set using min/max across all plots#
  if(any(is.na(c(colorLim, sizeLim)))) {
    if(is.na(c(colorLim[1]))){colorLim[1] <- min(colorMat[is.finite(colorMat)])}
    if(is.na(c(colorLim[2]))){colorLim[2] <- max(colorMat[is.finite(colorMat)])}
    if(is.na(c(sizeLim[1]))){sizeLim[1] <- min(sizeMat[is.finite(sizeMat)])}
    if(is.na(c(sizeLim[2]))){sizeLim[2] <- max(sizeMat[is.finite(sizeMat)])}}
  #If scale limits are preset check they are outside data limits#
  if (!is.null(sizeBreaks)) {
    if (sizeBreaks[1] > sizeLim[1] || tail(sizeBreaks, 1) < sizeLim[2]) {
      warning("Values in size matrix or specified in sizeLim
              are outside limits given by sizeBreaks!")
    }
  }
  if (!is.null(colorBreaks)) {
    if (colorBreaks[1] > colorLim[1] || tail(colorBreaks, 1) < colorLim[2]) {
      warning("Values in color matrix or specified in colorLim
              are outside limits given by colorBreaks!")
    }
  }
  #If scale limits are not preset then generate from data limits#
  #& update data limits to match scale limits#
  if (is.null(sizeBreaks)) {
    sizeBreaks <- pretty(c(sizeLim[1], sizeLim[2]), legendHeight)
    if (sizeBreaks[1] > sizeLim[1]) sizeBreaks <-
        c(sizeBreaks[1] - (sizeBreaks[2] - sizeBreaks[1]), sizeBreaks)
    if (tail(sizeBreaks, 1) < sizeLim[2]) sizeBreaks <-
        c(sizeBreaks, tail(sizeBreaks, 1) + (sizeBreaks[2] - sizeBreaks[1]))
    sizeLim <- c(sizeBreaks[1], tail(sizeBreaks, 1))
  }
  if (is.null(colorBreaks)) {
    if (!(is.null(sizeBreaks))) {
      legendHeight <- length(sizeBreaks)
    }
    colorBreaks <- pretty(c(colorLim[1], colorLim[2]), legendHeight)
    if (colorBreaks[1] > colorLim[1]) {
      colorBreaks <- c(colorBreaks[1] - (colorBreaks[2] - colorBreaks[1]), colorBreaks)
    }
    if (tail(colorBreaks, 1) < colorLim[2]) {
      colorBreaks <- c(colorBreaks, tail(sizeBreaks, 1) + (colorBreaks[2] - colorBreaks[1]))
    }
    colorLim <- c(colorBreaks[1], tail(colorBreaks, 1))
  }
  #Extract and scale data vectors, assign colors#
  colorSeq <- grDevices::colorRampPalette(colorSeq)(200)
  bColor <- colorSeq[floor((199.99 /
                        (colorLim[2] - colorLim[1]) *
                        (colorMat[is.finite(colorMat)] - colorLim[1])) + 1)]
  area <- 3.14 * (diameter / 2)^2
  bSize <- ((area / (sizeLim[2] - sizeLim[1])) * (sizeMat[is.finite(sizeMat)] - sizeLim[1]))
  pos <- which(is.finite(colorMat), arr.ind = TRUE)
  treeName <- gsub(" ", "", treeName)
  #### Make children####
  children <- makeChildren(bSize, bColor, pos, unitBase, treeName,
    topLabels = colnames(colorMat),
    leftLabels = rownames(colorMat),
    ncol = ncol(colorMat),
    nrow = nrow(colorMat), showTopLabels, showLeftLabels,
    leftLabelsTitle, showRowBracket, rowTitle,
    showColBracket, colTitle, plotTitle, xTitle, yTitle
  )
  # Create Legends#
  if (showBubbleLegend == TRUE || showColorLegend == TRUE) {
    legends <- bubbleHMLegends(sizeBreaks, colorBreaks, colorSeq, unitBase, diameter,
                               context = gpar(), legendHeight, legendTitles,
                               showBubbleLegend, showColorLegend)
    legends$vp <- vpPath(paste0(treeName, "VP"))
    legends$childrenvp$parent <- modifyVP(legends$childrenvp$parent,
                                          posCol = c(5, 5), posRow = c(4, 4))
  }
  #### Build tree####
  tree <- gTree(
    name = treeName, cl = "bubbleHeatmap", children = children$grobList,
    childrenvp = vpTree(viewport(layout = grid.layout(nrow = 5, ncol = 5), gp = context,
                                 name = paste0(treeName, "VP")), children = children$viewports)
  )
  if(showBubbleLegend == TRUE || showColorLegend == TRUE){
    tree <- addGrob(tree, legends)}

  # Design layout#
  tree <- getLayoutSizes(tree, unitBase)
  return(tree)
}

####Documentation: bubbleHeatmapList####

#' Output List of bubbleHeatmap Plots.
#'
#' Convenient wrapper for \code{bubbleHeatmap} and \code{bubbleHMLegend} for
#' easy output of multiple plots from lists of arguments.
#'
#' @param inputList A list of two lists, trees and legend. trees should contain
#' a list of lists of arguments for multiple calls to bubbleHeatmap and legend
#' should contain a list of arguments for a call to bubbleHMLegends. This
#' object can be generated automatically using \code{\link{multiPlotInput}}.
#'
#' @return A list of bubbleHeatmap \code{\link[grid]{gTree}} objects. Any
#' legends specified by the legend list will also be produced as separate trees
#' in the output list.
#'
#' @family build functions
#' @export

####bubbleHeatmapList function####
bubbleHeatmapList <- function(inputList) {
  treeList <- lapply(inputList$trees, function(x) {
    do.call(bubbleHeatmap, x)
  })
  if (inputList$legends$showColorLegend != FALSE | inputList$legends$showBubbleLegend != FALSE) {
    legends <- do.call(bubbleHMLegends, inputList$legends)
    treeList <- c(treeList, Legends = list(legends))
  }
  return(treeList)
}

####Documenation: bubbleHMLegends####
#' Build Legend Trees for bubbleHeatmap Plots
#'
#' Generate a bubble size legend and/or color legend for bubbleHeatmap plots.
#'
#' @inheritParams bubbleHeatmap
#'
#' @family build functions
#' @return A \code{\link[grid]{gTree}} object representing the bubbleHeatmap
#' legends requested by the showBubbleLegend and showColorLegend parameters.
#' @param orientation one of "horizontal" or "vertical", (default horizontal)
#' indicating whether legends should be arranged stacked or side by side.
#' Ignored if only one legend is required.
#' @param name character string naming the gTree containing the function output.
#' @examples
#' legends <- bubbleHMLegends(
#'   sizeBreaks = seq(0, 80, 10), colorBreaks = seq(-2, 2, 0.5), diameter = 0.8,
#'   context = gpar(cex = 0.8), colorSeq = c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0",
#'   "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"), unitBase = unit(0.5, "cm"),
#'   legendHeight = 9, legendTitles = c("Estimate", "NegLog10P"))
#' grid.draw(legends)
#' @export

####bubbleHMLegends function####
bubbleHMLegends <- function(sizeBreaks,
                            colorBreaks,
                            colorSeq,
                            unitBase,
                            diameter,
                            context,
                            legendHeight,
                            legendTitles,
                            showBubbleLegend = TRUE,
                            showColorLegend = TRUE,
                            name = "Legends",
                            orientation = c("horizontal", "vertical")) {
  #### Print Device Warning ####
  if(getOption("bubbleLegends.device.warning", TRUE)){
    warning("\nbubbleHeatmap must be drawn on a graphics device with gradient
            fill support (e.g. cairo_pdf or png(type = 'cairo')) in order to
            render the color legend correctly.\n
            This warning will be shown  once per session and may be disabled
            by setting options('bubbleLegends.device.warning' = FALSE)",
            call. = FALSE)
    options('bubbleLegends.device.warning' = FALSE)}
  orientation = match.arg(orientation)
  #### Set Limits ####
  sizeLim <- c(sizeBreaks[1], tail(sizeBreaks, 1))
  colorLim <- c(colorBreaks[1], tail(colorBreaks, 1))
  #### Build grob tree####
  children <- gList()
  childrenvp <- vpList()
  if(all(showBubbleLegend, showColorLegend)){
    if(orientation == "horizontal"){
      BL_pos <- c(2, 2)
      CL_pos <- c(2, 4)
      layout <- grid.layout(nrow = 3, ncol = 5)
    }else if(orientation == "vertical"){
      BL_pos <- c(2, 2)
      CL_pos <- c(4, 2)
      layout <- grid.layout(nrow = 5, ncol = 3)
    }
  }else if(showBubbleLegend){
    BL_pos <- c(2, 2)
    layout <- grid.layout(nrow = 3, ncol = 3)
  }else if(showColorLegend){
    CL_pos <- c(2, 2)
    layout <- grid.layout(nrow = 3, ncol = 3)
  }
  #### Build Bubble Legend####
  if (showBubbleLegend == TRUE) {
    start <- unit(0.5, "npc") - ((unitBase + unit(max(sapply(sizeBreaks, nchar)), "char")) / 2)
    area <- 3.14 * (diameter / 2)^2
    bl.bSize <- (area / (sizeLim[2] - sizeLim[1]) * (sizeBreaks - sizeLim[1]))
    children[["BubbleLegend"]] <-
      gTree(name = "BubbleLegend",
            cl = "bubbleLegend",
            vp = vpPath(name, "BubbleLegend"),
            children = gList(
              rectGrob(x = start,
                 y = c(0.5:(length(sizeBreaks) - 0.5)) * unitBase,
                 height = unitBase,
                 width = unitBase,
                 name = "BLGridSq",
                 just = c("left", "centre")),
              circleGrob(x = start + 0.5 * unitBase,
                y = c(0.5:(length(sizeBreaks) - 0.5)) * unitBase,
                r = sqrt(bl.bSize / pi) * unitBase,
                gp = gpar(col = "#707070", fill = "#707070"),
                name = "BLBubbles"),
              textGrob(sizeBreaks,
                       x = start + unitBase + unit(1, "char"),
                       y = seq(0.5, length(sizeBreaks) - 0.5, 1) * unitBase,
                       just = c("left", "centre"), name = "BLLabels"),
              textGrob(legendTitles[1],
                       x = 0.5,
                       y = grobY("BLGridSq", 90) + unit(1, "line"),
                       just = c("centre", "bottom"),
                       name = "BLTitle")))
    childrenvp[["BubbleLegend"]] <- viewport(name = "BubbleLegend",
                                             layout.pos.row = BL_pos[1],
                                             layout.pos.col = BL_pos[2])
  }
  #### Calculate positions of labels & color blocks####
  if (showColorLegend == TRUE) {
    colorSeq <- colorRampPalette(colorSeq)(200)
    cN <- length(colorSeq)
    segY <- (seq(0, legendHeight, legendHeight / cN)[1:cN]) * unitBase
    labelsY <- (segY[floor(((cN - 0.01) /
                (colorLim[2] - colorLim[1]) * (colorBreaks - colorLim[1])) + 1)] +
                  ((legendHeight / cN) / 2) * unitBase)
    #### Build Color Legend####
    start <- unit(0.5, "npc") -
      ((unitBase + unit(max(sapply(colorBreaks, nchar)) - 2, "char")) / 2)
    children[["ColorLegend"]] <-
      gTree(name = "ColorLegend",
            cl = "colorLegend",
            vp = vpPath(name, "ColorLegend"),
            children = gList(
              rectGrob(x = start,
                       y = 0,
                       height = legendHeight * unitBase,
                       width = unitBase,
                       just = c("left", "bottom"),
                       name = "CLOutline"),
              segmentsGrob(x0 = start + unitBase,
                           x1 = start + unitBase + unit(0.5, "char"),
                           y0 = labelsY,
                           y1 = labelsY,
                           name = "CLTicks"),
              textGrob(x = start + unitBase + unit(1, "char"),
                       y = labelsY, colorBreaks,
                       just = c("left", "centre"),
                       name = "CLLabels"),
              textGrob(legendTitles[2],
                       x = 0.5,
                       y = grobY("CLOutline", 90) + unit(1, "line"),
                       just = c("centre", "bottom"),
                       name = "CLTitle")
    ))
    children[["ColorLegend"]] <- editGrob(children[["ColorLegend"]],
                                          "CLOutline",
                                          gp = gpar(fill = linearGradient(colours = colorSeq)))
    childrenvp[["ColorLegend"]] <- viewport(name = "ColorLegend",
                                            layout.pos.row = CL_pos[1],
                                            layout.pos.col = CL_pos[2])
  }


  #### Set layout sizes function####
  tree <- gTree(name = name,
                children = children,
                childrenvp = vpTree(
                  viewport(layout = layout,
                  name = name, gp = context), children = childrenvp))
  convertUnit(grobWidth(tree), "cm")
  if(layout$ncol == 3){
    tree$childrenvp$parent$layout$widths <- unit(c(1.5, 1, 1.5), "null")
  }else{
    tree$childrenvp$parent$layout$widths <- unit(c(1.5, 1, 1, 1, 1.5), "null")}
  if(layout$nrow == 3){
    tree$childrenvp$parent$layout$heights <- unit(c(1.5, 1, 1.5), "null")
  }else{
    tree$childrenvp$parent$layout$heights <- unit(c(1.5, 1, 1, 1, 1.5), "null")}
  if(showBubbleLegend == TRUE){
    tree$childrenvp$parent$layout$heights[BL_pos[1]] <-
      convertUnit(grobHeight(tree$children$BubbleLegend), "cm")
    tree$childrenvp$parent$layout$widths[BL_pos[2]] <-
      convertUnit(grobWidth(tree$children$BubbleLegend), "cm")}
  if(showColorLegend == TRUE){
    tree$childrenvp$parent$layout$heights[CL_pos[1]] <-
      convertUnit(grobHeight(tree$children$ColorLegend), "cm")
    tree$childrenvp$parent$layout$widths[CL_pos[2]] <-
      convertUnit(grobWidth(tree$children$ColorLegend), "cm")}
  dev.off()
  tree$childrenvp$parent$width <-
    sum(tree$childrenvp$parent$layout$widths) + 1.5 * unitBase
  tree$childrenvp$parent$height <-
    sum(tree$childrenvp$parent$layout$heights) + 1.5 * unitBase
  return(tree)
}


#### Make children function ####
makeChildren <- function(bSize, bColor, pos, unitBase, treeName,
                         topLabels, leftLabels, ncol, nrow,
                         showTopLabels, showLeftLabels, leftLabelsTitle,
                         showRowBracket, rowTitle, showColBracket, colTitle,
                         plotTitle, xTitle, yTitle) {
  # Initialise viewport list#
  glC <- gList()
  vpC <- vpList()
  # Create main plot grobs#
  glC[["GridSq"]] <- rectGrob((pos[, "col"] - 0.5) * unitBase,
                              unit(1, "npc") - ((pos[, "row"] - 0.5) * unitBase),
                              height = unitBase,
                              width = unitBase,
                              name = "GridSq",
                              vp = vpPath(paste0(treeName, "VP"), "PlotGrid"))
  glC[["Bubbles"]] <- circleGrob((pos[, "col"] - 0.5) * unitBase,
                                 unit(1, "npc") - ((pos[, "row"] - 0.5) * unitBase),
                                 r = sqrt(bSize / pi) * unitBase,
                                 gp = gpar(col = bColor, fill = bColor),
                                 name = "Bubbles", vp = vpPath(paste0(treeName, "VP"), "PlotGrid"))
  vpC[["PlotGrid"]] <- viewport(layout.pos.col = 3,
                                layout.pos.row = 4,
                                x = 0,
                                y = 1,
                                name = "PlotGrid",
                                just = c("left", "top"))
  # Create grobs above plot#
  if (showTopLabels != FALSE) {
    glC[["TopLabels"]] <- textGrob(topLabels,
                                   x = seq(0.5, ncol - 0.5, by = 1) * unitBase,
                                   unit(0.35, "line"),
                                   rot = 45,
                                   just = c("left", "bottom"),
                                   name = "TopLabels",
                                   vp = vpPath(paste0(treeName, "VP"), "TopLabels"))
    vpC[["TopLabels"]] <- viewport(layout.pos.col = c(3, 4),
                                   layout.pos.row = 3,
                                   x = 0,
                                   y = 1,
                                   name = "TopLabels",
                                   just = c("left", "bottom"))
  }
  if (xTitle != FALSE) {
    glC[["XTitle"]] <- textGrob(xTitle,
                                x = 0.5,
                                y = 0.5,
                                name = "XTitle",
                                vp = vpPath(paste0(treeName, "VP"), "XTitle"))
    vpC[["XTitle"]] <- viewport(layout.pos.col = 3,
                                layout.pos.row = 2,
                                x = 0.5,
                                y = 0.5,
                                name = "XTitle",
                                just = c("centre", "bottom"))
  }
  if (plotTitle != FALSE) {
    glC[["PlotTitle"]] <- textGrob(plotTitle, x = 0.5, y = unit(2, "line"), name = "PlotTitle",
                                   vp = vpPath(paste0(treeName, "VP"), "PlotTitle"))
    vpC[["PlotTitle"]] <- viewport(layout.pos.col = 3, layout.pos.row = 1, x = 0.5, y = 0.5,
                                   name = "PlotTitle", just = c("centre", "bottom"))
  }
  # Create grobs left of plot#
  if (yTitle != FALSE) {
    glC[["YTitle"]] <- textGrob(yTitle, x = 0.5, y = 0.5, name = "YTitle",
                                rot = 270, vp = vpPath(paste0(treeName, "VP"), "YTitle"))
    vpC[["YTitle"]] <- viewport(layout.pos.col = 1, layout.pos.row = 4, x = 0.5,
                                y = 0.5, name = "YTitle", just = c("right", "centre"))
  }
  if (showLeftLabels != FALSE) {
    glC[["LeftLabels"]] <- textGrob(leftLabels, x = unit(1, "npc") - unit(0.5, "char"),
                                    y = unit(1, "npc") - (seq(0.5, nrow - 0.5, by = 1) * unitBase),
                                    just = c("right", "centre"), name = "LeftLabels",
                                    vp = vpPath(paste0(treeName, "VP"), "LeftLabels"))
    vpC[["LeftLabels"]] <- viewport(layout.pos.col = 2, layout.pos.row = 4, x = 0.5, y = 0.5,
                                    name = "LeftLabels", just = c("centre", "centre"))
  }
  if (leftLabelsTitle != FALSE) {
    glC[["LeftLabelsTitle"]] <- textGrob(leftLabelsTitle, x = unit(1, "npc") - unit(0.5, "char"),
                                         y = 0.5 * unitBase,
                                         just = c("right", "centre"), name = "LeftLabelsTitle",
                                         gp = gpar(fontface = "bold"),
                                         vp = vpPath(paste0(treeName, "VP"), "LeftLabelsTitle"))
    vpC[["LeftLabelsTitle"]] <- viewport(layout.pos.col = 2, layout.pos.row = 3, x = 1, y = 0,
                                         name = "LeftLabelsTitle", just = c("right", "bottom"))
  }
  # Create grobs right of plot
  if (showRowBracket != FALSE) {
    RB <- list(x = c(0, 0.6, 0.6, 0.6, 1, 0.6, 0.6, 0.6, 0),
               y = c(1, 1, 1, 0.5, 0.5, 0.5, 0, 0, 0),
               yy = c(0, 0, -1, 0.5, 0, -0.5, 1, 0, 0),
               s = c(1, 0.5, 1, 0.3, 0, 0.3, 1, 0.5, 1))
    glC[["RowBracket"]] <- xsplineGrob(x = (RB$x * unitBase) + 0.5 * unitBase,
                                       y = unit(RB$y, "npc") + RB$yy * unitBase,
                                       shape = RB$s, name = "RowBracket",
                                       vp = vpPath(paste0(treeName, "VP"), "RowTitle"))
    if (rowTitle != FALSE) {
      glC[["RowTitle"]] <- textGrob(rowTitle, 2 * unitBase, 0.5, rot = 270,
                                    name = "RowTitle",
                                    vp = vpPath(paste0(treeName, "VP"), "RowTitle"))
    } else if (rowTitle != FALSE) {
      glC[["RowTitle"]] <- textGrob(rowTitle, 0.5 * unitBase, 0.5,
                                    rot = 270, name = "RowTitle",
                                    vp = vpPath(paste0(treeName, "VP"), "RowTitle"))
    }
    if (showRowBracket != FALSE || rowTitle != FALSE) {
      vpC[["RowTitle"]] <- viewport(layout.pos.col = 4,
                                    layout.pos.row = 4,
                                    name = "RowTitle",
                                    just = c("left", "centre"))
    }
  }
  # Create grobs below plot
  if (showColBracket != FALSE) {
    CB <- list(x = c(0, 0, 0, 0.5, 0.5, 0.5, 1, 1, 1),
               xx = c(0, 0, 1, -0.3, 0, 0.3, -1, 0, 0),
               y = c(0, 0.4, 0.4, 0.4, 1, 0.4, 0.4, 0.4, 0),
               s = c(1, 0.5, 1, 0.3, 0, 0.3, 1, 0.5, 1))
    glC[["ColBracket"]] <- xsplineGrob(x = unit(CB$x, "npc") + CB$xx * unitBase,
                                       y = unit(1, "npc") - (CB$y * unitBase) - 0.5 * unitBase,
                                       shape = CB$s, name = "ColBracket",
                                       vp = vpPath(paste0(treeName, "VP"), "ColTitle"))
  }
  if (colTitle != FALSE) {
    glC[["ColTitle"]] <- textGrob(colTitle, 0.5, unit(1, "npc") - 2 * unitBase,
                                  name = "ColTitle",
                                  vp = vpPath(paste0(treeName, "VP"), "ColTitle"))
  } else if (colTitle != FALSE) {
    glC[["ColTitle"]] <- textGrob(colTitle, 0.5 * unitBase, 0.5, rot = 270,
                                  name = "ColTitle",
                                  vp = vpPath(paste0(treeName, "VP"), "ColTitle"))
  }
  if (showColBracket != FALSE || colTitle != FALSE) {
    vpC[["ColTitle"]] <- viewport(layout.pos.col = 3,
                                  layout.pos.row = 5,
                                  name = "ColTitle",
                                  just = c("left", "centre"))
  }
  return(list(grobList = glC, viewports = vpC))
}
