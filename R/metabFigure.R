#' Create Nightingale Figure From Template
#'
#' Produce a single \code{\link[grid]{gTree}} object representing plots arranged
#' according to the provided Nightingale metabolomics figure template.
#'
#' @inheritParams bubbleHeatmap
#'
#' @param data Nightingale results data merged with the ntngale225 or ntngale249
#' template.
#' @param colorValue String naming the variable of \code{data} containing the
#'     values to be plotted on the color scale.
#' @param sizeValue String naming the variable of \code{data} containing the
#'     values to scale bubble sizes.
#' @param legendHeight Numeric, preferred height of legends in multiples of
#' 0.5cm. This will be supplied to pretty function.
#'
#' @details
#' Quick function to produce a final Nightingale figure from results data merged
#' with one of the supplied template datasets. This function wraps the functions
#' \code{\link{formatData}}, \code{\link{multiPlotInput}},
#' \code{\link{bubbleHeatmapList}} and \code{\link{metabFigurePlot}} applying
#' default settings.
#'
#' @return  Produces a complete metabolomics figure consisting of 14
#' bubbleHeatmap blocks in a pre-set layout. Output is not drawn but returned as
#' a \code{\link[grid]{gTree}} object which can be edited or drawn using
#' the \code{\link[grid]{grid.draw}} function. NOTE: A cairo output device with
#' gradient support is necessary to correctly render the color legend.
#'
#' @examples
#' myData <- merge_template(cetp, "ckb_id")
#' metabTree <- metabFigure(myData)
#' grid.draw(metabTree)
#'
#' @export


metabFigure <- function(data, colorValue="estimate", sizeValue = "negLog10P", legendHeight=8, context = gpar(cex = 0.8),
                        unitBase = unit(0.5, "cm"), colorLim = c(NA, NA), sizeLim = c(NA, NA)){
  gData <- formatData(data, colorValue = colorValue, sizeValue = sizeValue)
  tInput <- multiPlotInput(colorList=gData$colorList, sizeList=gData$sizeList, colorLim = colorLim,
                           sizeLim = sizeLim, legendHeight=8, context=context, unitBase=unitBase)
  tList <- bubbleHeatmapList(tInput)
  metabTree <- metabFigurePlot(tList)}

metabFigure <- function(data,
                        colorValue="estimate",
                        sizeValue = "negLog10P",
                        legendHeight=8,
                        context = gpar(cex = 0.8),
                        unitBase = unit(0.5, "cm"),
                        colorLim = c(NA, NA),
                        sizeLim = c(NA, NA)){
  gData <- formatData(data, colorValue = colorValue, sizeValue = sizeValue)
  tInput <- multiPlotInput(colorList=gData$colorList, sizeList=gData$sizeList,
                           colorLim = colorLim, sizeLim = sizeLim,
                           legendHeight=8, context=context, unitBase=unitBase)
  tList <- bubbleHeatmapList(tInput)
  metabTree <- metabFigurePlot(tList)
}


#' Arrange List of bubbleHeatmap Plots According to Preset Nightingale Template.
#'
#' Produce a single \code{\link[grid]{gTree}} object representing plots arranged
#' according to the provided Nightingale metabolomics figure template.
#'
#' @param treeList List of bubbleHeatmap trees. This function expects a treeList
#' of particular length, containing trees with specified names and dimensions.
#' The easiest way to generate this is via the \code{\link{formatData}},
#' \code{\link{multiPlotInput}} and \code{\link{bubbleHeatmapList}} functions.
#' See vignette for full details of this workflow.
#'
#' @return A \code{\link[grid]{gTree}} object representing the input
#' bubblehHeatmaps arranged into a Nightingale metabolomics figure.
#' @export

metabFigurePlot <- function(treeList) {

  #Get unit size
  unitBase <- treeList[[1]]$children$GridSq$y[1] - treeList[[1]]$children$GridSq$y[2]

  # List layout groupings#
  pG <- list(
    pH = c("Absolute Measures", "Aggregate", "Apolipoproteins"),
    pW = c("Absolute Measures", "Ratio to Total Lipids", "Other Lipids", "Glycolysis"),
    p1 = "Absolute Measures",
    p2 = c("Aggregate", "Apolipoproteins", "Mean Diameter"),
    p2B = c("Apolipoproteins", "Mean Diameter"),
    p3 = "Ratio to Total Lipids",
    p4 = "Fatty Acids",
    p5A = c("Other Lipids", "Glycolysis"),
    p5B = c("Ketones", "Fluid Balance", "Inflammation"),
    p5C = c("Amino Acids", "BCAAs"),
    p6 = "Legends")

  #Split Fatty Acids
  if("PUFA:MUFA" %in% treeList$`Fatty Acids`$children$TopLabels$label){
    len <- length(treeList$`Fatty Acids`$children$TopLabels$x)
    treeList$`Fatty Acids`$children$TopLabels$x[(len-1):len] <-
      treeList$`Fatty Acids`$children$TopLabels$x[(len-1):len] + 0.5*unitBase
    len <- length(treeList$`Fatty Acids`$children$GridSq$x)
    treeList$`Fatty Acids`$children$GridSq$x[(len-1):len] <-
      treeList$`Fatty Acids`$children$GridSq$x[(len-1):len] + 0.5*unitBase
    len <- length(treeList$`Fatty Acids`$children$Bubbles$x)
    treeList$`Fatty Acids`$children$Bubbles$x[(len-1):len] <-
      treeList$`Fatty Acids`$children$Bubbles$x[(len-1):len] + 0.5*unitBase
    treeList$`Fatty Acids` <- getLayoutSizes(treeList$`Fatty Acids`, unitBase)}

  ##### Align widths#####
  treeList[pG$p1]$`Absolute Measures`$childrenvp$parent$layout$widths[4] <- unit(1, "null")
  treeList[pG$pW] <- matchLayoutWidths(treeList[pG$pW], 1, 2)
  treeList[c(pG$p1, pG$p3)] <- matchLayoutWidths(treeList[c(pG$p1, pG$p3)], 1, 5)
  treeList[c(pG$p4, pG$p2B)] <- matchLayoutWidths(treeList[c(pG$p4, pG$p2B)], 4, 4)
  treeList[pG$p2B] <- matchLayoutWidths(treeList[pG$p2B], 1, 2)
  treeList[pG$p5B] <- matchLayoutWidths(treeList[pG$p5B], 1, 2)
  treeList[pG$p5C] <- matchLayoutWidths(treeList[pG$p5C], 1, 2)
  treeList[pG$p2[1]][[1]]$childrenvp$parent$layout$widths[4] <- unit(0, "cm")
  treeList[pG$p4][[1]]$childrenvp$parent$layout$widths[1] <- unit(1, "null")

  ##### Align heights#####
  treeList[pG$pH] <- matchLayoutHeights(treeList[pG$pH], 1, 3)
  treeList[pG$pH[1:2]] <- matchLayoutHeights(treeList[pG$pH[1:2]], 1, 5)
  treeList[pG$p4][[1]]$childrenvp$parent$layout$heights[1] <- unit(1, "null")
  treeList[pG$p3][[1]]$childrenvp$parent$layout$heights[1] <- unit(1, "null")

  # Combine apolipoproteins & mean diameter (panel 2B)
  p2B <- arrangePanels("Panel2B", subTrees = treeList[pG$p2B],
                       nrow = 2, ncol = 1, just = c("right", "top"),
                       spacer = unit(0, "null"))
  p2B <- gTree(name="Panel2B", children=do.call(gList, p2B$grobList), childrenvp=p2B$vp)


  # Combine nine miscellaneous plots into one tree (panel 5)#
  treeList[pG$p5C[2]][[1]]$children$LeftLabelsTitle$gp <- gpar(fontface="italic")
  p5A <- arrangePanels("Misc1", subTrees = treeList[pG$p5A],
                       nrow = 2, ncol = 1, just = c(0, 0),
                       spacer = unit(1, "null"))
  p5B <- arrangePanels("Misc2", subTrees = treeList[pG$p5B],
                       nrow = 3, ncol = 1, just = c(0, 0),
                       spacer = unit(1, "null"))
  p5C <- arrangePanels("Misc3", subTrees = treeList[pG$p5C],
                       nrow = 2, ncol = 1, just = c(0, 0),
                       spacer = unit(1, "null"))
  p5 <- combinePanels("Misc", panels = list(p5A, p5B, p5C),
                      nrow = 1, ncol = 3, just = c(0, 0),
                      spacer = unit(1, "null"))

  # Arrange final panels
  metabPanel <-
    arrangePanels("metabTree",
                  subTrees = c(treeList[pG$p1],
                               treeList[pG$p2[1]],
                               LP_Other = list(p2B),
                               treeList[pG$p3],
                               treeList[pG$p4],
                               list(nullGrob()), Misc = list(p5),
                               treeList[pG$p6], list(nullGrob())),
                  nrow = 3, ncol = 3, spacer = unitBase)

  # Fine adjustments to alignment
  metabPanel$grobList[pG$p4][[1]]$childrenvp$parent$layout.pos.col <- as.integer(c(4, 6))
  metabPanel$grobList[pG$p4][[1]]$childrenvp$parent$valid.pos.col <- as.integer(c(4, 6))
  metabPanel$grobList[pG$p6][[1]]$childrenvp$parent$layout.pos.col <- as.integer(c(4, 6))
  metabPanel$grobList[pG$p6][[1]]$childrenvp$parent$valid.pos.col <- as.integer(c(4, 6))
  metabPanel$vp$layout$heights[3] <- 0.5*unitBase
  metabPanel$vp$layout$widths[4] <-
    sum(metabPanel$grobList[pG$p2[1]][[1]]$childrenvp$parent$layout$widths)
  metabPanel$vp$layout$widths[6] <-
    sum(metabPanel$grobList$LP_Other$childrenvp$layout$widths)
  metabPanel$vp$layout$widths[5] <- max(
    sum(metabPanel$grobList[pG$p4][[1]]$childrenvp$parent$layout$widths) -
      (sum(metabPanel$grobList[pG$p2[1]][[1]]$childrenvp$parent$layout$widths) +
         sum(metabPanel$grobList$LP_Other$childrenvp$layout$widths)),
    unit(1, "char"))
  metabTree <- gTree(name="metabTree", children=do.call(gList,
        metabPanel$grobList), childrenvp=metabPanel$vp)
return(metabTree)
}
