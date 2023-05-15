## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  bubbleLegends.device.warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(bubbleHeatmap)

## ----bubbleHeatmap------------------------------------------------------------
#Simulate data
names <- list(paste0("leftLabels", 1:6), paste0("topLabels", 1:10))
colorMat <- matrix(rnorm(60), nrow=6, ncol=10, dimnames = names)
sizeMat <- matrix(abs(rnorm(60)), nrow=6, ncol=10, dimnames = names)

#Create sample plot tree containing all elements
tree <-  bubbleHeatmap(colorMat, sizeMat, treeName = "example",
             leftLabelsTitle = "leftLabelsTitle", showRowBracket = T,
             rowTitle = "rowTitle", showColBracket = T, colTitle="colTitle",
             plotTitle="plotTitle", xTitle="xTitle", yTitle="yTitle",
             legendTitles = c("legendTitles[1]", "legendTitles[2]"))

## ----draw-plot1, dev = "png", dev.args = list(type = "cairo-png"), fig.width = 6.5, fig.height=3.5, out.width="100%"----
#Draw plot
grid.newpage()
grid.draw(tree)

## ----merge-data---------------------------------------------------------------
myData <- merge_template(cetp, "ckb_id")


## ----one-step-----------------------------------------------------------------
metabTree <- metabFigure(myData)

## ----format-data--------------------------------------------------------------
gridData <- formatData(myData, colorValue="estimate", sizeValue = "negLog10P", 
                       nightingale = TRUE)

## ----tree-input---------------------------------------------------------------
treeInput <- multiPlotInput(colorList=gridData$colorList, 
                            sizeList=gridData$sizeList, 
                            nightingale=TRUE, legendHeight=8)

## ----create-trees-------------------------------------------------------------
treeList <- bubbleHeatmapList(treeInput)

## ----assemble-figure----------------------------------------------------------
metabTree <- metabFigurePlot(treeList)

## ----plot2,  dev = "png", dev.args = list(type = "cairo-png"), fig.width = 8, fig.height=6, out.width="100%"----
grid.newpage()
grid.draw(metabTree)

