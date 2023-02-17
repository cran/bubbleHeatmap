#### Documentation: getLayoutSizes ####
#' Recalculate Layout Sizes for bubbleHeatmap gTree
#'
#' This function recalculates the viewport layout dimensions for a bubbleHeatmap
#' tree based on the sizes of the children grobs, and returns the updated tree.
#' This is useful for matching widths exactly to grob dimensions in an updated
#' drawing context, and for resetting any previous layout modifications.
#'
#' @param tree A bubbleHeatmap \code{\link[grid]{gTree}}.
#' @param unitBase The unitBase parameter used when the tree was created.
#' @return The input bubbleHeatmap \code{\link[grid]{gTree}} with viewport
#' dimensions matched to the size of the children grobs.
#' @importFrom grDevices dev.off
#' @export

#### getLayoutSizes function ####
getLayoutSizes <- function(tree, unitBase) {
  # Identify grobs in tree#
  grobs <- c("YTitle", "LeftLabels", "LeftLabelsTitle", "GridSq", "RowBracket",
             "RowTitle", "PlotTitle", "XTitle", "TopLabels", "ColBracket",
             "ColTitle", "Legends")
  isGrob <- sapply(grobs, FUN = function(x) {
    is.grob(tree$children[[x]])
  })
  # Collect grob widths and add spacers#
  gW <- unit(rep.int(0, length(grobs)), rep.int("cm", length(grobs)))
  gW[which(isGrob)] <- do.call(unit.c, lapply(grobs[which(isGrob)],
                                              function(x) grobWidth(
                                                tree$children[[x]])))
  gW[which(grobs %in% c("LeftLabels", "LeftLabelsTitle"))] <-
    gW[which(grobs %in% c("LeftLabels", "LeftLabelsTitle"))] + unit(0.5, "char")
  gW[which(grobs %in% c("RowBracket", "RowTitle", "TopLabels"))] <-
    gW[which(grobs %in% c("RowBracket", "RowTitle",
                          "TopLabels"))] + 0.5 * unitBase
  gW[which(grobs == "YTitle")] <-
    gW[which(grobs == "YTitle")] + unit(0.35, "line")
  if (isGrob[["Legends"]]) {
    gW[which(grobs == "Legends")] <-
      tree$children$Legends$childrenvp$parent$width
  }
  gW[which(!(isGrob))] <- unit(0, "cm")
  # Collect grob heights and add spacers#
  gH <- unit(rep.int(0, length(grobs)), rep.int("cm", length(grobs)))
  gH[which(isGrob)] <- do.call(unit.c, lapply(grobs[which(isGrob)],
                                              function(x) grobHeight(
                                                tree$children[[x]])))
  gH[which(grobs %in% c("XTitle", "TopLabels"))] <-
    gH[which(grobs %in% c("XTitle", "TopLabels"))] + unit(0.35, "line")
  gH[which(grobs %in% c("LeftLabels", "LeftLabelsTitle",
                        "ColBracket", "ColTitle"))] <-
    gH[which(grobs %in% c("LeftLabels", "LeftLabelsTitle",
                          "ColBracket", "ColTitle"))] + 0.5 * unitBase
  gH[which(grobs == "PlotTitle")] <-
    gH[which(grobs == "PlotTitle")] + unit(2, "lines")
  gH[which(!(isGrob))] <- unit(0, "cm")
  # Calculate row heights for parent viewport layout
  rowH <- unit.c(
    gH[which(grobs == "PlotTitle")],
    gH[which(grobs == "XTitle")],
    max(gH[which(grobs %in% c("TopLabels", "LeftLabelsTitle"))]),
    max(gH[which(grobs %in% c("YTitle", "LeftLabels", "GridSq", "RowTitle"))]),
    sum(gH[which(grobs %in% c("ColBracket", "ColTitle"))])
  )
  # Calculate column heights for parent viewport layout
  colW <- unit.c(
    gW[which(grobs == "YTitle")],
    max(gW[which(grobs %in% c("LeftLabels", "LeftLabelsTitle"))]),
    max(gW[which(grobs %in% c("PlotTitle", "XTitle", "GridSq", "ColTitle"))]),
    max(sum(gW[which(grobs %in% c("RowBracket", "RowTitle"))]),
        gW[which(grobs == "TopLabels")] - max(gW[which(grobs %in%
                                                c("PlotTitle", "XTitle",
                                                  "GridSq", "ColTitle"))])),
    gW[which(grobs == "Legends")]
  )
  # Convert measurements to cm to prevent recursion#
  convertUnit(grobWidth(tree), "cm")
  tree$childrenvp$parent$layout$widths <- convertUnit(colW, "cm")
  tree$childrenvp$parent$layout$heights <- convertUnit(rowH, "cm")
  dev.off()
  return(tree)
}

#### Documentation: matchLayoutHeights/Widths####
#' Match Viewport Layout Dimensions.
#'
#' Align the dimensions of the viewport layouts in a list of bubbleHeatmap
#' gTrees. The maximum height/width of a given row/column in any input tree will
#' be applied that row/column for all trees in the list. This enables the plots
#' to be aligned in mult-plot figure layouts, for example the "TopLabels"
#' element can be given the same height in two plots that are to be positioned
#' side by side.
#'
#' @param x A list of bubbleHeatmap trees
#' @param r1,r2 Numeric between 1 and 6, the first (r1) and last (r2) viewport
#' layout rows to be aligned.
#' @param c1,c2 Numeric between 1 and 9, the first (c1) and last (c2) viewport
#' layout columns to be aligned.
#'
#' @return The input list of bubbleHeatmap trees modified so that the maximum
#' height/width of a given row/column in any input tree is applied to that
#' row/column for all trees in the list, for the rows/columns specified by the
#' r/c parameters.
#'
#' @describeIn matchLayoutHeights Align layout row heights across multiple
#' bubbleHeatmap trees.
#' @export

#### matchLayout functions####
matchLayoutHeights <- function(x, r1 = 1, r2 = 5) {
  if(!is.null(x[[1]]$childrenvp$parent$layout$heights)){
    H <- lapply(x, function(x) x$childrenvp$parent$layout$heights)
  }else if(!is.null(x[[1]]$childrenvp$layout$heights)){
    H <- lapply(x, function(x) x$childrenvp$layout$heights)
  }else{stop("Cannot identify heights to match.")}
  H2 <- H[[1]]
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      H2 <- mapply(unit.c, H2, H[[i]], SIMPLIFY = FALSE)
      H2 <- do.call(unit.c, lapply(H2, max))
    }}
  for (i in 1:length(x)) {
    if(!is.null(x[[1]]$childrenvp$parent$layout$heights)){
      for (h in r1:r2) {x[[i]]$childrenvp$parent$layout$heights[h] <- H2[h]}
    }else if(!is.null(x[[1]]$childrenvp$layout$heights)){
      for (h in r1:r2) {x[[i]]$childrenvp$layout$heights[h] <- H2[h]}
    }
  }
  return(x)}

#' @describeIn matchLayoutHeights Align layout column widths across multiple
#' bubbleHeatmap trees.
#' @export
matchLayoutWidths <- function(x, c1 = 1, c2 = 5) {
  if(!is.null(x[[1]]$childrenvp$parent$layout$widths)){
    W <- lapply(x, function(x) x$childrenvp$parent$layout$widths)
  }else if(!is.null(x[[1]]$childrenvp$layout$widths)){
    W <- lapply(x, function(x) x$childrenvp$layout$widths)
  }else{stop("Cannot identify widths to match.")}
  W2 <- W[[1]]
  if (length(x) > 1) {
    for (t in 2:length(x)) {
      W2 <- mapply(unit.c, W2, W[[t]], SIMPLIFY = FALSE)
      W2 <- do.call(unit.c, lapply(W2, max))
    }}
  for (t in 1:length(x)) {
    if(!is.null(x[[1]]$childrenvp$parent$layout$widths)){
      for (w in c1:c2) {x[[t]]$childrenvp$parent$layout$widths[w] <- W2[w]}
    }else if(!is.null(x[[1]]$childrenvp$layout$widths)){
      for (w in c1:c2) {x[[t]]$childrenvp$layout$widths[w] <- W2[w]}
    }
  }
  return(x)}


####Documentation: arrangePanels####
#' Arrange Multiple bubbleHeatmap gTrees Inside a New Viewport.
#'
#' The arrangePanels function creates a new viewport with a grid layout, and
#' directs the input trees to be drawn at positions within the new layout. The
#' new layout is filled by rows.
#'
#' @param name Character string naming the new viewport (will have suffix "VP")
#' @param subTrees gTrees to be arranged in the new panel
#' @param nrow Numeric, number of rows in the new layout
#' @param ncol Numeric, number of columns in the new layout
#' @param spacer Grid \code{\link[grid]{unit}} object specifying size of space
#' left between plots.
#' @param margin Grid \code{\link[grid]{unit}} object specifying size of margin
#' around panel.
#' @param just Numeric or character vector specifying justification of tree
#' viewports within panel viewport layout cells.
#' See \code{\link[grid]{viewport}}.
#' @return List containing grobList (a list of modified gTrees) and vp, the new
#' viewport object.
#' @export

####arrangePanels function####
arrangePanels <- function(name, subTrees, nrow, ncol, spacer = unit(0, "cm"),
                          margin = unit(0, "cm"), just = c(0.5, 0.5)) {
  #### Set blank vector for widths and heights####
  panelRows <- rep(1:nrow, each = ncol)[1:length(subTrees)]
  panelCols <- rep(1:ncol, nrow)[1:length(subTrees)]
  lH <- unit(rep.int(0, (nrow * 2) + 1), rep.int("cm", (nrow * 2) + 1))
  lW <- unit(rep.int(0, (ncol * 2) + 1), rep.int("cm", (ncol * 2) + 1))
  #### Set layout widths and heights####
  getWidths <- function(x) {
    if (!is.null(x$childrenvp$parent$layout)) {
      return(sum(x$childrenvp$parent$layout$widths))
    } else if (!is.null(x$childrenvp$layout)) {
      return(sum(x$childrenvp$layout$widths))
    } else if ("null" %in% attr(x, "class") | !("grob" %in% attr(x, "class"))){
      unit(0, "cm")
    } else {
      grobWidth(x)
    }
  }
  getHeights <- function(x) {
    if (!is.null(x$childrenvp$parent$layout)) {
      return(sum(x$childrenvp$parent$layout$heights))
    } else if (!is.null(x$childrenvp$layout)) {
      return(sum(x$childrenvp$layout$heights))
    } else if ("null" %in% attr(x, "class") | !("grob" %in% attr(x, "class"))){
      unit(0, "cm")
    } else {
      grobWidth(x)
    }
  }
  widths <- do.call(unit.c, lapply(subTrees, getWidths))
  heights <- do.call(unit.c, lapply(subTrees, getHeights))
  lH[seq(from = 2, by = 2, length = nrow)] <- do.call(unit.c,
                                                  lapply(1:nrow, function(x) {
    return(max(heights[which(panelRows == x)]))
  }))
  lW[seq(from = 2, by = 2, length = ncol)] <- do.call(unit.c,
                                                  lapply(1:ncol, function(x) {
    return(max(widths[which(panelCols == x)]))
  }))
  if (nrow > 1) lH[seq(from = 3, by = 2, length = nrow - 1)] <- spacer
  if (ncol > 1) lW[seq(from = 3, by = 2, length = ncol - 1)] <- spacer
  lH[c(1, length(lH))] <- margin
  lW[c(1, length(lW))] <- margin
  #### Set vp variables per subtree####
  for (i in 1:length(subTrees)) {
    if ("childrenvp" %in% names(subTrees[[i]])) {
      subTrees[[i]]$vp <- vpPath(paste0(name, "VP"), subTrees[[i]]$vp$name)
      parent <- subTrees[[i]]$vp
      posCol <- c(panelCols[i] * 2, panelCols[i] * 2)
      posRow <- c(panelRows[i] * 2, panelRows[i] * 2)
      if ("vpTree" %in% class(subTrees[[i]]$childrenvp)) {
        subTrees[[i]]$childrenvp$parent <- modifyVP(
          subTrees[[i]]$childrenvp$parent,
          parent, just, posCol, posRow
        )
      } else {
        subTrees[[i]]$childrenvp <- modifyVP(
          subTrees[[i]]$childrenvp,
          parent, just, posCol, posRow
        )
      }
    } else if (!is.null(subTrees[[i]]$vp)) {
      subTrees[[i]]$vp <- vpPath(paste0(name, "VP"),
                                 paste0(gsub(" ", "",
                                             subTrees[[i]]$name), "VP"))
    } else if (!("null" %in% attr(subTrees[[i]], "class")) | ("grob" %in% attr(subTrees[[i]], "class"))){
      subTrees[[i]]$vp <- viewport(name = paste0(gsub(" ", "", subTrees[[i]]$name), "VP"),
                 layout.pos.col = panelCols[i] * 2,
                 layout.pos.row = panelRows[i] * 2)
    }
  }
  subTrees <- subTrees[(which(sapply(subTrees, FUN=function(x){!("null" %in% attr(x, "class"))})))]
  panelLayout <- grid.layout(nrow = (nrow * 2) + 1,
                             ncol = (ncol * 2) + 1, heights = lH, widths = lW)
  panelVP <- viewport(layout = panelLayout,
                        name = paste0(name, "VP"), height = sum(lH),
                        width = sum(lW))
  return(list(grobList = subTrees, vp = panelVP))
}

####Documentation: combinePanels####
#' Combine Multiple Panels In A New gTree.
#'
#' The combinePanels function creates a new gTree containing a viewport with a
#' grid layout, and arranges multiple panels (created with the
#' \code{\link{arrangePanels}} function) within that grid. The new layout is
#' filled by rows.
#'
#' @param name Character string naming the new tree and viewport (viewport will
#' have suffix "VP")
#' @param panels Panels to be arranged in the new tree
#' @param nrow Numeric, number of rows in the new layout
#' @param ncol Numeric, number of columns in the new layout
#' @param spacer Grid \code{\link[grid]{unit}} object specifying size of space
#' left between panels.
#' @param margin Grid \code{\link[grid]{unit}} object specifying size of margin
#' around tree.
#' @param just Numeric or character vector specifying justification of panel
#' viewports within new tree viewport layout cells.
#' See \code{\link[grid]{viewport}}.
#' @return New \code{\link[grid]{gTree}} object with children slot containing
#' all trees in the grobLists of input panels, and a childrenvp slot containing
#' all viewports in input panels arranged within a new parent viewport layout.
#' @importFrom methods is
#' @export

####combinePanels function####
combinePanels <- function(name, panels, nrow, ncol, spacer = unit(0, "cm"),
                          margin = unit(0, "cm"), just = c(0.5, 0.5)) {
  #### Separate grobs and viewports####
  if ("grobList" %in% names(panels)) {
    grobLists <- panels$grobList
  } else {
    grobLists <- do.call(c, lapply(panels, function(x) {
      return(x$grobList)
    }))
  }
  if ("vp" %in% names(panels)) {
    subVP <- panels$vp
  } else {
    subVP <- lapply(panels, function(x) {
      return(x$vp)
    })
  }
  #### Split viewport trees####
  if (is(subVP, "viewport")) {
    subVP <- list(subVP)
  }
  splitVP <- function(x) {
    if ("vpTree" %in% class(x)) {
      return(list(P = x$parent, C = x$children))
    } else {
      return(list(P = x))
    }
  }
  subVP <- lapply(subVP, splitVP)
  #### Set new vp path for grobs####
  for (i in 1:length(grobLists)) {
    grobLists[[i]]$vp <- vpPath(paste0(name, "VP"), grobLists[[i]]$vp$path,
                                grobLists[[i]]$vp$name)
    #### Build viewport tree####
    if (!is.null(grobLists[[i]]$childrenvp)) {
      grobLists[[i]]$childrenvp$parent$parent <- grobLists[[i]]$vp
    }
  }
  #### Set vectors to allocate panels to rows and columns####
  panelRows <- rep(1:nrow, each = ncol)[1:length(panels)]
  panelCols <- rep(1:ncol, nrow)[1:length(panels)]
  #### Initiate blank vectors to hold master layout widths and heights####
  lH <- unit(rep.int(0, (nrow * 2) + 1), rep.int("cm", (nrow * 2) + 1))
  lW <- unit(rep.int(0, (ncol * 2) + 1), rep.int("cm", (ncol * 2) + 1))
  #### Get master layout widths and heights####
  getWidths <- function(x) {
    if (!is.null(x$P$layout)) {
      return(sum(x$P$layout$widths))
    } else {
      x$P$width
    }
  }
  getHeights <- function(x) {
    if (!is.null(x$childrenvp$parent$layout)) {
      return(sum(x$childrenvp$parent$layout$heights))
    } else {
      x$P$height
    }
  }
  widths <- do.call(unit.c, lapply(subVP, getWidths))
  heights <- do.call(unit.c, lapply(subVP, getHeights))
  lH[seq(from = 2, by = 2, length = nrow)] <-
    do.call(unit.c, lapply(1:nrow, function(x) {
    return(max(heights[which(panelRows == x)]))
  }))
  lW[seq(from = 2, by = 2, length = ncol)] <-
    do.call(unit.c, lapply(1:ncol, function(x) {
    return(max(widths[which(panelCols == x)]))
  }))
  if (nrow > 1) lH[seq(from = 3, by = 2, length = nrow - 1)] <- spacer
  if (ncol > 1) lW[seq(from = 3, by = 2, length = ncol - 1)] <- spacer
  lH[c(1, length(lH))] <- margin
  lW[c(1, length(lW))] <- margin
  # Set vp variables for each child
  for (i in 1:length(subVP)) {
    subVP[[i]]$P <- modifyVP(subVP[[i]]$P, parent = vpPath(paste0(
      name, "VP"
    ), subVP[[i]]$P$parent), just, posCol = c(
      panelCols[i] * 2,
      panelCols[i] * 2
    ), posRow = c(panelRows[i] * 2, panelRows[i] * 2))
    if ("C" %in% names(subVP[[i]])) {
      subVP[[i]] <- vpTree(subVP[[i]]$P, subVP[[i]]$C)
    } else {
      subVP[[i]] <- subVP[[i]]$P
    }
  }
  parentLayout <- grid.layout(nrow = (nrow * 2) + 1,
                              ncol = (ncol * 2) + 1, heights = lH, widths = lW)
  #### Create Tree####
  vpM <- vpTree(viewport(layout = parentLayout, name = paste0(name, "VP"),
                         width = sum(lW),
                         height = sum(lH)),
                do.call(vpList, subVP))
  tree <- gTree(name = name,
                children = do.call(gList, grobLists),
                childrenvp = vpM)
  return(tree)
}

####modifyVP function####
modifyVP <- function(vp, parent, just, posCol, posRow, x, y) {
  if (!missing(x)) {
    vp$x <- x
  }
  if (!missing(y)) {
    vp$y <- y
  }
  if (!missing(parent)) {
    vp$parent <- parent
  }
  if (!missing(just)) {
    vp$justification <- just
    vp$valid.just <- just
  }
  if (!missing(posCol)) {
    vp$layout.pos.col <- as.integer(posCol)
    vp$valid.pos.col <- as.integer(posCol)
  }
  if (!missing(posRow)) {
    vp$layout.pos.row <- as.integer(posRow)
    vp$valid.pos.row <- as.integer(posRow)
  }
  return(vp)
}
