#' Merge Nightingale Figure Templates with User Data
#'
#' Custom merge function to help users align their data with the template
#' datasets containing the variable location information for the Nightingale
#' figures. User datasets should contain the variables representing bubble color
#' and size, and a key column to merge rows with the template. The key column
#' should either be named "ukbb_id" and contain the UK Biobank Field ID for each
#' variable (e.g. "23474") or named "ckb_id" and contain the China Kadoorie
#' Biobank variable ID, (e.g. "nmr_bohbut").
#'
#' @param data User dataset
#' @param id Name of key column, either "ukbb_id" or "ckb_id"
#' @examples
#' data <- merge_template(cetp, "ckb_id")
#' @return Merged dataset
#' @export

merge_template <- function(data,
                           id){
  if(length(unique(data[,id])) != length(data[,id])){warning("Duplicate IDs in input data.")}
  if(id == "ukbb_id"){
    if("HDL2" %in% data[,"ukbb_id"]){ntn <- bubbleHeatmap::ntngale225
    }else if("23404" %in% data[, "ukbb_id"]){ntn <-bubbleHeatmap::ntngale249
    }else{stop("Failed to identify Nightingale variable set (225/249)")}
  }else if(id == "ckb_id"){
    if("nmr_hdl2_c" %in% data[,"ckb_id"]){ntn <- bubbleHeatmap::ntngale225
    }else{stop("Failed to identify Nightingale variable set 225 using CKB IDs")}
  }else{stop("id value not == 'ukbb_id' or 'ckb_id'")}
  if(length(which(!(data[, id] %in% ntn[, id]))) != 0){
    warning(paste("Unmatched variables in input data:", paste(data[, id][which(!data[, id] %in% ntn[, id])], collapse = ", ")))}
  if(length(which(!(ntn[, id][which(!(ntn[, id] %in% c("nmr_x", "DUMMY")))] %in% data[, id]))) != 0){
    warning(paste("Unmatched variables in template:", paste(data[,id][which(!ntn[, id] %in% data[, id])], collapse = ", ")))}
  merge(data, ntn, by = id, all.y = TRUE, sort = FALSE)}




#' Split Data Into Matrices and Reorder Rows and Columns.
#'
#' Prepare data variables for plot building by splitting into matrices and
#' ordering rows and columns. Includes pre-defined settings for plotting
#' Nightingale metabolomics data.
#'
#' This function takes an input dataset with one row for each plot cell
#' and splits it into a list of plot datasets as defined by the variable
#' \code{plotGroup}. Each plot dataset is then recast (using the
#' \code{cast()} function from the \code{reshape} package) into matrices defined
#' by the \code{colName} and \code{rowName} variables. Two matrices are produced
#' for each plot, containing the values of \code{colorValue} and
#' \code{sizeValue} respectively. To use the included Nightingale metabolomics
#' plot templates, the \code{rowName}, \code{colName} and \code{plotGroup}
#' variables can be merged from the options in the package datafiles
#' \code{ntngale225/249}.
#'
#' @param data Dataset containing variables to be plotted on color and size
#'     scales with additional matrix layout specification variables
#'     (see details).
#' @param plotGroup String naming the variable of \code{data} which groups the
#'     dataset into plots.
#' @param rowName String naming the variable of \code{data} which defines rows
#'     of each matrix.
#' @param colName String naming the variable of \code{data} which defines
#'     columns of each matrix.
#' @param colorValue String naming the variable of \code{data} containing the
#'     values to be plotted on the color scale.
#' @param sizeValue String naming the variable of \code{data} containing the
#'     values to scale bubble sizes.
#' @param listOrder Vector of character strings matching values of
#'     \code{plotGroup} variable, or \code{NULL} (default) if no sorting
#'     required or when using inbuilt Nightingale ordering.
#' @param rowOrder List of vectors of character strings matching the values of
#'     \code{rowName} for each plot. List names should match values of
#'     \code{plotGroup}. Can be \code{NULL} (default) if no sorting required or
#'     when using inbuilt Nightingale ordering. If used, it is not necessary for
#'     all plots to have list entries, but each entry should include all row
#'     names for that plot.
#' @param colOrder As \code{rowOrder} for columns and \code{colName}.
#' @param nightingale Logical - use Nightingale style settings?
#' @examples
#' data <- merge_template(cetp, "ckb_id")
#' formatData(data, colorValue = "estimate", sizeValue = "negLog10P",
#' nightingale = TRUE)
#' @return A list of two lists of numeric matrices. The matrices in
#' \code{colorList} contain values of \code{data$colorValue} and those in
#' \code{sizeList} contain values of \code{data$sizeValue}. The matrice layouts
#' are defined by \code{data$rowName} and \code{data$colName} and each list has
#' one item for each unique value of \code{data$plotGroup}.
#' @importFrom stats as.formula setNames
#' @export


formatData <- function(data,
                       rowName = "rowName",
                       colName = "colName",
                       plotGroup = "plotGroup",
                       colorValue = "colorValue",
                       sizeValue = "sizeValue",
                       rowOrder = NULL,
                       colOrder = NULL,
                       listOrder = NULL,
                       nightingale = TRUE) {
  data <- as.data.frame(data)
  # Identify Nightingale Style
  if(nightingale == TRUE){
    if("HDL2" %in% data[[colName]]){nightingale <- 225
    }else if("Clinical LDL" %in% data[[colName]]){nightingale <- 249
    }else{stop("Failed to identify Nightingale variable set (225/249)")}
  }
  # Split data into plot groups
  data <- split(data, data[, plotGroup])
  # Cast matrices
  reformatCast <- function(dat, val) {
    if (nrow(dat) == 1) {
      m <- matrix(data = as.numeric(dat[val]),
                  nrow = 1,
                  ncol = 1,
                  dimnames = list(dat[, rowName], dat[, colName]))
    } else if (nrow(unique(dat[colName])) == 1) {
      m <- matrix(data = as.numeric(as.matrix(dat[val])),
                  nrow = nrow(dat),
                  ncol = 1,
                  dimnames = list(dat[, rowName], dat[, colName][1]))
    } else {
      m <- as.matrix(
        reshape::cast(dat,
                      formula = as.formula(paste(rowName, " ~ ", colName)),
                      value = val))
    }
  }
  colorList <- lapply(data, reformatCast, val = colorValue)
  sizeList <- lapply(data, reformatCast, val = sizeValue)
  # Sort list of plot matrices#
  if (nightingale == 249) {
    listOrder <- c("Absolute Measures", "Ratio to Total Lipids", "Aggregate",
                   "Fatty Acids", "Mean Diameter", "Other Lipids",
                   "Apolipoproteins", "Amino Acids", "BCAAs", "Glycolysis",
                   "Ketones", "Fluid Balance", "Inflammation")
  } else if (nightingale == 225) {
    listOrder <- c("Absolute Measures", "Ratio to Total Lipids", "Aggregate",
                   "Fatty Acids", "Mean Diameter", "Other Lipids",
                   "Apolipoproteins", "Amino Acids", "BCAAs", "Glycolysis",
                   "Ketones", "Fluid Balance", "Inflammation")
  }
  if (!(is.null(listOrder))) {
    colorList <- colorList[listOrder]
    sizeList <- sizeList[listOrder]
  }
  # Sort rows and columns of plot matrices
  matSort <- function(x, listIn, order, RC) {
    if (x %in% names(order)) {
      if (RC == "C") {
        m <- as.matrix(rbind(listIn[[x]][, colOrder[[x]]]))
        row.names(m) <- row.names(listIn[[x]])
        return(m)
      } else if (RC == "R") {
        m <- as.matrix(cbind(listIn[[x]][rowOrder[[x]], ]))
        colnames(m) <- colnames(listIn[[x]])
        return(m)
      }
    } else {
      return(listIn[[x]])
    }
  }
  if (nightingale == 225) {
    colOrder <- list(
      `Absolute Measures` = c("CM & XL VLDL", "VL VLDL", "L VLDL", "M VLDL",
                              "S VLDL", "VS VLDL", "IDL", "L LDL", "M LDL",
                              "S LDL", "VL HDL", "L HDL", "M HDL", "S HDL"),
      `Ratio to Total Lipids` = c("CM & XL VLDL", "VL VLDL", "L VLDL", "M VLDL",
                                  "S VLDL", "VS VLDL", "IDL", "L LDL", "M LDL",
                                  "S LDL", "VL HDL", "L HDL", "M HDL", "S HDL"),
      `Aggregate` = c("Total", "VLDL", "LDL", "HDL", "HDL2", "HDL3", "Remnant"),
      `Fatty Acids` = c("Total FA", "o Unsaturation", "Sat FA", "PUFA", "MUFA",
                        "n-6", "22:6 DHA", "n-3", "18:2 LA")
    )
    rowOrder <- list(
      `Absolute Measures` = c("Particle Concentration", "Total Lipids",
                              "Total Cholesterol", "Free Cholesterol",
                              "Cholesterol Esters", "Triglycerides",
                              "Phospholipids"),
      `Apolipoproteins` = c("ApoA1", "ApoB", "ApoB:ApoA1"),
      `Aggregate` = c("Particle Concentration", "Total Lipids", "Total Cholesterol",
                      "Free Cholesterol", "Cholesterol Esters", "Triglycerides",
                      "Phospholipids"),
      `Mean Diameter` = c("VLDL", "HDL", "LDL"),
      `Ratio to Total Lipids` = c("Total Cholesterol", "Free Cholesterol",
                                  "Cholesterol Esters", "Triglycerides",
                                  "Phospholipids"),
      `Fatty Acids` = c("Concentration", "Ratio to Total"),
      `Amino Acids` = c("Ala", "Gln", "His", "Phe", "Tyr"),
      `BCAAs` = c("Ile", "Leu", "Val"),
      `Fluid Balance` = c("Albumin", "Creatinine"),
      `Glycolysis` = c("Glucose", "Lactate", "Citrate"),
      `Ketones` = c("3OH-butyrate", "Acetate", "Acetoacetate"),
      `Other Lipids` = c("PG", "Trig:PG", "Phosphatidylcholine",
                         "Cholines", "Sphingomyelins")
    )
  } else if (nightingale == 249) {
    colOrder <- list(
      `Absolute Measures` = c("CM & XL VLDL", "VL VLDL", "L VLDL", "M VLDL",
                              "S VLDL", "VS VLDL", "IDL", "L LDL", "M LDL",
                              "S LDL", "VL HDL", "L HDL", "M HDL", "S HDL"),
      `Ratio to Total Lipids` = c("CM & XL VLDL", "VL VLDL", "L VLDL", "M VLDL",
                                  "S VLDL", "VS VLDL", "IDL", "L LDL", "M LDL",
                                  "S LDL", "VL HDL", "L HDL", "M HDL", "S HDL"),
      `Aggregate` = c("Total", "VLDL", "LDL", "HDL", "Clinical LDL", "Non-HDL", "Remnant"),
      `Fatty Acids` = c("Total FA", "o Unsaturation", "Sat FA", "PUFA", "MUFA", "n-6", "22:6 DHA",
                        "n-3","18:2 LA", "PUFA:MUFA", "n-6:n-3")
    )
    rowOrder <- list(
      `Absolute Measures` = c("Particle Concentration", "Total Lipids",
                              "Total Cholesterol", "Free Cholesterol",
                              "Cholesterol Esters", "Triglycerides",
                              "Phospholipids"),
      `Aggregate` = c("Particle Concentration", "Total Lipids", "Total Cholesterol",
                      "Free Cholesterol", "Cholesterol Esters", "Triglycerides",
                      "Phospholipids"),
      `Apolipoproteins` = c("ApoA1", "ApoB", "ApoB:ApoA1"),
      `Mean Diameter` = c("VLDL", "HDL", "LDL"),
      `Ratio to Total Lipids` = c("Total Cholesterol", "Free Cholesterol",
                                  "Cholesterol Esters", "Triglycerides",
                                  "Phospholipids"),
      `Fatty Acids` = c("Concentration", "Ratio to Total*"),
      `Amino Acids` = c("Ala", "Gln", "Gly", "His", "Phe", "Tyr"),
      `BCAAs` = c("Ile", "Leu", "Val", "Total"),
      `Fluid Balance` = c("Albumin", "Creatinine"),
      `Glycolysis` = c("Glucose", "Lactate", "Citrate",
                       "Pyruvate"),
      `Ketones` = c("3OH-butyrate", "Acetate",
                    "Acetoacetate", "Acetone"),
      `Other Lipids` = c("PG", "Trig:PG", "Phosphatidylcholine",
                         "Cholines", "Sphingomyelins")
    )
  }
  if (!is.null(colOrder)) {
    colorList <- lapply(setNames(names(colorList), names(colorList)), matSort,
                        listIn = colorList, order = colOrder, RC = "C")
    sizeList <- lapply(setNames(names(sizeList), names(sizeList)), matSort,
                       listIn = sizeList, order = colOrder, RC = "C")
  }
  if (!is.null(rowOrder)) {
    colorList <- lapply(setNames(names(colorList), names(colorList)), matSort,
                        listIn = colorList, order = rowOrder, RC = "R")
    sizeList <- lapply(setNames(names(sizeList), names(sizeList)), matSort,
                       listIn = sizeList, order = rowOrder, RC = "R")
  }
  return(list(colorList = colorList, sizeList = sizeList))
}
