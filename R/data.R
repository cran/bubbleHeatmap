#' Associations of Nightingale Health Metabolomics Panel with a Cholesterol
#' Esterase Transfer Protein Genetic Risk Score in China Kadoorie Biobank
#'
#' Example dataset containing association results of the 225 Nightingale
#' metabolomics traits with a CETP Genetic Risk Score, scaled to 10-mg/dL
#' higher levels of HDL-C.
#'
#' @format Data frame with 225 rows and 5 variables
#' \describe{
#' \item{trait}{trait identifier as used in China Kadoorie Biobank, can be
#' used to merge results with ntngale template datasets}
#' \item{estimate}{association beta}
#' \item{stdErr}{standard error of association estimate}
#' \item{pval}{p-value of association}
#' \item{negLog10P}{-log10(pval)}
#' }
#'
#' @source Millwood IY, Bennett DA, Holmes MV et al. Association of CETP Gene
#' Variants With Risk for Vascular and Nonvascular Diseases Among Chinese
#' Adults. JAMA Cardiol. 2018 Jan 1;3(1):34-43.
#'
#' @details
#' The sample data included in this package represents associations between Nightingale
#' metabolic measures and a genetic risk score (GRS) for Cholesterol Ester Transfer
#' Protein (CETP) in the China Kadoorie Biobank (CKB), a prospective study of
#' Chinese adults from 10 distinct areas of China. The full design of the CKB
#' cohort and the methods and results of this study have been previously published.
#'
#' CETP transfers esterified cholesterol from HDL to apolipoprotein B-containing
#' lipoproteins in exchange for triglycerides. This process is a key component of
#' the atheroprotective reverse cholesterol transport pathway, which modulates the
#' return of excess cholesterol from peripheral cells such as macrophages to the
#' liver, where it can be redistributed or excreted. Reduced CETP activity results
#' in higher levels of HDL cholesterol (HDL-C), which has resulted in interest in
#' CETP as a drug target due to the inverse correlation between HDL-C and risk of
#' atherosclerotic disease. In this analysis, five CETP SNP variants were selected
#' on the basis of previously reported associations with HDL cholesterol and CETP
#' activity. Genotyping data for these variants and conventional lipid biochemistry
#' measures were available for a subset of 17,854 CKB participants selected for a
#' CVD case-control study. This data was used to generate a CETP GRS weighted by
#' HDL-C association, derived internally with 100-fold cross-validation. The
#' Nightingale Health metabolomics panel consisting of 225 metabolomics measures
#' was available for 4657 of these individuals, and association with the GRS was
#' assessed by linear regression with adjustment for age and sex, stratified by
#' geographical region. In generation of the GRS and association of metabolomics,
#' outcomes were standardized by rank inverse normal transformation, stratified by
#' region, after adjustment for age and sex.
#'
"cetp"

#' Nightingale Health Metabolomics Plot Template Format
#'
#' Template datasets containing grouping and labeling variables to
#' information to plot 225 or 249 variable Nightingale Health metabolomics data.
#'
#' @format Data frame with 228/249 rows and 5 variables
#' \describe{
#' \item{trait}{trait identifier as used in China Kadoorie Biobank, can be
#' used to merge template with results data}
#' \item{traitTitle}{trait description to assist with merging results}
#' \item{rowName}{row label for trait in plot matrix}
#' \item{colName}{column label for trait in plot matrix}
#' \item{plotGroup}{this variable splits the traits into 10 plot matrices that
#' make up the overall figure}
#' }
#'
#' @details
#' Nightingale Health is a Finnish company which has developed a fully
#' automated platform for deriving over 200 quantitative metabolomics measures
#' from a single blood serum sample using NMR technology, at a comparable cost
#' to standard lipid clinical chemistry. These measures include concentration
#' and composition of 14 lipoprotein subclasses, apolipoproteins, fatty acids,
#' amino acids and glycolysis related metabolites. Over 1M samples have already
#' been processed, including biobank participants, population cohorts and
#' clinical studies, with capacity to process an additional 250K samples per
#' year. Over 100K profiles of UK Biobank participants have recently been
#' released, with data on the remaining individuals to follow. The wide
#' availability of standardised Nightingale datasets in global cohorts
#' facilitates collaborative research and is contributing to the identification
#' of new biomarkers across a range of diseases.
#'
#' @name nightingaleTemplates
#' @keywords datasets
#' @rdname nightingaleTemplates
"ntngale225"
#' @rdname nightingaleTemplates
"ntngale249"
