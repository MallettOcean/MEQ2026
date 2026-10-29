# This file takes the cleaned data (that excludes those who choose Research Observation and those who failed
# the attention checks) and performs the analyses for Study 1

library(openxlsx)
library(jmv)
library(dplyr)

# Import MEQ-R item info

MEQItemTextFull = read.csv("Study 1 MEQ-R Item Names.csv")
MEQItemText = MEQItemTextFull$ItemText
names(MEQItemText) = MEQItemTextFull$ItemName

# Load the MEQ Data

MEQ = readRDS(file = "Study 1 Data.rds")

# Some quick descriptives

table(MEQ$Gender, useNA = 'ifany')

descriptives(MEQ, "Age")

# Create a function to take loadings from jamova efa procedure to and create a organized table of them that includes the actual
# item text and also creates a formatted Excel file that can be printed

createItemTable = function(loadings, itemText = paste0("item_", 1:nrow(loadings)), 
                           excelFileName = "Factor Analysis.xlsx", headerTitle = "Factor Analysis") {
  numFactors = ncol(loadings)  - 2
  itemTable = data.frame(ItemName = character(), Loading = numeric(), Uniqueness = numeric(), ItemText = character())
  for (i in 1:numFactors) {
    itemTable[nrow(itemTable) + 1,] = c(paste0("Factor ", i), NA_real_, NA_real_, "")
    row.names(loadings) = gsub("\"", "", row.names(loadings))
    factorNames = loadings$name[!(is.na(loadings[[paste0("pc", i)]]))]
    shortfactorNames = sub("MEQ.", "", factorNames)
    shortfactorNames = sub("Block.", "", shortfactorNames)
    newItems = data.frame(ItemName = shortfactorNames, 
                          Loading = round(loadings[factorNames, paste0("pc", i)], 3),
                          Uniqueness = round(loadings[factorNames, "uniq"], 3),
                          ItemText = itemText[shortfactorNames])
    newItems = newItems[order(abs(newItems$Loading), decreasing = TRUE),]
    itemTable = rbind(itemTable, newItems)
  }
  crossLoaded = names(table(itemTable$ItemName))[table(itemTable$ItemName) > 1]
  itemTable$Crossed = NA_character_
  itemTable$Crossed[itemTable$ItemName %in% crossLoaded] = "Y"
  
  dividingRows = grep("Factor", itemTable$ItemName) + 3
  pageBreakIndex = which(dividingRows >  33)[1]
  if (is.na(pageBreakIndex)) {
    pageBreakFactorEnd = nrow(itemTable) + 3
    pageBreakFactorBegin = dividingRows[length(dividingRows)]
  } else {
    pageBreakFactorEnd = dividingRows[pageBreakIndex]
    pageBreakFactorBegin = dividingRows[pageBreakIndex - 1]
  }
  
  wb = createWorkbook()
  addWorksheet(wb, 1)
  writeData(wb, sheet = 1, x = itemTable, startRow = 3)
  mergeCells(wb, 1, cols = 1:5, rows = 1)
  writeData(wb, sheet = 1, headerTitle)
  setColWidths(wb, 1, cols = 4:5, widths = c(50,6))
  wrapText = createStyle(wrapText = TRUE)
  centre = createStyle(halign = "center", valign = "center")
  titleStyle = createStyle(fontSize = 14, halign = "center", textDecoration = "BOLD")
  headerStyle = createStyle(fontSize = 12, halign = "center", textDecoration = "BOLD", border = c("top", "bottom"))
  factorNameStyle = createStyle(textDecoration = "BOLD", border = "top")
  lastRowStyleCol3 = createStyle(border = "bottom", wrapText = TRUE)
  lastRowStyleRest = createStyle(halign = "center", valign = "center", border = "bottom")
  addStyle(wb, 1, wrapText, rows = 1:100, cols = 4:5, gridExpand = TRUE)
  addStyle(wb, 1, centre, rows = 1:100, cols = c(1,2,3,5), gridExpand = TRUE)
  addStyle(wb, 1, titleStyle, rows = 1, cols = 1)
  addStyle(wb, 1, headerStyle, rows = 3, cols = 1:5)
  addStyle(wb, 1, factorNameStyle, rows = dividingRows, cols = 1:5, gridExpand = TRUE)
  addStyle(wb, 1, lastRowStyleCol3, rows = nrow(itemTable) + 3, cols = 4)
  addStyle(wb, 1, lastRowStyleRest, rows = nrow(itemTable) + 3, cols = c(1,2,3,5))
  
  if ((pageBreakFactorEnd > 35) & (35 - pageBreakFactorBegin < 6)) pageBreak(wb, 1, pageBreakFactorBegin - 1)
  
  saveWorkbook(wb, file = excelFileName, overwrite = TRUE)
  
  return(itemTable)
}

# Check the data

teacherItems = paste0("MEQ.Teacher.Block.Q", 1:26)
parentItems = paste0("MEQ.Parent.Block.Q", 1:12)
peerItems = paste0("MEQ.Peer.Block.Q", 1:9)

descriptives(MEQ, vars = c(MEQ.Teacher.Block.Q1, MEQ.Teacher.Block.Q2, MEQ.Teacher.Block.Q3, MEQ.Teacher.Block.Q4, 
                           MEQ.Teacher.Block.Q5, MEQ.Teacher.Block.Q6, MEQ.Teacher.Block.Q7, MEQ.Teacher.Block.Q8, 
                           MEQ.Teacher.Block.Q9, MEQ.Teacher.Block.Q10, MEQ.Teacher.Block.Q11, MEQ.Teacher.Block.Q12, 
                           MEQ.Teacher.Block.Q13, MEQ.Teacher.Block.Q14, MEQ.Teacher.Block.Q15, MEQ.Teacher.Block.Q16, 
                           MEQ.Teacher.Block.Q17, MEQ.Teacher.Block.Q18, MEQ.Teacher.Block.Q19, MEQ.Teacher.Block.Q20, 
                           MEQ.Teacher.Block.Q21, MEQ.Teacher.Block.Q22, MEQ.Teacher.Block.Q23, MEQ.Teacher.Block.Q24, 
                           MEQ.Teacher.Block.Q25, MEQ.Teacher.Block.Q26), freq = TRUE,
             desc = "rows")

descriptives(MEQ, vars = c(MEQ.Parent.Block.Q1, MEQ.Parent.Block.Q2, MEQ.Parent.Block.Q3, MEQ.Parent.Block.Q4, 
                           MEQ.Parent.Block.Q5, MEQ.Parent.Block.Q6, MEQ.Parent.Block.Q7, MEQ.Parent.Block.Q8, 
                           MEQ.Parent.Block.Q9, MEQ.Parent.Block.Q10, MEQ.Parent.Block.Q11, MEQ.Parent.Block.Q12), 
             freq = TRUE,
             desc = "rows")

descriptives(MEQ, vars = c(MEQ.Peer.Block.Q1, MEQ.Peer.Block.Q2, MEQ.Peer.Block.Q3, MEQ.Peer.Block.Q4, 
                           MEQ.Peer.Block.Q5, MEQ.Peer.Block.Q6, MEQ.Peer.Block.Q7, MEQ.Peer.Block.Q8, 
                           MEQ.Peer.Block.Q9), 
             freq = TRUE,
             desc = "rows")

# There are missing value codes and Do Not Recall Codes that we need to remove

MEQ[,c(teacherItems, parentItems, peerItems)] = 
  lapply(MEQ[,c(teacherItems, parentItems, peerItems)], FUN = function(x) {x = as.numeric(x); x[x > 5] = NA_integer_;
  return(x)})

# Select all MEQ items for the factor analyses

MEQ_selected <- MEQ %>%
  select(
    "MEQ.Teacher.Block.Q1", "MEQ.Teacher.Block.Q2", "MEQ.Teacher.Block.Q3", "MEQ.Teacher.Block.Q4",
    "MEQ.Teacher.Block.Q5", "MEQ.Teacher.Block.Q6", "MEQ.Teacher.Block.Q7", "MEQ.Teacher.Block.Q8",
    "MEQ.Teacher.Block.Q9", "MEQ.Teacher.Block.Q10", "MEQ.Teacher.Block.Q11", "MEQ.Teacher.Block.Q12",
    "MEQ.Teacher.Block.Q13", "MEQ.Teacher.Block.Q14", "MEQ.Teacher.Block.Q15", "MEQ.Teacher.Block.Q16",
    "MEQ.Teacher.Block.Q17", "MEQ.Teacher.Block.Q18", "MEQ.Teacher.Block.Q19", "MEQ.Teacher.Block.Q20",
    "MEQ.Teacher.Block.Q21", "MEQ.Teacher.Block.Q22", "MEQ.Teacher.Block.Q23", "MEQ.Teacher.Block.Q24",
    "MEQ.Teacher.Block.Q25", "MEQ.Teacher.Block.Q26",
    "MEQ.Parent.Block.Q1", "MEQ.Parent.Block.Q2", "MEQ.Parent.Block.Q3", "MEQ.Parent.Block.Q4",
    "MEQ.Parent.Block.Q5", "MEQ.Parent.Block.Q7", "MEQ.Parent.Block.Q8",
    "MEQ.Parent.Block.Q9", "MEQ.Parent.Block.Q10", "MEQ.Parent.Block.Q11", "MEQ.Parent.Block.Q12",
    "MEQ.Peer.Block.Q1", "MEQ.Peer.Block.Q2", "MEQ.Peer.Block.Q3", "MEQ.Peer.Block.Q4",
    "MEQ.Peer.Block.Q5", "MEQ.Peer.Block.Q6", "MEQ.Peer.Block.Q7", "MEQ.Peer.Block.Q8", "MEQ.Peer.Block.Q9"
  )

# Start with the 3 factor EFA

Study1EFA3 <- jmv::efa(
  data = MEQ_selected,         # The dataset you selected
  vars = colnames(MEQ_selected),# Select all the columns
  nFactors = 3,          # Set the number of factors
  nFactorMethod = "fixed", # Determine number of set factors
  rotation = "promax",  # Rotation method
  eigen = TRUE,                # Include eigenvalues
  screePlot = TRUE             # Produce a scree plot
)

Study1EFA3


# Calculate variance explained by adding the first 3 eigenvalues together and dividing by the number of variables

# Three factor solution
13.32962643 + 4.38173042 + 2.16948698
19.88084 / 46 * 100



# Now we can do the exploratory factor analyses using the parallel method to choose the number of factors.

Study1EFA <- jmv::efa(
  data = MEQ,         # The dataset you selected
  vars = colnames(MEQ_selected), # Select all the columns
  rotation = "promax",  # Rotation method
  eigen = TRUE,                # Include eigenvalues
  screePlot = TRUE,             # Produce a scree plot
  factorCor = TRUE              # Prodcue a correlations matrix of the factor
)

Study1EFA 


# Calculate variance explained by adding the first 6 eigenvalues together and dividing by the number of variables
# Six factor solution
13.32962643 + 4.38173042 + 2.16948698 + 1.45016563 + 0.99834978 + 0.86297280
23.19233 / 46 * 100

# Print the table of the 6 factor solution

Study1EFA_Table = createItemTable(Study1EFA$loadings$asDF, itemText = MEQItemText,
                                  excelFileName = "Study 1 Factors.xlsx", headerTitle = "Study 1 Data with 6 Factors")


# Create the subscales of the MEQ based on the six factors

MEQ$MEQ.TeacherSupport = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.Teacher.Block.Q8", "MEQ.Teacher.Block.Q10","MEQ.Teacher.Block.Q19", "MEQ.Teacher.Block.Q1", 
                                                              "MEQ.Teacher.Block.Q4", "MEQ.Teacher.Block.Q20", "MEQ.Teacher.Block.Q22", "MEQ.Teacher.Block.Q18", 
                                                              "MEQ.Teacher.Block.Q9", "MEQ.Teacher.Block.Q15", "MEQ.Teacher.Block.Q11", "MEQ.Teacher.Block.Q24", 
                                                              "MEQ.Teacher.Block.Q6R", "MEQ.Teacher.Block.Q13R", "MEQ.Teacher.Block.Q2",
                                                              "MEQ.Teacher.Block.Q7", "MEQ.Teacher.Block.Q23", "MEQ.Teacher.Block.Q5", "MEQ.Teacher.Block.Q14", 
                                                              "MEQ.Teacher.Block.Q25", "MEQ.Teacher.Block.Q17R", "MEQ.Teacher.Block.Q26R", "MEQ.Teacher.Block.Q21", 
                                                              "MEQ.Teacher.Block.Q3R")],
                                                       as.numeric)),
                                  na.rm = TRUE)

# Same items, but ranking slightly different(i.e. items 11 and 24 in reverse order from before, and same with 25 and 14). 17 should be reversed

MEQ$MEQ.ParentSupport = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.Parent.Block.Q8", "MEQ.Parent.Block.Q11", "MEQ.Parent.Block.Q5R", "MEQ.Parent.Block.Q1", 
                                                             "MEQ.Parent.Block.Q2", "MEQ.Parent.Block.Q3", "MEQ.Parent.Block.Q7R", "MEQ.Parent.Block.Q10",
                                                             "MEQ.Parent.Block.Q4", "MEQ.Parent.Block.Q9")],
                                                      as.numeric)),
                                 na.rm = TRUE)

# Same items, but ranking slightly different(i.e. items 7 and 3 in reverse order from before)

MEQ$MEQ.PeerSupport = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.Peer.Block.Q5", "MEQ.Peer.Block.Q1", "MEQ.Peer.Block.Q2", "MEQ.Peer.Block.Q7", 
                                                           "MEQ.Peer.Block.Q9", "MEQ.Peer.Block.Q4")],
                                                    as.numeric)),
                               na.rm = TRUE)

# Same items

MEQ$MEQ.OthersPerspectives = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.Peer.Block.Q3", "MEQ.Parent.Block.Q12R", "MEQ.Teacher.Block.Q12R")],
                                                           as.numeric)),
                                      na.rm = TRUE)

# Same items, Teacher 12 should be reversed

MEQ$MEQ.NegativeTeacherOutlook = as.numeric(MEQ[,c("MEQ.Teacher.Block.Q16")])

# Same item, but we will use the non-reverse-coded item to match the name.

MEQ$MEQ.PeerComparisons = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.Peer.Block.Q8R", "MEQ.Peer.Block.Q6R")],
                                                        as.numeric)),
                                   na.rm = TRUE)

# Reliabilities

reliability(as.data.frame(lapply(MEQ[,c("MEQ.Teacher.Block.Q8", "MEQ.Teacher.Block.Q10","MEQ.Teacher.Block.Q19", "MEQ.Teacher.Block.Q1", 
                                        "MEQ.Teacher.Block.Q4", "MEQ.Teacher.Block.Q20", "MEQ.Teacher.Block.Q22", "MEQ.Teacher.Block.Q18", 
                                        "MEQ.Teacher.Block.Q9", "MEQ.Teacher.Block.Q15", "MEQ.Teacher.Block.Q11", "MEQ.Teacher.Block.Q24", 
                                        "MEQ.Teacher.Block.Q6R", "MEQ.Teacher.Block.Q13R", "MEQ.Teacher.Block.Q2",
                                        "MEQ.Teacher.Block.Q7", "MEQ.Teacher.Block.Q23", "MEQ.Teacher.Block.Q5", "MEQ.Teacher.Block.Q14", 
                                        "MEQ.Teacher.Block.Q25", "MEQ.Teacher.Block.Q17R", "MEQ.Teacher.Block.Q26R", "MEQ.Teacher.Block.Q21", 
                                        "MEQ.Teacher.Block.Q3R")],
                                 as.numeric)),
            omegaScale = TRUE)

reliability(as.data.frame(lapply(MEQ[,c("MEQ.Parent.Block.Q8", "MEQ.Parent.Block.Q11", "MEQ.Parent.Block.Q5R", "MEQ.Parent.Block.Q1", 
                                        "MEQ.Parent.Block.Q2", "MEQ.Parent.Block.Q3", "MEQ.Parent.Block.Q7R", "MEQ.Parent.Block.Q10",
                                        "MEQ.Parent.Block.Q4", "MEQ.Parent.Block.Q9")],
                                 as.numeric)),
            omegaScale = TRUE)

reliability(as.data.frame(lapply(MEQ[,c("MEQ.Peer.Block.Q5", "MEQ.Peer.Block.Q1", "MEQ.Peer.Block.Q2", "MEQ.Peer.Block.Q7", 
                                        "MEQ.Peer.Block.Q9", "MEQ.Peer.Block.Q4")],
                                 as.numeric)),
            omegaScale = TRUE)

reliability(as.data.frame(lapply(MEQ[,c("MEQ.Peer.Block.Q3", "MEQ.Parent.Block.Q12R", "MEQ.Teacher.Block.Q12R")],
                                 as.numeric)),
            omegaScale = TRUE)

reliability(as.data.frame(lapply(MEQ[,c("MEQ.Peer.Block.Q8R", "MEQ.Peer.Block.Q6R")],
                                 as.numeric)),
            omegaScale = TRUE)



# Report descriptives for Table 3

Study1Descriptives = descriptives(MEQ, c("MARS.S", "TAI", "BAI", "PSWQ", "MEQ.TeacherSupport", "MEQ.ParentSupport",
                                         "MEQ.PeerSupport", "MEQ.OthersPerspectives", "MEQ.NegativeTeacherOutlook",
                                         "MEQ.PeerComparisons", "AMS_Intrinsic", "AMS_ExtrinsicIdentified", 
                                         "AMS_ExtrinsicIntrojected", "AMS_ExtrinsicExternal", "AMS_Amotivation",
                                         "High.School.Mark"),
                                  skew = TRUE, kurt = TRUE, desc = "rows")
Study1Descriptives
table3 = Study1Descriptives$descriptivesT$asDF[, c("vars", "mean", "sd", "skew", "seSkew", "kurt", "seKurt")]
table3

# Print out a version of table3 that can be copy and pasted

cat(sprintf("%.2f (%.2f)\t%.3f (%.3f)\t%.3f (%.3f)\n", table3$mean, table3$sd, table3$skew, table3$seSkew, table3$kurt, table3$seKurt), sep = "")



Study1Corrs = corrMatrix(MEQ, c("MARS.S", "TAI", "BAI", "PSWQ", "MEQ.TeacherSupport", "MEQ.ParentSupport", 
                                "MEQ.PeerSupport", "MEQ.OthersPerspectives", "MEQ.NegativeTeacherOutlook", 
                                "MEQ.PeerComparisons", "AMS_Intrinsic", "AMS_ExtrinsicIdentified", 
                                "AMS_ExtrinsicIntrojected", "AMS_ExtrinsicExternal", "AMS_Amotivation", 
                                "High.School.Mark"))
Study1Corrs

# Print out a version of table4 that can be copy and pasted
table4 = Study1Corrs$matrix$asDF
table4Ps = table4[, grep("\\[rp\\]", names(table4))]
table4Ps = table4Ps[,3:18]
table4 = table4[, grep("\\[r\\]", names(table4))]
table4 = table4[,3:18]

for (i in 1:nrow(table4)) {
  for (j in 1:ncol(table4)) {
    if (i == j) {
      cat("-")
    } else if (is.na(table4[i,j])) {
      cat("")
    } else {
      cat(sub("0\\.", "\\.", sprintf("%.2f", as.numeric(table4[i,j]))))
      if (as.numeric(table4Ps[i,j]) < .05) cat ("*")
      if (as.numeric(table4Ps[i,j]) < .01) cat ("*")
      if (as.numeric(table4Ps[i,j]) < .001) cat ("*")
    }
    if (j < nrow(table4)) {
      cat("\t")
    }
  }
  cat("\n")
}


#  Break down the correlation matrix a bit and look at the bivariate correlations between the variables

corrMatrix(MEQ, c("MARS.S", "TAI", "BAI", "PSWQ"))

corrMatrix(MEQ, c("MARS.S", "MEQ.TeacherSupport", "MEQ.ParentSupport", "MEQ.PeerSupport", "MEQ.OthersPerspectives", 
                  "MEQ.NegativeTeacherOutlook", "MEQ.PeerComparisons"))

corrMatrix(MEQ, c("MARS.S", "TAI", "BAI", "PSWQ", 
                  "AMS_IntrinsicKnow", "AMS_ExtrinsicIdentified",
                  "AMS_ExtrinsicIntrojected", "AMS_ExtrinsicExternal", "AMS_Amotivation"))


corrMatrix(MEQ, c("MARS.S", "TAI", "BAI", "PSWQ", "MEQ.TeacherSupport", "MEQ.ParentSupport", "MEQ.PeerSupport", "MEQ.OthersPerspectives", 
                  "MEQ.NegativeTeacherOutlook", "MEQ.PeerComparisons"))


# Linear regressions to test for MEQ subscales predicting MA once general and test anxiety are controlled for

linReg(MEQ, dep = MARS.S, covs = vars(MEQ.TeacherSupport, PSWQ, BAI, TAI), blocks = list(list("MEQ.TeacherSupport",
                                                                                              "PSWQ",
                                                                                              "BAI",
                                                                                              "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)


linReg(MEQ, dep = MARS.S, covs = vars(MEQ.ParentSupport, PSWQ, BAI, TAI), blocks = list(list("MEQ.ParentSupport",
                                                                                             "PSWQ",
                                                                                             "BAI",
                                                                                             "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)

linReg(MEQ, dep = MARS.S, covs = vars(MEQ.PeerSupport, PSWQ, BAI, TAI), blocks = list(list("MEQ.PeerSupport",
                                                                                           "PSWQ",
                                                                                           "BAI",
                                                                                           "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)


linReg(MEQ, dep = MARS.S, covs = vars(MEQ.OthersPerspectives, PSWQ, BAI, TAI), blocks = list(list("MEQ.OthersPerspectives",
                                                                                                  "PSWQ",
                                                                                                  "BAI",
                                                                                                  "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)

linReg(MEQ, dep = MARS.S, covs = vars(MEQ.NegativeTeacherOutlook, PSWQ, BAI, TAI), blocks = list(list("MEQ.NegativeTeacherOutlook",
                                                                                                  "PSWQ",
                                                                                                  "BAI",
                                                                                                  "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)

linReg(MEQ, dep = MARS.S, covs = vars(MEQ.PeerComparisons, PSWQ, BAI, TAI), blocks = list(list("MEQ.PeerComparisons",
                                                                                               "PSWQ",
                                                                                               "BAI",
                                                                                               "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)



# Linear regressions to test for AMS subscales predicting MA once general and test anxiety are controlled for

linReg(MEQ, dep = MARS.S, covs = vars(AMS_Intrinsic, PSWQ, BAI, TAI), blocks = list(list("AMS_Intrinsic",
                                                                                              "PSWQ",
                                                                                              "BAI",
                                                                                              "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)


linReg(MEQ, dep = MARS.S, covs = vars(AMS_ExtrinsicIdentified, PSWQ, BAI, TAI), blocks = list(list("AMS_ExtrinsicIdentified",
                                                                                             "PSWQ",
                                                                                             "BAI",
                                                                                             "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)

linReg(MEQ, dep = MARS.S, covs = vars(AMS_ExtrinsicIntrojected, PSWQ, BAI, TAI), blocks = list(list("AMS_ExtrinsicIntrojected",
                                                                                           "PSWQ",
                                                                                           "BAI",
                                                                                           "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)


linReg(MEQ, dep = MARS.S, covs = vars(AMS_ExtrinsicExternal, PSWQ, BAI, TAI), blocks = list(list("AMS_ExtrinsicExternal",
                                                                                                  "PSWQ",
                                                                                                  "BAI",
                                                                                                  "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)

linReg(MEQ, dep = MARS.S, covs = vars(AMS_Amotivation, PSWQ, BAI, TAI), blocks = list(list("AMS_Amotivation",
                                                                                                      "PSWQ",
                                                                                                      "BAI",
                                                                                                      "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)



# Only Others Perspectives still predicted MA after controlling for General Anxiety and Test Anxiety among
# the MEQ subscales.  Only Extrinsic Introjected still predicted MA after controlling for General Anxiety and Test Anxiety among
# the AMS subscales.  Let's test for this mediation.  We will use the JAMM package used in Jamovi, which means we need
# devtools to install it.

install.packages("devtools")
install.packages("jmvcore", repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
devtools::install_github("jamovi-amm/jamm")

jamm::jammGLM(
  formula = list( AMS_ExtrinsicIntrojected ~ MEQ.OthersPerspectives,
                  MARS.S ~ AMS_ExtrinsicIntrojected + MEQ.OthersPerspectives + BAI + TAI + PSWQ ),
  data = MEQ,
  ciType = "bca",
  tableOptions = c("beta", "component", "regression"))
