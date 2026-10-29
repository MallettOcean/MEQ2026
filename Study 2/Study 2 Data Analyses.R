library(jmv)
library(openxlsx)
library(psych)

# Import the cleaned data generated from Study 1 Data Coding

MEQ = readRDS("Study 2 Data.rds")

# Demographics from the Participants section

table(MEQ$Gender, useNA = 'ifany')
table(MEQ$Gender, useNA = 'ifany') / nrow(MEQ) * 100  # Percentages of gender categories

descriptives(MEQ, "Age")

table(MEQ$Ethnicity, useNA = 'ifany')
table(MEQ$Ethnicity, useNA = 'ifany') / nrow(MEQ) * 100  # Percentages of ethnicity categories

table(MEQ$Education, useNA = 'ifany')
table(MEQ$Education, useNA = 'ifany') / nrow(MEQ) * 100  # Percentages of education categories

table(MEQ$StudyArea, useNA = 'ifany')
table(MEQ$StudyArea, useNA = 'ifany') / nrow(MEQ) * 100  # Percentages of study area categories

table(MEQ$Occupation, useNA = 'ifany')
table(MEQ$Occupation, useNA = 'ifany') / nrow(MEQ) * 100  # Percentages of occupation categories

descriptives(MEQ, "HighSchoolMark")


# Factor Analyses

# Import a named character array with the text values for each item

MEQItemTextFull = read.csv(file = "Study 2 MEQ-R Item Names.csv", row.names = 1)
MEQItemText = MEQItemTextFull$ItemText
names(MEQItemText) = row.names(MEQItemTextFull)

# Create a function to take loadings from jamova efa procedure to and create a organized table of them that includes the actual
# item text and also creates a formated Excel file that can be printed

createItemTable = function(loadings, itemText = paste0("item_", 1:nrow(loadings)), 
                           excelFileName = "Prolific Factor Analysis.xlsx", 
                           headerTitle = "Prolific Factor Analysis") {
  
  # Set row names using the 'name' column for safe subsetting
  rownames(loadings) <- loadings$name
  
  numFactors = ncol(loadings) - 2
  itemTable = data.frame(ItemName = character(), Loading = numeric(), ItemText = character())
  
  for (i in 1:numFactors) {
    
    # Get item codes for current factor
    factorNames = rownames(loadings)[!is.na(loadings[[paste0("pc", i)]])]
    
    # Get rid of MEQ. out of variable names, also needed to match itemText
    displayNames = gsub("^MEQ\\.", "", factorNames)
    
    
    # Build new rows
    newItems = data.frame(
      ItemName = displayNames,  # displayed name
      Loading = round(loadings[factorNames, paste0("pc", i)], 3),
      ItemText = itemText[displayNames],
      stringsAsFactors = FALSE
    )
    
    # Sort by absolute loading
    newItems = newItems[order(abs(newItems$Loading), decreasing = TRUE), ]
    
    # Add factor heading
    itemTable[nrow(itemTable) + 1, ] = c(paste0("Factor ", i), NA_real_, "")
    itemTable = rbind(itemTable, newItems)
  }
  
  # Identify cross-loaded items
  crossLoaded = names(table(itemTable$ItemName))[table(itemTable$ItemName) > 1]
  itemTable$Crossed = NA_character_
  itemTable$Crossed[itemTable$ItemName %in% crossLoaded] = "Y"
  
  # Page break logic for Excel export
  dividingRows = grep("Factor", itemTable$ItemName) + 3
  pageBreakIndex = which(dividingRows > 33)[1]
  if (is.na(pageBreakIndex)) {
    pageBreakFactorEnd = nrow(itemTable) + 3
    pageBreakFactorBegin = dividingRows[length(dividingRows)]
  } else {
    pageBreakFactorEnd = dividingRows[pageBreakIndex]
    pageBreakFactorBegin = dividingRows[pageBreakIndex - 1]
  }
  
  # Create Excel workbook
  wb = createWorkbook()
  addWorksheet(wb, 1)
  writeData(wb, sheet = 1, x = itemTable, startRow = 3)
  mergeCells(wb, 1, cols = 1:4, rows = 1)
  writeData(wb, sheet = 1, headerTitle)
  setColWidths(wb, 1, cols = 3:4, widths = c(50, 6))
  
  # Styling
  wrapText = createStyle(wrapText = TRUE)
  centre = createStyle(halign = "center", valign = "center")
  titleStyle = createStyle(fontSize = 14, halign = "center", textDecoration = "BOLD")
  headerStyle = createStyle(fontSize = 12, halign = "center", textDecoration = "BOLD", border = c("top", "bottom"))
  factorNameStyle = createStyle(textDecoration = "BOLD", border = "top")
  lastRowStyleCol3 = createStyle(border = "bottom", wrapText = TRUE)
  lastRowStyleRest = createStyle(halign = "center", valign = "center", border = "bottom")
  
  addStyle(wb, 1, wrapText, rows = 1:100, cols = 3:4, gridExpand = TRUE)
  addStyle(wb, 1, centre, rows = 1:100, cols = c(1, 2, 4), gridExpand = TRUE)
  addStyle(wb, 1, titleStyle, rows = 1, cols = 1)
  addStyle(wb, 1, headerStyle, rows = 3, cols = 1:4)
  addStyle(wb, 1, factorNameStyle, rows = dividingRows, cols = 1:4, gridExpand = TRUE)
  addStyle(wb, 1, lastRowStyleCol3, rows = nrow(itemTable) + 3, cols = 3)
  addStyle(wb, 1, lastRowStyleRest, rows = nrow(itemTable) + 3, cols = c(1, 2, 4))
  
  if ((pageBreakFactorEnd > 35) & (35 - pageBreakFactorBegin < 6)) {
    pageBreak(wb, 1, pageBreakFactorBegin - 1)
  }
  
  saveWorkbook(wb, file = excelFileName, overwrite = TRUE)
  
  return(itemTable)
}

# Select all MEQ items for the exploratory factor analyses, using the Reverse-coded items where appropriate

MEQ_selected = c("MEQ.T1_1", "MEQ.T2_1", "MEQ.T3R", "MEQ.T4_1",
         "MEQ.T5_1", "MEQ.T6R", "MEQ.T7_1", "MEQ.T8_1",
         "MEQ.T9_1", "MEQ.T10_1", "MEQ.T11_1", "MEQ.T12R",
         "MEQ.T13R", "MEQ.T14_1", "MEQ.T15_1", "MEQ.T16R",
         "MEQ.T17R", "MEQ.T18_1", "MEQ.T19_1", "MEQ.T20_1",
         "MEQ.T21_1", "MEQ.T22_1", "MEQ.T23_1", "MEQ.T24_1",
         "MEQ.T25_1", "MEQ.T26R","MEQ.T27_1",
         "MEQ.PAR1_1", "MEQ.PAR2_1", "MEQ.PAR3_1", "MEQ.PAR4_1",
         "MEQ.PAR5R", "MEQ.PAR7R", "MEQ.PAR8_1","MEQ.PAR9_1", 
         "MEQ.PAR10_1", "MEQ.PAR11_1", "MEQ.PAR12R", "MEQ.PAR13_1",
         "MEQ.PR1_1", "MEQ.PR2_1", "MEQ.PR3_1", "MEQ.PR4_1",
         "MEQ.PR5_1", "MEQ.PR6R", "MEQ.PR7_1", "MEQ.PR8R", 
         "MEQ.PR9_1", "MEQ.PR10_1", "MEQ.PR11_1", "MEQ.PR12_1"
  )


# Now we can do the exploratory factor analyses using the parallel method.

ProlificFA <- jmv::efa(
  data = MEQ[, MEQ_selected],         # The dataset with the selected MEQ variables
  rotation = "promax",  # Rotation method
  eigen = TRUE,                # Include eigenvalues
  screePlot = TRUE             # Produce a scree plot
)

# Check Eigenvalues
ProlificFA 

# Calculate variance explained

# Four factor solution
21.36743354 + 3.68019273 + 2.64350218 + 1.51627637
29.2074 / 51 * 100

# The jamovi efa defaults to uses parallel analysis to choose the number of factors.  In this case it picked 4 factors.
# Let's create the nice table of it  

ProlificEFA_Table = createItemTable(ProlificFA$loadings$asDF, itemText = MEQItemText,
                                    excelFileName = "Study 2 4-Factor EFA.xlsx", headerTitle = "Study 2 MEQ Factor Analysis with 4 Factors")


# There were two items that didn't load any factors.  Here they are

MEQItemText["T3R"]
MEQItemText["PR3_1"]

# Lets run an EFA with 5 factors to see how that fourth factor splits.

FA5 <- jmv::efa(
  data = MEQ[, MEQ_selected],         # The dataset with the selected MEQ variables
  nFactors = 5,          # Set the number of factors
  nFactorMethod = "fixed", # Determine number of set factors
  rotation = "promax",  # Rotation method
  eigen = TRUE,                # Include eigenvalues
  screePlot = TRUE             # Produce a scree plot
)

FA5


FA_Table5 = createItemTable(FA5$loadings$asDF, itemText = MEQItemText,
                            excelFileName = "Study 2 5-Factor EFA.xlsx", headerTitle = "Study 2 MEQ Factor Analysis with 4 Factors")

# Variance explained - four factor
21.36743354 + 3.68019273 +  2.64350218 + 1.51627637 
29.2074 / 51 *100

# Variance explained - five factor
21.36743354 + 3.68019273 +  2.64350218 + 1.51627637 + 0.75687451
29.96428 / 51 *100


# Go with the four-factor solution as it makes more theoretical sense and adding the fifth factor does not split
# up the Others Perspectives factor as anticipated.  We will create the four subscales based on this solution
# and rename Others Perspectives to Negative Experiences and not use the reverse coded items for them.

MEQ$MEQ.TeacherSupport = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.T10_1", "MEQ.T11_1", "MEQ.T8_1", "MEQ.T5_1",
                                                              "MEQ.T25_1", "MEQ.T19_1", "MEQ.T9_1", "MEQ.T15_1",
                                                              "MEQ.T4_1", "MEQ.T24_1", "MEQ.T27_1", "MEQ.T1_1",
                                                              "MEQ.T18_1", "MEQ.T7_1", "MEQ.T20_1", "MEQ.T23_1",
                                                              "MEQ.T14_1", "MEQ.T21_1", "MEQ.T2_1", "MEQ.T22_1")],
                                                       as.numeric)),
                                  na.rm = TRUE)

MEQ$MEQ.ParentSupport = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.PAR4_1", "MEQ.PAR3_1", "MEQ.PAR8_1", "MEQ.PAR10_1",
                                                             "MEQ.PAR9_1", "MEQ.PAR1_1", "MEQ.PAR11_1", "MEQ.PAR5R",
                                                             "MEQ.PAR2_1", "MEQ.PAR13_1", "MEQ.PAR7R")],
                                                      as.numeric)),
                                 na.rm = TRUE)

MEQ$MEQ.PeerSupport = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.PR5_1", "MEQ.PR7_1", "MEQ.PR10_1", "MEQ.PR2_1",
                                                           "MEQ.PR1_1", "MEQ.PR11_1", "MEQ.PR6R", "MEQ.PR4_1",
                                                           "MEQ.PR12_1", "MEQ.PR9_1")],
                                                    as.numeric)),
                               na.rm = TRUE)

MEQ$MEQ.NegativeExperiences = rowMeans(as.data.frame(lapply(MEQ[,c("MEQ.PAR12_1", "MEQ.T13_1", "MEQ.T17_1", "MEQ.T26_1",
                                                                   "MEQ.T16_1", "MEQ.T6_1")],
                                                           as.numeric)),
                                      na.rm = TRUE)


# Now lets do Omega reliabilities for these MEQ Factors

MEQ_omegas = numeric()

# Define the vector of Teacher Support
tsupp_vars <- c("MEQ.T10_1", "MEQ.T11_1", "MEQ.T8_1", "MEQ.T5_1",
                "MEQ.T25_1", "MEQ.T19_1", "MEQ.T9_1", "MEQ.T15_1",
                "MEQ.T4_1", "MEQ.T24_1", "MEQ.T27_1", "MEQ.T1_1",
                "MEQ.T18_1", "MEQ.T7_1", "MEQ.T20_1", "MEQ.T23_1",
                "MEQ.T14_1", "MEQ.T21_1", "MEQ.T2_1", "MEQ.T22_1")
# Select and convert to numeric
tsupp_items <- as.data.frame(lapply(MEQ[, tsupp_vars], as.numeric))
# Run omega analysis
omega_result <- omega(tsupp_items, nfactors = 1)
# Extract and save omega total to vector of MEQ omegas 
MEQ_omegas = c(MEQ_omegas, "Teacher Support" = omega_result$omega.tot)


# Define the vector of Parent Support
parsupp_vars <- c("MEQ.PAR4_1", "MEQ.PAR3_1", "MEQ.PAR8_1", "MEQ.PAR10_1",
                  "MEQ.PAR9_1", "MEQ.PAR1_1", "MEQ.PAR11_1", "MEQ.PAR5R",
                  "MEQ.PAR2_1", "MEQ.PAR13_1", "MEQ.PAR7R")
# Select and convert to numeric
parsupp_items <- as.data.frame(lapply(MEQ[, parsupp_vars], as.numeric))
# Run omega analysis
omega_result <- omega(parsupp_items, nfactors = 1)
# Extract and save omega total to vector of MEQ omegas 
MEQ_omegas = c(MEQ_omegas, "Parent Support" = omega_result$omega.tot)

# Define the vector of Peer Support
prsupp_vars <- c("MEQ.PR5_1", "MEQ.PR7_1", "MEQ.PR10_1", "MEQ.PR2_1",
                 "MEQ.PR1_1", "MEQ.PR11_1", "MEQ.PR6R", "MEQ.PR4_1",
                 "MEQ.PR12_1", "MEQ.PR9_1")
# Select and convert to numeric
prsupp_items <- as.data.frame(lapply(MEQ[, prsupp_vars], as.numeric))
# Run omega analysis
omega_result <- omega(prsupp_items, nfactors = 1)
# Extract and save omega total to vector of MEQ omegas 
MEQ_omegas = c(MEQ_omegas, "Peer Support" = omega_result$omega.tot)

# Define the vector of Others Perspectives/Negative Experiences
op_vars <- c("MEQ.PAR12_1", "MEQ.T13_1", "MEQ.T17_1", "MEQ.T26_1",
             "MEQ.T16_1", "MEQ.T6_1")
# Select and convert to numeric
op_items <- as.data.frame(lapply(MEQ[, op_vars], as.numeric))
# Run omega analysis
omega_result <- omega(op_items, nfactors = 1)
# Extract and save omega total to vector of MEQ omegas 
MEQ_omegas = c(MEQ_omegas, "Negative Experiences" = omega_result$omega.tot)

MEQ_omegas


# Now report descriptives of main study variables

# First lets look at descriptive statistics for each main measure

desc = describe(MEQ[, c("MARS", "TAI", "BAI", "PSWQ", "MIS",
                        "MEQ.TeacherSupport", "MEQ.ParentSupport", "MEQ.PeerSupport", 
                        "MEQ.NegativeExperiences",
                        "AMS_Intrinsic", "AMS_ExtrinsicIdentified","AMS_ExtrinsicIntrojected", "AMS_ExtrinsicExternal",
                        "AMS_Amotivation", "FractionTotal", "AlgebraTotal", "FluencyTotal", "HighSchoolMark")])

# Compute Skew & Kurtosis SE's
desc$Skew_SE = sqrt(6 / desc$n)
desc$Kurtosis_SE = sqrt(24 / desc$n)

# Format Descriptives Output
desc_table = data.frame(
  Measure = rownames(desc),
  'Mean (SD)' = sprintf("%.2f (%.2f)", desc$mean, desc$sd),
  'Skew (SE)' = sprintf("%.3f (%.3f)", desc$skew, desc$Skew_SE),
  'Kurtosis (SE)' = sprintf("%.3f (%.3f)", desc$kurtosis, desc$Kurtosis_SE)
)

# Reset rownames and print
rownames(desc_table) = NULL
print(desc_table)

# Now let's look at the bivariate correlations between the variables

corrMatrix(MEQ, c("MARS", "TAI", "BAI", "PSWQ"))

corrMatrix(MEQ, c("MARS", "TAI", "BAI", "PSWQ",
                  "MEQ.TeacherSupport", "MEQ.ParentSupport", "MEQ.PeerSupport", 
                  "MEQ.NegativeExperiences",
                  "AMS_Intrinsic", "AMS_ExtrinsicIdentified","AMS_ExtrinsicIntrojected", "AMS_ExtrinsicExternal",
                  "AMS_Amotivation", "MIS", "FractionTotal", "AlgebraTotal", "FluencyTotal", "HighSchoolMark"))


# Teachers
linReg(MEQ, dep = MARS, covs = vars(MEQ.TeacherSupport, PSWQ, BAI, TAI), blocks = list(list("MEQ.TeacherSupport",
                                                                                            "PSWQ",
                                                                                            "BAI",
                                                                                            "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE,  stdEst = TRUE)

# Calculate the semi-partial correlations

semiCorrs = numeric()
allPredictors = c("MEQ.TeacherSupport", "PSWQ", "BAI", "TAI")
for (i in 1:length(allPredictors)) {
  targetVars = c("MARS", allPredictors[i])
  predictors = allPredictors[-i]
  results = corrPart(
    data = MEQ,
    vars = targetVars[1:2],
    controls = predictors[1:3],
    type = "semi")
  resultMatrix = results$matrix$asDF
  semiCorrs[i] = as.numeric(resultMatrix[1, paste0(allPredictors[i], "[r]") == names(resultMatrix)])
  names(semiCorrs)[i] = allPredictors[i] 
}

semiCorrs
  
# Parents
linReg(MEQ, dep = MARS, covs = vars(MEQ.ParentSupport, PSWQ, BAI, TAI), blocks = list(list("MEQ.ParentSupport",
                                                                                           "PSWQ",
                                                                                           "BAI",
                                                                                           "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)

# Calculate the semi-partial correlations

semiCorrs = numeric()
allPredictors = c("MEQ.ParentSupport", "PSWQ", "BAI", "TAI")
for (i in 1:length(allPredictors)) {
  targetVars = c("MARS", allPredictors[i])
  predictors = allPredictors[-i]
  results = corrPart(
    data = MEQ,
    vars = targetVars[1:2],
    controls = predictors[1:3],
    type = "semi")
  resultMatrix = results$matrix$asDF
  semiCorrs[i] = as.numeric(resultMatrix[1, paste0(allPredictors[i], "[r]") == names(resultMatrix)])
  names(semiCorrs)[i] = allPredictors[i] 
}

semiCorrs

# Peers
linReg(MEQ, dep = MARS, covs = vars(MEQ.PeerSupport, PSWQ, BAI, TAI), blocks = list(list("MEQ.PeerSupport",
                                                                                         "PSWQ",
                                                                                         "BAI",
                                                                                         "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)


# Calculate the semi-partial correlations

semiCorrs = numeric()
allPredictors = c("MEQ.PeerSupport", "PSWQ", "BAI", "TAI")
for (i in 1:length(allPredictors)) {
  targetVars = c("MARS", allPredictors[i])
  predictors = allPredictors[-i]
  results = corrPart(
    data = MEQ,
    vars = targetVars[1:2],
    controls = predictors[1:3],
    type = "semi")
  resultMatrix = results$matrix$asDF
  semiCorrs[i] = as.numeric(resultMatrix[1, paste0(allPredictors[i], "[r]") == names(resultMatrix)])
  names(semiCorrs)[i] = allPredictors[i] 
}

semiCorrs

# Negative Experiences
linReg(MEQ, dep = MARS, covs = vars(MEQ.NegativeExperiences, PSWQ, BAI, TAI), blocks = list(list("MEQ.NegativeExperiences",
                                                                                             "PSWQ",
                                                                                             "BAI",
                                                                                             "TAI")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)

# Calculate the semi-partial correlations

semiCorrs = numeric()
allPredictors = c("MEQ.NegativeExperiences", "PSWQ", "BAI", "TAI")
for (i in 1:length(allPredictors)) {
  targetVars = c("MARS", allPredictors[i])
  predictors = allPredictors[-i]
  results = corrPart(
    data = MEQ,
    vars = targetVars[1:2],
    controls = predictors[1:3],
    type = "semi")
  resultMatrix = results$matrix$asDF
  semiCorrs[i] = as.numeric(resultMatrix[1, paste0(allPredictors[i], "[r]") == names(resultMatrix)])
  names(semiCorrs)[i] = allPredictors[i] 
}

semiCorrs



# Let's see how predictive the Negative Experiences is

# Check for controlling for Math Ability

linReg(MEQ, dep = MARS, covs = vars(MEQ.NegativeExperiences, PSWQ, BAI, TAI, FractionTotal, AlgebraTotal, FluencyTotal), blocks = list(list("MEQ.NegativeExperiences",
                                                                                                                                           "PSWQ",
                                                                                                                                           "BAI",
                                                                                                                                           "TAI", 
                                                                                                                                           "FractionTotal",
                                                                                                                                           "AlgebraTotal",
                                                                                                                                           "FluencyTotal")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)

# Check for controlling for both math ability & reported mark

linReg(MEQ, dep = MARS, covs = vars(MEQ.NegativeExperiences, PSWQ, BAI, TAI, HighSchoolMark, FractionTotal, AlgebraTotal, FluencyTotal), blocks = list(list("MEQ.NegativeExperiences",
                                                                                                                                                           "PSWQ",
                                                                                                                                                           "BAI",
                                                                                                                                                           "TAI",
                                                                                                                                                           "HighSchoolMark",
                                                                                                                                                           "FractionTotal",
                                                                                                                                                           "AlgebraTotal",
                                                                                                                                                           "FluencyTotal")),
       ci = TRUE, model = TRUE, r2Adj = TRUE, stdEst = TRUE)



# Like in study one, we will test for a mediation, this time with Math Identity.  We will again use the JAMM package 
# used in Jamovi, which means we need devtools to install it.

install.packages("devtools")
install.packages("jmvcore", repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
devtools::install_github("jamovi-amm/jamm")


jamm::jammGLM(
  formula = list( MIS ~ MEQ.NegativeExperiences,
                  MARS ~ MIS + MEQ.NegativeExperiences + BAI + TAI + PSWQ ),
  data = MEQ,
  ciType = "bca",
  tableOptions = c("beta", "component", "regression"))
