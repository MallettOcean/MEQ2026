library(jmv)

# Import the de-identified raw data.  The ID variable is simply the row number of the original raw data file.

MEQ = read.csv("Study 2 Raw Data.csv")

# The following details how the data was screened for attention checks. Those who failed the attention check did not get
# paid for their data, so their data is not included in the raw file.  The following syntax demonstrates how those 
# attention checks were calculated.  One attention check item, FRA_Attention, was dropped as an attention check because
# it was not a clear attention check that confused too many people.  Everybody included in the raw data passed all
# four of the following attention checks.

MEQ$AttentionCheck1 = MEQ$PSWQ12_1 == "5"
MEQ$AttentionCheck2 = MEQ$TAI8_1 == "1"
MEQ$AttentionCheck3 = MEQ$MARS31_1 == "1"
MEQ$AttentionCheck4 = MEQ$MEQ.PAR6_1 == "4"

MEQ$AttentionSum = rowSums(MEQ[, c("AttentionCheck1", "AttentionCheck2", "AttentionCheck3", "AttentionCheck4")], na.rm = TRUE)

MEQ = MEQ[MEQ$AttentionSum == 4, ]



# Create the scales from the items

# First, the PSWQ
# PSWQ12 is an attention check

# Reverse code some of the items

descriptives(MEQ[,grep("PSWQ", names(MEQ))], desc = "rows")

MEQ$PSWQ1R = 6 - as.numeric(MEQ$PSWQ1_1)
MEQ$PSWQ3R = 6 - as.numeric(MEQ$PSWQ3_1)
MEQ$PSWQ8R = 6 - as.numeric(MEQ$PSWQ8_1)
MEQ$PSWQ10R = 6 - as.numeric(MEQ$PSWQ10_1)
MEQ$PSWQ11R = 6 - as.numeric(MEQ$PSWQ11_1)

MEQ$PSWQ = rowMeans(as.data.frame(lapply(MEQ[,c("PSWQ1R", "PSWQ2_1", "PSWQ3R", "PSWQ4_1", 
                                                "PSWQ5_1", "PSWQ6_1", "PSWQ7_1", "PSWQ8R",
                                                "PSWQ9_1", "PSWQ10R", "PSWQ11R", "PSWQ13_1",
                                                "PSWQ14_1", "PSWQ15_1", "PSWQ16_1", "PSWQ17_1")],
                                         as.numeric)),
                    na.rm = TRUE)


# Subscales of the AMS


descriptives(MEQ[,grep("AMS", names(MEQ))], desc = "rows")

MEQ$AMS_IntrinsicKnow = rowMeans(as.data.frame(lapply(MEQ[,c("AMS1_1", "AMS8_1", "AMS15_1", "AMS22_1")],
                                                      as.numeric)))
MEQ$AMS_IntrinsicAccomplish = rowMeans(as.data.frame(lapply(MEQ[,c("AMS5_1", "AMS12_1", "AMS19_1", "AMS26_1")],
                                                            as.numeric)))
MEQ$AMS_IntrinsicStimulation = rowMeans(as.data.frame(lapply(MEQ[,c("AMS3_1", "AMS10_1", "AMS17_1", "AMS24_1")],
                                                             as.numeric)))
MEQ$AMS_ExtrinsicIdentified = rowMeans(as.data.frame(lapply(MEQ[,c("AMS2_1", "AMS9_1", "AMS16_1", "AMS23_1")],
                                                            as.numeric)))
MEQ$AMS_ExtrinsicIntrojected = rowMeans(as.data.frame(lapply(MEQ[,c("AMS6_1", "AMS13_1", "AMS20_1", "AMS27_1")],
                                                             as.numeric)))
MEQ$AMS_ExtrinsicExternal = rowMeans(as.data.frame(lapply(MEQ[,c("AMS8_1", "AMS14_1", "AMS21_1")],
                                                          as.numeric)))
MEQ$AMS_Amotivation = rowMeans(as.data.frame(lapply(MEQ[,c("AMS4_1", "AMS11_1", "AMS18_1", "AMS25_1")],
                                                    as.numeric)))

# 5 Factor Subscale of the AMS
MEQ$AMS_Intrinsic = rowMeans(as.data.frame(lapply(MEQ[,c("AMS1_1", "AMS8_1", "AMS15_1", "AMS22_1",
                                                         "AMS5_1", "AMS12_1", "AMS19_1", "AMS26_1",
                                                         "AMS3_1", "AMS10_1", "AMS17_1", "AMS24_1")],
                                                  as.numeric)))



# Test Anxiety Inventory coding
# TAI8 is an attention check

# Reverse coded item for the TAI

descriptives(MEQ[,grep("TAI", names(MEQ))], desc = "rows")

MEQ$TAI1R = 5 - as.numeric(MEQ$TAI1_1)

MEQ$TAI = rowMeans(as.data.frame(lapply(MEQ[,c("TAI1R", "TAI2_1", "TAI3_1", "TAI4_1",
                                               "TAI5_1", "TAI6_1", "TAI7_1", "TAI9_1",
                                               "TAI10_1", "TAI11_1", "TAI12_1", "TAI13_1",
                                               "TAI14_1", "TAI15_1", "TAI16_1", "TAI17_1",
                                               "TAI18_1", "TAI19_1", "TAI20_1", "TAI21_1")],
                                        as.numeric)),
                   na.rm = TRUE)

# Math Anxiety Rating Scale (Short Version) coding
# MARS31 is an attention check

# Items for Mars-S

descriptives(MEQ[,grep("MARS", names(MEQ))], desc = "rows")

MEQ$MARS  = rowMeans(as.data.frame(lapply(MEQ[,c("MARS1_1", "MARS2_1", "MARS3_1", "MARS4_1",
                                                  "MARS5_1", "MARS6_1", "MARS7_1", "MARS8_1",
                                                  "MARS9_1", "MARS10_1", "MARS11_1", "MARS12_1",
                                                  "MARS13_1", "MARS14_1", "MARS15_1", "MARS16_1",
                                                  "MARS17_1", "MARS18_1", "MARS19_1", "MARS20_1",
                                                  "MARS21_1", "MARS22_1", "MARS23_1", "MARS24_1",
                                                  "MARS25_1", "MARS26_1", "MARS27_1", "MARS28_1",
                                                  "MARS29_1", "MARS30_1")],
                                           as.numeric)),
                      na.rm = TRUE)

# Beck Anxiety Inventory coding
# Items for BAI

descriptives(MEQ[,grep("BAI", names(MEQ))], desc = "rows")

MEQ$BAI = rowMeans(as.data.frame(lapply(MEQ[,c("BAI1_1", "BAI2_1", "BAI3_1", "BAI4_1",
                                               "BAI5_1", "BAI6_1", "BAI7_1", "BAI8_1",
                                               "BAI9_1", "BAI10_1", "BAI11_1", "BAI12_1",
                                               "BAI13_1", "BAI14_1", "BAI15_1", "BAI16_1",
                                               "BAI17_1", "BAI18_1", "BAI19_1", "BAI20_1")],
                                        as.numeric)),
                   na.rm = TRUE)

# Math Identity Scale 

descriptives(MEQ[,grep("MIS", names(MEQ))], desc = "rows")

# Reverse code one item
MEQ$MIS6R = 5 - as.numeric(MEQ$MIS6_1)

# MIS Items
MEQ$MIS = rowMeans(as.data.frame(lapply(MEQ[, c("MIS1_1", "MIS2_1", "MIS3_1",
                                                "MIS4_1", "MIS5_1", "MIS6R")],
                                        as.numeric)),
                   na.rm = TRUE)

MEQ$MIS.Personal = rowMeans(as.data.frame(lapply(MEQ[, c("MIS1_1", "MIS5_1", "MIS6R")],
                                        as.numeric)),
                   na.rm = TRUE)

MEQ$MIS.Perceived = rowMeans(as.data.frame(lapply(MEQ[, c("MIS2_1", "MIS3_1", "MIS4_1")],
                                        as.numeric)),
                   na.rm = TRUE)
# MEQ

# MEQ Teacher Block coding

descriptives(MEQ[,grep("MEQ.T", names(MEQ))], desc = "rows")

# Take out the "Do Not Recall" option and recode it as missing

MEQ[,grep("MEQ.T", names(MEQ))] = lapply(MEQ[,grep("MEQ.T", names(MEQ))], function(x) {x[x == 9] = NA_integer_
                                                                                       return(x)})

descriptives(MEQ[,grep("MEQ.T", names(MEQ))], desc = "rows")

# Look for straight lining

MEQ$TeacherItemSD = apply(MEQ[,grep("MEQ.T", names(MEQ))], 1, sd, na.rm = TRUE)
View(MEQ[order(MEQ$TeacherItemSD),c(names(MEQ)[grep("MEQ.T", names(MEQ))], "TeacherItemSD")])

# There seems to be one clear case of repeated pattern (i.e., 5, 4, 5, 4, etc.)

# Reverse coded items for MEQ Teacher Block
MEQ$MEQ.T3R = 6 - as.numeric(MEQ$MEQ.T3_1)
MEQ$MEQ.T6R = 6 - as.numeric(MEQ$MEQ.T6_1)
MEQ$MEQ.T12R = 6 - as.numeric(MEQ$MEQ.T12_1)
MEQ$MEQ.T13R = 6 - as.numeric(MEQ$MEQ.T13_1)
MEQ$MEQ.T16R = 6 - as.numeric(MEQ$MEQ.T16_1)
MEQ$MEQ.T17R = 6 - as.numeric(MEQ$MEQ.T17_1)
MEQ$MEQ.T26R = 6 - as.numeric(MEQ$MEQ.T26_1)


# MEQ Parent block coding
# MEQ.PAR6 is an attention check

descriptives(MEQ[,grep("MEQ.PAR", names(MEQ))], desc = "rows")

# Take out the "Do Not Recall" option and recode it as missing

MEQ[,grep("MEQ.PAR", names(MEQ))] = lapply(MEQ[,grep("MEQ.PAR", names(MEQ))], function(x) {x[x == 7] = NA_integer_
                                                                                           return(x)})

descriptives(MEQ[,grep("MEQ.PAR", names(MEQ))], desc = "rows")

# Reverse coded items for MEQ Parent block

MEQ$MEQ.PAR5R = 6 - as.numeric(MEQ$MEQ.PAR5_1)
MEQ$MEQ.PAR7R = 6 - as.numeric(MEQ$MEQ.PAR7_1)
MEQ$MEQ.PAR12R = 6 - as.numeric(MEQ$MEQ.PAR12_1)


# MEQ Peer block coding

descriptives(MEQ[,grep("MEQ.PR", names(MEQ))], desc = "rows")

# Take out the "Do Not Recall" option and recode it as missing

MEQ[,grep("MEQ.PR", names(MEQ))] = lapply(MEQ[,grep("MEQ.PR", names(MEQ))], function(x) {x[x == 6] = NA_integer_
return(x)})

descriptives(MEQ[,grep("MEQ.PR", names(MEQ))], desc = "rows")
# Reverse coded items for MEQ Peer block

MEQ$MEQ.PR6R = 6 - as.numeric(MEQ$MEQ.PR6_1)
MEQ$MEQ.PR8R = 6 - as.numeric(MEQ$MEQ.PR8_1)



# Now Convert demographic variables to proper formats for analysis and assess descriptives

MEQ$Age = as.numeric(MEQ$DEM1)

descriptives(MEQ, "Age")

# Check to see all the variations in Gender

table(MEQ$DEM2, useNA = "ifany")
MEQ$Gender = MEQ$DEM2

MEQ$Gender[MEQ$DEM2 == "F"] = "Female"
MEQ$Gender[MEQ$DEM2 == "female"] = "Female"
MEQ$Gender[MEQ$DEM2 == "FEMALE"] = "Female"
MEQ$Gender[MEQ$DEM2 == "Female "] = "Female"
MEQ$Gender[MEQ$DEM2 == "Femalee"] = "Female"
MEQ$Gender[MEQ$DEM2 == "woman"] = "Female"
MEQ$Gender[MEQ$DEM2 == "Woman"] = "Female"
MEQ$Gender[MEQ$DEM2 == "Girl"] = "Female"
MEQ$Gender[MEQ$DEM2 == "MALE"] = "Male"
MEQ$Gender[MEQ$DEM2 == "male"] = "Male"
MEQ$Gender[MEQ$DEM2 == "male "] = "Male"
MEQ$Gender[MEQ$DEM2 == "Male "] = "Male"
MEQ$Gender[MEQ$DEM2 == "malw"] = "Male"
MEQ$Gender[MEQ$DEM2 == "cis male"] = "Male"
MEQ$Gender[MEQ$DEM2 == "Non Binart"] = "Non-Binary"
MEQ$Gender[MEQ$DEM2 == "non-binary"] = "Non-Binary"
MEQ$Gender[MEQ$DEM2 == "nonbinary"] = "Non-Binary"
MEQ$Gender[MEQ$DEM2 == "transgender female"] = "Transgender Female"

# Check to see what codes still need to be classified

table(MEQ$Gender, useNA = "ifany")
table(MEQ$DEM2, MEQ$Gender, useNA = "ifany")

 # Check Gender Descriptives 

MEQ$Gender = factor(MEQ$Gender, levels = c("Female", "Male"))
table(MEQ$Gender, useNA = "ifany")

# Ethnicity

MEQ$Ethnicity = MEQ$DEM3

MEQ$Ethnicity[MEQ$DEM3 == "1"] = "Black"
MEQ$Ethnicity[MEQ$DEM3 == "2"] = "White"
MEQ$Ethnicity[MEQ$DEM3 == "3"] = "Middle Eastern / West Asian"
MEQ$Ethnicity[MEQ$DEM3 == "4"] = "West Asian"
MEQ$Ethnicity[MEQ$DEM3 == "5"] = "Pacific Islander"
MEQ$Ethnicity[MEQ$DEM3 == "6"] = "Latino/a"
MEQ$Ethnicity[MEQ$DEM3 == "7"] = "Indigenous"
MEQ$Ethnicity[MEQ$DEM3 == "8"] = "South Asian"
MEQ$Ethnicity[MEQ$DEM3 == "9"] = "Southeast Asian"
MEQ$Ethnicity[MEQ$DEM3 == "10"] = "Culture Not Listed"

# Check Ethnicity Descriptives 

table(MEQ$Ethnicity, useNA = "ifany")

# Highest Level of Education
MEQ$Education = MEQ$DEM4

MEQ$Education[MEQ$DEM4 == "1"] = "Less than High School"
MEQ$Education[MEQ$DEM4 == "2"] = "High School"
MEQ$Education[MEQ$DEM4 == "3"] = "Undergraduate Degree"
MEQ$Education[MEQ$DEM4 == "4"] = "Master's Degree"
MEQ$Education[MEQ$DEM4 == "5"] = "Ph.D"

# Check Education Descriptives 

table(MEQ$Education, useNA = "ifany")

# Check to see all the variations in Program of Study

table(MEQ$DEM5, useNA = "ifany")
MEQ$StudyArea = MEQ$DEM5

MEQ$StudyArea[MEQ$DEM5 == "accounting"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Accounting"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Anthropology and Journalism"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Applied Math, Economics, Law"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "Architecture"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "ARCHITECTURE"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "art"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Associate's Degree in Marketing"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Automotive"] = "Trade"
MEQ$StudyArea[MEQ$DEM5 == "B. Sc Computer Science"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "b. sc mathematics"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "B. Sc. Development Studies"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B. Sc. Marketing "] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B. Tech Computer Science"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "B.S. Computer Science"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. in Information Technology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc Education"] = "Education"
MEQ$StudyArea[MEQ$DEM5 == "B.A Honours Psychology; M.Sc Experiemental and Applied Psychology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.A Linguistics"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "B.A. Agricultural Economics"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.A. Arts & Humanities"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "B.A. Communication "] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "B.A. English"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "B.A. English Literature"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA and MA English Literature"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA Modern Languages"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "B.A. History"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "B.B.A"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.E Computer Science"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "b.eng electronics engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "B.Eng. (Software Engineering)"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "B.S Biology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.S. Mathematics"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "B.sc Accounting"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "BA Marketing"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Bsc Business Administration "] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc chemistry"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc Computer Science"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc Environmental Management "] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc Finance"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc human recourses management "] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.sc Information Technology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.SC MEDICAL"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc Statistics"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Accounting"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Business Administration "] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Computer science"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Computer Science"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Computer science "] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Computer technology."] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Economics"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B. Sc. Economics"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.S. Information Systems"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Business Administration"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "English"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "History"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "I did an NVQ level 3 in business and nursing studies"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "History"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Education"] = "Education"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Electrical Engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "Mechanical Engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Engineering / manufacturing and construction."] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc Environmental Management"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Finance"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Mathematics and Statistics"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "B. Sc. Marketing "] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Nursing"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "B.Sc. Statistics"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "BA"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA (Hons) in Journalism Studies"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA and MA English Literature "] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA Art Design"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "B.A. Agricultural Economics "] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "BA English, minor in Education"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA Film and Media"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA Media & Performance"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA History"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA Hons English and Drama"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA Hons Fine Art"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "BA Psychology"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Bachelor in Business Administration"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Bachelor of Arts"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "bachelor of science"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Bachelor of Science in Hospitality and Tourism Management"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Bachelor of Science in Nursing"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Baking Production and Management Technology"] = "Trade"
MEQ$StudyArea[MEQ$DEM5 == "BEng Electronic Engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "Biological Sciences"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "bs computer science"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "BS Electrical Engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "BS healthcare"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "bsc -hons - computer science 2:1"] = "Computer Science"
MEQ$StudyArea[MEQ$DEM5 == "BSC ADMINISTRATION"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "bsc cyber security"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "BSc Hons Biology (Marine Biology)"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Bsc in Art Therapy"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Bsc Information Technology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "BSc Physics with Astrophysics, MSc Physics, MSc Data Analytics"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "BSc Psychology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "BSC statistical engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "bsc,botany"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "business"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Bachelor of Management and Organizational Studies"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Business & Financial Economics"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Business Administration"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Business Administration "] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Business Adminstration"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Business Managment"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Business Studies with Law"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Carpentry"] = "Trade"
MEQ$StudyArea[MEQ$DEM5 == "ceramics, art and design BA hons"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "chemistry"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Civil Engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "Commerce"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Communication Studies"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Creative Computing"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "criminal justice"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Criminal Justice"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Criminal Justice"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "digtal media "] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Doctorate"] = "Other"
MEQ$StudyArea[MEQ$DEM5 == "economics"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Economics and finance BA"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Economics"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "economy"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Education"] = "Education"
MEQ$StudyArea[MEQ$DEM5 == "Electrical Engineering."] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "Engineering Diploma"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "english literature and law"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "FASHION AND TEXTILES"] = "Trade"
MEQ$StudyArea[MEQ$DEM5 == "Finance"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Finance & Accounting"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Fine Art"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Fine Arts"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "general studies"] = "Other"
MEQ$StudyArea[MEQ$DEM5 == "geography"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "German (BA), Information Science (MS)"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Graphic Design"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "health  sciences"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Health Science"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "HNC Interior Architecture"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Honours B.Sc. Psychology with a minor in Math and Bachelor of Education in Intermediate/Senior Teaching with Math and Social Sciences teachables"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "human kinetics"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "HUMAN RESOURCES"] = "Other"
MEQ$StudyArea[MEQ$DEM5 == "HUMANITIES"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "I did not"] = "No Schooling"
MEQ$StudyArea[MEQ$DEM5 == "I'm currently finishing the first year of BSc Psychology as a mature student."] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "ICAEW ACA after secondary school"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "ict"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Information and technology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Information Technology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Information technology "] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Interdisciplinary Studies"] = "Other"
MEQ$StudyArea[MEQ$DEM5 == "International Development"] = "Other"
MEQ$StudyArea[MEQ$DEM5 == "IT"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "law"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Law"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Law & History"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Legal Office Administration Diploma (2 years, no option available)"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Level 3 Extended Diploma in IT and Computing"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Linguistics"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "M.D doctor of medicine"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "M.Sc. Computer Science "] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "M.Sc. Engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "MA(Hons) Politics and History, MSc Politics"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Management"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Marketing"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Mathematic"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "Mathematics"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "mathemTICS"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "maths"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "MBA"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "MBA (business); MSC creative writing; MA English; HR and banking professional qualifications"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "mechanical engineering"] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "MMath Mathematics"] = "Math"
MEQ$StudyArea[MEQ$DEM5 == "MPhys Physics"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Msc. Financial Data Analytics"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Marine Biology, Nautical Science, Marine Operations and Management"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "MS in Biomedical science"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "n/a"] = "No Schooling"
MEQ$StudyArea[MEQ$DEM5 == "N/A"] = "No Schooling"
MEQ$StudyArea[MEQ$DEM5 == "na"] = "No Schooling"
MEQ$StudyArea[MEQ$DEM5 == "Neurology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Not applicable"] = "No Schooling"
MEQ$StudyArea[MEQ$DEM5 == "none"] = "No Schooling"
MEQ$StudyArea[MEQ$DEM5 == "Nursing"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "oil and gas management"] = "Trade"
MEQ$StudyArea[MEQ$DEM5 == "Organizational Leadership"] = "Other"
MEQ$StudyArea[MEQ$DEM5 == "Orthodontics"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Pharmacy"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Political Science"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Pre-Law"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Psychology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Psychology "] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Psychology BSc"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Psychology, biology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "Psycology"] = "Science"
MEQ$StudyArea[MEQ$DEM5 == "public administration"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "Public Administration"] = "Business"
MEQ$StudyArea[MEQ$DEM5 == "social work"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Social Work"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Sociology"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Sociology / English"] = "Arts"
MEQ$StudyArea[MEQ$DEM5 == "Software Engineering "] = "Engineering"
MEQ$StudyArea[MEQ$DEM5 == "VIDEO GAME ANIMATION"] = "Other"

table(MEQ$StudyArea, useNA = "ifany")

# Check to see all the variations in Occupation

table(MEQ$DEM6, useNA = "ifany")
MEQ$Occupation = MEQ$DEM6

# **Check Code for this one
MEQ$Occupation[MEQ$DEM6 == "Data Annotation Specialist"] = "Data/IT"

MEQ$Occupation[MEQ$DEM6 == "35"] = "Other"
MEQ$Occupation[MEQ$DEM6 == "Academic Administration"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Account Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Accountant"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "ACOCUNT RECOVERY"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Actuarial Analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Actuary"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Architect"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "ARCHITECT"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "archive/library (temp unemployed)"] = "Unemployed"
MEQ$Occupation[MEQ$DEM6 == "Art and illustration"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Artist"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Asset Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Associate Director "] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Auditor"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Automobile Assembly Production Worker"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "Barrister"] = "Law"
MEQ$Occupation[MEQ$DEM6 == "bartender "] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "billing supervisor"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Business Analytics"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Business Owner"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "cashier"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Cashier"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "CEO"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Chef/ Owner/ GM"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Chemical Engineer"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Chief Information Officer"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Civil Engineer"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Clerical assistant"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Clerk"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "code checker in retail"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "commercial and house cleaner"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Company Director"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "computer science"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "cook"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Courier"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Creative Director"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Customer Service"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "customer service rep"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Customer Support Analyst"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Cyber Security Analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Data Analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Data analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "data analyst "] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Data Processor"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Desidn"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Designer"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Digital Content Designer"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Director of HR"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Dispenser"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "doctor"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Doctor"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "DOCTOR"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "E-commerce"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Economist"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Education Manager"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "education professional"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Education professional"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Educator"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "educator"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Enforcement Officer"] = "First Responder"
MEQ$Occupation[MEQ$DEM6 == "engineer"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Engineering"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Engineering Consultant"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "engineering manager"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Entrepreneur "] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Escalation Support"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Exam invigilator"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Extension Officer "] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Faculty Assistant"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Fashion Designer "] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Finance Analyst"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Finance and Insurance"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Financial Accountant"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Financial Analyst"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Fire protection"] = "First Responder"
MEQ$Occupation[MEQ$DEM6 == "freelancer"] = "Self Employed"
MEQ$Occupation[MEQ$DEM6 == "Gig work online"] = "Self Employed"
MEQ$Occupation[MEQ$DEM6 == "Graphic Design"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Graphic Designer"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "head of department HR"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Head of Sales "] = "Management"
MEQ$Occupation[MEQ$DEM6 == "HEALTHCARE"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Healthcare worker"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "healthcare worker"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Help Desk worker"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "homemaker"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Homemaker"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "horticulture"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Hospitality"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "HR Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "HR Manager "] = "Management"
MEQ$Occupation[MEQ$DEM6 == "I am currently a student"] = "Student"
MEQ$Occupation[MEQ$DEM6 == "information services"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Information Technology"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Information Technology manager"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Insurance Claims handler"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "IT"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "IT developer"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "IT manager"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "IT Manager"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "IT MANAGER"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "IT specialist "] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "IT speciliast"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "IT support - SAP"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Junior Engineer Designer"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "JUNIOR MANAGENT"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "lab administrator/admin assistant"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Language Interpreter"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "lawyer"] = "Law"
MEQ$Occupation[MEQ$DEM6 == "Legal Assistant"] = "Law"
MEQ$Occupation[MEQ$DEM6 == "linguist"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Local Government Officer"] = "First Responder"
MEQ$Occupation[MEQ$DEM6 == "Logistic supervisor"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "logistics"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Management Consultant"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Management Consulting"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Marketer"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Marketing and sales"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Marketing"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Marketing manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Medical Billing"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "medical student"] = "Student"
MEQ$Occupation[MEQ$DEM6 == "merchant navigation officer"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "Minister of Religion"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "N\\A"] = "Unemployed"
MEQ$Occupation[MEQ$DEM6 == "none"] = "Unemployed"
MEQ$Occupation[MEQ$DEM6 == "Network administrator "] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Nurse"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Nurse "] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Nurse Practitioner"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Office Administrator"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Office Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "operations manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Operations Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Operator "] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "organ donor coordinator"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Orthodontist"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Owner"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "PA"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Package Handler"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Paralegal "] = "Law"
MEQ$Occupation[MEQ$DEM6 == "payroll"] = "Business"
MEQ$Occupation[MEQ$DEM6 == "Pharmacist"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Photographer"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Pilot"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Police Officer"] = "First Responder"
MEQ$Occupation[MEQ$DEM6 == "policy development"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "project assistant for a nonprofit"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "project coordinator"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Project Manager"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "Project Management"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "PROJECT MANAGER"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "Project Mnager"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "Product Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Professor"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Psychologist"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Psychologist"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Real Estate Agent"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Realtor"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "recording engineer"] = "Engineering"
MEQ$Occupation[MEQ$DEM6 == "Regional Property Manager "] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Registered Nurse"] = "Healthcare"
MEQ$Occupation[MEQ$DEM6 == "Research Analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Researcher "] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Restaurant General Manager"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "retail"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Retail Manager"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "retired"] = "Retired"
MEQ$Occupation[MEQ$DEM6 == "Retired "] = "Retired"
MEQ$Occupation[MEQ$DEM6 == "Retired English teacher"] = "Retired"
MEQ$Occupation[MEQ$DEM6 == "Retired librarian"] = "Retired"
MEQ$Occupation[MEQ$DEM6 == "risk manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "sales"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Sales"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Sales "] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "sales executive"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "sales manager"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Sales Manager"] = "Retail/Service"
MEQ$Occupation[MEQ$DEM6 == "Secondary education teacher"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Secretary"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "self employed "] = "Self Employed"
MEQ$Occupation[MEQ$DEM6 == "Self employed consultant"] = "Self Employed"
MEQ$Occupation[MEQ$DEM6 == "self employed small business owner"] = "Self Employed"
MEQ$Occupation[MEQ$DEM6 == "Senior Data Analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "senior logistical analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Senior manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Software Developer"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Software developer"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "Small business owner"] = "Self Employed"
MEQ$Occupation[MEQ$DEM6 == "software engineer"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Software engineer"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Software Engineer"] = "Engineer"
MEQ$Occupation[MEQ$DEM6 == "Special Education Teacher/IEP Coordinator/Case Manager"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "sr analyst"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "State data manager for elections//Independent contractor - gig/task-based worker"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "supervisor"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "Support"] = "Arts"
MEQ$Occupation[MEQ$DEM6 == "Sustainability Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "teacher"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Teacher"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "Technology Suervisor"] = "Data/IT"
MEQ$Occupation[MEQ$DEM6 == "telecommunications"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "Transport Manager"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "Transportation Manager"] = "Trade"
MEQ$Occupation[MEQ$DEM6 == "tutor"] = "Education"
MEQ$Occupation[MEQ$DEM6 == "unemployed"] = "Unemployed"
MEQ$Occupation[MEQ$DEM6 == "UNEMPLOYED"] = "Unemployed"
MEQ$Occupation[MEQ$DEM6 == "WAREHOUSE MANAGER FOR LOGISTICS COMPANY"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Workforce Intelligence Manager"] = "Management"
MEQ$Occupation[MEQ$DEM6 == "Yes Full Time"] = "Other"

table(MEQ$Occupation, useNA = "ifany")

# Change high school marks

table(MEQ$DEM7, useNA = "ifany")
MEQ$HighSchoolMark = MEQ$DEM7

MEQ$HighSchoolMark[MEQ$DEM7 == "2"] = "75"
MEQ$HighSchoolMark[MEQ$DEM7 == "2.5"] = "80"
MEQ$HighSchoolMark[MEQ$DEM7 == "2.5 GPA"] = "80"
MEQ$HighSchoolMark[MEQ$DEM7 == "2.8"] = "83"
MEQ$HighSchoolMark[MEQ$DEM7 == "3.0"] = "85"
MEQ$HighSchoolMark[MEQ$DEM7 == "3.1"] = "86"
MEQ$HighSchoolMark[MEQ$DEM7 == "3.2"] = "87"
MEQ$HighSchoolMark[MEQ$DEM7 == "3.24"] = "87"
MEQ$HighSchoolMark[MEQ$DEM7 == "3.3"] = "88"
MEQ$HighSchoolMark[MEQ$DEM7 == "3.5"] = "90"
MEQ$HighSchoolMark[MEQ$DEM7 == "3.8"] = "92"
MEQ$HighSchoolMark[MEQ$DEM7 == "3.5"] = "90"
MEQ$HighSchoolMark[MEQ$DEM7 == "4.0"] = "95"
MEQ$HighSchoolMark[MEQ$DEM7 == "4.0 "] = "95"
MEQ$HighSchoolMark[MEQ$DEM7 == "45%"] = "45"
MEQ$HighSchoolMark[MEQ$DEM7 == "68%"] = "68"
MEQ$HighSchoolMark[MEQ$DEM7 == "7 OUT OF 10"] = "70"
MEQ$HighSchoolMark[MEQ$DEM7 == "70 or B"] = "70"
MEQ$HighSchoolMark[MEQ$DEM7 == "70 percent"] = "70"
MEQ$HighSchoolMark[MEQ$DEM7 == "70%"] = "70"
MEQ$HighSchoolMark[MEQ$DEM7 == "70 to 80"] = "75"
MEQ$HighSchoolMark[MEQ$DEM7 == "75%"] = "75"
MEQ$HighSchoolMark[MEQ$DEM7 == "8"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "80%"] = "80"
MEQ$HighSchoolMark[MEQ$DEM7 == "85/100"] = "90"
MEQ$HighSchoolMark[MEQ$DEM7 == "85%"] = "85"
MEQ$HighSchoolMark[MEQ$DEM7 == "86%"] = "86"
MEQ$HighSchoolMark[MEQ$DEM7 == "87%"] = "87"
MEQ$HighSchoolMark[MEQ$DEM7 == "9/10"] = "90"
MEQ$HighSchoolMark[MEQ$DEM7 == "90%"] = "90"
MEQ$HighSchoolMark[MEQ$DEM7 == "95 "] = "95"
MEQ$HighSchoolMark[MEQ$DEM7 == "a"] = "95"
MEQ$HighSchoolMark[MEQ$DEM7 == "A"] = "95"
MEQ$HighSchoolMark[MEQ$DEM7 == "A+"] = "98"
MEQ$HighSchoolMark[MEQ$DEM7 == "A (I always finished with well over 100% by the end of the term)"] = "95"
MEQ$HighSchoolMark[MEQ$DEM7 == "A (top ~20% of class)"] = "95"
MEQ$HighSchoolMark[MEQ$DEM7 == "A-"] = "90"
MEQ$HighSchoolMark[MEQ$DEM7 == "A*"] = "95"
MEQ$HighSchoolMark[MEQ$DEM7 == "A/ 80% plus"] = "90"
MEQ$HighSchoolMark[MEQ$DEM7 == "A/B"] = "85"
MEQ$HighSchoolMark[MEQ$DEM7 == "About 80-85.  Math wasn't my strongest subject like Social Studies & Language Arts but I did ok"] = "80"
MEQ$HighSchoolMark[MEQ$DEM7 == "Average"] = "75"
MEQ$HighSchoolMark[MEQ$DEM7 == "b"] = "85"
MEQ$HighSchoolMark[MEQ$DEM7 == "B"] = "85"
MEQ$HighSchoolMark[MEQ$DEM7 == "b plus"] = "88"
MEQ$HighSchoolMark[MEQ$DEM7 == "B with A and A* being above it"] = "85"
MEQ$HighSchoolMark[MEQ$DEM7 == "B-"] = "80"
MEQ$HighSchoolMark[MEQ$DEM7 == "B- / C+"] = "79"
MEQ$HighSchoolMark[MEQ$DEM7 == "B+"] = "87"
MEQ$HighSchoolMark[MEQ$DEM7 == "c"] = "75"
MEQ$HighSchoolMark[MEQ$DEM7 == "C"] = "75"
MEQ$HighSchoolMark[MEQ$DEM7 == "C+"] = "78"
MEQ$HighSchoolMark[MEQ$DEM7 == "d"] = "65"
MEQ$HighSchoolMark[MEQ$DEM7 == "D"] = "65"
MEQ$HighSchoolMark[MEQ$DEM7 == "B+"] = "87"
MEQ$HighSchoolMark[MEQ$DEM7 == "d7"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "don't remember. not great lol"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "F-"] = "50"
MEQ$HighSchoolMark[MEQ$DEM7 == "GCSE grade E because I had auditory processing disorder and never had time to register what was being taught during the short lesson. "] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "Grade 5 / C"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "high, don't recall now"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "I can't remember"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "I do not remember. 50 possibly "] = "50"
MEQ$HighSchoolMark[MEQ$DEM7 == "I don't know"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "I was a good student, don't remember my marks in the school in Poland"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "I don't know it now, it was may many years ago"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "i don't remember but low"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "Im not sure"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "It was so long ago I can't remember. I'm 69 years old now but took A Level match at secondary school which I failed. I got an O Level though "] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "N/A"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "Not sure"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "O level"] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "Terrible "] = "Unknown"
MEQ$HighSchoolMark[MEQ$DEM7 == "ungraded"] = "Unknown"

table(MEQ$HighSchoolMark, useNA = "ifany")
MEQ$HighSchoolMark = as.numeric(MEQ$HighSchoolMark)

descriptives(MEQ, "HighSchoolMark")


# Now we'll code the Math Tasks

# Algebra

algebraKey = read.csv("Algebra Key.csv")

for (i in 1:nrow(algebraKey)) {
  MEQ[[paste0("ALG", i, "Correct")]] = MEQ[[paste0("ALG", i)]] == algebraKey$Answer[i]
}

MEQ$AlgebraTotal = rowSums(MEQ[,paste0("ALG", 1:10, "Correct")], na.rm = TRUE)
MEQ$AlgebraAllMissing = rowSums(as.data.frame(lapply(MEQ[,paste0("ALG", 1:10)], is.na))) == 10


# Fractions

fractionKey = read.csv("Fraction Key.csv")

for (i in 1:nrow(fractionKey)) {
  MEQ[[paste0("FRA", i, "Correct")]] = MEQ[[paste0("FRA", i)]] == fractionKey$Answer[i]
}

MEQ$FractionTotal = rowSums(MEQ[,paste0("FRA", 1:15, "Correct")], na.rm = TRUE)
MEQ$FractionAllMissing = rowSums(as.data.frame(lapply(MEQ[,paste0("FRA", 1:15)], is.na))) == 15


# Fluency

fluencyKey = read.csv("Fluency Key.csv")

for (i in 1:nrow(fluencyKey)) {
  MEQ[[paste0("FLU", i, "Correct")]] = MEQ[[fluencyKey$Question[i]]] == fluencyKey$Answer[i]
}

MEQ$FluencyTotal = rowSums(MEQ[,paste0("FLU", 1:160, "Correct")], na.rm = TRUE)
MEQ$FluencyMissing = rowSums(as.data.frame(lapply(MEQ[,paste0("FLU", 1:160)], is.na)))

table(MEQ$FluencyTotal, useNA = "ifany")
table(MEQ$FluencyMissing)

MEQ$FluencyTotal[MEQ$FluencyMissing == 160] = NA_integer_

# Reorder variables

MEQ = MEQ[, c("ID", "Age", "Gender", "Ethnicity", "Education", "StudyArea", "Occupation", "HighSchoolMark", "Duration..in.seconds.",
              "AMS_IntrinsicKnow", "AMS_IntrinsicAccomplish", "AMS_IntrinsicStimulation", "AMS_ExtrinsicIdentified", 
              "AMS_ExtrinsicIntrojected", "AMS_ExtrinsicExternal", "AMS_Amotivation", "AMS_Intrinsic",          
              "PSWQ", "TAI", "MARS", "BAI", "MIS", "MIS.Personal", "MIS.Perceived", 
              "AlgebraTotal", "AlgebraAllMissing", "FractionTotal", "FractionAllMissing", "FluencyTotal", "FluencyMissing",
              names(MEQ)[c(grep("^MEQ", names(MEQ)),
                           grep("^PSWQ.", names(MEQ)),
                           grep("^AMS(?!_)", names(MEQ), perl = TRUE),
                           grep("^TAI.", names(MEQ)),
                           grep("^MARS.", names(MEQ)),
                           grep("^BAI.", names(MEQ)),
                           grep("^MIS\\d", names(MEQ)),
                           grep("^DEM", names(MEQ)),
                           grep("^FRA", names(MEQ)),
                           grep("^ALG", names(MEQ)),
                           grep("^FLU", names(MEQ)),
                           grep("^Attention", names(MEQ)))])]

# Export data into a rds file to be used for analyses

saveRDS(MEQ, "Study 2 Data.rds")


