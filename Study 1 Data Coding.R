# This R script demonstrates how the subscales were created from the item data.  The supplied data set already has these variables calculated,
# so this script does not need to be run in order to be able to so subsequent analyses, but just illustrates how this step was done.

# Import the data

MEQ = read.csv("Study 1 Raw Data.csv")

# Eliminate those who choose not to have their data included (already done in the data posted here). 14 people are eliminated.


MEQ$Participation = factor(MEQ$Participation.vs.Obs, levels = c("Research Participation: I consent to provide data from my research experience to researchers for analysis",
                                                                "Research Observation: I do not consent to provide data from my research experience to researchers for analysis"),
                           labels = c("Participation", "Observation"))

MEQ = MEQ[MEQ$Participation == "Participation"  & !(is.na(MEQ$Participation)),]

# Convert the other variables to proper formats for analysis

MEQ$Age = as.numeric(MEQ$Age_1)

# Check to see all the variations in Gender_1

table(MEQ$Gender_1, useNA = "ifany")

MEQ$Gender = NA_character_
MEQ$Gender[MEQ$Gender_1 == "f"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "F"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "Femakw"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "female"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "Female"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "female "] = "Female"
MEQ$Gender[MEQ$Gender_1 == "Female "] = "Female"
MEQ$Gender[MEQ$Gender_1 == "female/woman"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "Femalr"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "Girl"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "Woman"] = "Female"
MEQ$Gender[MEQ$Gender_1 == "M"] = "Male"
MEQ$Gender[MEQ$Gender_1 == "male"] = "Male"
MEQ$Gender[MEQ$Gender_1 == "Male"] = "Male"
MEQ$Gender[MEQ$Gender_1 == "Male "] = "Male"
MEQ$Gender[MEQ$Gender_1 == "Man"] = "Male"

# Check to see what codes still need to be classified

table(MEQ$Gender, useNA = "ifany")
table(MEQ$Gender_1, MEQ$Gender, useNA = "ifany")

MEQ$Gender = factor(MEQ$Gender, levels = c("Female", "Male"))

# Change high school marks

MEQ$High.School.Mark = as.numeric(MEQ$High.School.Mark_1)
MEQ$High.School.Mark[MEQ$High.School.Mark < 40] = NA_real_

# Check to see what codes still need to be classified

table(MEQ$High.School.Mark, useNA = "ifany")
table(MEQ$High.School.Mark_1, MEQ$High.School.Mark, useNA = "ifany")

MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "~ 80%"] = 80
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "60%"] = 60
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "65%"] = 65
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "75-80"] = 78
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "75%"] = 75
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "80%"] = 80
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "85-90"] = 88
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "85-90's"] = 88
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "85-90%"] = 88
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "90-95"] = 93
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "90?"] = 90
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "90%"] = 90
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "93%"] = 93
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "95%"] = 95
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "97%"] = 97
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "98%"] = 98
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "99.8%"] = 99.8
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "Approximately 85%"] = 85
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "high 80's"] = 88
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "Low 90s"] = 92
MEQ$High.School.Mark[MEQ$High.School.Mark_1 == "Range of 90%"] = 90


# Create the scales from the items

# First, the PSWQ

MEQ[,grep("PSWQ.Matrix", names(MEQ))] = lapply(MEQ[,grep("PSWQ.Matrix", names(MEQ))], 
                                               factor, levels = c("1 - Not at all typical of me", "2", "3", "4", "5 - Very typical of me"))


# Reverse code some of the items

MEQ$PSWQ.Matrix_1R = 6 - as.numeric(MEQ$PSWQ.Matrix_1)
MEQ$PSWQ.Matrix_3R = 6 - as.numeric(MEQ$PSWQ.Matrix_3)
MEQ$PSWQ.Matrix_8R = 6 - as.numeric(MEQ$PSWQ.Matrix_8)
MEQ$PSWQ.Matrix_10R = 6 - as.numeric(MEQ$PSWQ.Matrix_10)
MEQ$PSWQ.Matrix_11R = 6 - as.numeric(MEQ$PSWQ.Matrix_11)

MEQ$PSWQ = rowMeans(as.data.frame(lapply(MEQ[,c("PSWQ.Matrix_1R", "PSWQ.Matrix_2", "PSWQ.Matrix_3R", "PSWQ.Matrix_4", 
                                                "PSWQ.Matrix_5", "PSWQ.Matrix_6", "PSWQ.Matrix_7", "PSWQ.Matrix_8R",
                                                "PSWQ.Matrix_9", "PSWQ.Matrix_10R", "PSWQ.Matrix_11R", "PSWQ.Matrix_13",
                                                "PSWQ.Matrix_14", "PSWQ.Matrix_15", "PSWQ.Matrix_16", "PSWQ.Matrix_17")],
                                         as.numeric)),
                    na.rm = TRUE)

# Academic Motivation Scale coding

MEQ[,grep("AMS.Matrix", names(MEQ))] = lapply(MEQ[,grep("AMS.Matrix", names(MEQ))], 
                                              factor, levels = c("1 - Does not correspond at all", "2", "3 - Corresponds a little", "4 - Corresponds moderately", "5 - Corresponds a lot", "6", "7 - Corresponds exactly"))

# Subscales of the AMS

MEQ$AMS_IntrinsicKnow = rowMeans(as.data.frame(lapply(MEQ[,c("AMS.Matrix_2", "AMS.Matrix_9", "AMS.Matrix_16", "AMS.Matrix_23")],
                                                      as.numeric)))
MEQ$AMS_IntrinsicAccomplish = rowMeans(as.data.frame(lapply(MEQ[,c("AMS.Matrix_6", "AMS.Matrix_13", "AMS.Matrix_20", "AMS.Matrix_27")],
                                                     as.numeric)))
MEQ$AMS_IntrinsicStimulation = rowMeans(as.data.frame(lapply(MEQ[,c("AMS.Matrix_4", "AMS.Matrix_11", "AMS.Matrix_18", "AMS.Matrix_25")],
                                                             as.numeric)))
MEQ$AMS_ExtrinsicIdentified = rowMeans(as.data.frame(lapply(MEQ[,c("AMS.Matrix_3", "AMS.Matrix_10", "AMS.Matrix_17", "AMS.Matrix_24")],
                                                            as.numeric)))
MEQ$AMS_ExtrinsicIntrojected = rowMeans(as.data.frame(lapply(MEQ[,c("AMS.Matrix_7", "AMS.Matrix_14", "AMS.Matrix_21", "AMS.Matrix_28")],
                                                             as.numeric)))
MEQ$AMS_ExtrinsicExternal = rowMeans(as.data.frame(lapply(MEQ[,c("AMS.Matrix_1", "AMS.Matrix_8", "AMS.Matrix_15", "AMS.Matrix_22")],
                                                          as.numeric)))
MEQ$AMS_Amotivation = rowMeans(as.data.frame(lapply(MEQ[,c("AMS.Matrix_5", "AMS.Matrix_12", "AMS.Matrix_19", "AMS.Matrix_26")],
                                                    as.numeric)))

# 5 Factor Subscale of the AMS
MEQ$AMS_Intrinsic = rowMeans(as.data.frame(lapply(MEQ[,c("AMS.Matrix_2", "AMS.Matrix_9", "AMS.Matrix_16", "AMS.Matrix_23",
                                                         "AMS.Matrix_6", "AMS.Matrix_13", "AMS.Matrix_20", "AMS.Matrix_27",
                                                         "AMS.Matrix_4", "AMS.Matrix_11", "AMS.Matrix_18", "AMS.Matrix_25")],
                                                  as.numeric)))

# Test Anxiety Inventory coding

MEQ[,grep("TAI.Matrix", names(MEQ))] = lapply(MEQ[,grep("TAI.Matrix", names(MEQ))], 
                                              factor, levels = c("Almost Never", "Sometimes", "Often", "Almost Always"))

# Reverse coded items for the TAI

MEQ$TAI.Matrix_1R = 5 - as.numeric(MEQ$TAI.Matrix_1)

MEQ$TAI = rowMeans(as.data.frame(lapply(MEQ[,c("TAI.Matrix_1R", "TAI.Matrix_2", "TAI.Matrix_3", "TAI.Matrix_4",
                                               "TAI.Matrix_5", "TAI.Matrix_6", "TAI.Matrix_7", "TAI.Matrix_9",
                                               "TAI.Matrix_10", "TAI.Matrix_11", "TAI.Matrix_12", "TAI.Matrix_13",
                                               "TAI.Matrix_14", "TAI.Matrix_15", "TAI.Matrix_16", "TAI.Matrix_17",
                                               "TAI.Matrix_18", "TAI.Matrix_19", "TAI.Matrix_20")],
                                        as.numeric)),
                   na.rm = TRUE)

# Math Anxiety Rating Scale (Short Version) coding

MEQ[,grep("MARS.S.Matrix", names(MEQ))] = lapply(MEQ[,grep("MARS.S.Matrix", names(MEQ))], 
                                                 factor, levels = c("Not at all", "A little", "A fair amount", "Much", "Very Much"))

# Items for Mars-S

MEQ$MARS.S = rowMeans(as.data.frame(lapply(MEQ[,c("MARS.S.Matrix_1", "MARS.S.Matrix_2", "MARS.S.Matrix_3", "MARS.S.Matrix_4",
                                                  "MARS.S.Matrix_5", "MARS.S.Matrix_6", "MARS.S.Matrix_7", "MARS.S.Matrix_8",
                                                  "MARS.S.Matrix_9", "MARS.S.Matrix_10", "MARS.S.Matrix_11", "MARS.S.Matrix_12",
                                                  "MARS.S.Matrix_13", "MARS.S.Matrix_14", "MARS.S.Matrix_15", "MARS.S.Matrix_16",
                                                  "MARS.S.Matrix_17", "MARS.S.Matrix_18", "MARS.S.Matrix_19", "MARS.S.Matrix_20",
                                                  "MARS.S.Matrix_21", "MARS.S.Matrix_22", "MARS.S.Matrix_23", "MARS.S.Matrix_24",
                                                  "MARS.S.Matrix_25", "MARS.S.Matrix_26", "MARS.S.Matrix_27", "MARS.S.Matrix_28",
                                                  "MARS.S.Matrix_29", "MARS.S.Matrix_30")],
                                           as.numeric)),
                      na.rm = TRUE)

# Beck Anxiety Inventory coding

MEQ[,grep("BAI.Matrix", names(MEQ))] = lapply(MEQ[,grep("BAI.Matrix", names(MEQ))], 
                                              factor, levels = c("Not at all", "Mildly - but it didn't bother me much", "Moderately - it wasn't pleasant at times", "Severely - it bothered me a lot"))

# Items for BAI

MEQ$BAI = rowMeans(as.data.frame(lapply(MEQ[,c("BAI.Matrix_1", "BAI.Matrix_2", "BAI.Matrix_3", "BAI.Matrix_4",
                                               "BAI.Matrix_5", "BAI.Matrix_6", "BAI.Matrix_7", "BAI.Matrix_9",
                                               "BAI.Matrix_10", "BAI.Matrix_11", "BAI.Matrix_12", "BAI.Matrix_13",
                                               "BAI.Matrix_14", "BAI.Matrix_15", "BAI.Matrix_16", "BAI.Matrix_17",
                                               "BAI.Matrix_18", "BAI.Matrix_19", "BAI.Matrix_20", "BAI.Matrix_21")],
                                        as.numeric)),
                   na.rm = TRUE)

# MEQ Teacher Block coding

MEQ[,grep("MEQ.Teacher.Block", names(MEQ))] = lapply(MEQ[,grep("MEQ.Teacher.Block", names(MEQ))], 
                                                     factor, levels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree", "Do Not Recall"))

# Reverse coded items for MEQ Teacher Block

MEQ$MEQ.Teacher.Block.Q3R = 6 - as.numeric(MEQ$MEQ.Teacher.Block.Q3)
MEQ$MEQ.Teacher.Block.Q6R = 6 - as.numeric(MEQ$MEQ.Teacher.Block.Q6)
MEQ$MEQ.Teacher.Block.Q12R = 6 - as.numeric(MEQ$MEQ.Teacher.Block.Q12)
MEQ$MEQ.Teacher.Block.Q13R = 6 - as.numeric(MEQ$MEQ.Teacher.Block.Q13)
MEQ$MEQ.Teacher.Block.Q16R = 6 - as.numeric(MEQ$MEQ.Teacher.Block.Q16)
MEQ$MEQ.Teacher.Block.Q17R = 6 - as.numeric(MEQ$MEQ.Teacher.Block.Q17)
MEQ$MEQ.Teacher.Block.Q26R = 6 - as.numeric(MEQ$MEQ.Teacher.Block.Q26)


# MEQ Parent block coding

MEQ[,grep("MEQ.Parent.Block", names(MEQ))] = lapply(MEQ[,grep("MEQ.Parent.Block", names(MEQ))], 
                                                    factor, levels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree", "Do Not Recall"))

# Reverse coded items for MEQ Parent block

MEQ$MEQ.Parent.Block.Q5R = 6 - as.numeric(MEQ$MEQ.Parent.Block.Q5)
MEQ$MEQ.Parent.Block.Q7R = 6 - as.numeric(MEQ$MEQ.Parent.Block.Q7)
MEQ$MEQ.Parent.Block.Q12R = 6 - as.numeric(MEQ$MEQ.Parent.Block.Q12)


# MEQ Peer block coding
MEQ[,grep("MEQ.Peer.Block", names(MEQ))] = lapply(MEQ[,grep("MEQ.Peer.Block", names(MEQ))], 
                                                  factor, levels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree", "Do Not Recall"))

# Reverse coded items for MEQ Peer block

MEQ$MEQ.Peer.Block.Q6R = 6 - as.numeric(MEQ$MEQ.Peer.Block.Q6)
MEQ$MEQ.Peer.Block.Q8R = 6 - as.numeric(MEQ$MEQ.Peer.Block.Q8)



# Now look for attention checks

MEQ$AttentionCheck1 = MEQ$PSWQ.Matrix_12 == "5 - Very typical of me"
MEQ$AttentionCheck2 = MEQ$TAI.Matrix_8 == "Almost Never"
MEQ$AttentionCheck3 = MEQ$MARS.S.Matrix_31 == "Not at all"
MEQ$AttentionCheck4 = MEQ$MEQ.Parent.Block.Q6 == "Agree"

table(MEQ$AttentionCheck1)
table(MEQ$AttentionCheck2)
table(MEQ$AttentionCheck3)
table(MEQ$AttentionCheck4)

table(MEQ$AttentionCheck1, MEQ$AttentionCheck2)
table(MEQ$AttentionCheck1, MEQ$AttentionCheck3)
table(MEQ$AttentionCheck2, MEQ$AttentionCheck3)

MEQ$AttentionSum = rowSums(MEQ[, c("AttentionCheck1", "AttentionCheck2", "AttentionCheck3", "AttentionCheck4")], na.rm = TRUE)
table(MEQ$AttentionSum)

MEQ$AttentionPass = MEQ$AttentionSum == 4


# Eliminate those that failed the attention check. 28 people eliminated.

MEQ = MEQ[MEQ$AttentionPass,]


# Reorder variables and save for analyses

MEQ = MEQ[, c("id", "Age", "Gender", "High.School.Mark", "MARS.S", "AMS_Intrinsic", "AMS_IntrinsicStimulation", "AMS_IntrinsicKnow", 
              "AMS_IntrinsicAccomplish", "AMS_ExtrinsicIntrojected", "AMS_ExtrinsicIdentified", "AMS_ExtrinsicExternal",
              "AMS_Amotivation", "TAI", "BAI", "PSWQ", names(MEQ)[grep("MARS.S.", names(MEQ))], names(MEQ)[grep("AMS\\.", names(MEQ))], 
              names(MEQ)[grep("TAI.", names(MEQ))], names(MEQ)[grep("BAI.", names(MEQ))], names(MEQ)[grep("PSWQ.", names(MEQ))],
              names(MEQ)[grep("MEQ\\.", names(MEQ))])]

saveRDS(MEQ, "Study 1 Data.rds")
