#### Reduce dimension by using embedding ####
library(CohortMethod)
library(SqlRender)
options(fftempdir = "/Users/chan/data/temp")

outputFolder <- "/Users/chan/data/drs"

# Eunomia
cdmDatabaseSchema <- "main"
resultsDatabaseSchema <- "main"
cdmVersion <- "5"

connectionDetails <- Eunomia::getEunomiaConnectionDetails()

connection <- DatabaseConnector::connect(connectionDetails)

#target cohort: osteoarthritis
sql <- loadRenderTranslateSql("coxibVsNonselVsGiBleedInOA.sql",
                              packageName = "CohortMethod",
                              dbms = connectionDetails$dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)
DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.coxibVsNonselVsGiBleed GROUP BY cohort_definition_id"
sql <- SqlRender::render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

#
ExcludedConceptIds <- c(1118084,1124300)

# Define which types of covariates must be constructed:
covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = ExcludedConceptIds, addDescendantsToExclude = TRUE)

#Load data
largeCohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               oracleTempSchema = resultsDatabaseSchema,
                                               targetId = 5,
                                               comparatorId = 2,
                                               outcomeIds = 3,
                                               studyStartDate = "",
                                               studyEndDate = "",
                                               exposureDatabaseSchema = resultsDatabaseSchema,
                                               exposureTable = "coxibVsNonselVsGiBleed",
                                               outcomeDatabaseSchema = resultsDatabaseSchema,
                                               outcomeTable = "coxibVsNonselVsGiBleed",
                                               cdmVersion = cdmVersion,
                                               excludeDrugsFromCovariates = FALSE,
                                               firstExposureOnly = TRUE,
                                               removeDuplicateSubjects = TRUE,
                                               restrictToCommonPeriod = FALSE,
                                               washoutPeriod = 180,
                                               covariateSettings = covSettings)

cohortSparseM<- PatientLevelPrediction::toSparseM(plpData = largeCohortMethodData,
                                                  population = largeCohortMethodData$cohorts [largeCohortMethodData$cohorts$treatment==1,]
                                                  )
autoencoder <- createEncoders(data=cohortSparseM$data,
                              latentDim = 16L, #16L
                              validationSplit= 0.3,
                              intermediateThreeDims = c(256L,128L,64L),
                              epochs = 100L, outputFolder = NULL)

#dimension reduction
encodedOverallPop<- autoencoder$encoder %>%
  keras::predict_on_batch(x = cohortSparseM$data)
hist(as.numeric(encodedOverallPop))

# cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
#                                           cdmDatabaseSchema = cdmDatabaseSchema,
#                                           oracleTempSchema = resultsDatabaseSchema,
#                                           targetId = 1,
#                                           comparatorId = 2,
#                                           outcomeIds = 3,
#                                           studyStartDate = "",
#                                           studyEndDate = "",
#                                           exposureDatabaseSchema = resultsDatabaseSchema,
#                                           exposureTable = "coxibVsNonselVsGiBleed",
#                                           outcomeDatabaseSchema = resultsDatabaseSchema,
#                                           outcomeTable = "coxibVsNonselVsGiBleed",
#                                           cdmVersion = cdmVersion,
#                                           excludeDrugsFromCovariates = FALSE,
#                                           firstExposureOnly = TRUE,
#                                           removeDuplicateSubjects = TRUE,
#                                           restrictToCommonPeriod = FALSE,
#                                           washoutPeriod = 180,
#                                           covariateSettings = covSettings)
#
# studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData, outcomeId = 3,
#                                   firstExposureOnly = FALSE,
#                                   restrictToCommonPeriod = FALSE,
#                                   washoutPeriod = 0,
#                                   removeDuplicateSubjects = "keep all",
#                                   removeSubjectsWithPriorOutcome = TRUE,
#                                   minDaysAtRisk = 1,
#                                   riskWindowStart = 0,
#                                   startAnchor = "cohort start",
#                                   riskWindowEnd = 30,
#                                   endAnchor = "cohort end")
#
#
#
# matchOnEmbedded <- function(population,
#                        caliper = 0.2,
#                        caliperScale = "standardized logit",
#                        maxRatio = 1,
#                        stratificationColumns = c()) {
#   if (!("diseaseRiskScore" %in% colnames(population)))
#     stop("Missing column diseaseRiskScore in population")
#   temp <- population$propensityScore
#   population$propensityScore <- population$diseaseRiskScore
#   population <- matchOnPs(population = population,
#                           caliper = caliper,
#                           caliperScale = caliperScale,
#                           maxRatio = maxRatio,
#                           stratificationColumns = stratificationColumns)
#   population$diseaseRiskScore <- population$propensityScore
#   population$propensityScore <- temp
#   return(population)
# }
#
# ps <- createPs(cohortMethodData = cohortMethodData, population = studyPop)
# computePsAuc(ps)
#
# stratifiedPop <- stratifyByPs(ps, numberOfStrata = 5)
# plotPs(stratifiedPop, ps, scale = "preference")
#
# balance <- computeCovariateBalance(stratifiedPop, cohortMethodData)
# sum(balance$beforeMatchingStdDiff > 0.1, na.rm =T)
# sum(balance$afterMatchingStdDiff > 0.1, na.rm =T)
#
# mean(balance$beforeMatchingStdDiff, na.rm =T)
# mean(balance$afterMatchingStdDiff, na.rm =T)
#
# plotCovariateBalanceOfTopVariables(balance)
# createCmTable1(balance)
#
# plotKaplanMeier(matchedPop, includeZero = FALSE)
