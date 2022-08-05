#' @title Get Submission Master Table Data
#'
#' @description Submissions Master Table is updated every month by Brian Tran.
#' This table joins SNAP, CODS, XLDP, and other data together and gets unique policies.
#' Rather than reinvent the wheel, we'll use this to join to xldp warehouse data to cut out all the duplicate quotes
#' Note this table only pulls policies with effective dates within the past year of whenever this is run.
#' That is to facilitate determining inforce status.
#'
#' @return Dataframe containing subset of data from SubmissionMaster_WithDevelopedClaims
#' @export
#'
getSubmissionMasterTable <- function(){
  submissionsConn <- odbcConnect("DC1RPTDBDEV01")
  SubmissionMasterTable <- sqlQuery(submissionsConn, "
     Select WarehouseId as XldpWarehouseId
      ,Quote_ID as SNAPSubmissionNumber
      ,proposal_number as SNAPProposalNumber
      ,DateEffective as PolicyEffectiveDate
      ,DateExpiration as PolicyExpirationDate
      ,submissionstatus as SubmissionStatus
      ,BoundCount as IsBound
      ,BoundPremium
      ,policy_number as PolicyNumber
      ,Name_Insured as NamedInsured
      ,GoverningState as PrincipalState
     From ActuarialPlayPantry.rrr.SubmissionMaster_WithDevelopedClaims

     where warehouseid is not null
     --and submissionstatus = 'issued'
     and PolicyEffectiveDate >= Dateadd(year,-1,getdate()-1)
       ")
  close(submissionsConn)
  return(SubmissionMasterTable)
}

#' Create Labels Function
#'
#' @description This function returns a vector of labels to use for the toBands function.
#' @param breaksVector This is a vector of numerical values that we want to segment on. Use -inf or inf
#' for unknown boundaries.
#' @param commas
#' @param decimalPlaces How many decimals to use for cutting and labels
#'
#' @return Vector of characters
#'
#' @examples createLabels(breaksVector = c("0.1, 1.2, 1.5, 5, Inf), decimalPlaces = 2)
createLabels <- function (breaksVector, commas = TRUE, decimalPlaces = 2) {
  labelsVector <- NULL
  for (i in 1:(length(breaksVector)-1)) {
    firstNumber <- ifelse(breaksVector[i] == 0, 0, ifelse(commas, formatC(breaksVector[i], big.mark = ",", digits = decimalPlaces, format = "f"), formatC(breaksVector[i], digits = decimalPlaces, format = "f")))
    secondNumber <- ifelse(breaksVector[i+1] == 0, 0, ifelse(commas, formatC(breaksVector[i+1], big.mark = ",", digits = decimalPlaces, format = "f"), formatC(breaksVector[i+1], digits = decimalPlaces, format = "f")))
    firstLabel <- ifelse(length(breaksVector)>10, str_pad(i, width = 2, side = "left", pad = "0"), i)
    labelsVector[i] <- str_c(firstLabel, ') ', firstNumber, ' - ', secondNumber)
  }
  return(labelsVector)
}

#' To Bands Function
#'
#' @description This function will cut the data and categorize using labels.
#' @param column Vector of data to be cut
#' @param sequenceVector Vector of values to create the segments on. Note: If Inf is not set to upper bound, this will set Inf as the upper bound by default.
#' @param commas
#' @param decimalPlaces How many decimals to use for cutting and labels
#' @param overrideLeft If this is on, function will set left boundary to be -Inf or 0 depending on if the first value is < 0 or not respectively.
#'
#' @return Vector of characters.
#'
#' @examples toBands(c(0.1, 0.12, 0.25, 0.33, 0.75, 1.25), sequenceVector = c(0, 0.2, 0.30, 0.40), decimalPlaces=2, overrideLeft = FALSE)
toBands <- function (column, sequenceVector, commas = TRUE, decimalPlaces=2, overrideLeft = FALSE) {
  #If the user desires an override, we'll set it to negative Infinity or zero depending on what the left most entry is.
  if (tail(sequenceVector, n=1) != Inf) {
    sequenceVector <- append(sequenceVector, Inf)
  }
  if (overrideLeft) {
    ifelse(tail(sequenceVector, n=1) < 0, sequenceVector <- c(-Inf, sequenceVector), sequenceVector <- c(0, sequenceVector))
  }
  bandBreaks <- sequenceVector
  bandLabels <- createLabels(bandBreaks, decimalPlaces = decimalPlaces)
  dataBand = cut(as.numeric(column), breaks = bandBreaks, labels = bandLabels, right = FALSE)
  return(dataBand)
}

