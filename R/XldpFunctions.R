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
