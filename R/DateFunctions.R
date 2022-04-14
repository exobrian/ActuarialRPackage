#' @title Inforce Check
#' @description Function to check if policy is still inforce as of an evaluation date (or current date).
#'
#' @param EffectiveDate Effective Date of policy.
#' @param ExpirationDate Expiration Date of policy.
#' @param EvaluationDate Date of the evaluation (this will be the date of reference or checking. Default is current date).
#' @return Bool indicating whether the policy is inforce at the time of evaluation.
#' @examples
#' inforceCheckTrue <- inforceCheck(as.Date("1/1/2020", "%m/%d/%Y"), as.Date("1/1/2021", "%m/%d/%Y"), as.Date("7/1/2020", "%m/%d/%Y"))
#' inforceCheckFalse <- inforceCheck(as.Date("1/1/2020", "%m/%d/%Y"), as.Date("1/1/2021", "%m/%d/%Y"), as.Date("7/1/2020", "%m/%d/%Y"))
inforceCheck <- function(EffectiveDate, ExpirationDate, EvaluationDate = Sys.Date()) {
  inforceFlag <- as.Date(EffectiveDate, "%m/%d/%Y") <= as.Date(EvaluationDate, "%m/%d/%Y") && as.Date(ExpirationDate, "%m/%d/%Y") > as.Date(EvaluationDate, "%m/%d/%Y")
  return(inforceFlag)
}