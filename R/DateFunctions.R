#' @title Inforce Check
#' @description Function to check if policy is still inforce as of an evaluation date (or current date).
#'
#' @param EffectiveDate Date type: Effective Date of policy.
#' @param ExpirationDate Date type: Expiration Date of policy.
#' @param EvaluationDate Date type: Date of the evaluation (this will be the date of reference or checking. Default is current date).
#' @return Bool indicating whether the policy is inforce at the time of evaluation.
#' @examples
#' inforceCheckTrue <- inforceCheck(as.Date("1/1/2020", "%m/%d/%Y"), as.Date("1/1/2021", "%m/%d/%Y"), as.Date("7/1/2020", "%m/%d/%Y"))
#' inforceCheckFalse <- inforceCheck(as.Date("1/1/2020", "%m/%d/%Y"), as.Date("1/1/2021", "%m/%d/%Y"), as.Date("7/1/2020", "%m/%d/%Y"))

inforceCheck <- function(EffectiveDate, ExpirationDate, EvaluationDate = Sys.Date()) {
  inforceFlag <- EffectiveDate <= EvaluationDate & ExpirationDate > EvaluationDate
  return(inforceFlag)
}
