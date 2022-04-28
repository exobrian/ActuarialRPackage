library(stringr)

#' Right Function
#'
#' @description This function extracts the right side of the input string by desired number of characters
#' @param string
#' @param char
#'
#' @return
#' @export
#'
#' @examples
#' right("Hello World", 5)
right <- function (string, char) {
  substr(string,nchar(string)-(char-1),nchar(string))
}

#' Left Function
#'
#' @description This function extracts the Left side of the input string by desired number of characters
#' @param string
#' @param char
#'
#' @return
#' @export
#'
#' @examples
#' left("Hello World", 5)
left <- function (string,char) {
  substr(string,1,char)
}
