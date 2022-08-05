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

#' latestFile Function
#'
#' @description This function returns the latest file name (including extension) matching the wildcard given in the path specified
#' @param filePath File path to search in. Note: this is not recursive.
#' @param fileName Wildcard of file searching for.
#'
#' @return
#' @export
#'
#' @examples
#' latestFile("C://", "Hello")
latestFile <- function(filePath, fileName) {
  if(!dir.exists(stateFilePath)) {
    print(str_c("The Path '", filePath, "' does not exist. Please check and try again."))
    exit()
  }
  pattern = paste("*", fileName, "*", sep="", collapse="")
  tempFileList <- file.info(list.files(filePath
                                       ,pattern = pattern
                                       ,full.names=T))
  basename(rownames(tempFileList[which.max(tempFileList$mtime),]))
}
