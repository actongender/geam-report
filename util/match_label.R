library(stringr)
#' Splits Quesion and Answer options
#' 
#' LimeSurvey exported R CSV file stores question and answer item text in a string of the form "[Answer option] Question text". This 
#' function extracts the answer option text or the question text. 
#' 
#' Replaces also the {VarOrgType.shown} variable name with the actual organizational unit in some question
#' texts and answer options. 
#' 
#' @param x column of data frame 
#' @param type string ["answer", "question"]
#' @param orgtype string Contents of VarOrgType default answer, i.e. organizational unity of survey
#' 
#' @return string 
#' 
#' NOTE: This is superseded by LimeRLabel package which constructs correctly labelled data frame from 
#'       CSV and *.lss archive
#' 
match_label <- function(x, type="answer", orgtype=""){
    
    # retrieve variable label which has been assigned by sourcing the LS R syntax file. 
    label <- attr(x, which="label")
    
    # Retrieve answer option text 
    if (type == "answer"){
        label <- stringr::str_extract(label,"\\[.*\\]")
    
    # Retrieve question text    
    } else if (type =="question"){
        label <- stringr::str_extract(label, "\\].*")
        label <- substr(label, 3, nchar(label))
        
    } else {
        warning("Unknown option for match_label ", type)
    }
    
    # Replace {VarOrgType.shown} placeholder with selected organizational unit. 
    if (orgtype != ""){
        label <- stringr::str_replace(label, "\\{VarOrgType.shown\\}", orgtype)
    }
    
    label <- trimws(label)
    
    label 
}
