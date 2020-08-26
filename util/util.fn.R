library(gpg)
library(sjlabelled)
library(sjmisc)
library(stringr)


#' Check if variable name exists in data frame. 
#' 
#' Useful to avoid error messages before generating figures and tables. Performs exact match.  
#' 
#' @param x a data frame 
#' @param var_name String of column variable name
#' 
varexists <- function(needle, stack=""){
    
    # if no stack provided, check if globally defined column names exist (in index.Rmd)
    if (stack == ""){
        stack <- cnames
    }
    
    exists <- needle %in% stack
    exists
}



#' Retrieve variable label
#' 
#' If variable does not exists, retrieves warning message 
#' 
#' @param var_name String of variable name 
#' 
varlabel <- function(var_name, df=NULL){
    
    #default label 
    label <- paste0("'",var_name,"' not found in data matrix") 
    
    if (is.null(df)){
        df <- data
    }
    
    
    # if variable exists, get actual label
    if (varexists(var_name)){
        
        # matrix answer items
        if (stringr::str_detect(var_name, "_SQ")){
            
            label <- attr(df[,var_name], which="question_label")
            
            # all other questions
        } else {
            
            label <- sjlabelled::get_label(df[,var_name])    
        }
    } 
    
    label
}


#' Splits Quesion and Answer options
#' 
#' LimeSurvey stores question and answer item text in a string of the form "[Answer option] Question text". This 
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
match_label <- function(x, type="answer", orgtype=""){
    
    # retrieve variable label which has been assigned by sourcing the LS R syntax file. 
    label <- sjlabelled::get_label(x)
    
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


#' Prints frequency tables depending on output format. 
#' 
#' library(flextable) is used for word document outputs. Kabel for pdf and html output. 
#' 
#' @param x column of data frame with numeric variable
#' @para fsize font size used in flextable. 
#' 
#' 
print_frqtable <- function(x, fsize=12){
    
    frqtbl <- sjmisc::frq(x)
    isHTML <- knitr::is_html_output()
    
    #html / pdf 
    if (isHTML) {
        
        ftb <- knitr::kable(frqtbl, col.names=c("", "", "count", "%", "valid %", "cum %"))
        
        # word document
    } else {
        
        ftb <- flextable::flextable(frqtbl[[1]], col_keys = c("val", "frq", "raw.prc", "valid.prc", "cum.prc"))
        ftb <- flextable::set_header_labels(ftb, val="value", frq="count", raw.prc="%", valid.prc="valid %", cum.prc="cum %")
        ftb <- flextable::fontsize(ftb, size=fsize, part = "all")
        ftb <- flextable::bold(ftb, bold=T, part = "header")
        ftb <- flextable::autofit(ftb)
    }
    
    ftb
}



#' Utility function to store raw data as encrypted dataframes.
#' Only used upon first usage of raw data. Rest of preprocessing then
#' relies on encrypted dataframes directly.
#' @param df The data frame to be encrypted
#' @param file String. Name and path for writing file
#' @param receiver email address of receiver (which has gpg key associated)
#'
write_gpg <- function(df, file="df.gpg", receiver){
    df.s <- serialize(df, con=NULL, ascii=T)
    df.enc <- gpg_encrypt(df.s, receiver=receiver)
    writeBin(df.enc, con=file)
}

#' Read encrypted dataframe and unserialize
#'
#' @param file String. File path of file to be decrypted
read_gpg <- function(file, as_text=FALSE){
    df <- gpg_decrypt(file, as_text=as_text)
    df <- unserialize(df)
    df
}

