library(gpg)
library(sjmisc)
library(stringr)


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
    label <- get_label(x)
    
    # Retrieve answer option text 
    if (type == "answer"){
        label <- stringr::str_extract(label,"\\[.*\\]")
        #label <- regmatches(label, regexpr("\\[.*\\]", label))
    
    # Retrieve question text    
    } else if (type =="question"){
        label <- stringr::str_extract(label, "\\].*")
        #label <- regmatches(label, gregexpr("\\].*", label))
        label <- substr(label, 3, nchar(label))
        
    } else {
        warning("Unknown option for match_label ", type)
    }
    
    # Replace {VarOrgType.shown} placeholder with selected organizational unit. 
    label <- stringr::str_replace(label, "\\{VarOrgType.shown\\}", orgtype)
    
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

