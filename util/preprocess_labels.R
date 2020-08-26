library(dplyr)
library(readr)
library(sjlabelled)
library(stringr)

rm(list=ls())

source("util/util.fn.R")


# Set global variables 
#
# LS survey number which determines the name of the downloaded file
lsid <- 813685

# Path to the folder where R data and syntax files have been stored
download.path <- "~/Downloads/surveys/"

# GPG encryption receiver. Requires corresponding public keys to be available 
gpg.receiver <- c("jmuller@uoc.edu")




# 1. Prepare R syntax file by removing first line
path_syntax <- paste0("data-raw/survey_",lsid,"_R_syntax_file.R")

# read R syntax file
rsyntax <- readLines(paste0(download.path,"survey_",lsid,"_R_syntax_file.R"))

# remove first line which contains the LS generated import command for the R data 
if (stringr::str_detect(rsyntax[1], "read.csv")){
    rsyntax <- rsyntax[-1]
    writeLines(rsyntax, path_syntax)   
}


# 2. Convert CSV file to R data frame.
# If gpg receiver listed, the data frame will be stored encrypted, otherwise un-encrypted in the 
# data-raw folder of the geam-report. 

# name and path to store the encrypted df csv file 
path_raw_csv_gpg <- paste0("data-raw/df.",lsid,"_R_data_file.csv.gpg")
path_raw_csv <- paste0("data-raw/df.",lsid,"_R_data_file.csv")

if (file.exists(path_raw_csv_gpg)){
    data <- read_gpg(path_raw_csv_gpg)
    
} else if (file.exists(path_raw_csv)){
    data <- load(path_raw_csv) 
    
} else {
    
    path_data <- paste0(download.path, "survey_", lsid ,"_R_data_file.csv")
    
    # Read LimeSurvey exported R data file (with tabs!)
    data <- read_tsv(path_data)
    data <- as.data.frame(data)
    
    # write an encrypted R data frame of the raw downloaded result matrix 
    tryCatch({
        write_gpg(data, file=path_raw_csv_gpg, receiver=gpg.receiver)        
    },
    error = function(e){
        message("No valid gpg encryption receiver provided. Data will be writen in plain text, i.e. non-encrypted!")
        save(data, path_raw_csv)
    })
    
}




# 3. Assign R syntax file to R data file. This achieves various tasks: 
#    a) converts variables to factors, assigning value lables, 
#    b) stored the question text and answer options for each variable 
#    c) assignes variable types and 
#    d) names each column of the dataframe with the corresponding variable code. 
# 
# Assumes that point 2 has been executed and the 
if (file.exists(path_syntax)){
    source(path_syntax) 
    
} else {
    stop("No syntax file found. Need to store R syntax file in data-raw/ folder of geam-report first.")
} 

# Check results: should be "A man", "A women" ... for 004 and "Bisexual" , "Gay / lesbian" .... for 007
sjlabelled::get_labels(data$SDEM004)
sjlabelled::get_labels(data$SDEM007)


# # Load country labels into variable "clabels"
# source("util/country_labels_EN.R") 
# 
# # Assign country and citizen labels. 
# if (!is.null(data$SDEM012)){
#     data[, "SDEM012"] <- factor(data[, "SDEM012"], levels=c("L001","L002","L003","L004","L005","L006","L007","L008","L009","L010","L011","L012","L013","L014","L015","L016","L017","L018","L019","L020","L021","L022","L023","L024","L025","L026","L027","L028","L029","L030","L031","L032","L033","L034","L035","L036","L037","L038","L039","L040","L041","L042","L043","L044","L045","L046","L047","L048","L049","L050","L051","L052","L053","L054","L055","L056","L057","L058","L059","L060","L061","L062","L063","L064","L065","L066","L067","L068","L069","L070","L071","L072","L073","L074","L075","L076","L077","L078","L079","L080","L081","L082","L083","L084","L085","L086","L087","L088","L089","L090","L091","L092","L093","L094","L095","L096","L097","L098","L099","L100","L101","L102","L103","L104","L105","L106","L107","L108","L109","L110","L111","L112","L113","L114","L115","L116","L117","L118","L119","L120","L121","L122","L123","L124","L125","L126","L127","L128","L129","L130","L131","L132","L133","L134","L135","L136","L137","L138","L139","L140","L141","L142","L143","L144","L145","L146","L147","L148","L149","L150","L151","L152","L153","L154","L155","L156","L157","L158","L159","L160","L161","L162","L163","L164","L165","L166","L167","L168","L169","L170","L171","L172","L173","L174","L175","L176","L177","L178","L179","L180","L181","L182","L183","L184","L185","L186","L187","L188","L189","L190","L191","L192","L193","L194","L195","L196","L197","L198","L199","L200","L201","L202","L203","L204","L205","L206","L207","L208","L209","L210","L211","L212","L213","L214","L215","L216","L217","L218","L219","L220","L221","L222","L223","L224","L225","L226","L227","L228","L229","L230","L231","L232","L233","L234","L235","L236","L237","L238","L239","L240","L241","L242","L243","L244","L245","L246","L247","L248","L249","L250","L251","L252","L253","L254","L255","L256","L257","L258","L259","L260","L261","L262","L263","L264","L265","L266","L267","L268","L269","L270","L271","L272","L273","L274","L275","L276","L277","L278","L279","L280","L281","L282","L283","L284","L285"),labels=clabels)
# }
# 
# if (!is.null(data$SDEM013)){
#     data[, "SDEM013"] <- factor(data[, "SDEM013"], levels=c("L001","L002","L003","L004","L005","L006","L007","L008","L009","L010","L011","L012","L013","L014","L015","L016","L017","L018","L019","L020","L021","L022","L023","L024","L025","L026","L027","L028","L029","L030","L031","L032","L033","L034","L035","L036","L037","L038","L039","L040","L041","L042","L043","L044","L045","L046","L047","L048","L049","L050","L051","L052","L053","L054","L055","L056","L057","L058","L059","L060","L061","L062","L063","L064","L065","L066","L067","L068","L069","L070","L071","L072","L073","L074","L075","L076","L077","L078","L079","L080","L081","L082","L083","L084","L085","L086","L087","L088","L089","L090","L091","L092","L093","L094","L095","L096","L097","L098","L099","L100","L101","L102","L103","L104","L105","L106","L107","L108","L109","L110","L111","L112","L113","L114","L115","L116","L117","L118","L119","L120","L121","L122","L123","L124","L125","L126","L127","L128","L129","L130","L131","L132","L133","L134","L135","L136","L137","L138","L139","L140","L141","L142","L143","L144","L145","L146","L147","L148","L149","L150","L151","L152","L153","L154","L155","L156","L157","L158","L159","L160","L161","L162","L163","L164","L165","L166","L167","L168","L169","L170","L171","L172","L173","L174","L175","L176","L177","L178","L179","L180","L181","L182","L183","L184","L185","L186","L187","L188","L189","L190","L191","L192","L193","L194","L195","L196","L197","L198","L199","L200","L201","L202","L203","L204","L205","L206","L207","L208","L209","L210","L211","L212","L213","L214","L215","L216","L217","L218","L219","L220","L221","L222","L223","L224","L225","L226","L227","L228","L229","L230","L231","L232","L233","L234","L235","L236","L237","L238","L239","L240","L241","L242","L243","L244","L245","L246","L247","L248","L249","L250","L251","L252","L253","L254","L255","L256","L257","L258","L259","L260","L261","L262","L263","L264","L265","L266","L267","L268","L269","L270","L271","L272","L273","L274","L275","L276","L277","L278","L279","L280","L281","L282","L283","L284","L285"),labels=clabels)
# }


# Assign variable label (not value labels)
# by retrieving questions and answer items from the LS syntax file 
qitems <- attributes(data)$variable.labels

# remove timing meta information
# depending on additional meta-info exported, this can cause trouble. The length(qitems) needs to match 
# ncol(data)
data <- data %>% 
    select(-ends_with("Time"), -contains("groupTime"), -contains("Question time"))

# set the variable label with 
data <- sjlabelled::set_label(data, qitems)  

# Check. 
# Should be "Which best describes your sexual orientation?"
sjlabelled::get_label(data$SDEM007)

# Should be "[Administrative tasks related to managing other members of staff (e.g. conducting annual performance reviews, 
#  induction processes for new staff)] We are interested in the training opportunities available to you. Please indicate 
#  which, if any, of the following you have received training in."
sjlabelled::get_label(data$WCJC019_SQ001)


# Split / assign question and answer options. LimeSurvey stores question and answer 
# item text for matrix questions in a string of the form "[Answer option] Question text".
#
# For likert scale ratings of answer options, the variable answer options have the format "G09Q79_SQ01", "G09Q79_SQ02", etc.
# Need to extract the [Answer option] text and convert it to variable label. 

# select all likert scale / matrix answer options 
cols <- data %>% 
    select(contains("_SQ")) %>% 
    names()


# The actual question text is attached to each variable with a new attribute "question_label". Although the actual 
# question appears only once in the questionnaire, it gets attached to each answer item for this question with 
# a new attribute "question_label" in order to retrieve it later in the reports. 
for (i in cols){
    attr(data[,i], which="question_label") <- match_label(data[,i], type="question")     
}

# Check. 
# Should be question text: "We are interested in the training opportunities available to you. Please indicate which, if any, 
# of the following you have received training in."
attr(data[,"WCJC019_SQ001"], which="question_label")



# Finally, extract the "answer option" text and use it as variable label which will automatically appear
# in the likert plot items. 
data[,cols] <- sjlabelled::set_label(data[,cols], sapply(data[,cols], match_label, type="answer"))

# Check 
# Should be answer option text: "[Administrative tasks related to managing other members of staff (e.g. conducting annual performance reviews, induction processes for new staff)]" 
sjlabelled::get_label(data$WCJC019_SQ001)




# 4. Corrections of data 

# Date of birth
data %>% 
    filter(SDEM001 < 1900) %>% 
    select(id, SDEM001)

data[175, "SDEM001"] <- NA #was 60
data[175, "SDEM001"] <- NA #was 1890



# 5. Write clean data frame, ready for analysis 
path_df_gpg <- paste0("data/df.",lsid,".rdata.gpg")
path_df     <- paste0("data/df.",lsid,".rdata")

# if gpg encryption is available, store df encrypted, otherwise as plain data frame. 
tryCatch({
    write_gpg(data, file=path_df_gpg, receiver=gpg.receiver)        
},
error = function(e){
    message("No valid gpg encryption receiver provided. Data will be writen in plain text, i.e. non-encrypted!")
    save(data, path_df)
})








