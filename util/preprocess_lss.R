library(dplyr)
library(readr)
library(purrr)
library(forcats)
library(sjlabelled)
library(gpg)


# LimeRLabel can be installed from Github: 
#  
# library(remotes)
# remotes::install_github("jmueller17/LimeRLabel")
library(LimeRLabel)


# For direct download vie LimeSurvey Remote Api, install LimeRick
# 
# library(remotes)
# remotes::install_github("k127/LimeRick")
library(LimeRick)
library(httr)



# 1. Setup global variables 
#
# LS survey number which determines the name of the downloaded file
lsid <- 000000

# GPG encryption receiver. Requires corresponding public keys to be available 
# Leave "" if none is available. 
gpg.receiver <- c("")

# read LimeSurvey Structure file 
lss.read.path <- paste0("data-raw/limesurvey_survey_",lsid,".lss") 

# csv manually downloaded 
csvraw.read.path <- paste0("data-raw/survey_", lsid ,"_R_data_file.csv") 



# Direct download? Activate LimeSurvey RemoteControl 2 JSON-RPC API URL
#options(lsAPIurl = 'https://www.act-on-gender.eu/survey/index.php/admin/remotecontrol')

# Indicate language version for labels. Assumes that 
# this language version is available in the *.lss file. 
lsLangCode <- "en"

# remove timing meta-info columns? 
rmtiming = TRUE

# export SPSS file. 
export2spss = FALSE

# recode certain variables nedded for in-depth analysis and 
# add them to data frame
recode = FALSE



# Set some internal paths. No need to change anything.  
# csv file has been downloaded manually and encrypted previously
csvgpg.read.path <- paste0("data-raw/df.",lsid,"_R_data_file.csv.gpg")

#save raw csv file, encrypted
csvgpg.save.path <- paste0("data-raw/df.",lsid,"_R_data_file.csv.gpg")

# save R data frame gpg encrypted 
dfgpg.save.path <- paste0("data/df.", lsid, ".", lsLangCode, ".rdata.gpg")

# save R data frame without encryption 
df.save.path <- paste0("data/df.", lsid, ".", lsLangCode, ".rdata")

# export R data frame as SPSS file 
df.save.spss.path <- paste0("data/df.", lsid, ".", lsLangCode, ".sav")



# 2. Load Data
#
# In case the CSV file has already been downloaded and stored in encrypted format
if (file.exists(csvgpg.read.path)){
    df.geam <- read_gpg(csvgpg.read.path) 

# Read manually downloaded CSV (delimiter TAB!) file 
} else if (file.exists(csvraw.read.path)) {
    
    # Read LimeSurvey exported R data file (with tabs!)
    df.geam <- read_tsv(csvraw.read.path)
    df.geam <- as.data.frame(df.geam)
    
    # write an encrypted R data frame of the raw downloaded result matrix 
    tryCatch({
        write_gpg(df.geam, file=csvgpg.save.path, receiver=gpg.receiver)        
    },
    error = function(e){
        message("No valid gpg encryption receiver provided. Data will be writen in plain text!")
        save(df.geam, path_raw_csv)
    })

    
# download data directly from LimeSurvey Remote API    
} else if (!is.null(getOption("lsAPIurl"))){
    
    # otherwise produces SSL error... check site config.  
    httr::set_config(config( ssl_verifypeer = 0L ) )
    
    # Set LimeSurvey credentials
    options(lsUser = readline(prompt = "What is your LimeSurvey user name?"))
    options(lsPass = readline(prompt = "Please enter your LimeSurvey password?"))
    
    lsGetSessionKey()
    
    df.geam = lsExportResponses(lsid, completionStatus = "all", headingType = "code", responseType="short", lang=lsLangCode)
    
    lsReleaseSessionKey()
    
    if (exists("df.geam") & !is.null(df.geam)){
        message("Data downloaded OK!")
    }
}



# 3. Process data frame / lables. 

# 3.1 Remove meta data info 
# remove timing meta information. Depending on additional meta-info exported, this can cause trouble as length(qitems) 
# does not match the ncol(df.geam) 
if (rmtiming){
    df.geam <- df.geam %>% 
        select(-ends_with("Time"), -contains("groupTime"), -contains("Question time"))
}




# 3.2 Extract question and answer lables. 

# retrieve answer and question labels
qlabels <- extract_question_labels(lss.read.path)
alabels <- extract_response_labels(lss.read.path)

# Country code glitch for UK. In older versions of the GEAM core, for SDEM012 and SDEM013
# the country code for United Kingdom in the English version is "United Kingdom (UK)" 
# when for the rest of the language versions it is ".... (GB)". In order for the matching of 
# country codes across different language versions, need to replace (UK) with "(GB) in the 
# answer label framework 
alabels <- alabels %>%
    mutate(atxt = if_else(atxt == "United Kingdom (UK)", "United Kingdom (GB)", atxt) )



# Replace VarOrgType.shown with selected default answer 
#
if ("VarOrgType" %in% names(df.geam)){
    
    
    # Retrieve question id 
    votid <- qlabels %>% 
        filter(qcode == "VarOrgType") %>% 
        select(qid) %>%
        distinct() %>% 
        pull()
    
    # retrieve default answer code 
    votacode <- df.geam %>%
        filter(!is.na(VarOrgType)) %>% 
        select(VarOrgType) %>% 
        distinct() %>% 
        pull()
    
    # if code is default placeholder-check-de, switch to "institution"
    if (votacode == "A8") {
        votacode <- "A2"
    }
    
    # retrieve corresponding default label 
    votlab <- alabels %>% 
        filter(qid == votid & lang == lsLangCode & acode == votacode) %>% 
        select(atxt) %>% 
        pull()
    
    # replace in question and subquestion labels
    qlabels$qtxt <- stringr::str_replace_all(qlabels$qtxt, "\\{VarOrgType.shown\\}", votlab)
    qlabels$subqtxt <- stringr::str_replace_all(qlabels$subqtxt, "\\{VarOrgType.shown\\}", votlab)
    
}



#' Convert nationality answer codes for variable SDEM012, SDEM013
#' 
#' The order of response items for SDEM012 and SDEM013 is not identical across languages as 
#' country names are ordered alphabetically, i.e. German versus Deutschland will be at different
#' positions in the dropdown menu.    
#' 
#' The following section makes sure that country names match across languages, i.e. no matter in which 
#' language the given result data set is build. 
#' 
isofrom <- df.geam %>% 
    select(startlanguage) %>% 
    filter(startlanguage != lsLangCode) %>% 
    distinct() %>% 
    pull()


# if there are no other languages, skip this step. 
if (length(isofrom) > 0){

    # need the question id for SDEM012 and SDEM013 from qlabels in order to retrieve corresponding 
    # answer labels. 
    id12 <- qlabels %>% 
        filter(qcode == "SDEM012" | qcode == "SDEM013") %>% 
        distinct(qid) %>% 
        pull(qid)
    
    # retrieve the mapping for answer codes between languages. This constructs one map per
    # available language. For example, if the target language is "en" and submissions exist in 
    # "pl", "es", then there will be two mappings: "pl->en" and "es->en". 
    if ("SDEM012" %in% colnames(df.geam)){
        mcs1 <- bind_rows(lapply(isofrom, map_iso3166, lsLangCode, alabels, id12[1]))
        df.geam$SDEM012 <- purrr::map2_chr(df.geam$SDEM012, df.geam$startlanguage, convert_iso3166, mcs1)
    }
    
    if ("SDEM013" %in% colnames(df.geam)){
        mcs2 <- bind_rows(lapply(isofrom, map_iso3166, lsLangCode, alabels, id12[2]))
        df.geam$SDEM013 <- purrr::map2_chr(df.geam$SDEM013, df.geam$startlanguage, convert_iso3166, mcs2)
    }

}

# 3.2. Assign corrected labels to data frame 
df.geam <- set_question_labels(df.geam, qlabels, lsLangCode)
df.geam <- set_response_labels(df.geam, alabels, lsLangCode)


# check some attributes if everything is ok. 
# "Which best describes your current martial or ..."
attr(df.geam$SDEM006, which="label")
LimeRLabel::get_lsLabel(df.geam$SDEM006)

# "What is your current net ..."
attr(df.geam$WCJC005, which="label")
LimeRLabel::get_lsLabel(df.geam$WCJC005)

# "We are interested in your training opportunities..."
LimeRLabel::get_lsParentlabel(df.geam$WCJC019.SQ001.)
levels(df.geam$WCJC019.SQ001.)




# 4. Manual data corrections 
#
# date of birth check 
# df.geam %>% 
#     filter(SDEM001 < 1900) %>% 
#     select(id, SDEM001) 





# 5. Create additional variables 
#
# calculate age
curyear <- as.numeric(format(Sys.Date(), format="%Y"))

if (is.numeric(df.geam$SDEM001)){
    df.geam$age <- curyear - df.geam$SDEM001
    
    # make age discrete in 10 year steps. 
    df.geam$age_i10 <- cut(df.geam$age, c(seq(0,80,by=10),130))  
    
    # age groups: junior, middle, senior.
    # Junior < 30, Middle 31-45, Senior 46-65, > 65
    df.geam$age_4g <- cut(df.geam$age, c(0,30,45,65,100), labels=c("Junior (<30)", "Middle (31-45)", "Senior (46-65)", "+65"))
    
    
    # for some questionnaires, age question is a factor     
} else if (is.factor(df.geam$SDEM001)){
    df.geam$age_i10 <- df.geam$SDEM001
    df.geam$age_4g <- df.geam$SDEM001
}


# make net salary discrete in steps of 1000
if (is.numeric(df.geam$WCJC005)){
    maxsalary <- max(df.geam$WCJC005, na.rm=T)
    salarysteps <- 1000
    df.geam$WCJC005_i1000 <- cut(df.geam$WCJC005, breaks=c(seq(0,10000, by=salarysteps), (maxsalary+1)), dig.lab=5)
    
} else if (is.factor(df.geam$WCJC005)){
    df.geam$WCJC005_i1000 <- df.geam$WCJC005
}


# make binary gender variable.
df.geam$SDEM004.bin <- df.geam$SDEM004

# which rows have "Other" or "I do not want to answer" for Gender 
dropL <- which( (as.numeric(df.geam$SDEM004) == 3 | as.numeric(df.geam$SDEM004)==4))
df.geam[dropL, "SDEM004.bin"] <- NA 

# remove "other" and "I do not want to answer" from factor. 
df.geam$SDEM004.bin <- forcats::fct_drop(df.geam$SDEM004.bin)


#' Following variables are used mainly for the in-depth statistical analysis
#' They are not included in the descriptive statistical report. 
#' 
if (recode){

    #' create global care responsibility variable when WCWI006 "Are you the primary carer or assistant for
    #' an adult requiring care?" or WCWI008 "Are you the parent or legal guardian of any children aged 17 years or younger?"
    #' are true. 
    #'
    df.geam %<>% 
        mutate(CareResp = if_else( (as.numeric(WCWI006) == 2 | as.numeric(WCWI008) == 2), TRUE,FALSE))
    
    
    
    #' recode Work Family Conflict Scale as indicated by the literature 
    #' It is recommended to invert items before interpreting the item scores so that higher scores represent 
    #' a greater work-family conflict (1 = “never” to 4 = “several times a week”).
    df.geam %<>%
        mutate(WorkFamConfISSP.RE.SQ001. = as.numeric(forcats::fct_rev(df.geam$WorkFamConfISSP.SQ001.)),
               WorkFamConfISSP.RE.SQ002. = as.numeric(forcats::fct_rev(df.geam$WorkFamConfISSP.SQ002.)),
               WorkFamConfISSP.RE.SQ003. = as.numeric(forcats::fct_rev(df.geam$WorkFamConfISSP.SQ003.)),
               WorkFamConfISSP.RE.SQ004. = as.numeric(forcats::fct_rev(df.geam$WorkFamConfISSP.SQ004.)))
    
        
    #' create mean values for components "work-family" and "family-work" conflict and overall mean values. 
    #' 
    df.geam %<>% 
        rowwise() %>% 
        mutate(WorkFamMean = mean(c(WorkFamConfISSP.RE.SQ001., WorkFamConfISSP.RE.SQ002.), na.rm=T), 
               FamWorkMean = mean(c(WorkFamConfISSP.RE.SQ003., WorkFamConfISSP.RE.SQ004.), na.rm=T), 
               WorkFamISSPMean = (WorkFamMean+FamWorkMean)/2 ) %>% 
        ungroup()

}


# 6. Store clean data frame, ready for analysis
# 
tryCatch({
    message("Writing data frame to: ", dfgpg.save.path)
    write_gpg(df.geam, file=dfgpg.save.path, receiver=gpg.receiver)        
},
error = function(e){
    message("No valid gpg encryption receiver provided. \nData frame will be written without encryption to ", df.save.path)
    save(df.geam, file=df.save.path)
})



if (export2spss){
    sjlabelled::write_spss(df.geam, df.save.spss.path)    
}



