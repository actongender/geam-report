library(dplyr)
library(readr)
library(purrr)
library(LimeRick)
library(httr)
library(LimeRLabel)
library(forcats)


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
csvraw.read.path <- NULL #paste0("~/Downloads/surveys/survey_", lsid ,"_R_data_file.csv")
csvraw.read.path <- paste0("data-raw/survey_", lsid ,"_R_data_file.csv") 



# Direct download? Activate LimeSurvey RemoteControl 2 JSON-RPC API URL
#options(lsAPIurl = 'https://www.act-on-gender.eu/survey/index.php/admin/remotecontrol')

# Indicate language version for labels. Assumes that 
# this language version is available in the *.lss file. 
lsLangCode <- "pl"

# remove timing meta-info columns? 
rmtiming = TRUE



# Set some internal paths. No need to change anything.  
# csv file has been downloaded manually and encrypted previously
csvgpg.read.path <- paste0("data-raw/df.",lsid,"_R_data_file.csv.gpg")

#save raw csv file, encrypted
csvgpg.save.path <- paste0("data-raw/df.",lsid,"_R_data_file.csv.gpg")

# save R data frame gpg encrypted 
dfgpg.save.path <- paste0("data/df.", lsid, ".", lsLangCode, ".rdata.gpg")

# save R data frame without encryption 
df.save.path <- paste0("data/df.", lsid, ".", lsLangCode, ".rdata")
 



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




# convert nationality answer codes for variable SDEM012, SDEM013
# in how many different languages have respondents submitted data, other than the 
# target language? 
isofrom <- df.geam %>% 
    select(startlanguage) %>% 
    filter(startlanguage != lsLangCode) %>% 
    distinct() %>% 
    pull()

# need the question id for SDEM012 and SDEM013 from qlabels in order to retrieve corresponding 
# answer labels. 
id12 <- qlabels %>% 
    filter(qcode == "SDEM012" | qcode == "SDEM013") %>% 
    distinct(qid) %>% 
    pull(qid)

# retrieve the mapping for answer codes between languages. This constructs one map per
# available language. For example, if the target language is "en" and submissions exist in 
# "pl", "es", then there will be two mappings: "pl->en" and "es->en". 
mcs1 <- bind_rows(lapply(isofrom, map_iso3166, lsLangCode, alabels, id12[1]))
df.geam$SDEM012 <- purrr::map2_chr(df.geam$SDEM012, df.geam$startlanguage, convert_iso3166, mcs1)


mcs2 <- bind_rows(lapply(isofrom, map_iso3166, lsLangCode, alabels, id12[2]))
df.geam$SDEM013 <- purrr::map2_chr(df.geam$SDEM013, df.geam$startlanguage, convert_iso3166, mcs2)


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

df.geam$age <- curyear - df.geam$SDEM001

# make age discrete in 10 year steps. 
df.geam$age_i10 <- cut(df.geam$age, c(seq(0,80,by=10),130))

# make net salary discrete in steps of 1000
maxsalary <- max(df.geam$WCJC005, na.rm=T)
df.geam$WCJC005_i1000 <- cut(df.geam$WCJC005, breaks=c(seq(0,10000, by=1000), maxsalary), dig.lab=5)


# make binary gender variable.
df.geam$SDEM004.bin <- df.geam$SDEM004

# which rows have "Other" or "I do not want to answer" for Gender 
dropL <- which( (as.numeric(df.geam$SDEM004) == 3 | as.numeric(df.geam$SDEM004)==4))
df.geam[dropL, "SDEM004.bin"] <- NA 

# remove "other" and "I do not want to answer" from factor. 
df.geam$SDEM004.bin <- forcats::fct_drop(df.geam$SDEM004.bin)





# 6. Store clean data frame, ready for analysis
# 
tryCatch({
    message("Writing data frame to: ", dfgpg.save.path)
    write_gpg(df.geam, file=dfgpg.save.path, receiver=gpg.receiver)        
},
error = function(e){
    message("No valid gpg encryption receiver provided. Data frame will be written without encryption.")
    save(df.geam, df.save.path)
})


