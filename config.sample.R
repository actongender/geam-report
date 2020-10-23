#' Configuration file for GEAM Report 
#' 
#' The following variables concern global settings of the GEAM Report. Please note, that 
#' in addition, the graphics and tables can be modified individually directly by 
#' modifying the corresponding code sections. 
#' 
#' 
#' 
#' ################## 
#' The LimeSurvey ID. Please insert here the ID from the LimeSurvey platform or your survey
#' This is the ID number which is also used when exporting data and files from LS. It is 
#' visible in parenthesis in your LimeSurvey title. The  reporting template will try to infer 
#' and read the result data based from data/ folder. 
lsid <- 000000
#'
#'
#' ################## 
#' In which language do you want to generate the report? The languages available depend
#' upon the languages of the questionnaire. If you questionnaire was available in English
#' and Spanish for example, the frequency tables and charts can be generated either using the 
#' English or Spanish labels only. 
lsLangCode <- "en"
#'
#'
#' ##################
#' Should NA entries be shown in charts? Default is set to TRUE
#' which hides "No Answer" options. 
hideNA = TRUE
#'
#'
#' ##################
#' Frequency tables can be sorted, either in ascending, descending order or 
#' according to the order of the questions and subquestions. Set the following v
#' variable to either "none", "desc", or "asc". 
frqtsort <- "desc"  
#'
#'
#' ##################
#' Should frequency bar plots show total counts or precentage values? 
#' By default, frequency plots show percentages. 
plotperc <- TRUE
#'
#'
#' #################
#' The heading of each variable is shown as the question text used in the 
#' questionnaire. By setting lsVarCode=T, the heading will also contain the 
#' LimeSurvey question code. This makes it easier to reference the graphics
#' and data with the data matrix. 
lsVarCode <- TRUE
#'
#' 
#' #################
#' Should the cross tabs by gender be included? The report contains the 
#' cross tabs of selected variables and gender. This can be easily turn on/off
#' by setting the following variable to TRUE or FALSE. 
printXtable <- TRUE
#'
#'
#' ################
#' Should the cross tab by gender by printed as a bar plot as well? 
#' Turn globally these graphics on/off. 
printXbar <- TRUE







