#' Configuration file for GEAM Report 
#' 
#' The following variables concern global settings of the GEAM Report. Please note, that 
#' in addition, the graphics and tables can be modified individually directly by 
#' modifying the corresponding code sections. 
#' 
#' Rename this file to config.R and insert the survey ID below. 
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
#'
#'
#'
#'
#' ################
#' Default color palette. Affects the color palette of all charts. 
#' You can also provide a listing of manual color values, e.g. 
#' cpal <- c("#E41A1C","#377EB8","#4DAF4A")
cpal <- RColorBrewer::brewer.pal(9, "Set1")
#' 
#' 
#' ################
#' Percentage digits 
percdigits = 0
#' 
#' 
#' ################
#' Include question codes for which knitr chunk should not be evaluted. 
#' Heading will be printed thouh. 
skipsubquestion <- c("Covid.SQ01.", "Covid.SQ02.", "Covid.SQ03.", "Covid.SQ04.", "Covid.SQ05.", 
                     "Covid.SQ06.", "Covid.SQ07.", "Covid.SQ08.", "Covid.SQ09.", "Covid.SQ10.", 
                     "WCWI005.SQ001.",  "WCWI005.SQ002.", "WCWI005.SQ003.","WCWI005.SQ004.",
                     "EWCS89JobSatisfact8.SQ001.","EWCS89JobSatisfact8.SQ002.","EWCS89JobSatisfact8.SQ003.",
                     "EWCS89JobSatisfact8.SQ004.","EWCS89JobSatisfact8.SQ005.","EWCS89JobSatisfact8.SQ006.",
                     "EWCS89JobSatisfact8.SQ007.","EWCS89JobSatisfact8.SQ008.",
                     "EWCS36WorkIntensity2.SQ001.","EWCS36WorkIntensity2.SQ002.","EWCS36WorkIntensity2.SQ003.",
                     "EWCS36WorkIntensity2.SQ004.",
                     "WorkFamConfISSP.SQ001.", "WorkFamConfISSP.SQ002.", "WorkFamConfISSP.SQ003.", "WorkFamConfISSP.SQ004.",
                     "WCWI016.SQ001.", "WCWI016.SQ002.", "WCWI016.SQ003.", "WCWI016.SQ004.", "WCWI016.SQ005.",
                     "WCWI016.SQ006.", "WCWI016.SQ007.", "WCWI016.SQ008.", "WCWI016.SQ009.", 
                     "WCWI018.SQ001.", "WCWI018.SQ002.", "WCWI018.SQ003.", "WCWI018.SQ004.", "WCWI018.SQ005.",
                     "WCWI018.SQ006.", "WCWI018.SQ007.", "WCWI018.SQ008.", "WCWI018.SQ009.", 
                     "WCWI021.SQ001.", "WCWI021.SQ002.", "WCWI021.SQ003.", "WCWI021.SQ004.", "WCWI021.SQ005.", 
                     "WCWI021.SQ006.", "WCWI021.SQ007.", "WCWI021.SQ008.", "WCWI021.SQ009.", 
                     "BACD003.SQ001.", "BACD003.SQ002.", "BACD003.SQ003.","BACD003.SQ004.", "BACD003.SQ005.", 
                     "BACD003.SQ006.", "BACD003.SQ007.", 
                     "BACD005.SQ001.", "BACD005.SQ002.", "BACD005.SQ003.", "BACD005.SQ004.", "BACD005.SQ005.", 
                     "BACD005.SQ006.", "BACD005.SQ007.", "BACD005.SQ008.", 
                     "GlickMasculCont8.SQ001.", "GlickMasculCont8.SQ002.", "GlickMasculCont8.SQ006.", "GlickMasculCont8.SQ007.", 
                     "GlickMasculCont8.SQ011.", "GlickMasculCont8.SQ012.", "GlickMasculCont8.SQ016.", "GlickMasculCont8.SQ017.", 
                     "OCPER001.SQ001.", "OCPER001.SQ002.", "OCPER001.SQ003.", "OCPER001.SQ004.", "OCPER001.SQ005.", 
                     "OCPER002.SQ001.", "OCPER002.SQ002.", "OCPER002.SQ003.","OCPER002.SQ004.", 
                     "OCPER003.SQ001.", "OCPER003.SQ002.", "OCPER003.SQ003.", "OCPER003.SQ004.", "OCPER003.SQ005.", "OCPER003.SQ006.",
                     "OCPER003.SQ007.", "OCPER003.SQ008.", "OCPER003.SQ009.", "OCPER003.SQ010.", "OCPER003.SQ011.", "OCPER003.SQ012.", 
                     "OCPER003.SQ013.", "OCPER003.SQ014.", "OCPER003.SQ015.", "OCPER003.SQ016.","OCPER003.SQ017.", "OCPER003.SQ018.", 
                     "OCPER003.SQ019.", "OCPER003.SQ020.", "OCPER003.SQ021.", "OCPER003.SQ022.", "OCPER003.SQ023.", "OCPER003.SQ024.", 
                     "OCPER003.SQ025.", "OCPER003.SQ026.", "OCPER003.SQ027.", 
                     "OCWC002.SQ001.", "OCWC002.SQ002.","OCWC002.SQ003.", "OCWC002.SQ004.", "OCWC002.SQ005.", "OCWC002.SQ006.", 
                     "OCWC002.SQ007.", "OCWC002.SQ008.", "OCWC002.SQ009.", "OCWC002.SQ010.", "OCWC002.SQ011.", "OCWC002.SQ012.", 
                     "OCWC002.SQ013.", "OCWC002.SQ014.", "OCWC002.SQ015.", "OCWC002.SQ016.", "OCWC002.SQ017.", "OCWC002.SQ018.", 
                     "OCWC002.SQ019.", "OCWC002.SQ020.", "OCWC002.SQ021.", "OCWC002.SQ022.", "OCWC002.SQ023.", 
                     "BIMA001.SQ001.", "BIMA001.SQ002.", "BIMA001.SQ003.", "BIMA001.SQ004.", "BIMA001.SQ005.", "BIMA001.SQ006.", 
                     "BIMA001.SQ007.", "BIMA001.SQ008.", "BIMA001.SQ009.", "BIMA001.SQ010.", "BIMA001.SQ011.", 
                     "BISB001.SQ001.", "BISB001.SQ002.", "BISB001.SQ003.", "BISB001.SQ004.", "BISB001.SQ005.")















