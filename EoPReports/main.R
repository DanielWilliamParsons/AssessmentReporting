library(data.table)
library(tidyverse)
library(ggplot2)
library(fmsb)
library(magrittr)
library(flextable)
library(officer)

core.source.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/R Scripts/Testing/EoPReports/"
#### Update this every year ####
#### ALSO UPDATE YEAR IN RENDER MARKDOWN ####
core.data.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_EoP1_report/"

scores = data.table::fread(paste(core.data.path, "scores.csv", sep=""))
participation.scores = data.table::fread(paste(core.data.path, "participation.csv", sep=""))
print(scores)

for(i in 1:nrow(scores)){
  studentNameAndNumber = paste(scores[i]$StudentNumber, " ", scores[i]$StudentName, " OCSALSPhase1Report", sep = "")
  scores.report.data = scores[i] %>% select(Phase1Instr, OP2, LQ1, LQ2, ST1, Phase1Final, Type)
  course.average = round(mean(scores$Phase1Final), digits = 2)
  
  
  scores.report.data = scores.report.data %>% mutate_if(is.numeric, round, digits = 2)
  
  stdnum = scores[i]$StudentNumber
  participation = participation.scores$FINAL[participation.scores$StudentNumber==stdnum]
  
  student.details = data.table("Name" = c(scores[i]$StudentName),
                               "Number" = c(scores[i]$StudentNumber),
                               "Instructor" = c(scores[i]$Phase1Instr))
  student.details.flex = flextable(student.details)
  student.details.flex <- set_table_properties(student.details.flex, width = 0.9, layout="autofit") %>%
    theme_booktabs() %>%
    fontsize(part = "all", size = 11) %>%
    fontsize(part = "header", size = 12) %>%
    font(part = "all", fontname = "Times New Roman") %>%
    color(part = "header", color = "#000000") %>%
    bold(part = "header")
  
  
  if(scores.report.data$Type == "IMF"){
    scores.report = data.table("Component" = c("Oral Presentation 2",
                                               "Listening Quiz 1",
                                               "Listening Quiz 2",
                                               "Speaking Test 1",
                                               "Final OCSALS Score",
                                               "Course average"),
                               "Maximum Points" = c("100", "25", "25", "100", "100", ""),
                               "Score" = c(scores.report.data$OP2,
                                           scores.report.data$LQ1,
                                           scores.report.data$LQ2,
                                           scores.report.data$ST1,
                                           scores.report.data$Phase1Final,
                                           course.average))
    
    scores.report.flex = flextable(scores.report)
    scores.report.flex <- set_table_properties(scores.report.flex, width = 0.9, layout="autofit") %>%
      theme_booktabs() %>%
      fontsize(part = "all", size = 11) %>%
      fontsize(part = "header", size = 12) %>%
      font(part = "all", fontname = "Times New Roman") %>%
      color(part = "header", color = "#000000") %>%
      bold(part = "header") %>%
      bold(i = ~ Component %in% "Final OCSALS Score", bold = TRUE)
    
    
    rmarkdown::render(
      
      input = paste(core.source.path, "createReportIMF.Rmd", sep = ""),
      output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_EoP1_report/reports/{studentNameAndNumber}.docx"),
      params = list(scores.report.flex = scores.report.flex,
                    student.details.flex = student.details.flex,
                    participation = participation)
    )
  } else {
    scores.report = data.table("Component" = c("Oral Presentation 2",
                                               "Listening Quiz 1",
                                               "Listening Quiz 2",
                                               "Speaking Test 1",
                                               "Final OCSALS Score",
                                               "Course average"),
                               "Maximum Points" = c("100", "25", "25", "100", "100", ""),
                               "Score" = c(scores.report.data$OP2,
                                           scores.report.data$LQ1,
                                           scores.report.data$LQ2,
                                           scores.report.data$ST1,
                                           scores.report.data$Phase1Final,
                                           course.average))
    
    scores.report.flex = flextable(scores.report)
    scores.report.flex <- set_table_properties(scores.report.flex, width = 0.9, layout="autofit") %>%
      theme_booktabs() %>%
      fontsize(part = "all", size = 11) %>%
      fontsize(part = "header", size = 12) %>%
      font(part = "all", fontname = "Times New Roman") %>%
      color(part = "header", color = "#000000") %>%
      bold(part = "header") %>%
      bold(i = ~ Component %in% "Final OCSALS Score", bold = TRUE)
    
    
    rmarkdown::render(
      
      input = paste(core.source.path, "createReport.Rmd", sep = ""),
      output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_EoP1_report/reports/{studentNameAndNumber}.docx"),
      params = list(scores.report.flex = scores.report.flex,
                    student.details.flex = student.details.flex,
                    participation = participation)
    )
  }
  
}
