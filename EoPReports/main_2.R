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
core.data.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_EoP2_report/"

scores = data.table::fread(paste(core.data.path, "scores.csv", sep=""))
participation.scores = data.table::fread(paste(core.data.path, "participation.csv", sep=""))

for(i in 1:nrow(scores)){
  if(scores[i]$Type != "IMF"){
    studentNameAndNumber = paste(scores[i]$StudentNumber, " ", scores[i]$StudentName, sep = "")
    scores.report.data = scores[i] %>% select(Phase2Instr, OP3, LQ3, LQ4, ST2, ST3, GP, Phase2Final, Type)
    
    for.course.average = scores %>% filter(Type != "IMF")
    for.course.average = scores %>% filter(!is.na(Phase2Final))
    course.average = round(mean(for.course.average$Phase2Final), digits = 2)
    
    
    scores.report.data = scores.report.data %>% mutate_if(is.numeric, round, digits = 2)
    
    stdnum = scores[i]$StudentNumber
    participation = participation.scores$FINAL[participation.scores$StudentNumber==stdnum]
    
    student.details = data.table("Name" = c(scores[i]$StudentName),
                                 "Number" = c(scores[i]$StudentNumber),
                                 "Instructor" = c(scores[i]$Phase2Instr))
    student.details.flex = flextable(student.details)
    student.details.flex <- set_table_properties(student.details.flex, width = 0.9, layout="autofit") %>%
      theme_booktabs() %>%
      fontsize(part = "all", size = 9) %>%
      fontsize(part = "header", size = 11) %>%
      font(part = "all", fontname = "Times New Roman") %>%
      color(part = "header", color = "#000000") %>%
      bold(part = "header")
    
    scores.report = data.table("Component" = c("Oral Presentation 3",
                                               "Listening Quiz 3",
                                               "Listening Quiz 4",
                                               "Speaking Test 2",
                                               "Speaking Test 3",
                                               "Group Project",
                                               "Final OCSALS Score",
                                               "Course average"),
                               "Maximum Points" = c("100", "25", "25", "100", "100", "100", "100", ""),
                               "Score" = c(scores.report.data$OP3,
                                           scores.report.data$LQ3,
                                           scores.report.data$LQ4,
                                           scores.report.data$ST2,
                                           scores.report.data$ST3,
                                           scores.report.data$GP,
                                           scores.report.data$Phase2Final,
                                           course.average))
    
    scores.report.flex = flextable(scores.report)
    scores.report.flex <- set_table_properties(scores.report.flex, width = 0.9, layout="autofit") %>%
      theme_booktabs() %>%
      fontsize(part = "all", size = 9) %>%
      fontsize(part = "header", size = 11) %>%
      font(part = "all", fontname = "Times New Roman") %>%
      color(part = "header", color = "#000000") %>%
      bold(part = "header") %>%
      bold(i = ~ Component %in% "Final OCSALS Score", bold = TRUE)
    
    
    rmarkdown::render(
      
      input = paste(core.source.path, "createReportPhase2.Rmd", sep = ""),
      output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_EoP2_report/reports/{studentNameAndNumber}.docx"),
      params = list(scores.report.flex = scores.report.flex,
                    student.details.flex = student.details.flex,
                    participation = participation)
    )
    
  }
}
