library(data.table)
library(tidyverse)
library(ggplot2)
library(fmsb)
library(magrittr)
library(flextable)
library(officer)

options(digits = 5)
core.source.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/R Scripts/Testing/Speaking_Test/"
#### Update this every year ####
#### ALSO UPDATE YEAR IN RENDER MARKDOWN ####
core.data.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_Speaking_Tests/ST2/"

test.scores = data.table::fread(paste(core.data.path, "ST2_scores.csv", sep=""))

ave.scores = data.frame(row.names = c("Max", "Min", "Average"),
                        Comprehensibility = c(5, 1, mean(test.scores$Comprehensibility)),
                        Fluency = c(5, 1, mean(test.scores$Fluency)),
                        Grammar = c(5, 1, mean(test.scores$Grammar)),
                        Vocabulary = c(5, 1, mean(test.scores$Vocabulary)),
                        Content = c(5, 1, mean(test.scores$Content)),
                        CommSkills = c(5, 1, mean(test.scores$CommSkills)))


for(i in 1:nrow(test.scores)){
  
  studentNameAndNumber = paste(test.scores[i]$StudentName, test.scores[i]$StudentNumber, sep = " ")
  
  this.score = test.scores[i] %>% select(Comprehensibility, Fluency, Grammar, Vocabulary, Content, CommSkills)
  studentDetails = test.scores[i] %>% select(StudentName, StudentNumber) %>% rename(Name = StudentName, Number = StudentNumber)
  totalScore = test.scores[i] %>% select(TOTAL) %>% rename(Total = TOTAL) %>% mutate(Total = round(Total, digits = 1))
  
  studentDetails.flex <- flextable(studentDetails)
  studentDetails.flex <- set_table_properties(studentDetails.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  this.score.flex <- flextable(this.score)
  this.score.flex <- set_table_properties(this.score.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  totalScore.flex = flextable(totalScore)
  totalScore.flex <- set_table_properties(totalScore.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 14)
  
  this.score.with.name = this.score
  rownames(this.score.with.name) <- test.scores[i]$StudentName
  both.scores = rbind(ave.scores, this.score.with.name)
  
  rmarkdown::render(
    input = paste(core.source.path, "createSTFeedback.Rmd", sep = ""),
    output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_Speaking_Tests/ST2/Reports/{studentNameAndNumber}.docx"),
    params = list(this.score.flex = this.score.flex,
                  studentDetails.flex = studentDetails.flex,
                  totalScore.flex = totalScore.flex,
                  both.scores = both.scores)
  )
  
  
}