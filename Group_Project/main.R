library(data.table)
library(tidyverse)
library(ggplot2)
library(fmsb)
library(magrittr)
library(flextable)
library(officer)

core.source.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/R Scripts/Testing/Group_Project/"
#### Update this every year ####
#### ALSO UPDATE YEAR IN RENDER MARKDOWN ####
core.data.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_Group_Project/"

# Get the feedback and comments into a usable format
feedback.code.path = paste(core.source.path, "getFeedback.R", sep="")
source(feedback.code.path)
feedback = getFeedback(core.data.path)
fb = feedback[[1]]

#Use the scores in feedback[[5]] to calculate the average for each component
ave.scores = data.frame(row.names = c("Max", "Min", "Average"),
                        Delivery = c(5, 1, mean(feedback[[1]]$Delivery)),
                        Engagement = c(5, 1, mean(feedback[[1]]$Engagement)),
                        Language = c(5, 1, mean(feedback[[1]]$Language)),
                        Content = c(5, 1, mean(feedback[[1]]$Content)),
                        Visuals = c(5, 1, mean(feedback[[1]]$Visuals)),
                        GroupPerf = c(5, 1, mean(feedback[[1]]$GroupPerf)))

ave.scores

source(collate.feedback.code.path)
for(k in 1:nrow(fb)){
  
  # Get the details of this particular student
  student.details = ""
  studentName = fb[k]$StudentName
  studentNumber = fb[k]$StudentNumber
  studentNameAndNumber = paste(studentName, studentNumber, sep = " ")
  
  # Get the score of the current student being handled
  score.this.student = feedback[[1]] %>% filter(StudentNumber == studentNumber)
  
  # Score conversion table
  score.conversion.table = data.table(band.score = c(5, 4.5, 4, 3.5, 3, 2.5, 2, 1.5, 1),
                                      conv.score = c(100, 90, 85, 80, 75, 70, 65, 60, 50))
  
  delivery.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Delivery]
  engagement.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Engagement]
  language.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Language]
  content.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Content]
  visuals.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Visuals]
  groupPerf.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$GroupPerf]
  
  # Calculate this student's total score
  total.score = 0.15*delivery.score +
    0.15*engagement.score +
    0.2*language.score +
    0.2*content.score +
    0.2*visuals.score +
    0.1*groupPerf.score - 
    score.this.student$Penalty
  total.score = data.table("Total Score" = total.score)
  print(score.this.student$Penalty)
  
  # Penalty comment
  penalty.comment = ""
  if(score.this.student$Penalty > 0){
    if(score.this.student$Penalty == 1){
      penalty.comment = paste("Your score was reduced by ", score.this.student$Penalty, " point due to being over the time limit.", sep = "")
    } else {
      penalty.comment = paste("Your score was reduced by ", score.this.student$Penalty, " points due to being over the time limit.", sep = "")
    }
  }
  
  
  # Remove the metadata and keep the scores only for the radar chart
  scores.only = score.this.student %>% select(StudentName, Delivery, Engagement, Language, Content, Visuals, GroupPerf)
  scores.only.rownames <- scores.only[,-1]
  rownames(scores.only.rownames) <- scores.only[,1]
  
  # Add to average scores for the radar chart
  both.scores = rbind(ave.scores, scores.only.rownames)
  
  
  personal.details = data.table("Name" = studentName, "Number" = studentNumber, "Instructor" = fb[k]$Instructor)
  personal.details.flex <- flextable(personal.details)
  personal.details.flex <- set_table_properties(personal.details.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  
  
  rmarkdown::render(
    
    input = paste(core.source.path, "createFeedback.Rmd", sep = ""),
    output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_Group_Project/Reports/{studentNameAndNumber}.docx"),
    params = list(personal.details.flex = personal.details.flex,
                  both.scores = both.scores,
                  total.score = total.score,
                  penalty.comment = penalty.comment)
  )
  
}














