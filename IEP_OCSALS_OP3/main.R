library(data.table)
library(tidyverse)
library(ggplot2)
library(fmsb)
library(magrittr)
library(flextable)
library(officer)

core.source.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/R Scripts/Testing/IEP_OCSALS_OP3/"
#### Update this every year ####
#### ALSO UPDATE YEAR IN RENDER MARKDOWN ####
core.data.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_IEP_OCSALS_OP3/"

# Get the feedback and comments into a usable format
feedback.code.path = paste(core.source.path, "getFeedback.R", sep="")
source(feedback.code.path)
feedback = getFeedback(core.data.path)

# Create feedback forms on a per-student basis.
# This involve formatting flex-tables for printing in Word.
collate.feedback.code.path = paste(core.source.path, "collateFeedback.R", sep = "")
source(collate.feedback.code.path)
fb = feedback[[1]]

#Use the scores in feedback[[5]] to calculate the average for each component
ave.scores = data.frame(row.names = c("Max", "Min", "Average"),
                        Comprehensibility = c(5, 1, mean(feedback[[5]]$Comprehensibility)),
                        FluencyPacing = c(5, 1, mean(feedback[[5]]$FluencyPacing)),
                        Engagement = c(5, 1, mean(feedback[[5]]$Engagement)),
                        Language = c(5, 1, mean(feedback[[5]]$Language)),
                        Content = c(5, 1, mean(feedback[[5]]$Content)),
                        Visuals = c(5, 1, mean(feedback[[5]]$Visuals)))

ave.scores

source(collate.feedback.code.path)
for(k in 1:nrow(fb)){
  
  # Get the details of this particular student
  student.details = ""
  studentName = fb[k]$StudentName
  studentNumber = fb[k]$StudentNumber
  studentNameAndNumber = paste(studentNumber, studentName, sep = " ")
  
  # Get the score of the current student being handled
  score.this.student = feedback[[5]] %>% filter(StudentNumber == studentNumber)
  
  # Score conversion table
  score.conversion.table = data.table(band.score = c(5, 4.5, 4, 3.5, 3, 2.5, 2, 1.5, 1),
                                      conv.score = c(100, 90, 85, 80, 75, 70, 65, 60, 50))
  
  comprehensibility.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Comprehensibility]
  fluencypacing.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$FluencyPacing]
  engagement.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Engagement]
  language.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Language]
  content.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Content]
  visuals.score = score.conversion.table$conv.score[score.conversion.table$band.score==score.this.student$Visuals]
  
  # Calculate this student's total score
  total.score = 0.15*comprehensibility.score +
    0.15*fluencypacing.score +
    0.2*engagement.score +
    0.2*language.score +
    0.2*content.score +
    0.1*visuals.score - 
    score.this.student$Penalty
  total.score = data.table("Total Score" = total.score)
  
  # Penalty comment
  penalty.comment = ""
  print(score.this.student$Penalty)
  if(score.this.student$Penalty > 0){
    if(score.this.student$Penalty == 1){
      penalty.comment = paste("Your score was reduced by ", score.this.student$Penalty, " point due to being over the time limit.", sep = "")
    } else {
      penalty.comment = paste("Your score was reduced by ", score.this.student$Penalty, " points due to being over the time limit.", sep = "")
    }
  }
  
  
  # Remove the metadata and keep the scores only for the radar chart
  scores.only = score.this.student %>% select(StudentName, Comprehensibility, FluencyPacing, Engagement, Language, Content, Visuals)
  scores.only.rownames <- scores.only[,-1]
  rownames(scores.only.rownames) <- scores.only[,1]
  
  # Add to average scores for the radar chart
  both.scores = rbind(ave.scores, scores.only.rownames)
  
  collated.feedback = collateFeedback(fb[k], feedback[[2]], feedback[[3]], feedback[[4]], score.this.student)
  
  personal.details = data.table("Name" = studentName, "Number" = studentNumber, "Instructor" = fb[k]$Instructor)
  personal.details.flex <- flextable(personal.details)
  personal.details.flex <- set_table_properties(personal.details.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  intro.advice = data.table("COMMENTS" = collated.feedback[[2]],
                            "ADVICE" = c(collated.feedback[[4]],
                                         collated.feedback[[5]],
                                         collated.feedback[[6]]))
  intro.advice.flex = regulartable(intro.advice)
  intro.advice.flex <- set_table_properties(intro.advice.flex, width = 1.0) %>%
    autofit(add_w = 1) %>%
    theme_booktabs() %>%
    width(j = ~ COMMENTS, width = 3) %>%
    merge_v(j = ~ COMMENTS) %>%
    fontsize(j = ~ ADVICE, part = "body", size = 14)
  
  detailed.advice = data.table("SUGGESTIONS:" = c(collated.feedback[[7]]))
  detailed.advice.flex = flextable(detailed.advice)
  detailed.advice.flex <- set_table_properties(detailed.advice.flex, width = 0.9, layout="autofit") %>%
    theme_booktabs()
  
  impressive.points = data.table("EVALUATION:" = c(collated.feedback[[3]],
                                                   collated.feedback[[8]],
                                                   collated.feedback[[9]],
                                                   collated.feedback[[10]]))
  impressive.points.flex = flextable(impressive.points)
  impressive.points.flex <- set_table_properties(impressive.points.flex, width = 0.9, layout="autofit") %>%
    theme_booktabs()
  
  #definitions = data.table("Meaning of the advice:" = c(collated.feedback[[11]], 
   #                                                         collated.feedback[[12]], 
    #                                                        collated.feedback[[13]]))
  #definitions.flex = flextable(definitions)
  #definitions.flex <- set_table_properties(definitions.flex, width = 0.9, layout="autofit") %>%
  #  theme_booktabs() %>%
  #  fontsize(part = "all", size = 9) %>%
  #  fontsize(part = "header", size = 14) %>%
  #  italic(part = "all") %>%
  #  font(part = "all", fontname = "Courier") %>%
  #  color(part = "header", color = "#FF0000") %>%
  #  bold(part = "header")
  
  time.info = collated.feedback[[1]]
  
  key.word = collated.feedback[[11]]
  print(both.scores)
  
  rmarkdown::render(
    
    input = paste(core.source.path, "createFeedback.Rmd", sep = ""),
    output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_IEP_OCSALS_OP3/Reports/{studentNameAndNumber}.docx"),
    params = list(personal.details.flex = personal.details.flex,
                  intro.advice.flex = intro.advice.flex,
                  detailed.advice.flex = detailed.advice.flex,
                  impressive.points.flex = impressive.points.flex,
                  both.scores = both.scores,
                  time.info = time.info,
                  total.score = total.score,
                  penalty.comment = penalty.comment,
                  key.word = key.word)
  )
  
}