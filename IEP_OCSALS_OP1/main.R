library(data.table)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(flextable)
library(officer)

core.source.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/R Scripts/Testing/IEP_OCSALS_OP1/"
#### Update this every year ####
#### ALSO UPDATE YEAR IN RENDER MARKDOWN ####
core.data.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_IEP_OCSALS_OP1/"

# Get the feedback and comments into a usable format
feedback.code.path = paste(core.source.path, "getFeedback.R", sep="")
source(feedback.code.path)
feedback = getFeedback(core.data.path)

# Create feedback forms on a per-student basis.
# This involve formatting flex-tables for printing in Word.
collate.feedback.code.path = paste(core.source.path, "collateFeedback.R", sep = "")
source(collate.feedback.code.path)
fb = feedback[[1]]
for(k in 1:nrow(fb)){
  collated.feedback = collateFeedback(fb[k], feedback[[2]], feedback[[3]])
  
  student.details = ""
  studentName = fb[k]$StudentName
  studentNumber = fb[k]$StudentNumber
  studentNameAndNumber = paste(studentNumber, studentName, sep = " ")
  
  personal.details = data.table("Name" = studentName, "Number" = studentNumber, "Instructor" = fb[k]$Instructor)
  personal.details.flex <- flextable(personal.details)
  personal.details.flex <- set_table_properties(personal.details.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  intro.advice = data.table("COMMENTS" = collated.feedback[[2]],
                            "ADVICE" = c(collated.feedback[[3]],
                                         collated.feedback[[4]],
                                         collated.feedback[[5]]))
  intro.advice.flex = regulartable(intro.advice)
  intro.advice.flex <- set_table_properties(intro.advice.flex, width = 0.9) %>%
    autofit(add_w = 0) %>%
    theme_booktabs() %>%
    width(j = ~ COMMENTS, width = 3) %>%
    merge_v(j = ~ COMMENTS) %>%
    fontsize(j = ~ ADVICE, part = "body", size = 14)

  detailed.advice = data.table("SUGGESTIONS:" = c(collated.feedback[[6]]))
  detailed.advice.flex = flextable(detailed.advice)
  detailed.advice.flex <- set_table_properties(detailed.advice.flex, width = 0.9, layout="autofit") %>%
    theme_booktabs()
  
  impressive.points = data.table("EVALUATION:" = c(collated.feedback[[1]],
                                                   collated.feedback[[7]],
                                                   collated.feedback[[8]]))
  impressive.points.flex = flextable(impressive.points)
  impressive.points.flex <- set_table_properties(impressive.points.flex, width = 0.9, layout="autofit") %>%
    theme_booktabs()
  
  #definitions = data.table("What does the advice mean?" = c(collated.feedback[[9]], 
   #                                                         collated.feedback[[10]], 
    #                                                        collated.feedback[[11]]))
  #definitions.flex = flextable(definitions)
  #definitions.flex <- set_table_properties(definitions.flex, width = 0.9, layout="autofit") %>%
   # theme_booktabs() %>%
  #  fontsize(part = "all", size = 9) %>%
  #  fontsize(part = "header", size = 14) %>%
  #  italic(part = "all") %>%
  #  font(part = "all", fontname = "Courier") %>%
  #  color(part = "header", color = "#FF0000") %>%
  #  bold(part = "header")

  
  rmarkdown::render(
    
    input = paste(core.source.path, "createFeedback.Rmd", sep = ""),
    output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_IEP_OCSALS_OP1/Reports/{studentNameAndNumber}.docx"),
    params = list(personal.details.flex = personal.details.flex,
                  intro.advice.flex = intro.advice.flex,
                  detailed.advice.flex = detailed.advice.flex,
                  impressive.points.flex = impressive.points.flex)
  )
  
}

