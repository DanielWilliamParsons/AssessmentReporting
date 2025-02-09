library(data.table)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(flextable)
library(officer)

core.source.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/R Scripts/Testing/IEP_OCSALS_OP3_Peer/"
#### Update this every year ####
#### ALSO UPDATE YEAR IN RENDER MARKDOWN ####
core.data.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_IEP_OCSALS_OP3_Peer/"


feedback.path <- paste(core.data.path, "Peer Feedback All.csv", sep = "")
feedback <- data.table::fread(feedback.path)

feedback <- feedback %>% dplyr::rename(Name = `Classmate's Name`)
names(feedback)[4] <- "delivery"
names(feedback)[5] <- "content"
names(feedback)[6] <- "slides"
names(feedback)[7] <- "comment"

print(head(feedback))

feedback.summary <- feedback %>% group_by(Name) %>% summarize(average.delivery = mean(delivery),
                                                              average.content = mean(content),
                                                              average.slides = mean(slides),
                                                              comments = paste(comment, collapse = "\n"))
View(feedback.summary)

# Get student info
student.info.path <- paste(core.data.path, "studentInfo.csv", sep = "")
student.info <- data.table::fread(student.info.path)

feedback.summary <- feedback.summary %>% dplyr::left_join(student.info, by = c("Name" = "StudentName"))

# Make the flextables and the rmarkdown
for(k in 1:nrow(feedback.summary)){
  
  student.details = ""
  studentName = feedback.summary$Name[k]
  studentNumber = feedback.summary$StudentNumber[k]
  studentNameAndNumber = paste(studentNumber, studentName, sep = " ")
  
  personal.details = data.table("Name" = studentName, "Number" = studentNumber, "Instructor" = feedback.summary$Phase2Instr[k])
  print(personal.details)
  personal.details.flex <- flextable(personal.details)
  personal.details.flex <- set_table_properties(personal.details.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  scores = data.table("Average Delivery Score:" = feedback.summary$average.delivery[k],
                      "Average Content Score:" = feedback.summary$average.content[k],
                      "Average Slides Score:" = feedback.summary$average.slides[k])
  scores.flex = flextable(scores)
  scores.flex <- set_table_properties(scores.flex, width = 0.9, layout="autofit") %>%
    theme_booktabs()
  
  
  detailed.advice = data.table("COMMENTS FROM CLASSMATES:" = feedback.summary$comments[k])
  detailed.advice.flex = flextable(detailed.advice)
  detailed.advice.flex <- set_table_properties(detailed.advice.flex, width = 0.9, layout="autofit") %>%
    theme_booktabs()
  
  
  
  
  
  rmarkdown::render(
    
    input = paste(core.source.path, "createFeedback.Rmd", sep = ""),
    output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2023_IEP_OCSALS_OP3_Peer/Reports/{studentNameAndNumber}.docx"),
    params = list(personal.details.flex = personal.details.flex,
                  scores.flex = scores.flex,
                  detailed.advice.flex = detailed.advice.flex)
  )
  
}




