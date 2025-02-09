library(tidyverse)
library(data.table)
library(flextable)
library(officer)

#### ISSUES ####
# Issues usually arise when a question has been edited for spelling mistakes
# Go to checkAnswers.R and around lines 40-45 there is a View(construct.score) call
# Unmute this to see which questions have been edited

#### STEP 1: Pre-processing ####
# Before running this first step, make sure that the quiz has been
# answered one time with all the correct answers
# Copy the Google Form
# Answer it one time with all correct answers
# Download the csv answers into the relevant folder
# Then these can both be removed
# First step is to process the Q&A

core.source.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/R Scripts/Testing/ALS_Quizzes/"
core.data.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2022_ALS_Quizzes/ALS_Quiz_4/"

preprocess.path = paste(core.source.path, "preprocess.R", sep="")
source(preprocess.path)

# Only run the below IF NECESSARY!!!
#csvToProcess = "ALS_Quiz_4_QandA.csv"
#csvToSave = "ALS_Quiz_4_QandA_processed.csv"
#preprocess(core.data.path, csvToProcess, csvToSave)

####STEP 2: Preparing constructs####
# Once this has been done, edit the processed sheet to add the constructs
# and the possible score for each question (some questions are worth 2 points remember!)
# Label the columns "construct" and "poss.score"
# Then upload this back to the Google Drive

# After students have completed the test, download their answers into 
# the relevant folder. Call it "studentResponses.csv"

#### STEP 3: Collate the Feedback ####
checkAnswers.path = paste(core.source.path, "checkAnswers.R", sep = "")
source(checkAnswers.path)

QandASheet = "ALS_Quiz_4_QandA_processed.csv"
feedback.scores = checkAnswers(core.data.path, QandASheet)
constructs.path = paste(core.data.path, "constructs.csv", sep = "")
constructs = data.table::fread(constructs.path)

constructs.flex <- flextable(constructs)
constructs.flex <- set_table_properties(constructs.flex, width=0.9, layout = "autofit") %>%
  theme_booktabs() %>% 
  fontsize(part = "all", size = 10)

for(i in 1:length(feedback.scores)){
  
  feedback.score = feedback.scores[[i]][[2]]
  total.score = sum(feedback.score$construct.score)
  max.possible.score = sum(feedback.score$max.possible)
  total.score.table = data.table(`Your score` = total.score, `Maximum Possible Score` = max.possible.score, `% score` = round((total.score/max.possible.score)*100, digits = 3))
  total.score.table.flex <- flextable(total.score.table)
  total.score.table.flex <- set_table_properties(total.score.table.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  feedback.score = feedback.score %>% rename(`Listening Skill` = construct,
                                             `Your Score` = construct.score,
                                             `Maximum Possible Score` = max.possible,
                                             `Comment` = comment)
  feedback.score.flex <- flextable(feedback.score)
  feedback.score.flex <- set_table_properties(feedback.score.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  student.info = feedback.scores[[i]][[1]]
  student.info.flex <- flextable(student.info)
  student.info.flex <- set_table_properties(student.info.flex, width=0.9, layout = "autofit") %>%
    theme_booktabs() %>% 
    fontsize(part = "all", size = 10)
  
  name_number = paste(student.info$Number, student.info$Name, sep = " ")
  
  rmarkdown::render(
    input = paste(core.source.path, "ALS_Quiz_4_Feedback.Rmd", sep = ""),
    output_file = stringr::str_glue("/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2022_ALS_Quizzes/ALS_Quiz_4/Reports/{name_number}.docx"),
    params = list(total.score.table.flex = total.score.table.flex,
                  feedback.score.flex = feedback.score.flex,
                  student.info.flex = student.info.flex,
                  constructs.flex = constructs.flex)
  )
}


#### STEP 4: Analytics for teachers ####

teacher.analytics.path = paste(core.source.path, "teacherAnalytics.R", sep = "")
source(teacher.analytics.path)
analytics.data = teacherAnalytics(core.data.path, QandASheet)
iep.average = analytics.data[[1]]
section.average = analytics.data[[2]]
iep.construct.percent = analytics.data[[3]]
section.construct.percent = analytics.data[[4]]
difficulty.analysis = analytics.data[[5]]
discrimination.index = analytics.data[[6]]
section.incorrect.percentage = analytics.data[[7]]

rmarkdown::render(
  input = paste(core.source.path, "ALS_Quiz_4_Analysis.Rmd", sep = ""),
  output_file = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/Testing/2022_ALS_Quizzes/ALS_Quiz_4/Reports/Quiz_4_Analysis.docx",
  params = list(iep.average = iep.average,
                section.average = section.average,
                iep.construct.percent = iep.construct.percent,
                section.construct.percent = section.construct.percent,
                difficulty.analysis = difficulty.analysis,
                discrimination.index = discrimination.index,
                section.incorrect.percentage = section.incorrect.percentage)
)






