
teacherAnalytics = function(core.path, QandASheet){
  
  responses.path = paste(core.path, "studentResponses.csv", sep = "")
  responses = data.table::fread(responses.path)
  responses = responses %>% mutate(across(everything(), as.character))
  
  correct.answers.path = paste(core.path, QandASheet, sep = "")
  correct.answers = data.table::fread(correct.answers.path)
  
  students.path = paste(core.path, "students.csv", sep = "")
  students = data.table::fread(students.path)
  
  responses = responses %>% select(`Email Address`, c(6:ncol(responses))) %>%
    pivot_longer(!`Email Address`, names_to="question", values_to="response") %>%
    left_join(correct.answers, by = c("question"="question")) %>%
    mutate(poss.score = as.double(poss.score)) %>%
    rowwise() %>%
    mutate(score = if_else(answer == response, poss.score, 0)) %>%
    ungroup() %>%
    left_join(students, by = c("Email Address" = "Email Address"))
  
  
  # IEP and section-by-section average scores
  summary.average.score = responses %>% group_by(`Email Address`) %>%
    summarize(total.score = sum(score)) %>% ungroup() %>% summarize(mean.score.iep = mean(total.score))
  
  summary.average.section.score = responses %>% group_by(`Email Address`, Section) %>%
    summarize(total.score = sum(score)) %>% ungroup() %>% group_by(Section) %>%
    summarize(section.mean.score = mean(total.score))
  
  # IEP and section-by-section percentage correct within construct
  summary.percent.construct = responses %>% group_by(construct) %>%
    summarize(percent.correct = 100 * (sum(score)/sum(poss.score))) %>% ungroup() %>%
    arrange(-percent.correct)
  
  summary.percent.section.construct = responses %>% group_by(construct, Section) %>%
    summarize(percent.correct = 100 * (sum(score)/sum(poss.score))) %>% ungroup() %>%
    group_by(construct) %>% arrange(-percent.correct) %>% ungroup()
  
  # IEP and section-by-section questions incorrect
  # These tell the difficulty of the question
  summary.incorrect = responses %>% 
    mutate(number.incorrect = if_else(score == 0, 1, 0)) %>%
    group_by(question) %>%
    summarize(ratio.incorrect = sum(number.incorrect)/n()) %>%
    arrange(-ratio.incorrect) %>% 
    mutate(difficulty = if_else(ratio.incorrect >= 0.8, "difficult", "")) %>%
    mutate(difficulty = if_else(ratio.incorrect < 0.8 & ratio.incorrect >= 0.6, "a little difficult", difficulty)) %>%
    mutate(difficulty = if_else(ratio.incorrect < 0.6 & ratio.incorrect >= 0.3, "reasonable", difficulty)) %>%
    mutate(difficulty = if_else(ratio.incorrect < 0.3, "easy", difficulty)) %>% select(-ratio.incorrect)
  
  summary.incorrect.section = responses %>%
    mutate(number.incorrect = if_else(score == 0, 1, 0)) %>%
    group_by(question, Section) %>%
    summarize(ratio.incorrect = sum(number.incorrect)/n()) %>%
    arrange(-ratio.incorrect, question) %>% ungroup()
  
  # Question discrimination analysis
  ranked.students = responses %>% group_by(`Email Address`) %>%
    summarize(total.score = sum(score)) %>% ungroup() %>% arrange(-total.score)
  
  top.students = ranked.students %>% slice_head(prop = 0.33333334) %>% pull(`Email Address`)
  bottom.students = ranked.students %>% slice_tail(prop = 0.33333334) %>% pull(`Email Address`)
  
  top.responses = responses %>% filter(`Email Address` %in% top.students) %>% 
    mutate(number.correct = if_else(score == 0, 0, 1)) %>%
    group_by(question) %>%
    summarize(ratio.correct.top = sum(number.correct)/n()) %>%
    arrange(-ratio.correct.top)
  
  bottom.responses = responses %>% filter(`Email Address` %in% bottom.students) %>% 
    mutate(number.correct = if_else(score == 0, 0, 1)) %>%
    group_by(question) %>%
    summarize(ratio.correct.bottom = sum(number.correct)/n()) %>%
    arrange(-ratio.correct.bottom)
  
  top.responses.bottom.responses = top.responses %>% 
    left_join(bottom.responses, by = c("question" = "question")) %>%
    mutate(discr.index = ratio.correct.top - ratio.correct.bottom) %>%
    arrange(-discr.index) %>%
    mutate(discrimination = if_else(discr.index >= 0.4, "very good discrimination", "")) %>%
    mutate(discrimination = if_else(discr.index < 0.4 & discr.index >= 0.3, "reasonable discrimination", discrimination)) %>%
    mutate(discrimination = if_else(discr.index < 0.3 & discr.index >= 0.2, "marginal discrimination", discrimination)) %>%
    mutate(discrimination = if_else(discr.index < 0.2, "does not discriminate", discrimination))
  
  return.data = list(summary.average.score,
                     summary.average.section.score,
                     summary.percent.construct,
                     summary.percent.section.construct,
                     summary.incorrect,
                     top.responses.bottom.responses,
                     summary.incorrect.section)
  return(return.data)
  
}