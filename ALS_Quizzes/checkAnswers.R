# checkAnswers
# Takes the core path to the data and retrieves the responses from students
# Grades the answers and provides relevant feedback comments to each construct

checkAnswers = function(core.path, QandASheet){
  
  responses.path = paste(core.path, "studentResponses.csv", sep = "")
  responses = data.table::fread(responses.path)
  responses = responses %>% mutate(across(everything(), as.character))
  
  correct.answers.path = paste(core.path, QandASheet, sep = "")
  correct.answers = data.table::fread(correct.answers.path)
  
  students.path = paste(core.path, "students.csv", sep = "")
  students = data.table::fread(students.path)
  
  comments.table = data.table(
    "welldone" = c("Well done!", "Excellent work!", "Good job!", "Impressive work!", "Great job!", "Impressive job!", "Marvelous work!", "Fantastic work!"),
    "reasonable" = c("Not bad! Review this skill if needed.", "Okay, but some more rehearsal of this skill could be helpful.", "Pretty good! Continue to rehearse this skill.", "Quite reasonable! But think about reviewing this skill a little more.", "Pretty good, but think about reviewing this skill some more.", "Reasonably well done, but consider reviewing this skill.", "Quite good, but consider practicing this skill more.", "Reasonably good work, but make sure to review this skill a little more."),
    "better" = c("Work on this skill more.", "This skill needs more practice.", "Be sure to practice this skill.", "This skill needs improving.", "Make sure you check this skill.", "Work on this skill some more.", "Be sure to work on improving this skill.", "Give this skill more attention when practicing.")
  )
  
  
  responses = responses %>% select(`Email Address`, c(6:ncol(responses))) %>%
    pivot_longer(!`Email Address`, names_to="question", values_to="response") %>%
    left_join(correct.answers, by = c("question"="question")) %>%
    mutate(poss.score = as.double(poss.score)) %>%
    rowwise() %>%
    mutate(score = if_else(answer == response, poss.score, 0)) %>%
    ungroup() %>%
    group_by(`Email Address`) %>%
    group_split()
  
  # Process each student's feedback one by one
  score.feedback = list()
  for(i in 1:length(responses)){
    response = responses[[i]]
    student = response %>% slice_head(n=1) %>% pull(`Email Address`)
    student = students %>% filter(`Email Address` == student)
    
    #Summarize the construct scores for this student
    #construct.score = response %>% group_by(construct)
    #View(construct.score)
    
    construct.score = response %>% group_by(construct) %>%
      summarize(construct.score = sum(score), max.possible = sum(poss.score)) %>%
      mutate(comment = as.character(""))
    

    
    construct.score= construct.score %>% 
      mutate(comment = if_else(max.possible <= 4 & construct.score > 0.6*max.possible, "welldone", comment)) %>%
      mutate(comment = if_else(max.possible > 4 & construct.score > 0.7*max.possible, "welldone", comment)) %>%
      mutate(comment = if_else(max.possible <= 4 & construct.score <= 0.6*max.possible, "reasonable", comment)) %>%
      mutate(comment = if_else(max.possible > 4 & construct.score <= 0.7*max.possible, "reasonable", comment)) %>%
      mutate(comment = if_else(max.possible <= 4 & construct.score <= 0.25*max.possible,"better", comment)) %>%
      mutate(comment = if_else(max.possible > 4 & construct.score <= 0.3*max.possible,"better", comment))
    
    #print(construct.score)
    
    for(j in 1:nrow(construct.score)){
      if(construct.score$comment[j] == "welldone"){
        random.comment = comments.table %>% slice_sample()
        construct.score$comment[j] = random.comment$welldone
      } else if(construct.score$comment[j] == "reasonable"){
        random.comment = comments.table %>% slice_sample()
        construct.score$comment[j] = random.comment$reasonable
      } else {
        random.comment = comments.table %>% slice_sample()
        construct.score$comment[j] = random.comment$better
      }
    }
    
    studentAndScore = list(student, construct.score)
    
    
    score.feedback[[i]] = studentAndScore
  }
  
  return(score.feedback)
  
  
}