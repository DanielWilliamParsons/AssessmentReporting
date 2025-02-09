
collateFeedback = function(responses, comments, impressive, improved, scores){
  # Make the scores data table more easily searchable by score category.
  scores = scores %>% select(StudentNumber, Comprehensibility, FluencyPacing, Engagement, Language, Content, Visuals) %>%
    pivot_longer(!StudentNumber, names_to = "Component", values_to = "Score") %>%
    mutate(Score = as.numeric(Score)) %>%
    mutate(Score = floor(Score))
  print(scores)
  comments = comments %>% mutate(Score = as.numeric(Score))
  
  fb = ""
  
  # Get the time taken for your presentation
  time = paste("Your presentation lasted for ", responses$b, ".", sep="")
  
  # Get the "key word" summarizing the presentation.
  key.word = paste("The word that best summarizes my impression of your presentation is: ", responses$c, ".", sep = "")
  
  # Get the introductory comments from the teacher
  intro = responses$d
  
  # Get the instructor's comment about which point was most improved since the previous OP
  component.improved <- improved %>% pull(Component)
  if(responses$e %in% component.improved){
    improved = improved %>% filter(Component == responses$e) %>% slice_sample()
    improved.comment = improved$Comment
  } else {
    improved.comment = responses$e
  }
  
  # The instructor selected points for advice. Get random but relevant advice from the comments database
  # We also need to filter for the component and correct score
  component.comments <- comments %>% pull(Sub_Component)
  if(responses$f %in% component.comments){
    advice.1 = comments %>% filter(Sub_Component == responses$f) %>% left_join(scores, by = c("Component"="Component", "Score"="Score")) %>% filter(!is.na(StudentNumber)) %>% slice_sample()
    advice.1.main = advice.1$advice
  } else {
    advice.1.main = responses$f
  }
  
  if(responses$g %in% component.comments){
    advice.2 = comments %>% filter(Sub_Component == responses$g) %>% left_join(scores, by = c("Component"="Component", "Score"="Score")) %>% filter(!is.na(StudentNumber)) %>% slice_sample()
    advice.2.main = advice.2$advice
  } else {
    advice.2.main = responses$g
  }
  
  if(responses$h %in% component.comments){
    advice.3 = comments %>% filter(Sub_Component == responses$h) %>% left_join(scores, by = c("Component"="Component", "Score"="Score")) %>% filter(!is.na(StudentNumber)) %>% slice_sample()
    advice.3.main = advice.3$advice
  } else {
    advice.3.main = responses$h
  }
  

  # REMOVED IN 2023 #
  # Get the definitions of the criteria for advice for student reference.
  # This is so that students can check what the specific point means, e.g., what is signposting
  # advice.1.definition = advice.1$definition
  # advice.2.definition = advice.2$definition
  # advice.3.definition = advice.3$definition
  # REMOVED IN 2023 #
  
  if(responses$i %in% component.comments){
    advice.4 = comments %>% filter(Sub_Component == responses$i) %>% left_join(scores, by = c("Component"="Component", "Score"="Score")) %>% filter(!is.na(StudentNumber)) %>% slice_sample()
    advice.4.detailed = advice.4$detailed_advice
  } else {
    advice.4.detailed = responses$i
  }
  
  # Instructors also selected a criteria which they thought was impressive in the students' presentation.
  # Get this selection and match it to a comment praising the effort and explaining why it is useful
  components.impressive = impressive %>% pull(Sub_Component)
  if(responses$j %in% components.impressive){
    impressive.1 = impressive %>% filter(Sub_Component == responses$j) %>% left_join(scores, by = c("Component"="Component", "Score"="Score")) %>% filter(!is.na(StudentNumber)) %>% slice_sample()
    impressive.1.point = impressive.1$Impressive
  } else {
    impressive.1.point = responses$j
  }
  
  if(responses$k %in% components.impressive){
    impressive.2 = impressive %>% filter(Sub_Component == responses$k) %>% left_join(scores, by = c("Component"="Component", "Score"="Score")) %>% filter(!is.na(StudentNumber)) %>% slice_sample()
    impressive.2.point = impressive.2$Impressive
  } else {
    impressive.2.point = responses$k
  }
  
  impressive.1 = impressive %>% filter(Sub_Component == responses$j) %>% left_join(scores, by = c("Component"="Component", "Score"="Score")) %>% filter(!is.na(StudentNumber)) %>% slice_sample()
  impressive.2 = impressive %>% filter(Sub_Component == responses$k) %>% left_join(scores, by = c("Component"="Component", "Score"="Score")) %>% filter(!is.na(StudentNumber)) %>% slice_sample()

  
  
  
  # Instructors write a brief summary at the end of the feedback. Get this summary.
  summary.message = responses$l
  
  # Put all the feedback points into a list and return for further processing.
  all.feedback = list(time, intro, improved.comment,
                      advice.1.main, advice.2.main, advice.3.main, advice.4.detailed,
                      impressive.1.point, impressive.2.point, summary.message,
                      key.word)
  
  return(all.feedback)
  
}