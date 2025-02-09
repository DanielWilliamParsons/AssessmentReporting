
collateFeedback = function(responses, comments, impressive){
  
  fb = ""
  
  time = paste("Your presentation lasted for ", responses$b, ".", sep="")
  
  # Get the introductory comments from the teacher
  intro = responses$c
  
  # The instructor selected points for advice. Get random but relevant advice from the comments database
  # If the instructor provided an original comment then only include the original comment
  comments.components <- comments %>% pull(Component)
  if(responses$d %in% comments.components){
    advice.1 = comments %>% filter(Component == responses$d) %>% slice_sample()
    advice.1.main = advice.1$advice
  } else {
    advice.1.main = responses$d
  }
  
  if(responses$e %in% comments.components){
    advice.2 = comments %>% filter(Component == responses$e) %>% slice_sample()
    advice.2.main = advice.2$advice
  } else {
    advice.2.main = responses$e
  }
  
  if(responses$f %in% comments.components) {
    advice.3 = comments %>% filter(Component == responses$f) %>% slice_sample()
    advice.3.main = advice.3$advice
  } else {
    advice.3.main = responses$f
  }
  
  

  
  # REMOVED IN 2023 #
  # Get the definitions of the criteria for advice for student reference.
  # This is so that students can check what the specific point means, e.g., what is signposting
  # advice.1.definition = advice.1$definition
  # advice.2.definition = advice.2$definition
  # advice.3.definition = advice.3$definition
  # REMOVED IN 2023 #
  
  # Get the instructor detailed advice.
  # If the instructor wrote an original then only include the original comment
  if(responses$g %in% comments.components){
    advice.4 = comments %>% filter(Component == responses$g) %>% slice_sample()
    advice.4.detailed = advice.4$detailed_advice
  } else {
    advice.4.detailed <- responses$g
  }
  
  
  # Instructors also selected a criteria which they thought was impressive in the students' presentation.
  # Get this selection and match it to a comment praising the effort and explaining why it is useful
  impressive.components = impressive %>% pull(Component)
  if(responses$h %in% impressive.components){
    impressive = impressive %>% filter(Component == responses$h) %>% slice_sample()
    impressive.point = impressive$Impressive
  } else {
    impressive.point <- responses$h
  }
  
  
  # Instructors write a brief summary at the end of the feedback. Get this summary.
  summary.message = responses$i
  
  # Put all the feedback points into a list and return for further processing.
  all.feedback = list(time, intro, advice.1.main, advice.2.main, advice.3.main, advice.4.detailed,
                      impressive.point, summary.message)
  
  return(all.feedback)
  
}