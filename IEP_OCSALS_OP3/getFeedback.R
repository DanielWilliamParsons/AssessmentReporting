
getFeedback = function(core.path){
  
  # Get the feedback that the teachers provided with the Google Form
  feedback.data.path = paste(core.path, "Feedback.csv", sep="")
  feedback.responses = data.table::fread(feedback.data.path)
  
  # Rename the columns of the feedback to make them more 'programmable'
  new.colnames = letters[1:(ncol(feedback.responses)-5)]
  print(new.colnames)
  j = 1
  for(i in 6:ncol(feedback.responses)){
    colnames(feedback.responses)[i] = new.colnames[j]
    j = j + 1
  }
  
  # Get the comments and advice that are related to each of the feedback points
  comments.advice.path = paste(core.path, "Comments_advice.csv", sep="")
  comments.advice = data.table::fread(comments.advice.path, header=TRUE)
  
  # Get the comments related to what was impressive about the students' presentations
  impressive.points.path = paste(core.path, "Impressive_points.csv", sep="")
  impressive.points = data.table::fread(impressive.points.path)
  
  # Get the comments related to what was improved since OP1
  most.improved.path = paste(core.path, "Most_improved.csv", sep="")
  most.improved = data.table::fread(most.improved.path)
  
  # Get the OP2 scores
  scores.path = paste(core.path, "OP3_scores.csv", sep="")
  scores = data.table::fread(scores.path)
  
  
  
  
  feedback = list(feedback.responses, comments.advice, impressive.points, most.improved, scores)
  return(feedback)
  
}