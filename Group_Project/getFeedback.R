
getFeedback = function(core.path){

  # Get the OP2 scores
  scores.path = paste(core.path, "GP_scores.csv", sep="")
  scores = data.table::fread(scores.path)
  
  feedback = list(scores)
  return(feedback)
  
}