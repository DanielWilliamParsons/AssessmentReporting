
focusedAdvice = function(feedback){
  
  feedback$checkProp = feedback$construct.score / feedback$max.possible
  
  percentScore = sum(feedback$construct.score) / sum(feedback$max.possible)
  
  feedback$construct.score = feedback$construct.score + 0.01
  
  if("Understanding Organization" %in% feedback$construct & feedback$checkProp[feedback$construct == "Understanding Organization"] != 1){
    feedback$max.possible[feedback$construct == "Understanding Organization"] = 10000 * feedback$max.possible[feedback$construct == "Understanding Organization"]
  }
  
  if("Gist" %in% feedback$construct & feedback$checkProp[feedback$construct == "Gist"] != 1){
    feedback$max.possible[feedback$construct == "Gist"] = 20 * feedback$max.possible[feedback$construct == "Gist"]
  }
  
  if("Detail" %in% feedback$construct & feedback$checkProp[feedback$construct == "Detail"] != 1){
    feedback$max.possible[feedback$construct == "Detail"] = 100 * feedback$max.possible[feedback$construct == "Detail"]
  }
  
  if("Connecting Content" %in% feedback$construct & feedback$checkProp[feedback$construct == "Connecting Content"] != 1){
    feedback$max.possible[feedback$construct == "Connecting Content"] = 10 * feedback$max.possible[feedback$construct == "Connecting Content"]
  }
  
  feedback = feedback %>% mutate(ratio = construct.score/max.possible) %>% arrange(ratio)
  if(percentScore < 0.70){
    
    comment = "To improve your score in the next quiz, we recommend you focus on "
    constructToFocusOn = tolower(feedback[1,]$construct)
    comment = paste(comment, constructToFocusOn, ".", sep = "")
    
  } else {
    if(feedback[1,]$checkProp < 1){
      comment = "You have done well in this quiz, but you might want to focus on "
      constructToFocusOn = tolower(feedback[1,]$construct)
      comment = paste(comment, constructToFocusOn, " for the next quiz.", sep = "")
    } else {
      comment = "Overall, you have done very well. Keep up the good work for the next quiz."
    }
  }
  
  return(comment)
  
}