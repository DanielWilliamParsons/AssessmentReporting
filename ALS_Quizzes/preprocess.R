# Preprocess the question and answer sheet
# The question and answer CSV file is in a wide format, which makes
# classification of each question difficult
# This preprocessing turns the sheet into long form to allow for classifying the questions

preprocess = function(core.path, csvToProcess, csvToSave){
  print(core.path)
  qa.path = paste(core.path, csvToProcess, sep="")
  qa = data.table::fread(qa.path)

  maxcol = ncol(qa)
  print(maxcol)
  qa = qa %>% select(c(5:maxcol)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(!`Your instructor's name`, names_to = "question", values_to = "answer") %>%
    select(question, answer)
  
  data.table::fwrite(qa, paste(core.path, csvToSave, sep = ""))
  
  
}