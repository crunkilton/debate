setwd("C:/Users/Cody/Dropbox/debate_r")
rm(list = ls())

## Note: the judge numbers on the URL for paradigms do not match the IDs from the results XMLs, so this is not helpful. 

# collecting judge names --------------------------------------------------

library(tidyverse)
library(xml2)
library(rvest)
library(purrr)

start <- Sys.time()


get_judge_names <- function(url) {
  read_html(url) %>% 
    html_text %>% 
    str_remove_all("\\t") %>% 
    str_remove_all("\\n") %>% 
    str_remove_all("\\r") %>% 
    str_remove(".*ResultsParadigmsHelpAbout") %>% 
    str_remove(" Paradigm.*") 
}

rm(judge_dict)

## if not done by morning: x <- x+1. if x %% 500, write.csv thing. 

for (id in 1:200000) {
  url <- paste0("https://www.tabroom.com/index/paradigm.mhtml?judge_person_id=", id)
  
  judge_name <- url %>% 
    map_chr(possibly(get_judge_names, "Error"))
      
    this_judge <- data.frame(judge_name = judge_name, judge_id = id)
  
  if (judge_name != "Judge") {
    
    if (!exists("judge_dict")) {
      
      judge_dict <- this_judge
      
    } else {
      
      judge_dict <- rbind(this_judge, judge_dict)
      
    }
  }
}

judge_dict %>% write_csv("judge_dict.csv")


judge_dict %>% as.tibble

end <- Sys.time()
end-start