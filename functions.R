library(tidyverse)

# debate: tabroom scraping ------------------------------------------------


words <- c("TOURN", "EVENT", "JUDGE",  "entry", "entry_student", "school","round",  "panel", "ballot", 'ballot_score', "elimseed", "TIMESLOT") %>% toupper ## note: room was an option too but it messed up a function and I don't care. Round also causes some to break, as did tiebreak and tiebreak set for KYRR. 



# functions for extraction ------------------------------------------------

## make a function to get the info for every tournament

extract_child <- function(xml_object, word) {
  
  a <- xml_object %>% 
    xml_find_all(paste0("//", word)) %>% 
    map(xml_children) %>% 
    map_df(~map(setNames(xml_text(.), xml_name(.) %>% tolower), type.convert, as.is=TRUE))
  
  colnames(a)[colnames(a) == "id"] <- paste0(word %>% tolower, "_id") #  having trouble with rename in plyr and dplyr so doing this
  
  return(a)
  
}


make_all <- function(xml_object, words) {
  
  mylist <- list()
  
  for (i in 1:length(words)) {
    mylist[[i]] <- extract_child(xml_object, words[i])
  }
  
  names(mylist) <- words %>% tolower
  
  return(mylist) 
}


one_df <- function(xml_object, words) {
  
  c <- make_all(xml_object, words)
  
  df <- with(c, 
             entry %>% 
               select(entry_id, school, event, code, fullname) %>% 
               left_join(ballot %>% 
                           select(ballot_id, judge, panel, entry, side),
                         by = c("entry_id" = "entry")) %>% 
               left_join(ballot_score %>% 
                           rename(win = score),
                         by = c("entry_id" = "recipient", "ballot_id" = "ballot"))  %>% 
               cbind(tourn %>% 
                       select(tournname, tourn_id, startdate, enddate)) %>% 
               as.tibble %>% 
               left_join(panel %>% 
                           select(-c(room, flight)),
                         by = c("panel" = "panel_id")) %>% 
               left_join(event %>% 
                           select(eventname, event_id),
                         by = c("event" = "event_id")) %>%
               left_join(round %>%
                           select(round_id, timeslot, rd_name, judgesperpanel, pairingscheme),
                         by = c("round" = "round_id")) %>%
               left_join(timeslot,
                         by = c("timeslot" = "timeslot_id")) %>% 
               left_join(school %>% 
                           select(school_id, code, schoolname) %>% 
                           rename(schoolcode = code),
                         by = c("school" = "school_id")) %>% 
               left_join(entry_student %>% 
                           select(-downloadrecord),
                         by = c("entry_id" = "entry", "school" = "school")) %>% 
               left_join(judge %>% 
                           rename(j_school = school, j_first = first, j_last = last, j_id = person),
                         by = c("judge" = "judge_id"))
             
  ) 
  
  return(df)
  
}
