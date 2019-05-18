## Cody Crunkilton
## Misc Projects - Debate
## Tabroom XML extraction

## note: functions sourced in. 


setwd("C:/Users/Cody/Dropbox/debate_r")
source("C:/Users/Cody/Dropbox/functions.R")

library(xml2)
library(tidyverse)
library(readxl)


# downloading XML ---------------------------------------------------------


# load("tournaments_xml.Rdata") ## load the data directly


urls <- read_excel("debateresults_2018_2019.xlsx") %>% 
  mutate(tourn_id = id %>% str_remove(".*tourn_id=") %>% 
           str_remove("&result_id=.*")) %>% 
  arrange(major %>% desc)


## pulling the results

xml_list <- list()

for (i in 1:nrow(urls)) {
  
  x <- NULL
  attempt <- 0
  
  while( is.null(x) && attempt <= 10 ) {
    attempt <- attempt + 1
    try(
      x <- read_xml(paste0("http://www.tabroom.com/api/tourn_published.mhtml?tourn_id=", urls$tourn_id[i]))
    )
    Sys.sleep(10)
  }
  
  xml_list[[i]] <- x

  # if(exists(xml_list[[i]])) {
  #   paste("success! completed", i)
  # } ## tell me if it worked
  
}

#save(xml_list, file = "tournaments_xml_updated.Rdata")

# extracting --------------------------------------------------------------

one_df_forlapply <- function(x) {
  one_df(x, words)
}

# remove label column function - because the "label" column made map break

remove_label <- function(x) {
  x %>% 
    xml_find_all(paste0("//", "LABEL")) %>% 
    xml_remove() 
}

## making a copy of the list to be safe

try2 <- xml_list

## removing the label

lapply(try2, remove_label)

season22 <- lapply(xml_list[-43], FUN = one_df_forlapply) ## removing D8, which is a problem for some reason. See "testing where the error happened" to explore

season222 <- do.call(rbind, lapply(season22, data.frame, stringsAsFactors=FALSE)) %>% as.tibble()

season222 %>% write_csv("season_2019.csv")





# testing for where the error happened ------------------------------------


for (i in 1:length(try3)){
  one_df(try3[[i]], words) %>% print()
}

length(try2)

one_df(try2[[47]], words) ## prob w/ 43


make_all(try2[[42]], words)


for (i in 4:length(words)) {
  extract_child(try2[[43]], words[i]) %>% print
}

extract_child(try2[[43]], words[7])

extract_child(try2[[43]], "JUDGE")

xml_object = try2[[43]]

try2[[42]] %>% 
  xml_find_all(paste0("//", "JUDGE")) %>% 
  map(xml_children) %>% 
  map_df(~map(setNames(xml_text(.), xml_name(.) %>% tolower), type.convert, as.is=TRUE)) # this is the line where the error happens. 
