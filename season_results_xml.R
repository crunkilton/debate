## Cody Crunkilton
## Misc Projects - Debate
## Tabroom XML extraction

## note: functions sourced in. 


setwd("C:/Users/Cody/Dropbox/debate_r")
source("C:/Users/Cody/Dropbox/functions.R")

library(xml2)
library(tidyverse)


# downloading XML ---------------------------------------------------------


load("tournaments_xml.Rdata") ## load the data directly


urls <- read_csv("debateresults_2018_2019.csv") %>% 
  mutate(tourn_id = id %>% str_remove(".*tourn_id=") %>% 
           str_remove("&result_id=.*")) %>% 
  arrange(major %>% desc)


## the function to grab all the xml results

xml_list <- list()

for (i in 1:nrow(urls)) {
  
  x <- NULL
  attempt <- 0
  
  while( is.null(x) && attempt <= 3 ) {
    attempt <- attempt + 1
    Sys.sleep(3)
    try(
      x <- read_xml(paste0("http://www.tabroom.com/api/tourn_published.mhtml?tourn_id=", urls$tourn_id[i]))
    )
  } 
  
  xml_list[[i]] <- x
  
}
xml_list

save(xml_list, file = "tournaments_xml.Rdata")



# extracting --------------------------------------------------------------

one_df_forlapply <- function(x) {
  one_df(x, words)
}

# remove label column function

remove_label <- function(x) {
  x %>% 
    xml_find_all(paste0("//", "LABEL")) %>% 
    xml_remove() 
}

try2 <- xml_list

lapply(try2, remove_label)

season2 <- lapply(try2, FUN = one_df_forlapply) %>% 
  bind_rows()


season %>% write_csv("season_v2.csv")
