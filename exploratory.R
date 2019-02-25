source("C:/Users/Cody/Dropbox/functions.R")
# looking at stuff --------------------------------------------------------

season <- read_csv("season_v2.csv")

# formatting df -----------------------------------------------------------

## making team identifiers
season %>% 
  select(school, schoolname, first, last) %>% unique

## making sure there are not duplicate names
season %>% 
  select(school, schoolname) %>% unique %>% 
  group_by(school) %>% 
  summarise(n = n_distinct(schoolname)) %>% 
  arrange(n %>% desc)

## creating identifiers

team_ids <- season %>% 
  select(school, schoolname, fullname) %>% unique %>% 
  mutate(team = paste0(schoolname, " ") %>% 
           paste0(fullname %>% substr(1,1), fullname %>% str_remove(".* & ") %>% substr(1,1)), 
         team_id = seq(1, nrow(.), 1))

## splitting down to only the team results, not individual ones, and merging correct team identifiers
season %>% 
  select(-c(entry_student_id, first, last)) %>% 
  unique %>% 
  left_join(team_ids)

season
season %>% View


## making dataframe more usable
season
newseason

s <- season %>% 
  select(code, judge, panel, side, win, tournname, eventname, timeslotname, rd_name, judgesperpanel, schoolname, entry_id, fullname) %>% 
  rename(team = code, tournament = tournname, division = eventname, round = timeslotname, round_number = rd_name) %>% 
  mutate(elim = case_when(judgesperpanel > 1 ~ "elim",
                          judgesperpanel == 1 ~ "prelim",
                          TRUE ~ "error"))

s %>% 
  group_by(schoolname, fullname) %>% 
  summarise(winpct = mean(win, na.rm = T)*100,
            rounds = n_distinct(panel)) %>% 
  filter(rounds > 20) %>% 
  arrange(winpct %>% desc) 

## win percent: about 50-50, out of 7500 rounds

s %>% 
  mutate(side = case_when(side == 1 ~ "Aff",
                          side == 2 ~ "Neg")) %>% 
  filter(side %>% is.na == F) %>% 
  group_by(side) %>% 
  summarise(win = mean(win, na.rm = T),
            rounds = n())


## looking at a person
season %>% 
  filter(fullname %>% str_detect("Kostelny") == T) %>% 
  select(judge, round, win, tournname) %>% 
  print(n = Inf)
