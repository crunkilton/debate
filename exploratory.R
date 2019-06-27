source("C:/Users/Cody/Dropbox/functions.R")
setwd("C:/Users/Cody/Dropbox/debate_r")
setwd("/Users/codycrunkilton/Dropbox/debate_r")

library(ggraph)
library(tidygraph)
library(tidyverse)

# looking at stuff --------------------------------------------------------

season <- read_csv("season_2019.csv") %>% rename(judge_id = judge)
season
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
           paste0(fullname %>% substr(1,2), fullname %>% str_remove(".* & ") %>% substr(1,2)), 
         team_id = seq(1, nrow(.), 1)) 


## creating judge names

judge_ids <- season %>% 
  select(j_first, j_last, j_id) %>% unique %>% 
  mutate(judge = paste(j_first, j_last))

## splitting down to only the team results, not individual ones, and merging correct team identifiers
season2 <- season %>% 
  select(-c(entry_student_id, first, last)) %>% 
  left_join(team_ids) %>% 
  left_join(judge_ids)

season2

## making dataframe more usable

s <- season2 %>% 
  select(tournname, eventname, school, team, team_id, judge, judge_id, panel, side, win, timeslotname, rd_name, judgesperpanel, schoolname, fullname) %>% 
  rename(tournament = tournname, division = eventname, round = timeslotname, round_number = rd_name) %>% 
  unique
  # mutate(elim = case_when(judgesperpanel > 1 ~ "elim",
  #                         judgesperpanel == 1 ~ "prelim",
  #                         TRUE ~ "error"))

## number of rounds and win percent
s %>% 
  filter(side %>% is.na == F) %>% 
  group_by(team) %>% 
  summarise(winpct = mean(win, na.rm = T),
            rounds = n_distinct(panel)) %>% 
  arrange(rounds %>% desc, winpct %>% desc)
  
  ## 16k rounds this year

## win percent: about 50-50, out of 7500 rounds

s %>% 
  mutate(side = case_when(side == 1 ~ "Aff",
                          side == 2 ~ "Neg")) %>% 
  filter(side %>% is.na == F & judge != "NA NA") %>% 
  group_by(side) %>% 
  summarise(win = mean(win, na.rm = T),
            rounds = n_distinct(panel))


## looking at a person
season %>% 
  filter(fullname %>% str_detect("Kostelny") == T) %>% 
  select(j_first, j_last, round, win, tournname) %>% 
  print(n = Inf)

## most times judged by someone

s %>% 
  filter(judge != "NA NA") %>% 
  group_by(team, judge) %>% 
  summarise(ct = n(),
            winpct = mean(win)) %>% 
  arrange(ct %>% desc) %>% 
  print(n = 30)


s %>% 
  filter(judge == "David Cram Helwich") %>% 
  group_by(team) %>% 
  summarise(win = mean(win),
            rounds = n()) %>% 
  arrange(rounds %>% desc) 

# Networks ----------------------------------------------------------------

library(tidygraph)
library(igraph)


d <- s %>% 
  filter(team %>% str_detect("BYE") == F &
           judge %>% str_detect("NA NA") == F)

## number of judges and teams
d %>% 
  summarise(
    n_judges = n_distinct(judge),
    n_teams = n_distinct(team)
  )



## Who has been judged by whom?
edgelist <- d %>% 
 # filter(judge %>% str_detect("Crunkilton") == T) %>% 
 # filter(judge != -1) %>% 
  group_by(team, judge) %>% 
  summarise(weight = n()) %>% 
  arrange(weight %>% desc)


## making the graph
# type 0 = debaters, 1 = judges
g <- graph_from_data_frame(edgelist, directed = F)
# V(g)$type <- ifelse(V(g)$name %in% edgelist$judge == T, 1, 0) # this doesn't work for some reason

V(g)$type <- V(g)$name %in% edgelist$judge

plot(g, vertex.label.cex = 1, vertex.color = V(g)$type %>% as.factor, layout = layout.bipartite) # not pretty

cluster_algos <- function(g) {
  walktrap = cluster_walktrap(g)
  louvain = cluster_louvain(g)
  infomap = cluster_infomap(g)
  fast_greedy = cluster_fast_greedy(g)
  
  V(g)$walktrap = walktrap$membership
  V(g)$louvain = louvain$membership
  V(g)$infomap = infomap$membership
  V(g)$fast_greedy = fast_greedy$membership
  
  return(g)
}


## Tidygraph

tidy_algos <- function(g) {

  t <- g %>% as_tbl_graph()
  
  cluster_nodes <- t %>% 
    activate(nodes) %>% 
    as_tibble() %>%
    gather("cluster_algorithm", "community", -c(name, type))
  
  return(cluster_nodes)
}

cluster_nodes %>% 
  group_by(cluster_algorithm, community) %>% 
  summarise(count = n())

cluster_nodes %>% 
  group_by(cluster_algorithm) %>% 
  summarise(count = n_distinct(community))



## not pretty graph that is too big

ggraph(t) +
  geom_node_point(size = .01) +
  geom_edge_link(width = .0001) +
  g #+
#  geom_node_text(aes(label = name), size = .001)


# looking at subgraph - only 3+ connections --------------------------------------------------------

el <- edgelist %>% 
  filter(weight > 2)

e <- graph_from_data_frame(el, directed = FALSE)

V(e)$type <- V(e)$name %in% edgelist$judge


pic <- ggraph(e, layout = 'bipartite') + 
  geom_node_point(aes(color = type), size = .5) +
  geom_edge_link(aes(width = weight)) +
  scale_edge_width(range = c(.1, 1))+ # control size
  geom_node_text(aes(label = name), size = 1) +
  theme_graph() 

png('/Users/codycrunkilton/Desktop/g1.png', width = 100000, height = 100000)

pic

dev.off()

## cluster try2

e2 <- cluster_algos(e)

e3 <- tidy_algos(e2)

e3 %>% 
  group_by(cluster_algorithm, community) %>% 
  summarise(count = n())

e3 %>% 
  group_by(cluster_algorithm) %>% 
  summarise(count = n_distinct(community))

e3 %>% 
  arrange(cluster_algorithm, community) %>% 
  filter(type == TRUE) %>% 
  #filter(cluster_algorithm == "louvain") %>% 
  print(n = 300)
  
  
e3 %>% 
  arrange(cluster_algorithm %>% desc, community) %>% 
  group_by(cluster_algorithm, community) %>% 
  summarise(ct = n()) %>% 
  arrange(ct %>% desc)
