library(stringr)
library(tidyr)
library(dplyr)
library(readr)

raw_edges <- stream %>%
        filter(!is.na(`Original Tweet User Name`) | !is.na(`User Mentions`))  %>%
        select(Nickname, RTs, `Is a RT`, `Original Tweet User Name`, `User Mentions`)

#write.csv(raw_edges, file = "raw_edges.csv")
#raw_edges <- read_csv("raw_edges.csv")      

#test <- raw_edges %>% filter(`Is a RT` == F)
men_edges <- raw_edges %>% filter(`Is a RT` == F)
rt_edges <- raw_edges %>% filter(`Is a RT` == T)

menMax <- max(str_count(men_edges$`User Mentions`, "@"))
men_edges <- men_edges %>% 
       separate(`User Mentions`, c(as.character(1:menMax)),
                ",", extra = "merge", fill = "left")

#gathering all users mentioned into one column
men_edges <- men_edges %>% gather(from_col, to, 5:15 )

#cleaning NAs and removing from_col
men_edges <- men_edges %>% filter(is.na(to) == FALSE) %>% select(from = Nickname, to)

#remove @ signs
men_edges$to <- gsub('@','', men_edges$to)


#fixing retweet df
rt_edges <- rt_edges %>% select(from = `Original Tweet User Name`, to =  Nickname)

#remove @ signs
rt_edges$from <- gsub('@','', rt_edges$from)

new_edges <- rbind(men_edges, rt_edges)

#aggregating to column
new_edges <- aggregate(rep(1, nrow(new_edges)), by = list(from = new_edges$from, to = new_edges$to), sum)

new_edges <- new_edges[order(new_edges$from, new_edges$to),]

colnames(new_edges)[3] <- "weight"
rownames(new_edges) <- NULL

#save rds
saveRDS(new_edges, file = "new_edges.RDS")
