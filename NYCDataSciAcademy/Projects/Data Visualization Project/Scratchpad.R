install.packages('readr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('data.table')


#FOR SELECTING RELEVANT COLUMNS ONLY
select(`User Name` , Nickname, Bio, Latitude, Longitude, Country, `Place (as appears on Bio)` , Followers, Following )



anyDuplicated(stream$`Tweet Id`)
todump <- duplicated(stream$`Tweet Id`)
as.data.frame(todump)
sum(duplicated(stream$`Tweet Id`))
anyDuplicated(stream$`Tweet Id`)
unique(stream$`Tweet Id`)
length(unique(stream$`Tweet Id`))
sum(duplicated(stream$`Tweet Id`))
sum(duplicated(stream))

#FIND TWEET ID OCCURENCE
n_occur <- data.frame(table(stream$`Tweet Id`))

#MOST # OF OCCURENCE
max(n_occur$Freq)

#FIND TWEET ID WITH OCCURENCE ==21
n_occur %>% filter(Freq == 21)
# Tweet ID 736669096096337920

#EXAMINE TWEET ID 736669096096337920
test <- stream %>% filter(`Tweet Id` == 736669096096337920)
View(test)

test2 <- stream %>% filter(`Tweet Id` == 731998310786072448)
View(test2)

duplicated(c(1,1,1,2,1,4,3,5,4,2))
#get list of unique users
u_Nick_all <- stream %>% filter(duplicated(Nickname) == FALSE) %>% 
       select(`User Name` , Nickname, Bio, Latitude, Longitude, Country, `Place (as appears on Bio)` , Followers, Following )
#remove zero followers
u_Nick_flwd <- u_Nick_all %>% filter(!is.na(Followers))
#users without followers
nrow(u_Nick_all) - nrow(u_Nick_flwd)
#lowest #followers
min(u_Nick_flwd$Followers)


(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))
r1
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
x2
names(x2)
names(x2) <- letters[1:11]
r2 <- rank(x2) # ties are averaged
r2
install.packages('data.table')
library(data.table)

#Getting fractional rank
IndegRank <- frank(u_Nick_flwd, -Followers, ties.method = 'average', na.last = TRUE)
length(IndegRank)

#ADD INDEGREE RANK COL
Indegranked <- u_Nick_flwd %>% 
       mutate(IndegRank)

##GETTING RETWEET RANK
max(stream$RTs, na.rm = T)
min(stream$RTs, na.rm = T)

#Get original tweets with RTs > 0
u_RT <- stream %>% filter(`Is a RT` == FALSE & !is.na(RTs))
nrow(u_RT)
##104849

#Get fractional rank
RTRank <- frank(u_RT, -RTs, ties.method = 'average')
length(RTRank)

#ADD RTRANK COL
RTranked <- u_RT %>% 
       mutate(RTRank = frank(-RTs, ties.method = 'average'))

##BUGGY GROUP BY
RTtotalPerNick <-0
retw_all <- stream %>% 
       filter(`Is a RT` == FALSE & !is.na(RTs))  %>% 
       filter(duplicated(`Tweet Id`) == FALSE)  %>% 
       group_by(Nickname) %>% 
       mutate(RTtotalPerNick = sum(RTs))


#GET DATA FRAME OF TWEETS WITH MENTIONS BUT NOT RETWEET
u_Mention <- stream %>% filter(`Is a RT` == FALSE & !is.na(`User Mentions`)) %>% select(`User Mentions`)
dim(u_Mention)
#64133
View(u_Mention)

#Convert to vector
mention_vec <- gsub('@','', as.vector(u_Mention[['User Mentions']]))
mention_vec[4]
class(mention_vec)
gsub('@', '', mention_vec[4])

strsplit(mention_vec[1:4], ',')[[1]]


x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
x
class(x)
length(x)
# split x on the letter e
unlist(strsplit(x, "e"))

# split all usernames
uMention_vec <- unlist(strsplit(mention_vec, ','))
uMention_vec
length(uMention_vec)
uMention_vec[1:2]

as.data.frame(table(uMention_vec))

R.Version()

#VennDiagram

library(VennDiagram)


grid.newpage()
draw.triple.venn(area1 = 22, area2 = 20, area3 = 13, n12 = 11, n23 = 4, n13 = 5, 
                 n123 = 1, category = c("Dog People", "Cat People", "Lizard People"), 
                 lty = "blank", 
                 fill = c("skyblue", "pink1", "mediumorchid"))

venn.diagram(
       xt,
       euler.d = TRUE,
       scaled = TRUE,
       filename = "top100Venn.tiff",
       lty = "blank",
       cex = 1,
       cat.cex = 1,
       fill = c("skyblue", "pink1", "mediumorchid")
)


x <- c('abc', 'def', 'efg', 'hij')
x
xt <- merge(sample(x, 20, replace = TRUE), as.integer(1:20))
xt
class(xt)

names(xt) <- c('Nickname', 'RTs')
head(xt)
class(xt)

xt

aggregate(xt$RTs, by= list(xt$Nickname), FUN = sum)

#CHECKING TO SEE WHY TOP INDEG HAVE 0 RT
see <- stream %>% filter(Nickname == 'cnnbrk' & RTs > 0)# & `Is a RT` == FALSE)  #%>%
filter(duplicated(`Tweet Id`) == FALSE) %>%
       select(Nickname, RTs)


#SPEARMAN CORRELATION
cor.test(a, b, method="spearman")

names(indegRanked)
names(mentionRanked)

class(indegRanked)
class(mentionRanked)
class(mentionRanked)

merge(as.data.frame(indegRanked), mentionRanked, by = 'Nickname')

c.test <- right_join(right_join(as.data.frame(indegRanked), mentionRanked, by = 'Nickname'), retwRanked, by = 'Nickname')

cor.test(c.test$IndegRank, c.test$menRank, method = "spearman")
cor.test(c.test$retwRank, c.test$menRank, method = "spearman")
cor.test(c.test$retwRank, c.test$IndegRank, method = "spearman")
cor.test(c.test$IndegRank[1:1000], c.test$menRank[1:1000], method = "spearman")

test.c <- as.data.frame(cbind(c.test$IndegRank, c.test$menRank,c.test$retwRank))
class(test.c)
names(test.c) <- c('Indeg', 'Men', 'RT')
dim(test.c)[1] * .1

#!is.na(RT) = 10462
#!is.na(mention) = 18754

st <- head(test.c, n = 100L)

test.sorted <- test.c %>% arrange(Men)

sts <- head(test.c %>% arrange(Indeg), n = 10000L)

cor(sts, method = 'spearman')

##wordcloud
library(wordcloud)

c(letters, LETTERS, 0:9)


filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)
class(text)
text
VectorSource(text)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))
class(docs)
inspect(docs)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
m
v <- sort(rowSums(m),decreasing=TRUE)
v
d <- data.frame(word = names(v),freq=v)
d

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

duration(36360)

dur <- duration(hours = 10, minutes = 6)
am(dur)
as.numeric(dur, "hours")
as.numeric(dur, "minutes")

library(lubridate)
hms(36360)
Sys.getlocale("LC_TIME")

class(paste(dedupeStream$Date[3], dedupeStream$Hour[3]))

strptime(stream$Hour[3])strptime(stream$Hour[[3]], format = "%H:%M")

library(lubridate)



f <- seconds_to_period(stream$Hour[[3]])
                  
minute(f)


format(x, "%H:%M:%S")
format(dedupeStream$Hour[[3]], "%H:%M:%S")


###network

#Create a network with three edges
m<-matrix(0,3,3)
m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
g<-network(m)
get.edges(g,1,neighborhood="out")
get.edgeIDs(g,1,neighborhood="in")

##SEPARATE
df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df
df %>% separate(x, c(as.character(1:2)))
df2 <- data.frame(x = c("x: 123", "y: error: 7"))
df2
df2 %>% separate(x, c("key", "value"), ": ", extra = "merge")
