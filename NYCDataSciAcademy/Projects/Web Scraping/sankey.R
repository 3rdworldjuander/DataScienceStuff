library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(googleVis)


df <- read_csv("forEDA_moredeets.csv")
#dropping index remnants from python
df <- df[,c(-1,-2)]
#creating year column
df$release_year <- substr(df$release_date, 1,4)
#splitting chord progressions
df <- df %>% separate(cprog, into = paste("C", 1:4, sep = "_"), remove = FALSE)
#removing 6.4.1.5, 4.2.6.3
df <- df %>% filter(cprog != c("6.4.1.5", "4.2.6.3"))

#adding spaces to 2nd, 3rd, and 4th col
#to workaround sankey callback error
df$C_1 <- as.character(df$C_1)
df$C_2 <- paste(df$C_2," ",sep="")
df$C_3 <- paste(df$C_3,"  ",sep="")
df$C_4 <- paste(df$C_4,"   ",sep="")

#write_csv(df, "OamarGianan/forShiny.csv")

d1 <- df %>% select(C_1, C_2)
names(d1) <- c("from", 'to')
d1 <- d1 %>% group_by(from, to) %>% summarise(n=n())


d2 <- df %>% select(C_2, C_3)
names(d2) <- c("from", 'to')
d2 <- d2 %>% group_by(from, to) %>% summarise(n=n())

d3 <- df %>% select(C_3, C_4)
names(d3) <- c("from", 'to')
d3 <- d3 %>% group_by(from, to) %>% summarise(n=n())

d.all <- rbind(d1, d2, d3)
d.all <- as.data.frame(d.all)
str(d.all)
d.plot <- gvisSankey(d.all, from="from", to="to", weight = "n",
                     options=list(
                       width=1000, height=500,
                       sankey="{link: {color: { fill: '#d799ae' } },
                         node: { width: 15, 
                                color: { fill: '#a61d4c' },
                                label: { fontName: 'Garamond',
                                         fontSize: 20,
                                         bold: true} }}"))
plot(d.plot)
