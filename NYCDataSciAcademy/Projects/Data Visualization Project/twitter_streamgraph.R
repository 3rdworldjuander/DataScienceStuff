# Script for producing a streamgraph of tweet hashtags

# Load packages
library("readr")
library("dplyr")
library("lubridate")
library("streamgraph")
library("htmlwidgets")

# MY Read my tweets
# mytweets_df <- read_csv("MyTwitterDataset/tweets.csv") %>%
#        select(timestamp, text) %>%
#        mutate(text = tolower(text))

# NASDAQ Load dataset tweets
 tweets_df <- event %>%
   select(Timestamp, Symbols) %>%
   mutate(Symbols = tolower(Symbols))

 tweets_df$Symbols <- gsub("[[:alnum:]]+",'[[:alnum:]]+', as.vector(tweets_df[['Symbols']]))
 
 
# # Pick hashtags with regexp
        # Symbols_list <- regmatches(mytweets_df$text, gregexpr("#[[:alnum:]]+", mytweets_df$text))

        # NASDAQ Pick hashtags with regexp
 Symbols_list_S <- regmatches(tweets_df$Symbols, gregexpr("[[:alnum:]]+", tweets_df$Symbols))
 Symbols_list_S[[1]]


# # Create a new data_frame with (Timestamp, hashtag) -pairs
# new_hashtags_df <- data_frame()
#  for (i in which(sapply(Symbols_list, length) > 0)) {
#         new_hashtags_df <- bind_rows(new_hashtags_df, data_frame(Timestamp = mytweets_df$timestamp[i],
#                                                          hashtag = Symbols_list[[i]]))
#  }

# NASDAQ Create a new data_frame with (Timestamp, hashtag) -pairs
new_hashtags_df <- data_frame()
for (i in which(sapply(Symbols_list_S, length) > 0)) {
       new_hashtags_df <- bind_rows(new_hashtags_df, data_frame(Timestamp = tweets_df$Timestamp[i],
                                                   hashtag = Symbols_list_S[[i]]))
}

# Process data for plotting
new2_hashtags_df <- new_hashtags_df %>%
       # Pick top 20 hashtags
       filter(hashtag %in% names(sort(table(hashtag), decreasing=TRUE))[1:20]) %>%
       # Group by year-month (daily is too messy)
       # Need to add '-01' to make it a valid date for streamgraph
       mutate(yearmonth = paste0(format(as.Date(Timestamp), format="%Y-%m"), "-01")) %>%
       group_by(yearmonth, hashtag) %>%
       dplyr::summarise(value = n())




# install.packages('devtools')
# 
# devtools::install_github("hrbrmstr/streamgraph")
# 
# library(streamgraph)


# Create streamgraph
sg <- streamgraph(data = new2_hashtags_df, key = "hashtag", value = "value", date = "yearmonth",
                  offset = "zero", interpolate = "cardinal",
                  width = "700", height = "400") %>%
       sg_legend(TRUE, "hashtag: ") %>%
       sg_axis_x(tick_interval = 1, tick_units = "year", tick_format = "%Y")

# Save it for viewing in the blog post
# For some reason I can not save it to files/R/ direclty so need to use file.rename()
saveWidget(sg, file="twitter_streamgraph.html", selfcontained = TRUE)
file.rename("twitter_streamgraph.html", "files/R/twitter_streamgraph.html")
