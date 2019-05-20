library(rvest)
library(stringr)
library(tidyr)
library(tidyverse)

links <- read_csv("links.csv")
my_urls <- links$link

c <- data.frame()
for (i in 1:nrow(links)){ 
  data <- links[i, ]
  
  my_page <- read_html(data$link)

    b <- my_page %>% 
      html_nodes('.scrollable') %>%
      html_text() %>% 
      str_extract("[12][09][901][1234567890].[1234567890][1234567890].[1234567890][1234567890].") %>% 
      as.Date(format = "%Y.%m.%d.", tz = "CET") %>% 
      data_frame()
      
    
    
    a <- my_page %>% 
      html_nodes("[class='portlet-body']") %>%
      html_text() %>% 
      as.character() %>% 
      str_remove_all("\\s*\\([^\\)]+\\)") %>% 
      str_remove_all("\\\n") %>% 
      strsplit("Ülésnap") %>% 
      unlist() %>% 
      str_remove("FelszólalásFelszólalás(.*):") %>% 
      str_remove_all("/\\*\\*.*") %>% 
      data_frame() %>% 
      dplyr::rename(word = ".") %>% 
      tail(-1)
    
    c_i <- cbind(data$ciklus, b, a)
    
    colnames(c_i) <- c("ciklus", "datum", "word")
    
    c <- rbind(c, c_i)
}
c <- c %>% tail(-1)

library(stringi)
hu_stop_word <- read.csv(paste0(getwd(), "/data/stopwords-hu.txt"),
                         encoding = "UTF-8") %>% 
  dplyr::rename(word = a)


library(tidytext)
words <- c %>%
  unnest_tokens(word, word) %>% 
  anti_join(hu_stop_word) %>% 
  filter(!str_detect(word, "^a$")) %>% 
  filter(!str_detect(word, "egyébként")) %>% 
  filter(!str_detect(word, "tisztelt")) %>%
  filter(!str_detect(word, "hölgyeim")) %>%
  filter(!str_detect(word, "uraim")) %>%
  filter(!str_detect(word, "képviselőtárs.*")) %>%
  filter(!str_detect(word, "2015")) %>%
  filter(!str_detect(word, "2010")) %>%
  filter(!str_detect(word, "figyelmét")) %>%
  filter(!str_detect(word, "Tisztelt")) %>%
  filter(!str_detect(word, "magyar(.*)")) %>%
  filter(!str_detect(word, "98")) %>%
  filter(!str_detect(word, "2002")) %>%
  filter(!str_detect(word, "2001")) %>%
  filter(!str_detect(word, "képviselőtársam")) %>%
  filter(!str_detect(word, "képviselő")) %>%
  filter(!str_detect(word, "tisztelettel")) %>%
  filter(!str_detect(word, "megtisztelő")) %>%
  filter(!str_detect(word, "elnök")) %>%
  filter(!str_detect(word, "emberek")) %>%
  filter(!str_detect(word, "tudom")) %>%
  filter(!str_detect(word, "ház")) %>%
  filter(!str_detect(word, "képviselőtársam")) %>%
  filter(!str_detect(word, "szeretném")) %>%
  filter(!str_detect(word, "megtisztelő")) %>%
  filter(!str_detect(word, "politikai")) %>%
  filter(!str_detect(word, "ország")) %>%
  filter(!str_detect(word, "tenni")) %>%
  filter(!str_detect(word, "illeti")) %>%
  filter(!str_detect(word, "mondani")) %>%
  filter(!str_detect(word, "áll")) %>%
  filter(!str_detect(word, "megtisztel.*")) %>%
  filter(!str_detect(word, "figyelmüket")) %>%
  filter(!str_detect(word, "bennünket")) %>%
  filter(!str_detect(word, "tudjuk")) %>%
  filter(!str_detect(word, "k[eé]pvisel.*")) %>%
  filter(!str_detect(word, "var")) %>%
  filter(!str_detect(word, "fontos")) %>%
  filter(!str_detect(word, "dolog")) %>%
  filter(!str_detect(word, "következő")) %>%
  filter(!str_detect(word, "érdekében")) %>%
  filter(!str_detect(word, "és")) %>%
  mutate(word = ifelse(word == "magyarországon", "magyarország", word))



library(wordcloud)
words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 15))


positive_words <- read_csv("data/PrecoSenti/PrecoPos.txt", col_names = F) %>%
  mutate(sentiment=1)

negative_words <- read_csv("data/PrecoSenti/PrecoNeg.txt", col_names = F) %>%
  mutate(sentiment=-1)

hungarian_sentiment <- rbind(positive_words, negative_words) %>% 
  dplyr::rename(word = X1)


str(hungarian_sentiment)
word_sentiments <- words %>%
  inner_join(hungarian_sentiment)

word_sentiments %>%
  count(word, sentiment) %>% 
  mutate(word = reorder(word, n),
         relsent = n * sentiment) %>% 
  arrange(-n) %>% 
  head(20) %>% 
  ggplot(aes(word, n * sentiment, fill = sentiment)) + 
    geom_col(show.legend = FALSE) +
    labs(y = "Contribution to sentiment", x = NULL) + 
    coord_flip()

library(lubridate)




get.seasons <- function(dates, hemisphere = "N"){
  library(data.table)
  library(zoo)
  library(dplyr)
  
  years <- unique(year(dates))
  years <- c(min(years - 1), max(years + 1), years) %>% sort
  
  if(hemisphere == "N"){
    seasons <- c("winter", "spring", "summer", "fall")}else{
      seasons <- c("summer", "fall", "winter", "spring")}
  
  dt.dates <- bind_rows(
    data.table(date = as.Date(paste0(years, "-12-21")), init = seasons[1], type = "B"),# Summer in south hemisphere
    data.table(date = as.Date(paste0(years, "-3-21")), init = seasons[2], type = "B"), # Fall in south hemisphere
    data.table(date = as.Date(paste0(years, "-6-21")), init = seasons[3], type = "B"), # Winter in south hemisphere
    data.table(date = as.Date(paste0(years, "-9-23")), init = seasons[4], type = "B"), # Winter in south hemisphere
    data.table(date = dates, i = 1:(length(dates)), type = "A") # dates to compute
  )[order(date)] 
  
  dt.dates[, init := zoo::na.locf(init)] 
  
  return(dt.dates[type == "A"][order(i)]$init)
}



word_sentiments %>%
  group_by(ciklus, year = year(datum), season = get.seasons(datum, "N")) %>% 
  dplyr::summarize(score = sum(sentiment) / n()) %>% 
  ungroup() %>% 
  mutate(datum = paste0(year, "-", season)) %>% 
  select(-year) %>% 
  ggplot(aes(datum, score, fill = ciklus)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

  # facet_wrap(~method, ncol = 1, scales = "free_y")


word_sentiments %>%
  group_by(ciklus) %>% 
  dplyr::summarize(score = sum(sentiment) / n()) %>% 
  ungroup() %>%   
  ggplot(aes(ciklus, score, fill = score)) +
  geom_col(show.legend = FALSE)
# facet_wrap(~method, ncol = 1, scales = "free_y")

season("2014-12-01")


word_count <- words %>%
  count(ciklus, word, sort = TRUE)

words_theme <- word_count %>%
  bind_tf_idf(word, ciklus, n)

word_count <- word_count %>% filter(!str_detect(word, "képvisel(.*)")) %>% 
  filter(!str_detect(word, "tenni")) %>% 
  filter(!str_detect(word, "mondani")) %>% 
  filter(!str_detect(word, "gmo"))


words_theme <- word_count %>%
  bind_tf_idf(word, ciklus, n) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(ciklus, desc(tf_idf))

head(words_theme)


words_theme %>% 
  group_by(ciklus) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>% 
  data.frame() %>%
  mutate(word = reorder(word, -tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = ciklus)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(. ~ ciklus, ncol = 2, scales = "free") +
    labs(x = NULL, y = "tf-idf") +
    coord_flip()






### TOKENIZING BY BIGRAM

library(dplyr)
library(tidytext)

c_bigrams <- c %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2)


library(tidyr)

bigrams_separated <- c_bigrams %>%
  group_by(ciklus, datum) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  ungroup()

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(ciklus, bigram) %>%
  bind_tf_idf(bigram, ciklus, n) %>%
  arrange(desc(tf_idf))





## Bigram tf_idf
str(bigram_tf_idf)
bigram_tf_idf %>% 
  ungroup() %>% 
  group_by(ciklus) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>% 
  data.frame() %>%
  mutate(bigram = reorder(bigram, -tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = ciklus)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(. ~ ciklus, ncol = 3, scales = "free") +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


#### Bigrams


library(ggraph)
library(igraph)

bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()


set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))


ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
