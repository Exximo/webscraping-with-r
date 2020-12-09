library(tidyverse)
library(rvest)

# Seite definieren
hdm_studycheck <- read_html("https://www.studycheck.de/studium/online-medien/hdm-stuttgart-15774/bewertungen")


# Bewertungen mit CSS-Selektoren auswählen
bewertungen_roh <- 
  hdm_studycheck %>% 
        html_nodes(".item-text") %>%
        html_text() %>%
        as.character()

bewertungen_roh

# Daten in einen Dataframe umwandeln
bewertungen_df <- tibble(bewertungen_roh)

library(tidytext)

# Unnest Tokens
bewertungen <- 
  bewertungen_df %>%
  unnest_tokens(word, bewertungen_roh)

library(tm)
# Stopwörter entfernen 

stopwords <- tibble(word = stopwords('german'))

bewertungen <- 
  bewertungen %>%
  anti_join(stopwords, by ="word")


# Wörter zählen
count(bewertungen, 
      word, 
      sort = TRUE)


# Darstellung erzeugen
bewertungen %>%
  count(word, sort = TRUE) %>%
  filter(n >= 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() 

library(wordcloud)
# Wordcloud erzeugen 
bewertungen %>%
  count(word, sort=TRUE) %>%
  with(wordcloud(word, n, max.words = 2))


