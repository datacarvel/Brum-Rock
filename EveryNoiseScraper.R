
### Loading packages (not sure I need all of those tbh)

library(httr)
library(httpuv)
library(rvest)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyverse)

library(showtext)
library(rlist)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(colorRamps)
library(grDevices)
library(scales)

library(extrafont)
library(extrafontdb)

### Doing it with one city only for testing, how about Brum?

### Grabbing the html page from EveryNoise

BirminghamReaded <- read_html("http://everynoise.com/everyplace.cgi?root=Birmingham%20GB&scope=all")

### Grabbing the genres specifically, CSS was found by inspecting the items on the webpage

BirminghamList <- html_text(html_node(BirminghamReaded, css = "div.note > div"))

### A wild set of "/n" appears! So we'll get rid of them, but first we'll use them to separate our elements

BirminghamGenresL <- strsplit(BirminghamList, "\n")

### Now it's a list, but it will probably be more useful as a vector

BirminghamGenresV <- unlist(BirminghamGenresL)

### Because the first few characters are "\n", which is meaningless, the first item will be a blank space, so we can remove it easily

BirminghamGenresV <- BirminghamGenresV[-1]

### It works! We got ourselves a vector of music genres that are especially popular in Birmingham. Now we'll create a function to do this for other big or medium cities of interest

### As a function now, to apply it to any other city

Cities <- c("London", "Birmingham", "Manchester", "Leeds", "Sheffield", "Glasgow", "Edinburgh", "Bristol", "Liverpool", "Bradford", "Cardiff", "Belfast")

ParticularLikesFUN <- function(x) {
  City <- x
  City_EN_URL <- str_c("http://everynoise.com/everyplace.cgi?root=", City, "%20GB&scope=all", sep = "", collapse = NULL)
  City_Readed <- read_html(City_EN_URL)
  City_Genres <- html_text(html_node(City_Readed, css = "div.note > div"))
  City_GenreL <- strsplit(City_Genres, "\n")
  City_GenreV <- unlist(City_GenreL)
  City_GenreV <- City_GenreV[-1]
  City_GenreV <- City_GenreV[1:20]
  factor(City_GenreV)
  print(City_GenreV)
}

### Note that I added an extra "City_GenreV <- City_GenreV[1:20]" in the function because 20 music genres per city is enough, especially since some cities don't have as much according to Spotify

### Now we're applying our function above to all of our cities within our vector

ParticularLikesAllCities <- lapply(Cities, ParticularLikesFUN)

### Giving each list item the city name it deserves

names(ParticularLikesAllCities) <- Cities

### Renaming it in a simpler way

CitiesLikes <- ParticularLikesAllCities

### Might be easier to make it a data.frame

CitiesLikesDF <- as.data.frame(CitiesLikes)
CitiesLikesDF_Jan14_2019 <- CitiesLikesDF

### Colors 


CitiesLikesDF_Jan14_2019 ### A data.frame, where each column is a city - I know there is 240 cells but 92 unique string values

CitiesLikesTidy <- CitiesLikesDF_Jan14_2019 %>% gather(City, Genre)

CitiesLikesTidyRank <- cbind(CitiesLikesTidy, Rank = 1:20)

library(scales)

            
### As labels, color-stroke

CitiesLikesTidyRank %>% ggplot(aes(x = City, y = Rank)) + 
  geom_label(aes(label = Genre, col = Genre), size = 3.5, family = "Arial", label.r = unit(0.3, "lines")) + 
  theme(legend.position = "none", axis.title.x = element_blank(), panel.background = element_blank(), axis.text.y = element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 18, family = "Franklin Gothic Demi")) +
  scale_y_reverse()

### As labels, color-filled

CitiesLikesTidyRank %>% ggplot(aes(x = City, y = Rank)) + 
  geom_label(aes(label = Genre, fill = Genre), color = "white", family = "Arial", size = 3.5, label.padding = unit(0.5, "lines")) + 
  theme(legend.position = "none", axis.title.x = element_blank(), panel.background = element_blank(), axis.text.y = element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 18, family = "Arial")) +
  scale_y_reverse()

### As text with colors

ggg <- CitiesLikesTidyRank %>% ggplot(aes(x = City, y = Rank)) + 
  geom_text(aes(label = Genre, col = Genre), size = 3.5) + 
  theme(legend.position = "none", axis.title.x = element_blank(), panel.background = element_blank(), axis.text.y = element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 18)) +
  scale_y_reverse()

ggsave(file="name1.svg", units = "cm", width = 50, height = 30, path = "C:/Users/moukm/Google Drive/Birmingham - BCU/Cours/ADM7001 Major project/Brum metal, then and now/")



### old stuff, fonts and all

font_import()

fonts()
fonttable()
loadfonts()

ggsave("ggplot_garamond.pdf", ggg, width=20, height=4)

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.26/bin/gswin64c.exe")

embed_fonts("ggplot_garamond.pdf", outfile="ggplot_garamond_embed.pdf")