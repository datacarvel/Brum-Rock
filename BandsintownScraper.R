### Loading packages

library(rvest)
library(stringr)
library(RSelenium)
library(httr)
library(rlist)
library(tidyverse)
library(RColorBrewer)
library(colorRamps)
library(grDevices)
library(scales)

### Now from all concerts listing websites, Bandsintown is interesting as things are already broadly tagged as either Rock, Pop, Electronic, Hip-Hop, Metal, etc.

Bandsintown_Bham_Readed <- read_html("https://www.bandsintown.com/?place_id=ChIJc3FBGy2UcEgRmHnurvD-gco&genre_filter=Alternative")

### Base city for the event list: "h1 > span" (ex: Birmingham, which will include Leicester and Notthingham, for instance)
### Date: div.event-02e85563
### Artist: h2.event-5daafce9
### Venue: div.event-6891d84c
### City: div.event-c5863c62

Bham_Entries_basecity <- html_text(html_node(Bandsintown_Bham_Readed, css = "h1 > span"))
Bham_Entries_genretag <- html_text(html_node(Bandsintown_Bham_Readed, css = "div.clickable-aea3aac9"))
Bham_Entries_date <- html_text(html_nodes(Bandsintown_Bham_Readed, css = "div.event-02e85563"))
Bham_Entries_artist <- html_text(html_nodes(Bandsintown_Bham_Readed, css = "h2.event-5daafce9"))
Bham_Entries_venue <- html_text(html_nodes(Bandsintown_Bham_Readed, css = "div.event-6891d84c"))
Bham_Entries_venuecity <- html_text(html_nodes(Bandsintown_Bham_Readed, css = "div.event-c5863c62"))


Bham_DF <- data.frame(Bham_Entries_basecity, Bham_Entries_genretag, Bham_Entries_date, Bham_Entries_artist, Bham_Entries_venue, Bham_Entries_venuecity)

nrow(Bham_DF)

### Now the part that is a bit more new / intimidating to me, although now I know that the RSelenium package is super cool

# Starting the server that will start a browser

rD <- rsDriver()
remDr <- rD[["client"]]

# Opening our page, I think it will use your device's default browser

remDr$navigate("https://www.bandsintown.com/?place_id=ChIJc3FBGy2UcEgRmHnurvD-gco&genre_filter=Alternative")

# Finding the "View All" button and click it

ViewAllButton <- remDr$findElement(using = "css selector", "div.eventList-ad8780cc") # Locating the button we'll want our bot to click on
Sys.sleep(5) # <- This is of course to let the script take a break and load the page smoothly
ViewAllButton$clickElement() # Clicking on the View All button as identified with ViewAllButton
Sys.sleep(5) # Some loading time

# Scrolling to the bottom of the page so all results will load (because concerts are listed and generated as you scroll down the page)

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end")) # Here we are using the "End" key on your keyboard, ensuring we're going full downhill to the bottom of the page. Type selKeys in the console to see all keys.
Sys.sleep(10) # It can take a while to load because it will now generate absolutely all scheduled concerts

# Saving our now fully loaded webpage

htmlpage <- webElem$getPageSource()
Sys.sleep(5)

# Closing the browser

remDr$close()

# Stopping Selenium server

rD[["server"]]$stop() 

# Testing whether everything we just did makes any sense, note that I had to specify [[1]] so R understands I'm interested in the content of the item, not the list itself

read_html(htmlpage[[1]])
read_html("https://www.bandsintown.com/?place_id=ChIJc3FBGy2UcEgRmHnurvD-gco&genre_filter=Alternative")

# Results above are the same (except that one will have much more entries than the other)

### We can now test our scraper and hope it will give us more than 18 results, but we'll just write a more convenient function now

Bham_fullpage_test <- read_html(htmlpage[[1]])

SinglePageScraper <- function(x) {
  Entries_basecity <- html_text(html_node(x, css = "h1 > span"))
  Entries_genretag <- html_text(html_node(x, css = "div.clickable-aea3aac9"))
  Entries_date <- html_text(html_nodes(x, css = "div.event-02e85563"))
  Entries_artist <- html_text(html_nodes(x, css = "h2.event-5daafce9"))
  Entries_venue <- html_text(html_nodes(x, css = "div.event-6891d84c"))
  Entries_venuecity <- html_text(html_nodes(x, css = "div.event-c5863c62"))
  SinglePageDF <- data.frame(Entries_basecity, Entries_genretag, Entries_date, Entries_artist, Entries_venue, Entries_venuecity)
}

Bham_fullpage_tested <- SinglePageScraper(Bham_fullpage_test)

nrow(Bham_fullpage_tested)

### A success, it is

### So now we have to do this for every of the dozen cities, but also for every genre (cities * genres).

### So we need to create our list of URLs, each URL containing a city and a music genre

### Cities are represented by the long ID code Google Maps attributed to them, as Bandsintown uses their search engine. 
### Since there are only 12 cities, and I can only get them through looking manually at search results and valid them with my human eyes, I'll just go get them manually: https://developers.google.com/maps/documentation/javascript/examples/places-placeid-finder

LondonID <- "ChIJdd4hrwug2EcRmSrV3Vo6llI"
BirminghamID <- "ChIJc3FBGy2UcEgRmHnurvD-gco"
ManchesterID <- "ChIJ2_UmUkxNekgRqmv-BDgUvtk"
LeedsID <- "ChIJmb1k2ko-eUgRqdwTAv26rVE"
SheffieldID <- "ChIJFb7o-qkKeUgReLAGr_UnKD4"
GlasgowID <- "ChIJ685WIFYViEgRHlHvBbiD5nE"
EdinburghID <- "ChIJIyaYpQC4h0gRJxfnfHsU8mQ"
BristolID <- "ChIJYdizgWaDcUgRH9eaSy6y5I4"
LiverpoolID <- "ChIJt2BwZIrfekgRAW4XP28E3EI"
BradfordID <- "ChIJGRnC6XpfeUgRTxknMtjt4I8"
CardiffID <- "ChIJ9VPsNNQCbkgRDmeGZdsGNBQ"
BelfastID <- "ChIJO4rQ1_3_YEgRMcXH7ywWVy4"

Cities <- c("London", "Birmingham", "Manchester", "Leeds", "Sheffield", "Glasgow", "Edinburgh", "Bristol", "Liverpool", "Bradford", "Cardiff", "Belfast")

CitiesID <- c(LondonID, BirminghamID, ManchesterID, LeedsID, SheffieldID, GlasgowID, EdinburghID, BristolID, LiverpoolID, BradfordID, CardiffID, BelfastID)
names(CitiesID) <- Cities

### Now we have to do something similar with all genre tags available on Bandsintown, probably the easiest part of all this project so far

BIT_Home <- read_html("https://www.bandsintown.com/")
GenreTags <- html_text(html_nodes(BIT_Home, css = "div.option-a1bd709b"))
GenreTags <- GenreTags[-1] # Removing the "All Genres" genre, as it is not very helpful.

### One slight modification: I expect "R&B/Soul" and "Christian/Gospel" to have a different URL output due to special characters

match("R&B/Soul", GenreTags)
match("Christian/Gospel", GenreTags)

### Replacing them, simply by verifying on Bandsintown's site

GenreTags[c(3,14)] <- c("Christian%2FGospel", "R%26B%2FSoul")

### And now we have to create our URLs from those IDs and Tags

URLs_DF <- GenreTags

CityURLs <- c()

URL_Creater <- function(x) {
  City <- x
  CityLoop <- for (i in GenreTags) {
    CityURLs[i] <- str_c("https://www.bandsintown.com/?place_id=", City, "&genre_filter=", i, sep = "", collapse = NULL)
  }
  CityURLs <- cbind(city = names(x), CityURLs) 
}

### Now this should work:

AllCitiesURLs <- sapply(CitiesID, URL_Creater, USE.NAMES = TRUE)
AllCitiesURLs_DF <- as.data.frame.table(AllCitiesURLs)
colnames(AllCitiesURLs_DF) <- c("Unit", "Base city", "URL") # "Unit" is the name attributed to the extra column I never asked for but am ok with

## 192 URLs. Makes sense (12 cities * 16 genres)

### We now have our URL to scrape, a functional JS scraper, now we have to think about how our data should look like in the end

# URLs: AllCitiesURLs_DF$URL
# Scraper: SinglePageScraper

SuperPageSaver <- function(x) {
  
  rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
  #rD <- rsDriver()
  remDr <- rD[["client"]]
  
  remDr$navigate(x) # Opening our page, I think it will use the device's default browser
  print("So far so good!")
  Sys.sleep(10)
  ViewAllButton <- try(remDr$findElement(using = "css selector", "div.eventList-ad8780cc")) # Finding the "View All" button - I went to great lengths to consider cases when there is no URL... and then I met try()
  print("So far so good! #2")
  Sys.sleep(10) # <- This is of course to let the script take a break and load the page smoothly
  try(ViewAllButton$clickElement()) # Click the "View All" - if there is any! This is why it's between try(). If not, the script won't make a scene in the console and will just move on
  Sys.sleep(10)
  print("So far so good! #3")
  webElem <- remDr$findElement("css", "body") # Scrolling to the bottom of the page so all results will load
  webElem$sendKeysToElement(list(key = "end")) # Here we are using the "End" key on your keyboard. Type selKeys in the console to see them all. 
  Sys.sleep(10)
  
  StoredPage <- webElem$getPageSource() # Saving our now fully loaded webpage
  
  Sys.sleep(10)
  print("So far so good! #4")
  ReadedStoredPage <- read_html(StoredPage[[1]])
  ConcertsData <- try(SinglePageScraper(ReadedStoredPage)) # SinglePageScraper is a function we built earlier, before this one we are in
  
  remDr$close() # Closing the browser
  
  rD[["server"]]$stop() # Stopping Selenium server
  
  Sys.sleep(10)
  
  ConcertsData
  
}

#FirstExample <- SuperPageSaver(AllCitiesURLs_DF[56,3])

# Works! Now we'll have to eventually do it on all of our 192 URLs, but before going further, we'll just test it with like 2 URLs before unleashing that big beast

#SmallBeast <- data.frame(AllCitiesURLs_DF[9,3], AllCitiesURLs_DF[56,3], row.names = NULL, check.names = FALSE, check.rows = FALSE)
#SmallBeastComplete <- lapply(SmallBeast, SuperPageSaver)
#ConcertsUK_test <- list.rbind(SmallBeastComplete)

# Because I'm paranoid, I'll just test it again with a few more random URLs...

#MediumBeast <- data.frame(
 # AllCitiesURLs_DF[9,3], 
  #AllCitiesURLs_DF[56,3], 
  #AllCitiesURLs_DF[72,3], 
  #AllCitiesURLs_DF[99,3],
  #AllCitiesURLs_DF[101,3],
  #AllCitiesURLs_DF[191,3],
  #AllCitiesURLs_DF[2,3],
  #AllCitiesURLs_DF[34,3],
  #AllCitiesURLs_DF[150,3],
  #llCitiesURLs_DF[122,3],
  #AllCitiesURLs_DF[175,3],
  #row.names = NULL, check.names = FALSE, check.rows = FALSE
  #)

#MediumBeastComplete <- lapply(MediumBeast, SuperPageSaver)

# I divided the scraping operation in two, because some occasional random bugs may (and did) occur (http errors) and void the whole progress

MasterBeast1of4 <- lapply(MasterBeast[1:50], SuperPageSaver)

MasterBeast234of4 <- lapply(MasterBeast[51:192], SuperPageSaver)

#MasterBeast1of4_Test <- MasterBeast1of4
#MasterBeast234of4_Test <- MasterBeast234of4

# In a previous scraping operation, some city for some genre had no event at all, and in that case it simply stored the error message as a string value
# What follows checks whether this is still the case, considering that all my list items (data frames) have a length of 6 (columns)

ErrorDetector <- function(x) {
    length(x)
}

lapply(MasterBeast1of4, ErrorDetector)
lapply(MasterBeast234of4, ErrorDetector)

# I still have some doubt about it, but everything seems to have a length of 6, so there would be no case in which a city has no gig to offer for a certain genre
# I'm guessing that if we missed something here, it will show up in one way or another later, probably during the data vizzing part

### Unlisting everything by making them all part of the same data.frame

MasterBeast1of4_DF <- list.rbind(MasterBeast1of4)

MasterBeast234of4_DF <- list.rbind(MasterBeast234of4)

### Combining those 2 new data.frames

MasterBeast_DF <- rbind(MasterBeast1of4_DF, MasterBeast234of4_DF)

str(MasterBeast_DF)

glimpse(MasterBeast_DF)

### Maybe at this point we can remove the ", UK", from every city - with the stringr package

MasterBeast_DF$Entries_basecity <- str_replace(MasterBeast_DF$Entries_basecity, ", UK", "")

### There are also cities overlapping each other in the results, especially in the North, like Leeds, Manchester, Sheffield, etc.

### After many moderately successful attempts, I decided to eliminate any gig that isn't happening in a city's official jurisdiction.
### This way, Glasgow and Edinburgh, Bristol and Cardiff and all the cities in the North won't overlap each other, and London won't consider the quite-far-from-it Brighton.
### This is because Bandsinstown's coevered radius for a city is way too large.

MasterBeast_DF <- MasterBeast_DF %>%
  filter(str_detect(Entries_venuecity, Entries_basecity) == TRUE) ## Isolating the obvious matches - for instance, gigs for the "London" basecity in Bandsintown in a venue that is in "London", not Brighton or Southend-on-Sea

### Now for each city we want to calculate the proportion of metal-tagged concerts... or of all genre tags, while we're at it with all this data

### A function to look up a given city's proportion of concerts of a given genre

#CityGenreFilter <- function(x, y) {
 # xx <- filter(MasterBeast_Tbl2, Entries_basecity == x)
 # yy <- filter(MasterBeast_Tbl2, Entries_basecity == x, Entries_genretag == y)
#  nrow(yy) / nrow(xx)
#}

#CityGenreFilter("London", "Folk")

### A function to look up a given city's proportion of metal concerts

#CityMetalFilter <- function(x) {
#  xx <- filter(MasterBeast_Tbl2, Entries_basecity == x)
 # yy <- filter(MasterBeast_Tbl2, Entries_basecity == x, Entries_genretag == "Metal")
#  nrow(yy) / nrow(xx)
#}

#CityMetalFilter("Birmingham")

### Looking at the metal share of concerts in all of our 12 cities

#MetalCities <- sapply(Cities, CityMetalFilter, USE.NAMES = TRUE)



### Now doing for everything, ideally a tidy table where genre tags are rows and cities are columns

# First reversing our Christian/Gospel and RnB issue

GenreTags2 <- GenreTags

GenreTags2[c(3,14)] <- c("Christian/Gospel", "R&B/Soul")
  
Share <- c()

GenreShareFinder <- function(x) {
  City <- x
  for (i in GenreTags2) {
    xx <- filter(MasterBeast_DF, Entries_basecity == City)
    yy <- filter(MasterBeast_DF, Entries_basecity == City, Entries_genretag == i)
    Share[i] <- nrow(yy) / nrow(xx)
  }
  Share <- cbind(names(City), Share)
}

#MasterTest <- GenreShareFinder("London")
#MasterTestA <- GenreShareFinder("Birmingham")

# cbind(MasterTest, MasterTestA, deparse.level = 2)

### Now this should work:

CitiesGenresPercent <- sapply(Cities, GenreShareFinder, USE.NAMES = TRUE)

### No idea why the GenreShareFinder on one city keeps the row names (genres) but can't keep it when I use the same function with sapply.

rownames(CitiesGenresPercent) <- GenreTags2

### Just figured that this isn't exactly the right database format for the chart I want to do

CitiesGenresPercent2 <- data.frame("Genre" = rownames(CitiesGenresPercent), CitiesGenresPercent)
rownames(CitiesGenresPercent2) <- NULL

CitiesGenresPercent3 <- gather(CitiesGenresPercent2, Basecity, ShareOfConcert, -Genre)

CitiesGenresPercent4 <- CitiesGenresPercent3

CitiesGenresPercent4$Genre <- levels(droplevels(CitiesGenresPercent4$Genre))

### Adjusting colors so they are random rather than scaled - I just copied + pasted + edited what follows really

MyColor <- primary.colors(16, steps = 3, no.white = TRUE)

MyColor.index <- ifelse(seq(MyColor) %% 2, 
                    seq(MyColor), 
                    (seq(ceiling(length(MyColor)/2), length.out=length(MyColor)) %% length(MyColor)) + 1)
mixed <- MyColor[MyColor.index]

### Plotting it

library(scales)

ggplot(CitiesGenresPercent4, aes(x = Basecity, y = ShareOfConcert, fill = Genre)) + geom_bar(stat = "identity") + coord_flip() + scale_fill_manual(values = rep(mixed, length.out = nrow(CitiesGenresPercent4))) + theme(text = element_text(size = 20), panel.background = element_blank()) + scale_y_continuous(labels = scales::percent_format(accuracy = 1))



