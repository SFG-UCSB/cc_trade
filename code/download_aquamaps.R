
# Setup
################################################################################

# Packages
library(XML)
library(RSelenium)
library(stringr)

# Launch Selenium jar
# Java must be installed on your computer (Java 8)
# You need the "selenium-stand-alone-server" and "chrome-driver" (3.0.1)
# Double-click the jar file yourself before running

# This code requires (and is working with) the following:
# Java 8: https://java.com/en/download/mac_download.jsp
# Selenium Stand Alone Server 3.0.1: http://www.seleniumhq.org/download/
# Chromedriver 2.27: https://sites.google.com/a/chromium.org/chromedriver/


# Webscrape function
################################################################################

# For testing
if(F){
  species <- "Gadus morhua"
  savedir <- "data/aquamaps"
  download_aquamaps(species, savedir)
}

# Function to webscrape
download_aquamaps <- function(species, savedir){
  
  # Break scientific name in genus and species
  nwords <- length(unlist(strsplit(species, split=" ")))
  gen <- unlist(strsplit(species, split=" "))[[1]]
  spp <- unlist(strsplit(species, split=" "))[[2:nwords]]
  
  # Setup Selenium browser
  remDr <- remoteDriver(remoteServerAddr = "localhost", 
                        port = 4444, browserName = "chrome")
  
  # Open browser
  remDr$open()
  remDr$getStatus()
  
  # Navigate to AquaMaps site
  remDr$navigate("http://www.aquamaps.org/search.php")
  
  # Setup object to hold old windows
  # This remote driver opens lots of new tabs and I have to keep track of
  # which ones I've already visited
  oldWins <- c()
  
  # Id prompts for taxa info
  gen_prompt <- remDr$findElement(using = "id", "acGenus")
  spp_prompt <- remDr$findElement(using = "id", "acSpecies")
  
  # Enter taxa info into prompt
  gen_prompt$sendKeysToElement(list(gen))
  spp_prompt$sendKeysToElement(list(spp))
  
  # Hit search button
  search_button <- remDr$findElement(using="xpath", "//input[@tabindex='6']")
  search_button$clickElement()
  
  # After hitting search, it sometimes leaps to the distribution page and opens a new tab
  nWins <- length(remDr$getWindowHandles())
  if(nWins>1){
    oldWins <- c(oldWins, remDr$getCurrentWindowHandle()[[1]])
    allWins <- unlist(remDr$getWindowHandles())
    newWin <- allWins[!(allWins %in% oldWins)]
    remDr$switchToWindow(newWin)
  }
  Sys.sleep(2)
  
  
  # After hitting search, it sometimes leaps to a list of potential names
  # Assume (with good reason) the first link in the table is the "accepted name"
  # When it does this, it doesn't open a new tab, it navigates within the already open tab
  current.url <- remDr$getCurrentUrl()[[1]]
  if(grepl("ScientificNameSearchList", current.url)==T){
    # Id best species (third link in all links)
    all_links <- remDr$findElements(using = 'css selector', "a")
    spp_link <- all_links[[4]]
    spp_link$clickElement()
    # Opens a new tab but Selenium is still looking inside the old tab
    oldWins <- c(oldWins, remDr$getCurrentWindowHandle()[[1]])
    allWins <- unlist(remDr$getWindowHandles())
    newWin <- allWins[!(allWins %in% oldWins)]
    remDr$switchToWindow(newWin)
  }
  Sys.sleep(2)
  
  # After hitting search, it sometimes leaps to a list of potential maps
  # Assume (with good reason) that the first map is the reviewed map
  current.url <- remDr$getCurrentUrl()[[1]]
  if(grepl("receive", current.url)==F){
    # Id best map (occurs first)
    best_map <- remDr$findElement(using="css", "img")
    best_map$clickElement()
    # Opens a new tab but Selenium is still looking inside the old tab
    oldWins <- c(oldWins, remDr$getCurrentWindowHandle()[[1]])
    allWins <- unlist(remDr$getWindowHandles())
    newWin <- allWins[!(allWins %in% oldWins)]
    remDr$switchToWindow(newWin)
  }
  Sys.sleep(4)
  
  # Hit "Download native range data" link
  # download_link <- remDr$findElement(using = "css selector", '[href*="javascript:open_downl()"]')
  # remDr$executeScript("arguments[0].click();", list(download_link))
  download_link <- remDr$findElement(using = "link text", 'csv format')
  download_link$clickElement()
  
  # Opens a new tab but Selenium is still looking inside the old tab
  oldWins <- c(oldWins, remDr$getCurrentWindowHandle()[[1]])
  allWins <- unlist(remDr$getWindowHandles())
  newWin <- allWins[!(allWins %in% oldWins)]
  remDr$switchToWindow(newWin)
  Sys.sleep(2)
  
  # Select "Complete Dataset" radio button
  download_option <- remDr$findElement(using="css", "input[value='all']")
  download_option$clickElement()
  
  # Press "Submit" button
  submit_button <- remDr$findElement(using="css", "input[value='Submit']")
  submit_button$clickElement()
  
  # Press "Download" link
  csv_link <- remDr$findElement(using = "link text", "-Download-")
  csv_name <- unlist(csv_link$getElementAttribute("href"))
  csv_name <- gsub("https://www.aquamaps.org/CSV/", "", csv_name)
  csv_link$clickElement()
  Sys.sleep(8)
  
  # Close driver
  remDr$close()
  remDr$quit()
  
  # Move downloaded file from DOWNLOADS to specified folder
  outfile <- paste0(gsub(" ", "_", species), ".csv")
  file.rename(file.path("~/Downloads/", csv_name), file.path(savedir, outfile)) # Make default download directory flexible
  
}


