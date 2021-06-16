# Dependencies
library(tidyverse)
library(rvest)
library(stringr)
library(xml2)
library(httr)

# Load the data
wikipedia <-read_html(
  "https://en.wikipedia.org/wiki/World_Health_Organization_ranking_of_health_systems_in_2000")

# What type of class?
class(wikipedia)

# List of objects that match this element
# This command allows us to look for ALL links
# Look for all anchor nodes with the href class
# // means "look through the document"
listOfANodes <- html_elements(wikipedia, xpath = '//a[@href]')

# Show the first link
listOfANodes[1]

# First element that matches that query
title <- html_element(wikipedia, xpath = "//title")
title

# Show the text that is within the "title" node
html_text(title)

# Find all tables
list_of_tables <- html_elements(wikipedia, xpath = "//table")
list_of_tables

# Find all table headers
list_table_headers <- html_elements(wikipedia, xpath = "//th")
list_table_headers
html_text(list_table_headers[3])

# EXTRACT A TABLE FROM A WEBPAGE ------------------------------------------

# HTML code for Wikipedia table
section_of_wikipedia <- html_node(wikipedia,
                                  xpath='//*[@id="mw-content-text"]/div/table') 

# Covert to table in R
mytable <- html_table(section_of_wikipedia)
head(mytable)


# HTML code for Wikipedia table
section_of_wikipedia2 <- html_node(wikipedia,
                                  xpath='//*[@class="wikitable sortable"]')
section_of_wikipedia2
html_table(section_of_wikipedia2)

# ANOTHER EXAMPLE ---------------------------------------------------------

## url for webpage
varycss <- read_html("http://varycss.org/groups.html")

## locate the table component of the html
groups <- html_node(varycss, xpath = '//table[1]')

## extract the table
mytable2 <- html_table(groups)

# GETTING INFORMATION FROM FORMS ------------------------------------------

# Import the data
firms <- read_csv("data/firms2.csv")[,1]
firms

# Blank vector
hq <- c()

# Download the HTML that contains the form
html <- read_html("http://www.google.com")

# Extract the form
search <- html_form(html)[[1]]

# Fill in the form to do the search
search <- search %>% 
  html_form_set(
  # Create the search query
  q = paste(firms[1], "bloomberg", sep = " "), 
  # Results are in English
  hl = "en"
  )

# Execute the search
resp <- html_form_submit(search, submit = "btnG")

# Retrieve the results in R
site <- read_html(resp)

# Show the HTML code---this is the Google page with the search term for the first company
html_element(site, "*")

# Locate all the links on the Google page
# Ran into an error here - will need to return to fix
# Conceptually, at least I know it is possible to search forms like Google
links <- html_elements(site,
                       xpath='//a[contains(@href, "bloomberg.com/profile")]')[1] %>%
  html_attr("href")
print(links)