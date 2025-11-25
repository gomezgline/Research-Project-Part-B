# This R file downloads the novel 'Around the World in 80 Days'
# from Project Gutenberg, does basic cleaning of the book,
# and saves the data in a csv file

# Author: Ann Gline Gomez
# Last Modified: 21 July 2025

# Loading the required  libraries

pacman::p_load(tidyverse, tidytext, gutenbergr)

# Loading the Book from Project Gutenberg

book<-gutenberg_download(103,mirror="http://mirror.csclub.uwaterloo.ca/gutenberg/")

# Cleaning

#Remove Header and footer

#first 53 rows are header so we have to remove it

book_text_only <- book %>%
  tail(nrow(book)-53)

#no footer to remove

#Separating the Book by its Chapters

# Find the chapter number for each line

book_chapters <- book_text_only %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,
                                     regex("^chapter [\\divxlc]",
                                           ignore_case=TRUE))))%>%
  ungroup()


# Removing the empty lines

book_chapters_clean <- book_chapters %>%
  filter(!(text == ""))

# Removing the chapter headings

chapter <- 0 #creating variable for the chapter number

rows <- c() # creating empty string of rows to be removed

for (i in 1:nrow(book_chapters_clean)){ # iterating every row of 
  # the data
  
  temp_chapter <- book_chapters_clean[i,4] # Find chapter number of particular row
  if (!(temp_chapter ==chapter)){#checking if chapter number has changed
    
    chapter <- temp_chapter # setting current chapter number
    
    rows <- c(rows, i) # Add row to those to be removed
  }
  else if (temp_chapter == chapter) { # if chapter number not changed
    if(str_detect(book_chapters_clean[i,2], "^([[:upper:]]|[[:digit:]]|[[:punct:]]|[[:blank:]])+$")){
      rows <- c(rows, i) # Add row to those to be removed
    }
  }
}
rows <- unique(rows) # rows to be removed

# remove the rows
book_chapters_clean_2 <- book_chapters_clean %>%
  filter(!row_number() %in% rows)

# Combining the text for each chapter

book_chapters_clean3 <- book_chapters_clean_2 %>%
  select(chapter, text) %>%
  group_by(chapter) %>%
  mutate(text=paste0(text, collapse= " ")) %>%
  slice(1) %>%
  ungroup()


## Saving the cleaned Data as CSV

write.csv(book_chapters_clean3, "around_world_80_days.csv")

