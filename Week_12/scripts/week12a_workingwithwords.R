### This is a script for Spring 2021 Computer Modeling covering getting help for R
### Created by: Emily Wilson
### Created on: 04/19/21
############################################################################################

# Load libraries ###########################################################################
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)


# Work with Words! ##########################################################################

# Stringr

## Manipulation
paste("High temp", "Low pH") # pasting words together (can do without quotes for pasting columns)

paste("High temp", "Low pH", sep = "-") # paste them together with a dash separator

paste0("High temp", "Low pH") # pastes without space

## Working with vectors
shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)

two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")

## Finding out how long a string is
shapes # vector of shapes
str_length(shapes) # how many letters are in each word?


## Extracting specific characters
seq_data<-c("ATCCCGTC")
str_sub(seq_data, start = 2, end = 4) # extract the 2nd to 4th AA

## Modify strings
str_sub(seq_data, start = 3, end = 3) <- "A" # add an A in the 3rd position
seq_data

## Duplicate patterns in strings
str_dup(seq_data, times = c(2, 3)) # times is the number of times to duplicate each string

## Remove white space
badtreatments<-c("High", " High", "High ", "Low", "Low")
badtreatments
str_trim(badtreatments) # this removes white space on both sides
str_trim(badtreatments, side = "left") # this removes white space on left side 

## Add white space to either side
str_pad(badtreatments, 5, side = "right") # add a white space to the right side after the 5th character (the 5 tells you how many characters you want in the string)
str_pad(badtreatments, 5, side = "right", pad = "1") # add a 1 to the right side after the 5th character

## Change cases
x<-"I love R!"

str_to_upper(x) #uppercase
str_to_lower(x) # lowercase
str_to_title(x) # title case

## Pattern matching
data<-c("AAA", "TATA", "CTAG", "GCTT")

str_view(data, pattern = "A") # find all the strings with an A
str_detect(data, pattern = "A") # detect a specific pattern
str_detect(data, pattern = "AT")
str_locate(data, pattern = "AT") # locate a pattern



# Regex: regular expressions

## Metacharacters . \ | ( ) [ { $ * + ?

vals<-c("a.b", "b.c","c.d")

str_replace(vals, "\\.", " ") #string, pattern, replace (\\ is escape the metacharacter)

vals<-c("a.b.c", "b.c.d","c.d.e")
str_replace(vals, "\\.", " ") #string, pattern, replace: only replaces the first instance 
str_replace_all(vals, "\\.", " ") # the _all replaces everything

## Sequences

val2<-c("test 123", "test 456", "test")
str_subset(val2, "\\d") # subset to only keep strings with digits

str_count(val2, "[aeiou]") # count number of lowercase vowels
str_count(val2, "[0-9]") # count any digit


# Quantifiers
#| symbol| Meaning|
  #|---|----|  
  #|^	|Beginning of String|  
  #|$	|End of String|
  #|\n	|Newline|
  #|+	|One or More of Previous|
  #|*	|Zero or More of Previous|
  #|?	|Zero or One of Previous|
  #|{5}|	Exactly 5 of Previous|
  #|{2, 5}|	Between 2 and 5 or Previous|
 #|{2, }	|More than 2 of Previous|

## Example: finding phone numbers
strings<-c("550-153-7578",
           "banana",
           "435.114.7586",
           "home: 672-442-6739")

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})" # set up a regex that finds all strings that contain a phone number

str_detect(strings, phone) # Which strings contain phone numbers?

test<-str_subset(strings, phone) # subset only the strings with phone numbers
test

test_clean <- test %>% 
  str_replace_all(pattern = "\\.", replacement = "-") %>% # replace . with -
  str_replace_all(pattern = "[a-z]", replacement = "") %>%  # remove any letters from that one response
  str_replace_all(pattern = "\\:",  replacement = "") %>% # remove that weird colon
  str_trim() # get rid of any extra white space

test_clean


# Tidytext

head(austen_books())
tail(austen_books())

original_books <- austen_books() %>% # get all of Jane Austen's books
  group_by(book) %>%
  mutate(line = row_number(), # find every line
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", # count the chapters (starts with the word chapter followed by a digit or roman numeral); ^ means starting with
                                                 ignore_case = TRUE)))) %>% #ignore lower or uppercase
  ungroup() # ungroup it so we have a dataframe again

tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text) # add a column named word, with the input as the text column

head(get_stopwords()) #see an example of all the stopwords

cleaned_books <- tidy_books %>%
  anti_join(get_stopwords()) # dataframe without the stopwords

cleaned_books %>%
  count(word, sort = TRUE)  # count most common words across books
# could use group_by() to get most common for each book

## Sentiment Analysis
sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>% # only keep pos or negative words
  count(word, sentiment, sort = TRUE) # count them

sent_word_counts %>% # plt results from sentiment analysis
  filter(n > 150) %>% # take only if there are over 150 instances of it
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% # add a column where if the word is negative make the count negative
  mutate(word = reorder(word, n)) %>% # sort it so it gows from largest to smallest
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

## Make a wordcloud

words<-cleaned_books %>%
  count(word) %>% # count all the words
  arrange(desc(n))%>% # sort the words
  slice(1:100) #take the top 100

wordcloud2(words, shape = 'triangle', size=0.3) # make a wordcloud out of the top 100 words
