library(rvest)
library(tidyverse)

# set wd
setwd("C:/Users/ss1216/Box/Other")

# get updated list of past wordle words, to eliminate
past_wordle = read_html("http://www.rockpapershotgun.com/wordle-past-answers")

# parse html (list of words starts with ul class = inline)
html_wordle = past_wordle %>% html_nodes(".inline")
past_wordle = tolower(str_replace_all(
              str_replace_all(
                      str_replace_all(
                          str_split(as.character(html_wordle[1]),"</li>\n<li>")[[1]],
                           "[<].*[>]", ""
                          ), "[\n]", ""
              ), "[[:punct:]]", "" 
            )
)
# save up to date list of previously used words
write.table(past_wordle, paste0("wordle_list_",
                       str_replace_all(Sys.Date(),
                                       "-","_"),".csv"))

## get list of possible wordle words (does not correspond with full dictionary)
# https://gist.github.com/scholtes/94f3c0303ba6a7768b47583aff36654d
valid = read.table(url("https://gist.githubusercontent.com/scholtes/94f3c0303ba6a7768b47583aff36654d/raw/d9cddf5e16140df9e14f19c2de76a0ef36fd2748/wordle-La.txt"))
#write.table(valid, paste0("valid_wordle_list_",
#                       str_replace_all(Sys.Date(),
#                                       "-","_"),".csv"))




# filter words that have already been used
#word5_unused = word5 %>% filter(!(word %in% pp))
word5_unused = valid[!(valid %in% past_wordle)] %>% rename(word = V1)

greenz = function(greens, dat){
  for (i in 1:nrow(greens)){
    dat = dat %>% filter(substr(word, greens$pos[i],greens$pos[i]) ==
                         greens$word[i])
  }
  return(dat)
}

yllwz = function(yllwz, dat){
  for (i in 1:nrow(yllwz)){
    dat = dat %>% filter(substr(word, yllwz$pos[i],yllwz$pos[i]) !=
                           yllwz$word[i] &
                           grepl(yllwz$word[i], word) ) 
  }

  return(dat)
}


blckz = function(blcks, dat){
  for (i in blcks$word){
    dat = dat %>% filter(str_count(word, i) == blcks$numvalid[blcks$word == i] )
    
  }
 return(dat) 
}


# colors = g , y, or b
# function that takes guess as 5-letter string, 
# colors correspond to a vector of 5 characters equal to either 'g', 'y', or 'b' in the order they appear after a wordle guess 
# dat is the vector of possible words, users should save the output of 
# the function this after each subsequent guess to maintain the list of possible words as more information is gained
# function will return a list of possible words left after using the information gained at each guess
wordle = function(guess, colors, dat){
  guess0 = data.frame(word = str_split(guess, "")[[1]],
                 cols = colors,
                 pos = 1:5) %>%
          group_by(word) %>%
          mutate(numreps = n(),
                 numgrn = sum(cols == "g"),
                 numblk = sum(cols == "b"),
                 numyllw = sum(cols == "y")) %>%
          ungroup()
  
  if(nrow(guess0 %>% filter(numblk == numreps)) > 0){
    # letters to outright remove - none in the word
    remove = guess0 %>% filter(numblk == numreps) %>%
                        distinct(word) %>% 
                        pull()
    
    # filter words with the letters no found in word
    words1 = dat %>% 
                  filter(!grepl(paste(remove, collapse = "|"),
                            word))
  } else{
    words1 = dat
  }
  
  
  # filter any greens and yellows
  if (sum(guess0$numgrn) != 0){
      words2 = greenz(guess0 %>% filter(cols == "g"), words1)
  } else {
      words2 = words1
  }

  if(sum(guess0$numyllw) != 0) {
    words3 = yllwz(guess0 %>% filter(cols == "y"), words2)
  } else {
    words3 = words2
  }
  
  # filter any black and green/yellow combinations, e.g certain # of letters in word
  dropdupes = guess0 %>% filter(numblk < numreps & 
                           numblk > 0) %>% 
                         group_by(word) %>% 
                         summarise(numreps = unique(numreps),
                                  # number that are yellow or green
                                   numvalid = unique(numgrn) + unique(numyllw),
                                  numblk = unique(numblk))
                   
  if (nrow(dropdupes) > 0){
      words4 = blckz(dropdupes, words3)  

  } else{
    words4 = words3
  }
  
  # number of unique letters that appear in word, may want to guess words with lots of unique letters
  numuniq = apply(words4, 1, function(x) sum(!!str_count(x, letters)))
  return(words4 %>% mutate(numuniq = numuniq) %>% arrange(desc(numuniq)))
  
}


######### EXAMPLE ###########
firstguess = wordle("stare", c("b", "y", "b", "y", "g"), word5_unused)
secguess = wordle("brute", c("b", "g", "b", "g", "g"), firstguess)
# word was write


