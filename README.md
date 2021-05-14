# wordRG()

wordRG() is a simple R function for extracting abstracts on ResearchGate and generating personal research word cloud.  

I built it for practice when learning text mining and, of course, for fun.

Why not Google Scholar?

- The abstracts in Google Scholar are often abridged versions with '...' ending.
- I can't access this website stably given my location.

Want to analyze the information of citations and the network of co-authors?

- see R package [scholar](https://cran.r-project.org/web/packages/scholar/) (Yu et al., 2021) which can extract citation data from Google Scholar.

## How to use this function

Here, I will show a quick implementation of this function.  

1. Copy the source code of the function and paste it in R / Rstudio

2. Find your id in ResearchGate

3. (Define stopwd, replacewd, wordFreq if necessary)

4. Run the function and get the figure

   ```R
   example (wait for update)
   ```

   



You can also modify the source code based on your needs. For example,

- reshape the word cloud based on the logo of your lab
- manually add the abstracts that are not in ReseachGate
- ...

## Source code of the function

```R
if(!require(rvest)) install.packages(pkgs="rvest",repos="http://cran.r-project.org")
if(!require(XML)) install.packages(pkgs="XML",repos="http://cran.r-project.org")
if(!require(dplyr)) install.packages(pkgs="dplyr",repos="http://cran.r-project.org")
if(!require(tidytext)) install.packages(pkgs="tidytext",repos="http://cran.r-project.org")
if(!require(wordcloud)) install.packages(pkgs="wordcloud",repos="http://cran.r-project.org")
if(!require(tm)) install.packages(pkgs="tm",repos="http://cran.r-project.org")

library(rvest)
library(XML)
library(dplyr)
library(tidytext)
library(wordcloud)
library(stringr)
library(tm)

wordRG <- function(rg_id, stopwd = NULL, replacewd = NULL, wordFreq = NULL){
  
  ## get the WebLink of all the publications  ############################
  rg_url = paste0("https://researchgate.net/profile/", rg_id, "/research")
  content <- read_html(rg_url)
  div_url = content %>% html_nodes('div a') %>% html_attr("href")
  
  div_url_items = NULL
  itemi = 1
  tag = "https://www.researchgate.net/publication/"
  for (i in 1:length(div_url)){
    if(!is.na(div_url[i])){
      if(substr(div_url[i], 1, nchar(tag)) == tag){
        
        div_url_items[itemi] = div_url[i]
        itemi = itemi + 1
      }
    }
  }
  
  div_url_items = div_url_items[seq(1,length(div_url_items),2)]
  
  
  ## get the abstracts of all the publications  ############################
  
  all_abstracts = rep("a", length(div_url_items))
  for (i in 1:length(div_url_items)){
    paper_url = div_url_items[i]
    
    paper_html = read_html(paper_url)
    
    # check whether there is an abstract
    text_all = paper_html %>% 
      html_nodes("div div") %>% html_text() 
    is_ab = which(text_all=="Abstract" | text_all=="Abstract and Figures")

    if(length(is_ab)!=0){

      # text in the abstract and figures
      text_abfig = paper_html %>% 
        html_nodes("div div.nova-c-card__body--spacing-inherit") %>% html_text() 
      
      # text in the figure (if exist)
      text_more = paper_html %>% 
        html_nodes("div div.nova-e-expandable-text__container") %>% html_text() 
      
      # delete the text in the figure (if exist)
      if(length(text_more)>0){ 
        # get the start location of the text of figures
        len_fig_text1 = nchar(text_more[1])
        for (chari in 1:nchar(text_abfig[1])){
          if(str_sub(text_abfig[1], chari, chari+len_fig_text1-1) == text_more[1]){
            loc_figtext = chari
          }
        }
        # delete it 
        abstract = str_sub(text_abfig[1], 1, (loc_figtext-1))
      }else{
        abstract = text_abfig[1]
      } 
      all_abstracts[i] = abstract 
    }    
    
  }
  
  # delete the publications with no abstract information in ResearchGate
  loc_de = which(all_abstracts=="a")
  cat(paste0())
  all_ab_selec = all_abstracts[-loc_de]
  
  # tidy the text data
  text_df <- tibble(line = 1:length(all_ab_selec),  text = removeNumbers(all_ab_selec))
  
  # replace replacewd
  # wait for update
  
  # cast paragraphs into words
  text_word = text_df %>%
    unnest_tokens(word, text)
  
  if(is.null(myStopWords)){
    myStopWords <- c("study", "studies", "analysis", "approaches", "approach", "results", "research", "procedure")
    # I just pick up some words with little information in my abstracts.
    # This part can be updated by analyzing your abstracts and extract words with low tf_idf
  }
  
  # word cloud
  text_forcloud = text_word %>%
    filter(!word %in% myStopWords)  %>%
    filter(!word %in% stop_words$word)  %>%
    filter(nchar(word)>1)  %>%
    count(word) %>%
    mutate(word = reorder(word, n)) 
  
  
  if(is.null(wordFreq)){
    wordFreq = median(text_forcloud$n) + 1
  }
  
  wordcloud(text_forcloud$word, text_forcloud$n, random.color=FALSE, random.order=FALSE, color=brewer.pal(8, "Dark2"), min.freq=wordFreq, scale=c(3, 1))
  
  
}

```




Update 2021-05-15

Lijin Zhang
