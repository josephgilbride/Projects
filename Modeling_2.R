
title: "R Markdown - Frameworks 2 project"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Define dataset

```{r}
dat = AssistanceListings_DataGov_PUBLIC_CURRENT
```

#Clean Data

This step requires the 'textclean' package.  We are then going to replace all instances of extra text in the file.

```{r}
library(textclean)
```

#Define Tokens and Split data

```{r}

#this list of tokens will expand on itself later on in our markdown

tokens = list("list","act","description","awarded","lump","other","isApplicable")

idrows = dat[,0:2]
rawrows = dat[,3:30]
goodrows = dat[,30:38]
```

#Clean data
```{r}

rawrows = apply(rawrows,2,function(x)add_comma_space(x))
rawrows = apply(rawrows,2,function(x)strip(x,char.keep = '"',digit.remove = FALSE, apostrophe.remove = FALSE, lower.case = FALSE))
rawrows = apply(rawrows,2,function(x)replace_tokens(x, tokens)) 
rawrows = gsub('" ',"",rawrows)
rawrows = apply(rawrows,2,function(x)add_missing_endmark(x))

dat = cbind(idrows,rawrows,goodrows)

```

#Text Processing

We will now use functions that process documents based on tokens. Each variable will be indexed, so that cross-variable analyses are possible, using individual words.  Each variable can be analyzed using these two functions: 

```{r}
Clean_String <- function(string){
    temp <- tolower(string)
    temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    temp <- stringr::str_split(temp, " ")[[1]]
    indexes <- which(temp == "")
    if(length(indexes) > 0){
      temp <- temp[-indexes]
    } 
    return(temp)
}

Clean_Text_Block <- function(text){
    indexes <- which(text == "")
    if (length(indexes) > 0) {
        text <- text[-indexes]
    }
    if (length(text) == 0) {
        cat("There was no text in this document! \n")
        to_return <- list(num_tokens = 0, 
		                     unique_tokens = 0, 
							 text = "")
    } else {
        clean_text <- NULL
        for (i in 1:length(text)) {
            clean_text <- c(clean_text, Clean_String(text[i]))
        }
        num_tok <- length(clean_text)
        num_uniq <- length(unique(clean_text))
        to_return <- list(num_tokens = num_tok, 
		                     unique_tokens = num_uniq, 
							 text = clean_text)
    }
	
    return(to_return)
}
# Source: http://www.mjdenny.com/Text_Processing_In_R.html # 

clean_data = Clean_Text_Block(rawrows$`Authorization (040)`)

```

We are now ready to run a cross-variable analyses using the cataloged words in each column.  In this instance, the dataset of rawrows contains most of the text-rich content that we will continue to use later on...
