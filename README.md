# Latent Semantic Scaling
Computer-aided dictionary making for text analysis

## How to install
devtools::install_github("koheiw/LSS")


## Sample code
```
rm(list = ls())
#devtools::install_github("kbenoit/quanteda", ref='dev_selectFeatures')
#devtools::install_github("koheiw/LSS")
#devtools::install_github("koheiw/Newsmap")
library(quanteda)
library(LSS)
library(Newsmap)

# Tokenize
docs <- readLines('data/uk_img/2009-2010.txt') # corpus of news stories on immigration
sents <- tokenize(docs, what='sentence', simplify = TRUE)
tokens <- tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)

# Feature selection (padding=TRUE is important for entry word selection)
tokens <- removeSpecialFeatures(tokens, marks=TRUE, numbers=TRUE)
tokens <- removeShortFeatures(tokens, padding=TRUE)
tokens <- selectFeatures2(tokens, stopwords('english'), 'remove', padding=TRUE)
tokens <- selectNames(tokens, 'remove', count_min = 1, padding=TRUE)
tokens <- toLower(tokens)

# Latent Semantic Analysis
mx <- dfm(removePadding(tokens2))
dim(mx) # 548,235 sentences and 143,235 types

mx2 <- decompose(mx)
dim(t(mx2)) # 300 components and 143,235 types

# Entryword selection
entwords <-selectEntrywords(tokens, '^(immigra|migra)', '^migraine') # exclude migraine headache

# Make dictionary
dict <- makeDictionary(mx2, head(entwords, 1000), seedwords('pos-neg')) # up to 1,000 entry words
head(dict, 20)

```
