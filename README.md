
# Latent Semantic Scaling

**NOTICE:** This R package is renamed from **LSS** to **LSX** for CRAN
submission.

In quantitative text analysis, the cost to train supervised machine
learning models tend to be very high when the corpus is large. LSS is a
semisupervised document scaling method that I developed to perform large
scale analysis at low cost. Taking user-provided *seed words* as weak
supervision, it estimates polarity of words in the corpus by latent
semantic analysis and locates documents on a unidimensional scale
(e.g. sentiment).

I used LSS for large scale analysis of media content in several research
projects:

  - Kohei Watanabe, 2017. “[Measuring News Bias: Russia’s Official News
    Agency ITAR-TASS’s Coverage of the Ukraine
    Crisis](http://journals.sagepub.com/eprint/TBc9miIc89njZvY3gyAt/full)”,
    *European Journal Communication*.
  - Kohei Watanabe, 2017. “[The spread of the Kremlin’s narratives by a
    western news agency during the Ukraine
    crisis](http://www.tandfonline.com/eprint/h2IHsz2YKce6uJeeCmcd/full)”,
    *Journal of International Communication*.
  - Tomila Lankina and Kohei Watanabe. 2017. ["Russian Spring’ or
    ‘Spring Betrayal’? The Media as a Mirror of Putin’s Evolving
    Strategy in
    Ukraine](http://www.tandfonline.com/eprint/tWik7KDfsZv8C2KeNkI5/full)",
    *Europe-Asia Studies*.

Please read my paper for the algorithm and methodology:

  - Kohei Watanabe, forthcoming, “[Latent Semantic Scaling: A
    Semisupervised Text Analysis Technique for New Domains and
    Languages](https://blog.koheiw.net/wp-content/uploads/2020/10/LSS-09-PREPRINT.pdf)”,
    *Communication Methods and Measures*.

## How to install

``` r
devtools::install_github("koheiw/LSX")
```

## How to use

LSS estimates semantic similarity of words based on their surrounding
contexts, so a LSS model should be trained on data where the text unit
is sentence. It is also affected by noises in data such as function
words and punctuation marks, so they should also be removed. It requires
larger corpus of texts (5000 or more documents) to accurately estimate
semantic proximity. The [sample corpus](https://bit.ly/2GZwLcN) contains
10,000 Guardian news articles from 2016.

### Fit a LSS model

``` r
require(quanteda)
require(LSX) # changed from LSS to LSX
```

``` r
corp <- readRDS(url("https://bit.ly/2GZwLcN", "rb"))
```

``` r
toks_sent <- corp %>% 
    corpus_reshape("sentences") %>% 
    tokens(remove_punct = TRUE) %>% 
    tokens_remove(stopwords("en"), padding = TRUE)
dfmt_sent <- toks_sent %>% 
    dfm(remove = "") %>% 
    dfm_select("^\\p{L}+$", valuetype = "regex", min_nchar = 2) %>% 
    dfm_trim(min_termfreq = 5)

eco <- char_context(toks_sent, "econom*", p = 0.05)
lss <- textmodel_lss(dfmt_sent, as.seedwords(data_dictionary_sentiment),
                     terms = eco, k = 300, cache = TRUE)
```

    ## Reading cache file: lss_cache/svds_819846d785c15a7a.RDS

### Sentiment seed words

Seed words are 14 generic sentiment words.

``` r
data_dictionary_sentiment
```

    ## Dictionary object with 2 key entries.
    ## - [positive]:
    ##   - good, nice, excellent, positive, fortunate, correct, superior
    ## - [negative]:
    ##   - bad, nasty, poor, negative, unfortunate, wrong, inferior

### Economic sentiment words

Economic words are weighted in terms of sentiment based on the proximity
to seed words.

``` r
head(coef(lss), 20) # most positive words
```

    ##        good       shape      driver      either    positive   challenge 
    ##  0.10086678  0.08100301  0.07652619  0.07610887  0.07287992  0.06801746 
    ##      expert  continuing     several sustainable      monday   expecting 
    ##  0.06578380  0.06577131  0.06497246  0.06489614  0.06481823  0.06459612 
    ##      remain    emerging      decent   continued      shares   candidate 
    ##  0.06327885  0.06173428  0.06158674  0.05958519  0.05804870  0.05799429 
    ## challenging          de 
    ##  0.05735492  0.05725742

``` r
tail(coef(lss), 20) # most negative words
```

    ##     compared implications        basic    americans       happen         debt 
    ##  -0.07677553  -0.07767036  -0.07848986  -0.07850078  -0.07874437  -0.07896652 
    ## policymakers    suggested    something     interest unemployment    borrowing 
    ##  -0.07970222  -0.08267444  -0.08549609  -0.08631343  -0.08879022  -0.09109017 
    ##         hike         poor         rate          rba        rates          cut 
    ##  -0.09224650  -0.09393369  -0.09598675  -0.09672486  -0.09754047  -0.11047689 
    ##     negative        wrong 
    ##  -0.12472812  -0.13293973

This plot shows that frequent words (“said”, “people”, “also”) are
neutral while less frequent words such as “borrowing”, “unemployment”,
“emerging” and “efficient” are either negative or positive.

``` r
textplot_terms(lss, 
               highlighted = c("said", "people", "also",
                               "borrowing", "unemployment",
                               "emerging", "efficient"))
```

![](images/words-1.png)<!-- -->

## Result of analysis

In the plots, circles indicate sentiment of individual news articles and
lines are their local average (solid line) with a confidence band
(dotted lines). According to the plot, economic sentiment in the
Guardian news stories became negative from February to April, but it
become more positive in April. As the referendum approaches, the
newspaper’s sentiment became less stable, although it became close to
neutral (overall mean) on the day of voting (broken line).

``` r
dfmt <- dfm(corp)

# predict sentiment scores
pred <- as.data.frame(predict(lss, se.fit = TRUE, newdata = dfmt))
pred$date <- docvars(dfmt, "date")

# smooth LSS scores
pred_sm <- smooth_lss(pred, from = as.Date("2016-01-01"), to = as.Date("2016-12-31"))

# plot trend
plot(pred$date, pred$fit, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-0.5, 0.5),
     xlab = "Time", ylab = "Negative vs. positive", main = "Economic sentiment in the Guardian")
lines(pred_sm$date, pred_sm$fit, type = "l")
lines(pred_sm$date, pred_sm$fit + pred_sm$se.fit * 2, type = "l", lty = 3)
lines(pred_sm$date, pred_sm$fit - pred_sm$se.fit * 2, type = "l", lty = 3)
abline(h = 0, v = as.Date("2016-06-23"), lty = c(1, 2))
text(as.Date("2016-06-23"), 0.4, "Brexit referendum")
```

![](images/trend-1.png)<!-- -->
