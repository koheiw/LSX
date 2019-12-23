
# Latent Semantic Scaling

In machine learning, the cost for users to train a model and the control
they have over its outputs usually correlate: supervised models are high
in control but high in cost; unsupervised models are low in cost but low
in control. However, social social scientists have to perform analysis
based on existing theories and concepts with very limited computational,
financial and human resources.

LSS is a semi-supervised document scaling model developed by Kohei
Watanabe to perform large scale analysis of textual data by keeping the
balance between the cost and control for [his PhD
thesis](http://etheses.lse.ac.uk/3658/). Taking pre-defined *seed words*
as weak supervision, it estimates word parameters in a latent semantic
space.

This model has been used for large scale analysis of media content in
recent research projects:

  - Kohei Watanabe, 2017. “[Measuring News Bias: Russia’s Official News
    Agency ITAR-TASS’s Coverage of the Ukraine
    Crisis](http://journals.sagepub.com/eprint/TBc9miIc89njZvY3gyAt/full)”,
    *European Journal Communication*
  - Kohei Watanabe, 2017. “[The spread of the Kremlin’s narratives by a
    western news agency during the Ukraine
    crisis](http://www.tandfonline.com/eprint/h2IHsz2YKce6uJeeCmcd/full)”,
    *Journal of International Communication*
  - Tomila Lankina and Kohei Watanabe. 2017. ["Russian Spring’ or
    ‘Spring Betrayal’? The Media as a Mirror of Putin’s Evolving
    Strategy in
    Ukraine](http://www.tandfonline.com/eprint/tWik7KDfsZv8C2KeNkI5/full)",
    *Europe-Asia Studies*

## How to install

``` r
devtools::install_github("koheiw/LSS")
```

## How to use

LSS estimates semantic similarity of words based on their surrounding
contexts, so a LSS model should be trained on data where the text unit
is sentence. It is also affected by noises in data such as function
words and punctuation marks, so they should also be removed. It requires
larger corpus of texts (5000 or more documents) to accurately estimate
semantic proximity. The [sample
corpus](https://www.dropbox.com/s/kfhdoifes7z7t6j/data_corpus_guardian2016-10k.RDS?dl=1)
contains 10,000 Guardian news articles from 2016.

### Fit a LSS model

``` r
require(quanteda)
require(LSS)
```

``` r
corp <- readRDS("/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds")

toks_sent <- corp %>% 
    corpus_reshape("sentences") %>% 
    tokens(remove_punct = TRUE) %>% 
    tokens_remove(stopwords(), padding = TRUE)
dfmt_sent <- toks_sent %>% 
    dfm(remove = "") %>% 
    dfm_select("^\\p{L}+$", valuetype = "regex", min_nchar = 2) %>% 
    dfm_trim(min_termfreq = 5)

eco <- char_keyness(toks_sent, "econom*", p = 0.05)
tmod_lss <- textmodel_lss(dfmt_sent, as.seedwords(data_dictionary_sentiment),
                          feature = eco, cache = TRUE)
```

    ## Reading cache file: lss_cache/svds_a588a327e03cb074.RDS

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

### Econimic sentiment words

Economic words are weighted in terms of sentiment based on the proximity
to seed
    words.

``` r
head(coef(tmod_lss), 20) # most positive words
```

    ##       legal       shape      either    positive      monday sustainable 
    ##  0.04467974  0.04030600  0.03729081  0.03594595  0.03324765  0.03267820 
    ##   expecting      decent    emerging     several        york   candidate 
    ##  0.03235872  0.03105931  0.03077907  0.03048184  0.02915092  0.02874260 
    ## challenging        able  powerhouse        asia    northern       thing 
    ##  0.02799373  0.02758163  0.02700428  0.02689114  0.02672865  0.02663936 
    ##        drag       stock 
    ##  0.02627880  0.02617401

``` r
tail(coef(tmod_lss), 20) # most negative words
```

    ##        allow      nothing       shrink      cutting      problem        grows 
    ##  -0.03653112  -0.03718793  -0.03752405  -0.03762728  -0.03795939  -0.03860088 
    ##         debt implications       happen policymakers    suggested    something 
    ##  -0.03907404  -0.03925240  -0.03925315  -0.04015877  -0.04092905  -0.04308314 
    ##     interest    borrowing unemployment         hike         rate        rates 
    ##  -0.04312183  -0.04493415  -0.04511959  -0.04540284  -0.04862177  -0.04875790 
    ##          rba     negative 
    ##  -0.05088908  -0.06187295

This plot shows that frequent words (“said”, “people”, “also”) are
neutral while less frequenty words such as “borrowing”, “unemployment”,
“emerging” and “efficient” are either negative or positive.

``` r
textplot_scale1d(tmod_lss, 
                 highlighted = c("said", "people", "also",
                                 "borrowing", "unemployment",
                                 "emerging", "efficient"))
```

![](images/unnamed-chunk-7-1.png)<!-- -->

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
pred <- as.data.frame(predict(tmod_lss, se.fit = TRUE, newdata = dfmt))
pred$date <- docvars(dfmt, "date")
pred <- na.omit(pred)

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

![](images/unnamed-chunk-8-1.png)<!-- -->
