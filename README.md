
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

Please read [my working
paper](https://blog.koheiw.net/wp-content/uploads/2020/04/LSS-06.pdf)
for the algorithm and methodology.

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
semantic proximity. The [sample corpus](https://bit.ly/2GZwLcN) contains
10,000 Guardian news articles from 2016.

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
lss <- textmodel_lss(dfmt_sent, as.seedwords(data_dictionary_sentiment),
                     feature = eco, cache = TRUE, k = 300)
```

    ## Reading cache file: lss_cache/svds_34e08f32419c9eca.RDS

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
to seed
    words.

``` r
head(coef(lss), 20) # most positive words
```

    ##       shape      either    positive      monday   expecting sustainable 
    ##  0.04017038  0.03745197  0.03602651  0.03352895  0.03307553  0.03277666 
    ##      decent     several    emerging        york   candidate challenging 
    ##  0.03084942  0.03080686  0.03046641  0.02925947  0.02883487  0.02801757 
    ##      argued        able  powerhouse        asia       thing        drag 
    ##  0.02770138  0.02753339  0.02724199  0.02715581  0.02663417  0.02638054 
    ##       stock         aid 
    ##  0.02608997  0.02603547

``` r
tail(coef(lss), 20) # most negative words
```

    ##     actually        allow      nothing      cutting       shrink        grows 
    ##  -0.03527976  -0.03671954  -0.03702909  -0.03735588  -0.03815107  -0.03851795 
    ##         debt implications policymakers    suggested     interest    something 
    ##  -0.03872739  -0.03942620  -0.04015681  -0.04115879  -0.04288522  -0.04311322 
    ##    borrowing unemployment         hike        rates         rate          rba 
    ##  -0.04490576  -0.04508967  -0.04572553  -0.04857283  -0.04878567  -0.05076572 
    ##          cut     negative 
    ##  -0.05599481  -0.06169430

This plot shows that frequent words (“said”, “people”, “also”) are
neutral while less frequent words such as “borrowing”, “unemployment”,
“emerging” and “efficient” are either negative or positive.

``` r
textplot_scale1d(lss, 
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

![](images/trend-1.png)<!-- -->
