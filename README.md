
# Latent Semantic Scaling

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/LSX)](https://CRAN.R-project.org/package=LSX)
[![Downloads](https://cranlogs.r-pkg.org/badges/LSX)](https://CRAN.R-project.org/package=LSX)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/LSX?color=orange)](https://CRAN.R-project.org/package=LSX)
[![R build
status](https://github.com/koheiw/LSX/workflows/R-CMD-check/badge.svg)](https://github.com/koheiw/LSX/actions)
[![codecov](https://codecov.io/gh/koheiw/LSX/branch/master/graph/badge.svg)](https://app.codecov.io/gh/koheiw/LSX)
<!-- badges: end -->

**NOTICE:** This R package is renamed from **LSS** to **LSX** for CRAN
submission.

In quantitative text analysis, the cost of training supervised machine
learning models tend to be very high when the corpus is large. LSS is a
semisupervised document scaling technique that I developed to perform
large scale analysis at low cost. Taking user-provided *seed words* as
weak supervision, it estimates polarity of words in the corpus by latent
semantic analysis and locates documents on a unidimensional scale
(e.g. sentiment).

Please read the following papers for the algorithm and methodology, and
its application to non-English texts (Japanese and Hebrew):

-   Watanabe, Kohei. 2020. [“Latent Semantic Scaling: A Semisupervised
    Text Analysis Technique for New Domains and
    Languages”](https://www.tandfonline.com/doi/full/10.1080/19312458.2020.1832976),
    *Communication Methods and Measures*.
-   Watanabe, Kohei, Segev, Elad, & Tago, Atsushi. (2022). [“Discursive
    diversion: Manipulation of nuclear threats by the conservative
    leaders in Japan and
    Israel”](https://journals.sagepub.com/doi/full/10.1177/17480485221097967),
    *International Communication Gazette*.

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
    dfm(remove_padding = TRUE) %>%
    dfm_select("^\\p{L}+$", valuetype = "regex", min_nchar = 2) %>% 
    dfm_trim(min_termfreq = 5)

eco <- char_context(toks_sent, "econom*", p = 0.05)
lss <- textmodel_lss(dfmt_sent, as.seedwords(data_dictionary_sentiment), 
                     terms = eco, k = 300, cache = TRUE)
```

    ## Writing cache file: lss_cache/svds_568607abc404ca43.RDS

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

    ##        good       shape    positive sustainable   expecting      remain 
    ##  0.10086678  0.08100301  0.07287992  0.06489614  0.06459612  0.06327885 
    ##    emerging      decent   continued challenging        asia  powerhouse 
    ##  0.06173428  0.06158674  0.05958519  0.05735492  0.05545359  0.05454087 
    ##        drag      argued       china         hit       stock       start 
    ##  0.05430134  0.05425140  0.05269536  0.05213953  0.05177975  0.05162649 
    ##   weakening consultancy 
    ##  0.05153202  0.05108261

``` r
tail(coef(lss), 20) # most negative words
```

    ##      raising         rise     sterling      cutting        grows       shrink 
    ##  -0.07002333  -0.07106325  -0.07220668  -0.07389086  -0.07568230  -0.07626922 
    ## implications        basic         debt policymakers    suggested     interest 
    ##  -0.07767036  -0.07848986  -0.07896652  -0.07970222  -0.08267444  -0.08631343 
    ## unemployment    borrowing         hike         rate          rba        rates 
    ##  -0.08879022  -0.09109017  -0.09224650  -0.09598675  -0.09672486  -0.09754047 
    ##          cut     negative 
    ##  -0.11047689  -0.12472812

This plot shows that frequent words (“world”, “country”, “uk”) are
neutral while less frequent words such as “borrowing”, “unemployment”,
“debt”, “emerging”, “efficient” are “sustainable” are either negative or
positive.

``` r
textplot_terms(lss, 
               highlighted = c("world", "business", "uk",
                               "borrowing", "unemployment", "debt",
                               "emerging", "efficient", "sustainable"))
```

![](images/words-1.png)<!-- -->

## Result of analysis

In the plot, circles indicate sentiment of individual news articles and
lines are their local average (solid line) with a confidence band
(dotted lines). According to the plot, economic sentiment in the
Guardian news articles became negative between March and May in 2016,
but it become more positive in June.

``` r
dfmt <- dfm_group(dfmt_sent)

dat <- docvars(dfmt)

# predict sentiment scores
dat$fit <- predict(lss, newdata = dfmt)

# smooth LSS scores
dat_sm <- smooth_lss(dat, span = 0.2, from = as.Date("2016-01-01"), to = as.Date("2016-12-31"))

# plot trend
plot(dat$date, dat$fit, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-0.5, 0.5),
     xlab = "Time", ylab = "Economic sentiment", main = "New coverage by the Guardian in 2016")
lines(dat_sm$date, dat_sm$fit, type = "l")
lines(dat_sm$date, dat_sm$fit + dat_sm$se.fit * 1.96, type = "l", lty = 3)
lines(dat_sm$date, dat_sm$fit - dat_sm$se.fit * 1.96, type = "l", lty = 3)
abline(h = 0, v = as.Date("2016-06-23"), lty = c(1, 2))
text(as.Date("2016-06-23"), 0.4, "Brexit referendum")
```

![](images/trend-1.png)<!-- -->

## Examples

LSS has been used for research in various fields of social science. For
example:

-   Brändle, Verena K., and Olga Eisele. 2022. [“A Thin Line:
    Governmental Border Communication in Times of European
    Crises”](https://onlinelibrary.wiley.com/doi/full/10.1111/jcms.13398)
    *Journal of Common Market Studies*.
-   Umansky, Natalia. 2022. [“Who gets a say in this? Speaking security
    on social
    media”](https://journals.sagepub.com/doi/10.1177/14614448221111009).
    *New Media & Society*.
-   Rauh, Christian, 2022. [“Supranational emergency politics? What
    executives’ public crisis communication may tell
    us”](https://www.tandfonline.com/doi/full/10.1080/13501763.2021.1916058),
    *Journal of European Public Policy*.
-   Trubowitz, Peter and Watanabe, Kohei. 2021. [“The Geopolitical
    Threat Index: A Text-Based Computational Approach to Identifying
    Foreign
    Threats”](https://academic.oup.com/isq/advance-article/doi/10.1093/isq/sqab029/6278490),
    *International Studies Quarterly*.
-   Vydra, Simon and Kantorowicz, Jaroslaw. 2020. [“Tracing
    Policy-relevant Information in Social Media: The Case of Twitter
    before and during the COVID-19
    Crisis”](https://www.degruyter.com/document/doi/10.1515/spp-2020-0013/html).
    *Statistics, Politics and Policy*.
-   Watanabe, Kohei. 2017. [“Measuring News Bias: Russia’s Official News
    Agency ITAR-TASS’s Coverage of the Ukraine
    Crisis”](http://journals.sagepub.com/eprint/TBc9miIc89njZvY3gyAt/full),
    *European Journal Communication*.
-   Lankina, Tomila and Watanabe, Kohei. 2017. [“‘Russian Spring’ or
    ‘Spring Betrayal’? The Media as a Mirror of Putin’s Evolving
    Strategy in
    Ukraine”](http://www.tandfonline.com/eprint/tWik7KDfsZv8C2KeNkI5/full),
    *Europe-Asia Studies*.

Other studies are available on [Google
Scholar](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=5312969973901591795).
