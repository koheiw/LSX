
# Latent Semantic Scaling

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/LSX)](https://CRAN.R-project.org/package=LSX)
[![Downloads](https://cranlogs.r-pkg.org/badges/LSX)](https://CRAN.R-project.org/package=LSX)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/LSX?color=orange)](https://CRAN.R-project.org/package=LSX)
[![R build
status](https://github.com/koheiw/LSX/workflows/R-CMD-check/badge.svg)](https://github.com/koheiw/LSX/actions)
[![codecov](https://codecov.io/gh/koheiw/LSX/branch/master/graph/badge.svg)](https://codecov.io/gh/koheiw/LSX)
<!-- badges: end -->

**NOTICE:** This R package is renamed from **LSS** to **LSX** for CRAN
submission.

In quantitative text analysis, the cost to train supervised machine
learning models tend to be very high when the corpus is large. LSS is a
semisupervised document scaling method that I developed to perform large
scale analysis at low cost. Taking user-provided *seed words* as weak
supervision, it estimates polarity of words in the corpus by latent
semantic analysis and locates documents on a unidimensional scale
(e.g. sentiment).

Please read my paper for the algorithm and methodology:

-   Watanabe, Kohei. 2020. “[Latent Semantic Scaling: A Semisupervised
    Text Analysis Technique for New Domains and
    Languages](https://www.tandfonline.com/doi/full/10.1080/19312458.2020.1832976)”,
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
    dfm(revemo_padding = TRUE) %>%
    dfm_select("^\\p{L}+$", valuetype = "regex", min_nchar = 2) %>% 
    dfm_trim(min_termfreq = 5)
```

    ## Warning: revemo_padding argument is not used.

``` r
eco <- char_context(toks_sent, "econom*", p = 0.05)
lss <- textmodel_lss(dfmt_sent, as.seedwords(data_dictionary_sentiment), 
                     terms = eco, auto_weight = TRUE, k = 300, cache = TRUE)
```

    ## Reading cache file: lss_cache/svds_e5089465ba658d1a.RDS

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

    ##        good    positive       shape        asia      remain sustainable 
    ##  0.14692531  0.14625752  0.13157273  0.10045343  0.09826269  0.09702084 
    ##    emerging   continued potentially challenging       china   expecting 
    ##  0.09443049  0.09426020  0.09143613  0.08905550  0.08633932  0.08550641 
    ##      decent     markets      expect       stock cooperation   uncertain 
    ##  0.08506407  0.08497540  0.08486704  0.08273883  0.08127225  0.07949043 
    ##      future         hit 
    ##  0.07941483  0.07888814

``` r
tail(coef(lss), 20) # most negative words
```

    ##     pantheon         debt        trend       brexit         data     sterling 
    ##   -0.1042108   -0.1049986   -0.1060193   -0.1064971   -0.1079335   -0.1081180 
    ##        rates         hike       shrink          rba        basic        grows 
    ##   -0.1082183   -0.1091890   -0.1127559   -0.1147965   -0.1156016   -0.1164406 
    ## unemployment         rate    borrowing implications    suggested          bad 
    ##   -0.1170247   -0.1176705   -0.1200251   -0.1220836   -0.1230534   -0.1441512 
    ##     negative          cut 
    ##   -0.1459998   -0.1469415

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

In the plots, circles indicate sentiment of individual news articles and
lines are their local average (solid line) with a confidence band
(dotted lines). According to the plot, economic sentiment in the
Guardian news stories became negative from February to April, but it
become more positive in April. As the referendum approaches, the
newspaper’s sentiment became less stable, although it became close to
neutral (overall mean) on the day of voting (broken line).

``` r
dfmt <- dfm_group(dfmt_sent)

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

## Examples

Please read the following papers for how to use LSS in social science
research:

-   Trubowitz, Peter and Watanabe, Kohei. 2021. [“The Geopolitical
    Threat Index: A Text-Based Computational Approach to Identifying
    Foreign
    Threats”](https://academic.oup.com/isq/advance-article/doi/10.1093/isq/sqab029/6278490),
    *International Studies Quarterly*.
-   Vydra, Simon and Kantorowicz, Jaroslaw. [“Tracing Policy-relevant
    Information in Social Media: The Case of Twitter before and during
    the COVID-19
    Crisis”](https://www.degruyter.com/document/doi/10.1515/spp-2020-0013/html).
    *Statistics, Politics and Policy*.
-   Kinoshita, Hiroko. 2020. [“A Quantitative Text Analysis Approach on
    LGBTQ Issues in Contemporary
    Indonesia”](https://so03.tci-thaijo.org/index.php/jpss/article/view/241133).
    *Journal of Population and Social Studies*.
-   Yamao, Dai. 2020. [“Re-securitization as Evasion of Responsibility:
    A Quantitative Text Analysis of Refugee Crisis in Major Arabic
    Newspapers”](https://so03.tci-thaijo.org/index.php/jpss/article/view/241130),
    *Journal of Population and Social Studies*.
-   Watanabe, Kohei. 2017. [“Measuring News Bias: Russia’s Official News
    Agency ITAR-TASS’s Coverage of the Ukraine
    Crisis”](http://journals.sagepub.com/eprint/TBc9miIc89njZvY3gyAt/full),
    *European Journal Communication*.
-   Watanabe, Kohei. 2017. [“The spread of the Kremlin’s narratives by a
    western news agency during the Ukraine
    crisis”](http://www.tandfonline.com/eprint/h2IHsz2YKce6uJeeCmcd/full)",
    *Journal of International Communication*.
-   Lankina, Tomila and Watanabe, Kohei. 2017. [“‘Russian Spring’ or
    ‘Spring Betrayal’? The Media as a Mirror of Putin’s Evolving
    Strategy in
    Ukraine”](http://www.tandfonline.com/eprint/tWik7KDfsZv8C2KeNkI5/full),
    *Europe-Asia Studies*.
