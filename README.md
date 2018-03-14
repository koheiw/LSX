
Latent Semantic Scaling
=======================

LSS is a highly efficient vector-space model for subject-specific sentiment analysis used by Kohei Watanabe in several published studies:

-   Kohei Watanabe, 2017. "[Measuring News Bias: Russia’s Official News Agency ITAR-TASS’s Coverage of the Ukraine Crisis](http://journals.sagepub.com/eprint/TBc9miIc89njZvY3gyAt/full)", *European Journal Communication*
-   Kohei Watanabe, 2017. "[The spread of the Kremlin’s narratives by a western news agency during the Ukraine crisis](http://www.tandfonline.com/eprint/h2IHsz2YKce6uJeeCmcd/full)", *Journal of International Communication*
-   Tomila Lankina and Kohei Watanabe. 2017. ["Russian Spring’ or ‘Spring Betrayal’? The Media as a Mirror of Putin’s Evolving Strategy in Ukraine](http://www.tandfonline.com/eprint/tWik7KDfsZv8C2KeNkI5/full)", *Europe-Asia Studies*

How to install
--------------

``` r
devtools::install_github("koheiw/LSS")
```

How to use
----------

LSS is created to perform sentiment analysis of long texts, but training of its models should be done on smaller units, typically sentences. The [sample dataset](https://www.dropbox.com/s/555sr2ml6wc701p/guardian-sample.RData?dl=0) contains 6,000 Guardian news articles, but larger corpus should be used to [estimate parameters accurately](https://koheiw.net/?p=629).

### Fit a LSS model

``` r
require(quanteda)
require(LSS)
```

``` r
load('/home/kohei/Dropbox/Public/guardian-sample.RData')

toks_sent <- data_corpus_guardian %>% 
    corpus_reshape('sentences') %>% 
    tokens(remove_punct = TRUE)
mt_sent <- toks_sent %>% 
    dfm(remove = stopwords()) %>% 
    dfm_select('^[0-9a-zA-Z]+$', valuetype = 'regex') %>% 
    dfm_trim(min_count = 5)
```

    ## Warning in dfm_trim.dfm(., min_count = 5): min_count is deprecated, use
    ## min_termfreq

``` r
#' sentiment model on economy
eco <- head(char_keyness(toks_sent, 'econom*'), 500)
```

    ## Warning in dfm_trim.dfm(m, min_count = min_count): min_count is deprecated,
    ## use min_termfreq

``` r
lss_eco <- textmodel_lss(mt_sent, seedwords('pos-neg'), features = eco)

# sentiment model on politics
pol <- head(char_keyness(toks_sent, 'politi*'), 500)
```

    ## Warning in dfm_trim.dfm(m, min_count = min_count): min_count is deprecated,
    ## use min_termfreq

``` r
lss_pol <- textmodel_lss(mt_sent, seedwords('pos-neg'), features = pol)
```

### Sentiment seed words

Seed words are 14 generic sentiment words.

``` r
seedwords('pos-neg')
```

    ##        good        nice   excellent    positive   fortunate     correct 
    ##           1           1           1           1           1           1 
    ##    superior         bad       nasty        poor    negative unfortunate 
    ##           1          -1          -1          -1          -1          -1 
    ##       wrong    inferior 
    ##          -1          -1

### Sentiment words

LSS identifies domain specific words and weight them by sentiment.

#### Economic words

``` r
head(coef(lss_eco), 20) # most positive words
```

    ##    positive       third      energy opportunity       force        note 
    ##  0.05505316  0.04890042  0.04765659  0.04650236  0.04413810  0.04197609 
    ##     reasons     success      status      future       model     quarter 
    ##  0.04172281  0.04170185  0.03465541  0.03436246  0.03395259  0.03271432 
    ##    strategy        paid     society      fourth       hopes       thing 
    ##  0.03231907  0.03201709  0.03021407  0.02951634  0.02913895  0.02792036 
    ##    regional         nhs 
    ##  0.02759222  0.02710538

``` r
tail(coef(lss_eco), 20) # most negative words
```

    ##     warning         fed      blamed   austerity       warns      warned 
    ## -0.04351791 -0.04390756 -0.04403180 -0.04436876 -0.04488856 -0.04516509 
    ##    interest       sharp     raising    treasury      caused       rates 
    ## -0.04647016 -0.04665759 -0.04715022 -0.04797562 -0.05044084 -0.05169764 
    ##        debt       fears       raise         low     turmoil        poor 
    ## -0.05208930 -0.05223961 -0.05362049 -0.05420565 -0.05466247 -0.05832461 
    ##    negative         bad 
    ## -0.06518192 -0.07795765

#### Political words

``` r
head(coef(lss_pol), 20) # most positive words
```

    ##       third        team       force     reasons     perfect     playing 
    ##  0.04890042  0.04728605  0.04413810  0.04172281  0.04161544  0.03751904 
    ##      future      writes       views      moment  opposition        paid 
    ##  0.03436246  0.03399342  0.03349441  0.03297239  0.03202198  0.03201709 
    ##     science       ideas       price        bill      modern        2016 
    ##  0.03010308  0.03000192  0.02972510  0.02842569  0.02779306  0.02679979 
    ## discussions       faith 
    ##  0.02503479  0.02481003

``` r
tail(coef(lss_pol), 20) # most negative words
```

    ##       central      rhetoric          lack           men participation 
    ##   -0.02964434   -0.03003867   -0.03080698   -0.03091979   -0.03154446 
    ##    criticised      pressure     discourse         power          deep 
    ##   -0.03221735   -0.03316489   -0.03372056   -0.03491120   -0.03697206 
    ##        crisis      struggle  increasingly         anger    chancellor 
    ##   -0.03849192   -0.03940885   -0.03957722   -0.04127278   -0.04245479 
    ##     austerity          talk         rates       happens       turmoil 
    ##   -0.04436876   -0.04473241   -0.05169764   -0.05363807   -0.05466247

Predict sentiment of news
-------------------------

``` r
mt <- dfm(data_corpus_guardian)
```

### Economic sentiment

``` r
pred_eco <- as.data.frame(predict(lss_eco, newdata = mt, density = TRUE))
pred_eco$date <- docvars(mt, 'date')
pred_eco <- subset(pred_eco, density > quantile(density, 0.25))

plot(pred_eco$date, pred_eco$fit, pch = 16, col = rgb(0, 0, 0, 0.1),
     ylim = c(-1, 1), ylab = 'economic sentiment')
lines(lowess(pred_eco$date, pred_eco$fit, f = 0.05), col = 1)
abline(h = 0)
```

![](man/images/unnamed-chunk-9-1.png)

### Political sentiment

``` r
pred_pol <- as.data.frame(predict(lss_pol, newdata = mt, density = TRUE))
pred_pol$date <- docvars(mt, 'date')
pred_pol <- subset(pred_pol, density > quantile(density, 0.25))

plot(pred_pol$date, pred_pol$fit, pch = 16, col = rgb(1, 0, 0, 0.1),
     ylim = c(-1, 1), ylab = 'political sentiment')
lines(lowess(pred_pol$date, pred_pol$fit, f = 0.05), col = 2)
abline(h = 0)
```

![](man/images/unnamed-chunk-10-1.png)

### Comparison

``` r
plot(docvars(mt, 'date'), rep(0, ndoc(mt)), type = 'n',
     ylim = c(-0.5, 0.5), ylab = 'economic/political sentiment')
grid()
lines(lowess(pred_eco$date, pred_eco$fit, f = 0.05), col = 1)
lines(lowess(pred_pol$date, pred_pol$fit, f = 0.05), col = 2)
abline(h = 0)
legend('topright', lty = 1, col = 1:2, legend = c('political', 'economic'))
abline(h = 0)
```

![](man/images/unnamed-chunk-11-1.png)
