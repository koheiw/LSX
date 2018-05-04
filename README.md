
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

LSS is created to perform sentiment analysis of long texts, but training of its models should be done on smaller units, typically sentences. The [sample sample](https://www.dropbox.com/s/kfhdoifes7z7t6j/data_corpus_guardian2016-10k.RDS?dl=1) contains 10,000 Guardian news articles from 2016.

### Fit a LSS model

``` r
require(quanteda)
require(LSS)
```

``` r
corp <- readRDS('/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds')

toks_sent <- corp %>% 
    corpus_reshape('sentences') %>% 
    tokens(remove_punct = TRUE)
mt_sent <- toks_sent %>% 
    dfm(remove = stopwords()) %>% 
    dfm_select('^[0-9a-zA-Z]+$', valuetype = 'regex') %>% 
    dfm_trim(min_termfreq = 5)

#' sentiment model on economy
eco <- head(char_keyness(toks_sent, 'econom*'), 500)
lss_eco <- textmodel_lss(mt_sent, seedwords('pos-neg'), features = eco)

# sentiment model on politics
pol <- head(char_keyness(toks_sent, 'politi*'), 500)
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

    ##    positive    emerging       china   expecting cooperation        drag 
    ##  0.03941849  0.03906230  0.03249657  0.03172653  0.03014628  0.02910002 
    ##        asia        prof sustainable    academic challenging     markets 
    ##  0.02873776  0.02849241  0.02765765  0.02732935  0.02682587  0.02644153 
    ##   investors   prospects       stock      better   uncertain   strategic 
    ##  0.02637175  0.02570061  0.02538831  0.02479903  0.02358457  0.02346433 
    ##         hit     chinese 
    ##  0.02302436  0.02291012

``` r
tail(coef(lss_eco), 20) # most negative words
```

    ##     downturn       macron     suggests     downbeat         debt 
    ##  -0.03257441  -0.03258820  -0.03277442  -0.03309793  -0.03386571 
    ##         data policymakers   unbalanced       shrink unemployment 
    ##  -0.03524238  -0.03745570  -0.03934074  -0.03944345  -0.03987688 
    ##    suggested          bad     pantheon      cutting       shocks 
    ##  -0.04036920  -0.04047418  -0.04054125  -0.04082423  -0.04267978 
    ##        rates          rba         rate          cut     negative 
    ##  -0.04405703  -0.04789902  -0.05417844  -0.05498620  -0.05697134

#### Political words

``` r
head(coef(lss_pol), 20) # most positive words
```

    ##            kong            hong  constitutional          robert 
    ##      0.04137591      0.03633278      0.03548125      0.02879851 
    ##         elected          career        guardian               f 
    ##      0.02865901      0.02862221      0.02838269      0.02735207 
    ##            play      activities    interference representatives 
    ##      0.02630253      0.02611407      0.02610918      0.02522913 
    ##           links        military         attempt         correct 
    ##      0.02309744      0.02309173      0.02290495      0.02283412 
    ##        lecturer          action       activists      washington 
    ##      0.02246062      0.02243432      0.02175429      0.02136932

``` r
tail(coef(lss_pol), 20) # most negative words
```

    ##       debate  calculation         rise  uncertainty        union 
    ##  -0.02469601  -0.02591610  -0.02603371  -0.02624996  -0.02637228 
    ##    prisoners       brexit     american     averages      divided 
    ##  -0.02651267  -0.02704382  -0.02865085  -0.02874191  -0.02876009 
    ##    landscape        often consequences        dirty      ratings 
    ##  -0.02886668  -0.02944733  -0.03048922  -0.03102530  -0.03131321 
    ##       causes    dominated  complicated       closer    americans 
    ##  -0.03188798  -0.03228346  -0.03351240  -0.03814986  -0.04611773

Predict sentiment of news
-------------------------

In the plots, circles indicate estimated sentiment of news articles. After the UK's referendum on EU membership on 23 June, we can see sharp drop in both economic and political sentiment in the newspaper articles. Economic sentiment recovered within a month, but negative political sentiment sustained.

``` r
mt <- dfm(corp)
```

### Economic sentiment

``` r
pred_eco <- as.data.frame(predict(lss_eco, newdata = mt, density = TRUE))
pred_eco$date <- docvars(mt, 'date')

plot(pred_eco$date, pred_eco$fit, pch = 16, col = rgb(0, 0, 0, 0.1),
     ylim = c(-1, 1), ylab = 'economic sentiment')
lines(lowess(pred_eco$date, pred_eco$fit, f = 0.05), col = 1)
abline(h = 0, v = as.Date("2016-06-23"), lty = c(1, 3))
```

![](man/images/unnamed-chunk-9-1.png)

### Political sentiment

``` r
pred_pol <- as.data.frame(predict(lss_pol, newdata = mt, density = TRUE))
pred_pol$date <- docvars(mt, 'date')

plot(pred_pol$date, pred_pol$fit, pch = 16, col = rgb(1, 0, 0, 0.1),
     ylim = c(-1, 1), ylab = 'political sentiment')
lines(lowess(pred_pol$date, pred_pol$fit, f = 0.05), col = 2)
abline(h = 0, v = as.Date("2016-06-23"), lty = c(1, 3))
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
abline(h = 0, v = as.Date("2016-06-23"), lty = c(1, 3))
```

![](man/images/unnamed-chunk-11-1.png)
