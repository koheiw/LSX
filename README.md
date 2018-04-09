
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

    ##    positive       third      energy opportunity       force     success 
    ##  0.05575652  0.04854844  0.04787103  0.04630764  0.04445485  0.04265223 
    ##        note     reasons       model      status    strategy        paid 
    ##  0.04190151  0.04122328  0.03510708  0.03497756  0.03353344  0.03329871 
    ##      future     quarter       hopes     society      fourth       thing 
    ##  0.03317974  0.03207682  0.03005912  0.02988894  0.02867550  0.02793827 
    ##         nhs    regional 
    ##  0.02749824  0.02734465

``` r
tail(coef(lss_eco), 20) # most negative words
```

    ##     cutting         fed     warning       warns      blamed     raising 
    ## -0.04292397 -0.04335132 -0.04398681 -0.04410646 -0.04431494 -0.04536274 
    ##    interest      warned       sharp    treasury       rates      caused 
    ## -0.04613241 -0.04662480 -0.04703221 -0.04828769 -0.05035697 -0.05085765 
    ##        debt       raise       fears         low     turmoil        poor 
    ## -0.05216854 -0.05221771 -0.05235718 -0.05376015 -0.05452033 -0.05900819 
    ##    negative         bad 
    ## -0.06527032 -0.07853763

#### Political words

``` r
head(coef(lss_pol), 20) # most positive words
```

    ##      third       team      force    reasons    perfect    playing 
    ## 0.04854844 0.04720371 0.04445485 0.04122328 0.03946390 0.03807861 
    ##     writes     moment       paid     future opposition      views 
    ## 0.03511611 0.03394436 0.03329871 0.03317974 0.03249943 0.03221336 
    ##      ideas      price    science       bill       2016     modern 
    ## 0.03167905 0.02991380 0.02897796 0.02874248 0.02768428 0.02752270 
    ##      faith  elections 
    ## 0.02478055 0.02453918

``` r
tail(coef(lss_pol), 20) # most negative words
```

    ##         data         lack      central          men     rhetoric 
    ##  -0.02875861  -0.03004615  -0.03084299  -0.03114291  -0.03126121 
    ##     pressure   criticised        power    discourse         deep 
    ##  -0.03267190  -0.03282361  -0.03423453  -0.03511987  -0.03751972 
    ##       crisis     struggle increasingly   chancellor        anger 
    ##  -0.03869216  -0.03988131  -0.04076471  -0.04156370  -0.04255494 
    ##    austerity         talk        rates      turmoil      happens 
    ##  -0.04289836  -0.04358038  -0.05035697  -0.05452033  -0.05498455

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
