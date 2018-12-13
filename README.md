
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

Seed words are 14 generic sentiment
    words.

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

LSS identifies domain specific words and weight them by
    sentiment.

#### Economic words

``` r
head(coef(lss_eco), 20) # most positive words
```

    ##    positive     markets    emerging   expecting        drag       weigh 
    ##  0.05214424  0.03698609  0.03501609  0.03403731  0.03342814  0.03301794 
    ## challenging  management        asia     impacts sustainable      senior 
    ##  0.03131688  0.03109008  0.03082225  0.02873497  0.02821411  0.02779674 
    ## cooperation      french  powerhouse       stock     chinese       china 
    ##  0.02757589  0.02742529  0.02717255  0.02711204  0.02644329  0.02633488 
    ##   efficient     slowing 
    ##  0.02630425  0.02609119

``` r
tail(coef(lss_eco), 20) # most negative words
```

    ##   suggesting      created     downbeat    inflation       norway 
    ##  -0.03082917  -0.03097888  -0.03111960  -0.03244584  -0.03292088 
    ## implications      deficit       single         data      cutting 
    ##  -0.03363656  -0.03451063  -0.03576445  -0.03814103  -0.03854116 
    ## consequences    dependent          low         hike unemployment 
    ##  -0.03874300  -0.03969037  -0.04090138  -0.04119885  -0.04393768 
    ##     interest         rate          rba        rates     negative 
    ##  -0.04396777  -0.04691304  -0.04920256  -0.04934219  -0.05966217

#### Political words

``` r
head(coef(lss_pol), 20) # most positive words
```

    ##          court constitutional           hong         robert      activists 
    ##     0.03683695     0.03448106     0.03303420     0.02960304     0.02919637 
    ##       rousseff         senior          north              f   interference 
    ##     0.02833502     0.02779674     0.02669770     0.02630391     0.02558804 
    ##         highly           near       leftwing       involved           case 
    ##     0.02538547     0.02536469     0.02484337     0.02452125     0.02407098 
    ##    westminster     consultant        correct         guests      professor 
    ##     0.02388269     0.02272020     0.02219132     0.02162323     0.02160143

``` r
tail(coef(lss_pol), 20) # most negative words
```

    ## economically     religion    primarily  calculation        union 
    ##  -0.02050284  -0.02214345  -0.02221894  -0.02229977  -0.02313443 
    ##    landscape       access    sensitive    departure    countries 
    ##  -0.02314414  -0.02331452  -0.02366598  -0.02387345  -0.02504337 
    ##     personal        often      happens       brexit     dictated 
    ##  -0.02658480  -0.02713668  -0.02716319  -0.02764422  -0.02847745 
    ##        dirty         data    movements consequences          cut 
    ##  -0.02933004  -0.03814103  -0.03834440  -0.03874300  -0.04932111

## Predict sentiment of news

In the plots, circles indicate estimated sentiment of news articles.
After the UK’s referendum on EU membership on 23 June, we can see sharp
drop in both economic and political sentiment in the newspaper articles.
Economic sentiment recovered within a month, but negative political
sentiment
sustained.

``` r
mt <- dfm(corp)
```

### Economic sentiment

``` r
pred_eco <- as.data.frame(predict(lss_eco, newdata = mt, density = TRUE))
pred_eco$date <- docvars(mt, 'date')
pred_eco <- na.omit(pred_eco)

plot(pred_eco$date, pred_eco$fit, pch = 16, col = rgb(0, 0, 0, 0.1),
     ylim = c(-1, 1), ylab = 'economic sentiment')
lines(lowess(pred_eco$date, pred_eco$fit, f = 0.05), col = 1)
abline(h = 0, v = as.Date("2016-06-23"), lty = c(1, 3))
```

![](images/unnamed-chunk-9-1.png)<!-- -->

### Political sentiment

``` r
pred_pol <- as.data.frame(predict(lss_pol, newdata = mt, density = TRUE))
pred_pol$date <- docvars(mt, 'date')
pred_pol <- na.omit(pred_pol)

plot(pred_pol$date, pred_pol$fit, pch = 16, col = rgb(1, 0, 0, 0.1),
     ylim = c(-1, 1), ylab = 'political sentiment')
lines(lowess(pred_pol$date, pred_pol$fit, f = 0.05), col = 2)
abline(h = 0, v = as.Date("2016-06-23"), lty = c(1, 3))
```

![](images/unnamed-chunk-10-1.png)<!-- -->

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

![](images/unnamed-chunk-11-1.png)<!-- -->
