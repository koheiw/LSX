## Changes in v1.4.5

* Enable grouping by multiple variables using `smooth_lss()`.
* Fix tests for `textplot_*()` for upcoming **ggplot2**.

## Changes in v1.4.4

* Fix a bug in `as.textmodel_lss()` when a `textmodel_wordvector` object is given.
* Add `sampling` to `textplot_terms()` to improve highlighting of words when the distribution of polarity scores is asymmetric.

## Changes in v1.4.3

* Improve the handling of `textmodel_wordvector` objects from the **wordvector** package in `as.textmodel_lss()`.
* Deprecate `auto_weight` in `textmodel_lss()`.
* Deprecate `textplot_simil()`.

## Changes in v1.4.2

* Add `as.textmodel_lss()` for objects from the **wordvector** package.
* Reduce dependent packages by moving **rsparse**, **irlba** and **rsvd** to Suggests.
* Fix handling of phrasal patterns in `textplot_terms()`.
* Improve objects created by `as.textmodel_lss.textmodel_lss()`.

## Changes in v1.4.1

* Add `group` to `smooth_lss()` to smooth LSS scores by group.
* Add `optimize_lss()` as an experimental function.

## Changes in v1.4.0

* Change the default value to `max_highlighted = 1000` in `textplot_terms()`.
* Add `...` to customize text labels to `textplot_terms()`.
* Highlight words in different colors when a dictionary is passed to `highlighted`.
* Add `mode = "predict"` and `remove = FALSE` to `bootstrap_lss()`.

## Changes in v1.3.2

* Fix the error in `textplot_terms()` when the frequency of terms are zero (#85).

## Changes in v1.3.1

* Fix the range of scores when `cut` is used.
* Add `bootstrap_lss()` as an experimental function.

## Changes in v1.3.0

* Add `cut` to `predict`.
* Move examples to the new package website: http://koheiw.github.io/LSX.
* Rename "rescaling" to "rescale" for simplicity and consistency.
* Improve random sampling of words to highlight in `textplot_terms()` to avoid congestion.

## Changes in v1.2.0

* Add `group_data` to `textmodel_lss()` to simplify the workflow.
* Add `max_highlighted` to `textplot_terms()` to automatically highlight polarity words.

## Changes in v1.1.4

* Update `as.textmodel_lss()` to avoid errors in `textplot_terms()` when `terms` is used.

## Changes in v1.1.3

* Restore examples for `textmodel_lss()`.
* Defunct `char_keyness()` that has been deprecated for long.

## Changes in v1.1.2

* Update examples to pass CRAN tests.

## Changes in v1.1.1

* Add `min_n` to `predict()` to make polarity scores of short documents more stable.

## Changes in v1.1.0

* Add `as.textmodel_lss()` for textmodel_lss objects to allow modifying existing models.
* Allow `terms` in `textmodel_lss()` to be a named numeric vector to give arbitrary weights.

## Changes in v1.0.2

* Add the `auto_weight` argument to `textmodel_lss()` and `as.textmodel_lss()` to improve the accuracy of scaling.
* Remove the `group` argument from `textplot_simil()` to simplify the object.
* Make `as.seedwords()` to accept multiple indices for `upper` and `lower`.

## Changes in v1.0.0

* Add `max_count` to `textmodel_lss.fcm()` that will be passed to `x_max` in `rsparse::GloVe$new()`.
* Add `max_words` to `textplot_terms()` to avoid overcrowding.
* Make `textplot_terms()` to work with objects from `textmodel_lss.fcm()`.
* Add `concatenator` to `as.seedwords()`.

## Changes in v0.9.9

* Correct how `textstat_context()` and `char_context()` computes statistics.
* Deprecate `char_keyness()`.

## Changes in v0.9.8

* Stop using functions and arguments deprecated in quanteda v3.0.0.

## Changes in v0.9.7

* Make `as.textmodel_lss.matrix()` more reliable.
* Remove **quanteda.textplots** from dependencies. 

## Changes in v0.9.6

* Updated to reflect changes in **quanteda** (creation of **quanteda.textstats**).

## Changes in v0.9.4

* Fix `char_context()` to always return more frequent words in context. 
* Experimental `textplot_factor()` has been removed.
* `as.textmodel_lss()` takes a pre-trained word-embedding.

## Changes in v0.9.3

* Add `textstat_context()` and `char_context()` to replace `char_keyness()`.
* Make the absolute sum of seed weight equal to 1.0 in both upper and lower ends. 
* `textplot_terms()` takes glob patterns in character vector or a dictionary object.
* `char_keyness()` no longer raise error when no patter is found in tokens object.
* Add `engine` to `smooth_lss()` to apply `locfit()` to large datasets.

## Changes in v0.9.2

* Updated unit tests for the new versions of stringi and quanteda.

## Changes in v0.9.0

* Renamed from LSS to LSX for CRAN submission.

## Changes in v0.8.7

* Added `textplot_terms()` to improve visualization of model terms.
