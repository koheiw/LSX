
* Fix `textmodel_lss.fcm()` to pass value to `x_max` in `rsparse::GloVe$new()`.
* Make `textplot_terms()` work with objects from `textmodel_lss.fcm()`.
* Add `concatenator` to `as.seedwords()`.

# Changes in v0.9.9

* Correct how `textstat_context()` and `char_context()` computes statistics.
* Deprecate `char_keyness()`.

# Changes in v0.9.8

* Stop using functions and arguments deprecated in quanteda v3.0.0.

# Changes in v0.9.7

* Make `as.textmodel_lss.matrix()` more reliable.
* Remove **quanteda.textplots** from dependencies. 

# Changes in v0.9.6

* Updated to reflect changes in **quanteda** (creation of **quanteda.textstats**).

# Changes in v0.9.4

* Fix `char_context()` to always return more frequent words in context. 
* Experimental `textplot_factor()` has been removed.
* `as.textmodel_lss()` takes a pre-trained word-embedding.

# Changes in v0.9.3

* Add `textstat_context()` and `char_context()` to replace `char_keyness()`.
* Make the absolute sum of seed weight equal to 1.0 in both upper and lower ends. 
* `textplot_terms()` takes glob patterns in character vector or a dictionary object.
* `char_keyness()` no longer raise error when no patter is found in tokens object.
* Add `engine` to `smooth_lss()` to apply `locfit()` to large datasets.

# Changes in v0.9.2

* Updated unit tests for the new versions of stringi and quanteda.

# Changes in v0.9.0

* Renamed from LSS to LSX for CRAN submission.

# Changes in v0.8.7

* Added `textplot_terms()` to improve visualization of model terms.
