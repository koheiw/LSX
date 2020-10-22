# Changes in v0.9.3

* Experimental `textplot_factor()` has been removed.
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
