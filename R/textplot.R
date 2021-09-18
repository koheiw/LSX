#' Plot similarity between seed words
#' @param x fitted textmodel_lss object
#' @export
textplot_simil <- function(x) {
    UseMethod("textplot_simil")
}

#' @method textplot_simil textmodel_lss
#' @import ggplot2
#' @export
textplot_simil.textmodel_lss <- function(x) {

    if (is.null(x$similarity) || is.null(x$seeds_weighted))
        stop("textplot_simil() does not work with dummy models")

    temp <- reshape2::melt(x$similarity, as.is = TRUE)
    names(temp) <- c("seed1", "seed2", "simil")
    temp$seed1 <- factor(temp$seed1, levels = unique(temp$seed2))
    temp$seed2 <- factor(temp$seed2, levels = unique(temp$seed2))
    temp$color <- factor(temp$simil > 0, levels = c(TRUE, FALSE),
                         labels = c("positive", "negative"))
    temp$size <- abs(temp$simil)
    seed1 <- seed2 <- simil <- size <- color <- NULL
    ggplot(data = temp, aes(x = seed1, y = seed2)) +
        geom_point(aes(colour = color, cex = size)) +
        guides(cex = guide_legend(order = 1),
               colour = guide_legend(order = 2)) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

#' Plot polarity scores of words
#' @param x a fitted textmodel_lss object.
#' @param highlighted [quanteda::pattern] to select words to highlight.
#' @param max_words the maximum number of words to plot. Words are randomly sampled
#'   to keep the number below the limit.
#' @export
textplot_terms <- function(x, highlighted = NULL, max_words = 10000) {
    UseMethod("textplot_terms")
}

#' @method textplot_terms textmodel_lss
#' @import ggplot2 ggrepel stringi
#' @importFrom quanteda is.dictionary meta
#' @export
textplot_terms.textmodel_lss <- function(x, highlighted = NULL, max_words = 10000) {

    if (is.null(highlighted))
        highlighted <- character()
    if (is.dictionary(highlighted)) {
        separator <- meta(highlighted, field = "separator", type = "object")
        valuetype <- meta(highlighted, field = "valuetype", type = "object")
        concatenator <- x$concatenator
        highlighted <- unlist(highlighted, use.names = FALSE)
        if (!nzchar(separator) && !is.null(concatenator)) # for backward compatibility
            highlighted <- stri_replace_all_fixed(highlighted, separator, concatenator)
    } else {
        highlighted <- unlist(highlighted, use.names = FALSE)
        valuetype <- "glob"
    }
    words_hl <- quanteda::pattern2fixed(
        highlighted,
        types = names(x$beta),
        valuetype = valuetype,
        case_insensitive = TRUE
    )

    beta <- frequency <- word <- NULL
    temp <- data.frame(word = names(x$beta), beta = x$beta, frequency = log(x$frequency),
                       stringsAsFactors = FALSE)
    is_hl <- temp$word %in% unlist(words_hl, use.names = FALSE)
    is_sm <- temp$word %in% sample(temp$word, min(length(temp$word), max_words))
    temp_black <- subset(temp, is_hl)
    temp_gray <- subset(temp, !is_hl & is_sm)
    ggplot(data = temp_gray, aes(x = beta, y = frequency, label = word)) +
           geom_text(colour = "grey70", alpha = 0.7) +
           labs(x = "Polarity", y = "Frequency (log)") +
           theme_bw() +
           theme(panel.grid= element_blank(),
                 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                 axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
           geom_text_repel(data = temp_black, aes(x = beta, y = frequency, label = word),
                           segment.size = 0.25, colour = "black") +
           geom_point(data = temp_black, aes(x = beta, y = frequency), cex = 0.7, colour = "black")

}
