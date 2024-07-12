#' Plot similarity between seed words
#' @param x fitted textmodel_lss object.
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
#' @param highlighted [quanteda::pattern] to select words to highlight. If a
#'   [quanteda::dictionary] is passed, words in the top-level categories are
#'   highlighted in different colors.
#' @param max_highlighted the maximum number of words to highlight. When
#'   `highlighted = NULL`, words to highlight are randomly selected
#'   proportionally to `polarity ^ 2 * log(frequency)`.
#' @param max_words the maximum number of words to plot. Words are randomly
#'   sampled to keep the number below the limit.
#' @param ... passed to underlying functions. See the Details.
#' @details Users can customize the plots through `...`, which is
#'   passed to [ggplot2::geom_text()] and [ggrepel::geom_text_repel()]. The
#'   colors are specified internally but users can override the settings by appending
#'   [ggplot2::scale_colour_manual()] or [ggplot2::scale_colour_brewer()]. The
#'   legend title can also be modified using [ggplot2::labs()].
#' @importFrom ggrepel geom_text_repel
#' @export
textplot_terms <- function(x, highlighted = NULL,
                           max_highlighted = 50, max_words = 1000, ...) {
    UseMethod("textplot_terms")
}

#' @method textplot_terms textmodel_lss
#' @import ggplot2 ggrepel stringi
#' @importFrom quanteda is.dictionary meta check_integer
#' @export
textplot_terms.textmodel_lss <- function(x, highlighted = NULL,
                                         max_highlighted = 50, max_words = 1000, ...) {

    max_words <- check_integer(max_words, min = 1)
    max_highlighted <- check_integer(max_highlighted, min = 0)

    x$frequency <- x$frequency[names(x$beta)] # fix for < v1.1.4
    x$frequency[is.na(x$frequency)] <- 0

    beta <- freq <- word <- NULL
    temp <- data.frame(word = names(x$beta), beta = x$beta,
                       freq = x$frequency,
                       stringsAsFactors = FALSE)

    temp <- subset(temp, freq > 0)
    temp$freq <- log(temp$freq)
    temp$id <- seq_len(nrow(temp))
    temp$group <- factor(rep("highlighted", nrow(temp)))

    if (is.null(highlighted)) {
        key <- NULL
    } else {
        if (is.dictionary(highlighted)) {
            separator <- meta(highlighted, field = "separator", type = "object")
            valuetype <- meta(highlighted, field = "valuetype", type = "object")
        } else {
            highlighted <- unlist(highlighted, use.names = FALSE)
            valuetype <- "glob"
        }
        if (is.null(x$concatenator)) {
            concatenator <- "_" # for old object
        } else {
            concatenator <- x$concatenator
        }
        ids <- quanteda::object2id(
            highlighted,
            types = temp$word,
            valuetype = valuetype,
            case_insensitive = TRUE,
            concatenator = concatenator
        )

        # flag nested patterns (see quanteda::dfm_lookup)
        if (length(ids)) {
          m <- factor(names(ids), levels = unique(names(ids)))
          dup <- unlist(lapply(split(ids, m), duplicated), use.names = FALSE)
        } else {
          dup <- logical()
        }

        key <- attr(ids, "key")
        ids <- ids[lengths(ids) == 1 & !dup] # drop phrasal and nested patterns
        id <- unlist(ids)

        if (!is.null(key) && !is.null(id)) {
            temp$group <- factor(names(id[match(temp$id, id)]), levels = key)
        } else {
            temp$group <- factor(ifelse(temp$id %in% id, "highlighted", NA),
                                 levels = "highlighted")
        }
    }
    temp$p <- as.numeric(!is.na(temp$group)) * temp$beta ^ 2 * temp$freq
    if (all(temp$p == 0)) {
        l <- rep(FALSE, length(temp$id))
    } else {
        l <- temp$id %in% sample(temp$id, min(sum(temp$p > 0), max_highlighted),
                                 prob = temp$p)
    }
    temp_hi <- temp[l,]
    temp_lo <- temp[!l,]

    group <- NULL # for cran check
    temp_lo <- head(temp_lo[sample(seq_len(nrow(temp_lo))),], max_words)
    gg <- ggplot(data = temp_lo, aes(x = beta, y = freq, label = word)) +
           geom_text(colour = "grey70", alpha = 0.7, ...) +
           labs(x = "Polarity", y = "Frequency (log)") +
           theme_bw() +
           theme(panel.grid= element_blank(),
                 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                 axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
           geom_text_repel(data = temp_hi, aes(x = beta, y = freq, label = word, colour = group),
                           segment.size = 0.25, show.legend = FALSE, ...) +
           geom_point(data = temp_hi, aes(x = beta, y = freq, shape = group, colour = group),
                      cex = 1)

    if (!is.null(key) && length(key) > 1) {
        gg <- gg + scale_colour_brewer(palette = "Set1", drop = FALSE) +
                   scale_shape_discrete(drop = FALSE) +
                   labs(colour = "Group", shape = "Group")
    } else {
        gg <- gg + scale_colour_manual(values = "black") +
                   guides(colour = "none", shape = "none")
    }
    return(gg)
}

#' \[experimental\] Plot clusters of word vectors
#'
#' Experimental function to find clusters of word vectors
#' @param x a fitted `textmodel_lss`.
#' @param n the number of cluster.
#' @param method the method for hierarchical clustering.
#' @param scale change the scale of y-axis.
#' @keywords internal
#' @export
textplot_components <- function(x, n = 5, method = "ward.D2",
                                scale = c("absolute", "relative")) {
    UseMethod("textplot_components")
}

#' @method textplot_components textmodel_lss
#' @import ggplot2
#' @importFrom stats hclust cutree
#' @importFrom quanteda check_integer
#' @export
textplot_components.textmodel_lss <- function(x, n = 5, method = "ward.D2",
                                              scale = c("absolute", "relative")) {


    if (is.null(x$k))
        stop("SVD must be used to generate word vectors", call. = FALSE)

    n <- check_integer(n, min_len = 1, max_len = 1, min = 2, max = x$k)
    scale <- match.arg(scale)

    seed <- names(x$seeds_weighted)
    emb <- x$embedding[,seed]
    suppressWarnings({
        sim <- proxyC::simil(Matrix(emb, sparse = TRUE))
    })
    dist <- as.dist(1 - abs(as.matrix(sim)))
    hc <- hclust(dist, method)
    b <- cutree(hc, k = n)

    index <- group <- cum <-NULL
    temp <- data.frame(index = seq_along(b), group = factor(b))

    if (scale == "absolute") {
        temp$cum <- ave(temp$group == temp$group, temp$group, FUN = cumsum)
        limit <- x$k / n
    } else {
        temp$cum <- ave(temp$group == temp$group, temp$group,
                        FUN = function(x) cumsum(x) / sum(x))
        limit <- 1.0
    }

    ggplot(temp, aes(x = index, y = cum, col = group)) +
        labs(x = "Rank", y = "Sum (cumulative)", col = "Cluster") +
        theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
        ylim(0, limit) +
        geom_step()

}
