#' Plot similarity of seed words
#' @param x fitted textmodel_lss object
#' @param group if \code{TRUE} group seed words by seed patterns and show
#'   average similarity
#' @export
textplot_simil <- function(x, group = FALSE) {
    UseMethod("textplot_simil")
}

    #' @method textplot_simil textmodel_lss
#' @import ggplot2
#' @export
textplot_simil.textmodel_lss <- function(x, group = FALSE) {
    if (!"similarity" %in% names(x))
        stop("similarity matrix is missing")

    temp <- reshape2::melt(x$similarity, as.is = TRUE)
    if (group) {
        seed <- rep(names(x$seeds_weighted), lengths(x$seeds_weighted))
        names(seed) <- names(unlist(unname(x$seeds_weighted)))
        temp$Var1 <- seed[temp$Var1]
        temp$Var2 <- seed[temp$Var2]
        temp <- stats::aggregate(list(value = temp$value),
                                 by = list(Var1 = temp$Var1,
                                           Var2 = temp$Var2), mean)
    }
    temp$Var1 <- factor(temp$Var1, levels = unique(temp$Var2))
    temp$Var2 <- factor(temp$Var2, levels = unique(temp$Var2))
    temp$color <- factor(temp$value > 0, levels = c(TRUE, FALSE),
                         labels = c("positive", "negative"))
    temp$size <- abs(temp$value)
    Var1 <- Var2 <- value <- NULL
    ggplot(data = temp, aes(x = Var1, y = Var2)) +
        geom_point(aes(colour = color, cex = size)) +
        guides(cex = guide_legend(order = 1),
               colour = guide_legend(order = 2)) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

#' Plot substructure of latent semanitic space
#' @export
textplot_substruct <- function(x) {
    UseMethod("textplot_substruct")
}

#' @export
textplot_substruct.textmodel_lss <- function(x) {

    temp <- data.frame(relevance = x$relevance,
                       importance = scale(x$importance, center = FALSE))
    temp <- temp[order(temp$relevance, decreasing = TRUE),]
    temp$factor <- seq_len(nrow(temp))
    ggplot(temp, aes(x = factor, y = relevance)) +
        geom_point(aes(size = importance), color = rgb(0, 0, 0, 0.2))
}
