#' A fitted LSS model on street protest in Russia
#'
#' This model was trained on a Russian media corpus (newspapers, TV transcripts
#' and newswires) to analyze framing of street protests. The scale is protests
#' as "freedom of expression" (high) vs "social disorder" (low). Although some
#' slots are missing in this object (because the model was imported from the
#' original Python implementation), it allows you to scale texts using
#' `predict`.
#' @name data_textmodel_lss_russianprotests
#' @docType data
#' @keywords data
#' @references Lankina, Tomila, and Kohei Watanabe. “'Russian Spring' or 'Spring
#'   Betrayal'? The Media as a Mirror of Putin's Evolving Strategy in Ukraine.”
#'   Europe-Asia Studies 69, no. 10 (2017): 1526–56.
#'   https://doi.org/10.1080/09668136.2017.1397603.
NULL

#' Seed words for analysis of positive-negative sentiment
#'
#' @examples
#' as.seedwords(data_dictionary_sentiment)
#' @name data_dictionary_sentiment
#' @docType data
#' @references Turney, P. D., & Littman, M. L. (2003). Measuring Praise and
#'   Criticism: Inference of Semantic Orientation from Association. ACM Trans.
#'   Inf. Syst., 21(4), 315–346. https://doi.org/10.1145/944012.944013
NULL

#' Seed words for analysis of left-right political ideology
#'
#' @examples
#' as.seedwords(data_dictionary_ideology)
#' @name data_dictionary_ideology
#' @docType data
NULL
