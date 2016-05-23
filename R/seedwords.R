#' @export
seedwords <- function(type){

  if(type=='pos-neg'){
    seeds <- c(rep(1, 7), rep(-1, 7))
    names(seeds) <- c('good', 'nice', 'excellent', 'positive', 'fortunate', 'correct', 'superior',
                      'bad', 'nasty', 'poor', 'negative', 'unfortunate', 'wrong', 'inferior')
  }else if(type=='left-right'){
    seeds <- c(rep(1, 7), rep(-1, 7))
    names(seeds) <- c('deficit', 'austerity', 'unstable', 'recession', 'inflation', 'currency', 'workforce',
                      'poor', 'poverty', 'free', 'benefits', 'prices', 'money', 'workers')
  }else{
    seeds <- NA
  }
  return(seeds)
}

#' @export
findSeeds <- function(mx, words, mx_doc, scores, method='pearson',
                      top=50, min=10, max=100, tol=0.001, dist=0.5, power=2, cache=TRUE, zigzag=FALSE){

  # Get pariwise consine similarity
  mx_sim <- similarity(mx, words, cache=cache)

  # Stage 0 (select seeds based on average proximity)
  prox <- sqrt((colSums(mx_sim * mx_sim) - 1) / (nrow(mx_sim) - 1)) # Exclude proximity to itself
  limit_prox <- quantile(prox, 1 - dist) # Obtaine threashold for proximity
  flag_prox <- prox <= limit_prox
  df_cand <- data.frame(word=colnames(mx_sim)[flag_prox], 
                        prox=prox[flag_prox], stringsAsFactors=FALSE)


  # Stage 1 (test individula seed words)
  mx_sim_sub <- mx_sim[,flag_prox, drop = FALSE]
  cat("Testing", ncol(mx_sim_sub), "temporary dictionaries ...\n")
  for(h in 1:nrow(df_cand)){
    dic_temp <- mx_sim_sub[, h, drop=FALSE]
    df_cand$cor[h] <- stats::cor(scores, unlist(calc_scores(mx_doc, dic_temp, score_only=TRUE)), method=method)
  }

  cat('Cor max', max(df_cand$cor, na.rm=TRUE), '\n')
  cat('Cor min', min(df_cand$cor, na.rm=TRUE), '\n')


  # Stage 2 (making paris of seed words)
  cat("Pairing seed candidates ...\n")
  df_cand$cor_abs <- abs(df_cand$cor)
  df_cand$sign <- sign(df_cand$cor)
  df_cand$score <- (1 / (df_cand$prox ** power)) * df_cand$sign

  # Reduce candidaes
  df_cand_pos <- df_cand[df_cand$sign > 0,]
  df_cand_neg <- df_cand[df_cand$sign < 0,]
  df_cand <- rbind(head(df_cand_pos[order(-df_cand_pos$cor_abs),], top),
                   head(df_cand_neg[order(-df_cand_neg$cor_abs),], top))
  df_cand <- df_cand[order(-df_cand$cor_abs),]

  # Set initial values
  df_cand$step <- NA
  df_cand$word_spou <- ''
  df_cand$pair_cor <- NA

  cors <- c()
  exhausted <- FALSE
  step <- 1
  l_prev <- FALSE # For zigzag search
  report <- matrix(NA, nrow=0, ncol=length(scores))
  for(i in 1:nrow(df_cand)){
    if(!is.na(df_cand[i,]$pair_cor)) next() # Seed is already paired
    if(is.numeric(l_prev) & zigzag){
      k <- l_prev
    }else{
      k <- i
    }
    df_spou <- df_cand[is.na(df_cand$pair_cor) & df_cand$sign!=df_cand[k, 'sign'], c('word', 'score')]

    # Find partner seeds
    df_spou$cor <- NA
    for(j in 1:nrow(df_spou)){
      seed_temp <- rbind(df_cand[k, c('word', 'score')], df_spou[j, c('word', 'score')])
      dict_temp <- rowsum_weighted(mx_sim, seed_temp$word, seed_temp$score)
      df_spou[j,]$cor <- stats::cor(unlist(calc_scores(mx_doc, dict_temp, score_only=TRUE)), scores, method=method)
    }

    # Do not include if correaltion is lower in pairs
    if(max(df_spou$cor) < df_cand[k, 'cor_abs']) next

    df_spou <- df_spou[which.max(df_spou$cor),] # Find best partner
    l <- which(df_cand$word==df_spou$word) # Get index of spaus

    # Save seed info
    df_cand[k, 'word_spou'] <- df_spou[1, 'word']
    df_cand[k, 'pair_cor'] <- df_spou[1, 'cor']
    df_cand[k, 'step'] <- step
    df_cand[l, 'word_spou'] <- df_cand[k, 'word']
    df_cand[l, 'pair_cor'] <- df_spou[1, 'cor']
    df_cand[l, 'step'] <- step
    l_prev <- l

    #return(df_cand)

    # Add pairs
    pairs <- df_cand[!is.na(df_cand$pair_cor), c('word', 'score', 'step')]

    # Dicitonary with current seeds only
    seed_current <- pairs[pairs$step==step,]
    dict_current <- rowsum_weighted(mx_sim, seed_current$word, seed_current$score)
    scores_current <- unlist(calc_scores(mx_doc, dict_current, score_only=TRUE))
    cor_current <- stats::cor(unlist(calc_scores(mx_doc, dict_current, score_only=TRUE)), scores, method=method)
    report <- rbind(report, scores_current)

    # Dicitonary with all seeds
    dict <- rowsum_weighted(mx_sim, pairs$word, pairs$score)
    cor <- stats::cor(unlist(calc_scores(mx_doc, dict, score_only=TRUE)), scores, method=method)
    #print(pairs)
    #print(head(dict[order(-dict),,drop=FALSE]))
    cat("Step", step, ":", df_cand[k, 'word'], '&', df_cand[l, 'word'], "\n")
    cat('Lone cor:', round(df_cand[k, 'cor'], 3), 'Pair cor:', round(cor_current, 3), 'Set cor:', round(cor, 3), '\n')

    plot(unlist(calc_scores(mx_doc, dict, score_only=TRUE)),
         scores, xlab='LSS score', ylab='True score', main=paste('Step', step))

    #plot(c(cors, cor), ylim=c(0.0, 1.0), type='b')
    if(length(unique(df_cand[is.na(df_cand$pair_cor), 'sign'])) == 1) exhausted <- TRUE
    if(exhausted | sum(is.na(df_cand$pair_cor)) < 2 | length(cors) > max | (length(cors) >= min && cor <= max(cors) - tol)){
      matplot(report, ylab='Score', xlab='Step')
      pairs <- pairs[pairs$step < step, c('step', 'word', 'score')] # Exclude current pair
      pairs <- pairs[order(-pairs$score),]
      if(exhausted){
        cat("Exhausted in", step-1, "steps\n\n")
      }else{
        cat("Complete in", step-1, "steps\n\n")
      }
      print(pairs)
      cat('\n')
      seeds <- pairs$score
      names(seeds) <- pairs$word
      #attr(seeds, 'candidates') <- df_cand
      return(seeds)
    }else{
      step <- step + 1
      cors <- c(cors, cor)
    }
  }
}
