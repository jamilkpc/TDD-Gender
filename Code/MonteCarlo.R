library(text2vec)
library(tm)
library(hdm)

set.seed(123)

td0 <- as.character(seq(0,20))
td1 <- as.character(seq(100,120))
xd0 <- as.character(seq(11,60))
xd1 <- as.character(seq(61,110))
nd  <- as.character(seq(11,110))

createDF <- function(n){
  rv <- 2*runif(n)-1
  x <- rv^3 - 2*runif(n) + 1
  t <- as.integer(rv > 0)
  c <- as.integer(x > 0)
  
  df <- data.frame(rv = rv, x = x, t = t, c = c)
  return(df)
}

createDV <- function(t,c, db, cb){
  dvs <- sample(21, 10, replace = T)
  cs <- sample(50, 10, replace = T)
  ns <- sample(100, 80, replace = T)
  
  if (t == 1){dv1 <- td1[dvs]} else {dv1 <- td0[dvs]}
  if (t == 1){dv2 <- xd1[cs]} else {dv2 <- xd0[cs]}
  dv3 <- nd[ns]
  
  dv <- c(dv1,dv2,dv3)
  dv <- paste0(dv, collapse = " ")

  return(dv)
}

simulation_batch <- function(n){
  df <- createDF(5000)
  df$dv <- purrr::map2(df$t,df$c,createDV) %>% unlist
  rdd <- rdlocrand::rdwinselect(df$rv,df$x, wmin = 0.05, wstep = 0.05, reps = 1000, quietly = T)
  dfw <- df %>% 
    filter(rv > rdd$w_left,
           rv < rdd$w_right)
  
  texts <- dfw$dv
  treatment <- dfw$t
  
  tokens <- word_tokenizer(texts)
  it <- itoken(tokens, progressbar = FALSE)
  vocab <- create_vocabulary(it)
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  
  X <- as.matrix(dtm)
  colnames(X) <- vocab$term
  sorted_order <- order(as.integer(colnames(X)))
  X <- X[, sorted_order]
  
  # Apply LASSO logistic regression
  # Here we use the rigorous LASSO approach (rigorous = TRUE)
  lasso_fit_rd <- rlassologit(y = treatment, x = X, post = FALSE, rigorous = TRUE)
  
  results <- matrix(NA,1,2)
  cm <- matrix(NA, 2,2)
  
  cm[1,1] = sum(lasso_fit_rd$coefficients[1:20]!=0) + sum(lasso_fit_rd$coefficients[101:120]!=0)
  cm[1,2] = 40 - cm[1,1]
  cm[2,1] = sum(lasso_fit_rd$coefficients[21:100]!=0)
  cm[2,2] = 80 - cm[2,1]
  
  precision <- cm[1, 1] / (cm[1, 1] + cm[2, 1])
  recall <- cm[1, 1] / (cm[1, 1] + cm[1, 2])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  results[1,1] <- f1_score
  
  texts <- df$dv
  treatment <- df$t
  
  tokens <- word_tokenizer(texts)
  it <- itoken(tokens, progressbar = FALSE)
  vocab <- create_vocabulary(it)
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  
  X <- as.matrix(dtm)
  colnames(X) <- vocab$term
  sorted_order <- order(as.integer(colnames(X)))
  X <- X[, sorted_order]
  
  # Apply LASSO logistic regression
  # Here we use the rigorous LASSO approach (rigorous = TRUE)
  lasso_fit_rd <- rlassologit(y = treatment, x = X, post = FALSE, rigorous = TRUE)
  
  cm[1,1] = sum(lasso_fit_rd$coefficients[1:20]!=0) + sum(lasso_fit_rd$coefficients[101:120]!=0)
  cm[1,2] = 40 - cm[1,1]
  cm[2,1] = sum(lasso_fit_rd$coefficients[21:100]!=0)
  cm[2,2] = 80 - cm[2,1]
  
  precision <- cm[1, 1] / (cm[1, 1] + cm[2, 1])
  recall <- cm[1, 1] / (cm[1, 1] + cm[1, 2])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  results[1,2] <- f1_score
  
  return(results)
}

safe_simulation_batch <- safely(simulation_batch, otherwise = NA)
results <- map(1:500, ~ safe_simulation_batch(5000))

results <- do.call(rbind,(map(results, 'result')))
colMeans(results, na.rm = T)
