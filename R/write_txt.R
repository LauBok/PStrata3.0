write.txt <- function(Y.formula, Y.family, S, ER,
                      prior_intercept = prior_flat(),
                      prior_coefficient = prior_normal(),
                      prior_sigma = prior_inv_gamma(),
                      prior_alpha = prior_inv_gamma(),
                      prior_lambda = prior_inv_gamma(),
                      prior_theta = prior_normal(),
                      filename = NULL){
  
  get.P <- function(myformula) {
    symbols_AST <- function(AST) {
      if (is.name(AST))
        return (as.character(AST))
      else if (is.call(AST))
        return (unlist(unique(sapply(AST[-1], symbols_AST))))
      else 
        return (NULL)
    }
    
    LHS <- if(length(myformula) == 3) myformula[[2]] else NULL
    LHS_symbol <- symbols_AST(LHS)
    return (length(LHS_symbol)-1)
  }
  
  change.base <- function(stratum, P){
    nchar_raw <- 32
    result_raw <- paste(rev(as.integer(intToBits(stratum))), collapse="")
    result <- substr(result_raw, start=nchar_raw-2*P+1, stop=nchar_raw)
    return(result)
  }
  
  prior_names <- c("intercept", "coefficient", "sigma", 
                   "alpha", "lambda", "theta")
  
  if (!is.null(filename))
    fileConn <- file(filename)
  
  P <- get.P(Y.formula)
  lines <- c()
  S_line <- 0
  G_line <- 0
  
  for(stratum in S){
    S_bin <- change.base(stratum, P)
    S_bin0 <- substr(S_bin, start=1, stop=P)
    D0 <- strtoi(S_bin0, base = 2)
    S_bin1 <- substr(S_bin, start=P+1, stop=P*2)
    D1 <- strtoi(S_bin1, base = 2)
    
    line0 <- paste0("SZDG ", S_line, " 0 ", D0, " ", G_line)

    if(!stratum %in% ER) G_line <- G_line + 1
    
    line1 <- paste0("SZDG ", S_line, " 1 ", D1, " ", G_line)
    
    lines <- c(lines, line0, line1)
    
    S_line <- S_line + 1
    G_line <- G_line + 1
  }
  lines <- c(lines, "")
  
  family_line <- paste0("Y ", Y.family$family, " ", Y.family$link)
  lines <- c(lines, family_line, "")
  
  for(name in prior_names){
    prior_curr <- eval(parse(text = paste0("prior_", name)))
    prior_args <- prior_curr$args
    prior_line <- paste0("prior ", name, " ", prior_curr$name, " ",
                         length(prior_args), " ",
                         paste0(unlist(prior_args), collapse = " "))
    lines <- c(lines, prior_line)
  }
  
  if (!is.null(filename)) {
    writeLines(lines, fileConn)
    close(fileConn)
  }
  return (lines)
}
