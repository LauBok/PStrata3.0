get.stan.data <- function(S.formula, Y.formula, data){
  
  parse.formula_new <- function(formula, data) {
    symbols_AST <- function(AST) {
      if (is.name(AST))
        return (as.character(AST))
      else if (is.call(AST))
        return (unlist(unique(sapply(AST[-1], symbols_AST))))
      else 
        return (NULL)
    }
    
    LHS <- if(length(formula) == 3) formula[[2]] else NULL
    LHS_symbol <- symbols_AST(LHS)
    terms <- terms.formula(formula)
    has_intercept <- attr(terms, "intercept")
    model_matrix <- model.matrix(
      update.formula(formula, ~ . + 1), 
      dplyr::mutate_if(data, function(x) is.numeric(x) && var(x) != 0, scale)
    )
    return (list(
      formula = formula, 
      response = LHS_symbol,
      has_intercept = has_intercept,
      predictors = colnames(model_matrix),
      num_of_predictors = ncol(model_matrix),
      model_matrix = model_matrix
    ))
  }
  
  prse_fml_S <- parse.formula_new(S.formula, data)
  prse_fml_Y <- parse.formula_new(Y.formula, data)
  length_S <- length(prse_fml_S$response)
  
  df <- list(
    N = nrow(data), 
    PS = ncol(prse_fml_S$model_matrix),
    PG = ncol(prse_fml_Y$model_matrix),
    Z = dplyr::pull(data, prse_fml_S$response[1]),
    D = dplyr::select(data, prse_fml_S$response[2:length_S]),
    Y = dplyr::pull(data, prse_fml_Y$response[1]),
    XS = prse_fml_S$model_matrix,
    XG = prse_fml_Y$model_matrix
  )
  return(df)
}
