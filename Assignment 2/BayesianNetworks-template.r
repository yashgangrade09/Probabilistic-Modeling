## Function to create a conditional probability table
## Conditional probability is of the form p(x1 | x2, ..., xk)
## varnames: vector of variable names (strings)
## -- NOTE: first variable listed will be x1, remainder will be parents, x2, ..., xk
## probs: vector of probabilities for the flattened probability table
## levelsList: a list containing a vector of levels (outcomes) for each variable
## See the BayesNetExamples.r file for examples of how this function works
createCPT = function(varnames, probs, levelsList)
{
  ## Check dimensions agree
  if(length(probs) != prod(sapply(levelsList, FUN=length)))
    return(NULL)

  ## Set up table with appropriate dimensions
  m = length(probs)
  n = length(varnames)
  g = matrix(0, m, n)

  ## Convert table to data frame (with column labels)
  g = as.data.frame(g)
  names(g) = varnames

  ## This for loop fills in the entries of the variable values
  k = 1
  for(i in n:1)
  {
    levs = levelsList[[i]]
    g[,i] = rep(levs, each = k, times = m / (k * length(levs)))
    k = k * length(levs)
  }

  return(data.frame(probs = probs, g))
}

## Build a CPT from a data frame
## Constructs a conditional probability table as above, but uses frequencies
## from a data frame of data to generate the probabilities.
createCPTfromData = function(x, varnames)
{
  levelsList = list()

  for(i in 1:length(varnames))
  {
    name = varnames[i]
    levelsList[[i]] = sort(unique(x[,name]))
  }

  m = prod(sapply(levelsList, FUN=length))
  n = length(varnames)
  g = matrix(0, m, n)

  ## Convert table to data frame (with column labels)
  g = as.data.frame(g)
  names(g) = varnames

  ## This for loop fills in the entries of the variable values
  k = 1
  for(i in n:1)
  {
    levs = levelsList[[i]]
    g[,i] = rep(levs, each = k, times = m / (k * length(levs)))
    k = k * length(levs)
  }

  ## This is the conditional probability column
  probs = numeric(m)
  numLevels = length(levelsList[[1]])
  skip = m / numLevels

  ## This chunk of code creates the vector "fact" to index into probs using
  ## matrix multiplication with the data frame x
  fact = numeric(ncol(x))
  lastfact = 1
  for(i in length(varnames):1)
  {
    j = which(names(x) == varnames[i])
    fact[j] = lastfact
    lastfact = lastfact * length(levelsList[[i]])
  }
  ## Compute unnormalized counts of subjects that satisfy all conditions
  a = as.matrix(x - 1) %*% fact + 1
  for(i in 1:m)
    probs[i] = sum(a == i)

  ## Now normalize the conditional probabilities
  for(i in 1:skip)
  {
    denom = 0 ## This is the normalization
    for(j in seq(i, m, skip))
      denom = denom + probs[j]
    for(j in seq(i, m, skip))
    {
      if(denom != 0)
        probs[j] = probs[j] / denom
    }
  }

  return(data.frame(probs = probs, g))
}

## Product of two factors
## A, B: two factor tables
##
## Should return a factor table that is the product of A and B.
## You can assume that the product of A and B is a valid operation.
productFactor = function(A, B)
{
  names_in_A = names(A);
  names_in_B = names(B);
  
  common_variable_list = intersect(names_in_A, names_in_B);
  common_variable_list = common_variable_list[common_variable_list != 'probs'];
  
  #inner join of the two tables
  new_table_merged = merge(A, B, by = common_variable_list);
  new_table_merged$probs <- new_table_merged$probs.x * new_table_merged$probs.y;
  ## Never use ->, made this mistake and spent an hour on debugging the code ##
  new_table_merged <- subset(new_table_merged, select = -c(probs.y, probs.x));
  # select = c(a, b) keeps the column a and b to make a table, on the other hand -c(a,b) will remove a and b columns and keep the rest intact #
  return(new_table_merged)
}

## Marginalize a variable from a factor
## A: a factor table
## margVar: a string of the variable name to marginalize
##
## Should return a factor table that marginalizes margVar out of A.
## You can assume that margVar is on the left side of the conditional.
marginalizeFactor = function(X, margVar)
{
  p1 = setdiff(names(X), c(margVar));
  if(isTRUE(all.equal(p1, names(X)))){
    return (X);
  }
  variables = setdiff(names(X), c("probs", margVar));
  templist = list();
  for(i in 1:length(variables)){
    templist[[i]] = X[, variables[i]];
  }
  X = aggregate(X$probs, by = templist, FUN = "sum");
  variables[length(variables)+1] = "probs";
  names(X) = variables;
  return (X)
}

## Marginalize a list of variables
## bayesnet: a list of factor tables
## margVars: a vector of variable names (as strings) to be marginalized
##
## Should return a Bayesian network (list of factor tables) that results
## when the list of variables in margVars is marginalized out of bayesnet.
marginalize = function(bayesnet, margVars)
{
  n = length(bayesnet);
  if(n>1){
    temp_p = productFactor(bayesnet[[1]], bayesnet[[2]]);
  }
  for(i in 3:length(bayesnet)){
    temp_p = productFactor(temp_p, bayesnet[[i]]);
  }
  bayesnet_new = marginalizeFactor(temp_p, margVars);
  return(bayesnet_new)
}

## Observe values for a set of variables
## bayesnet: a list of factor tables
## obsVars: a vector of variable names (as strings) to be observed
## obsVals: a vector of values for corresponding variables (in the same order)
##
## Set the values of the observed variables. Other values for the variables
## should be removed from the tables. You do not need to normalize the factors
## to be probability mass functions.
observe = function(bayesnet, obsVars, obsVals)
{
  n = length(bayesnet);
  for(i in 1:n){
    p1 = bayesnet[[i]];
    intersecting_variables = intersect(names(p1), obsVars);
    n2 = length(intersecting_variables);
    if(n2 != 0){
      for(j in 1:n2){
        p2 = p1[, intersecting_variables[j]];
        idx = match(intersecting_variables[j], obsVars);
        p1 = p1[p2 == obsVals[idx], ];
      }
    }
    bayesnet[[i]] = p1;
  }
  return(bayesnet)
}

## Run inference on a Bayesian network
## bayesnet: a list of factor tables
## margVars: a vector of variable names to marginalize
## obsVars: a vector of variable names to observe
## obsVals: a vector of values for corresponding variables (in the same order)
##
## This function should run marginalization and observation of the sets of
## variables. In the end, it should return a single joint probability table. The
## variables that are marginalized should not appear in the table. The variables
## that are observed should appear in the table, but only with the single
## observed value. The variables that are not marginalized or observed should
## appear in the table with all of their possible values. The probabilities
## should be normalized to sum to one.
infer = function(bayesnet, margVars, obsVars, obsVals)
{
  observed = observe(bayesnet, obsVars, obsVals);
  marginalized = marginalize(observed, margVars);
  marginalized$probs = marginalized$probs / sum(marginalized$probs);
  return(marginalized);
}
