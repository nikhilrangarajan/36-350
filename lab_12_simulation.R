generate_data = function(n, p){
  mat = matrix(rnorm(n * p, mean = 0, sd = 1), n, p)
  r = cov(mat)
  nex = rnorm(n)
  return (list(r, nex))
}

model_select = function(covariates, responses, cutoff)
{
  index = c() 
  df = data.frame(cov = covariates, res = responses)
  model = lm(formula = res ~ cov.1 + cov.2 + cov.3 + cov.4 + cov.5 + cov.6, data = df)
  sum1 = summary(model)
  for (i in 23:28)
  {
    if (sum1[[4]][i] <= cutoff) 
    {
      index = c(index, (i - 22)) #index of the column to keep
    }
  }
  if (length(index) == 0)
  {
    return(index)
  }
  lm_new = lm(formula = responses ~ covariates[, index])
  mod.sum = summary(lm_new)
  return(mod.sum$coefficients[,4])
}

make_plot = function(datapath){
  result = readLines(datapath)
  return (hist(result))
}

run_simulation = function(n_trials, n, p, cutoff){
  
  result = c()
  for(i in 1:n_trials){
    
    val = generate_data(n,p)
    mat = val$covariates
    vec = val$responses
    op = model_select(mat,vec,cutoff)
    result = c(result, op)
  }
  write(result, file = "res.txt")
  return( make_plot("res.txt") )
  
}

N = c(100, 1000,10000)
P = c(10,20,50)
for(i in 1:length(N)){
  for(j in 1:length(P)){
    run_simulation(5, N[i], P[j], 0.05)
  }
}