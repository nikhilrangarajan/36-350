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
  summ = summary(model)
  for (i in 23:28)
  {
    if (summ[[4]][i] <= cutoff) 
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
  return(hist(result))
}

run_simulation = function(n_trials, n, p, cutoff){
  
  result = c()
  for(i in 1:n_trials){
    
    val = generate_data(n,p)
    mat = val$covariates
    vec = val$responses
    output = model_select(mat,vec,cutoff)
    result = c(result, output)
  }
  write(result, file = "result.txt")
  return( make_plot("result.txt") )
  
}

allN = c(100, 1000,10000)

allP = c(10,20,50)

for(i in 1:length(allN)){
  
  for(j in 1:length(allP)){
    
    
    run_simulation(5, allN[i], allP[j], 0.05)
  }
}