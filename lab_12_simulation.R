generate_data = function(n, p){
  mat = matrix(rnorm(n * p, mean = 0, sd = 1), n, p)
  r = cov(mat)
  nex = rnorm(n)
  return (list(r, nex))
}

