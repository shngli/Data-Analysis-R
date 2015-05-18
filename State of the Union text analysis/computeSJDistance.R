computeSJDistance =
function(tf, df, terms, logdf = TRUE, verbose = TRUE)
{
# terms - a character vector of all the unique words, length numTerms
# df - a numeric vector, length numTerms, number of docs that contains term
# tf - matrix, numTerms rows, numCols cols (number of documents)

  numTerms = nrow(tf)
  numDocs = ncol(tf)

  tfn =  t(tf)/colSums(tf)
  if (logdf) tfidfs = t(tfn) * (log(numDocs) - log(df))
  else  tfidfs = numDocs * ( t(tfn) / df)
  
  D.SJ = matrix(0, numDocs, numDocs)
  for(i in 1:(numDocs -1)) {
     for(j in (i+1):numDocs) { 
        D.SJ[i,j] = D.SJ[j,i] = D.JensenShannon(tfidfs[, i], tfidfs[, j])
     }
     if(verbose)
       print(i)  
  }
  return(D.SJ)
}

D.BasicKullbackLeibler = function(p, q)
{
  tmp = !(p == 0 | q == 0)
  p = p[tmp]
  q = q[tmp]

  sum(- p*log(q) + p*log(p) )
}

D.JensenShannon = function(p, q)
{
  T = 0.5 * (p + q)  
  0.5 * D.BasicKullbackLeibler(p, T) + 0.5 * D.BasicKullbackLeibler(q, T)
}  
