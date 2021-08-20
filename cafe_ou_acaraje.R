markov.main <- function(posicao, m=50){
  transicoes = as.matrix(read.table("dado.txt"))
  transicoes = transicoes*(1/6)
  transicoes = t(transicoes)
  
  inicial = as.matrix(c(1,0,0,0,0,0,0))
  
  p.inferior = inicial
  p.superior = transicoes %*% inicial
  pr.inferior = p.inferior[posicao]
  pr.superior = p.inferior[posicao] + 1 - p.superior[7]
  pr.final = (pr.inferior + pr.superior)/2
  
  for (i in 2:m) {
    
    p.inferior = p.superior
    p.superior = transicoes %*% p.inferior
    pr.inferior = pr.inferior + (p.inferior[posicao])
    pr.superior = pr.inferior + 1 - p.superior[7]
    pr.final = (pr.inferior + pr.superior)/2
    
    print(pr.final)
    
  }
  
}