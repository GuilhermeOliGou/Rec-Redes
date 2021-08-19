markov.main <- function(iteracoes_extras=100, m=150){
  transicoes = read.table("dado.txt")
  transicoes = transicoes*(1/6)
  transicoes - t(transicoes)
  
  inicial = c(1,0,0,0,0,0,0)
  
  p.inferior = inicial
  p.superior = transicoes %*% inicial
  pr.inferior = p.inferior[6]
  pr.superior = p.inferior[6] + 1 - p.superior[7]
  pr.final = (pr.inferior + pr.superior)/2
  
  for (i in 2:m) {
    
    p.inferior = p.superior
    p.superior = transicoes %*% p.inferior
    pr.inferior = p.inferior[6]
    pr.superior = p.inferior[6] + 1 - p.superior[7]
    pr.final = (pr.inferior + pr.superior)/2
    
  }
  
  for (i in 1:iteracoes_extras) {
    
    p.inferior = p.superior
    p.superior = transicoes %*% p.inferior
    pr.inferior = p.inferior[6]
    pr.superior = p.inferior[6] + 1 - p.superior[7]
    pr.final = (pr.inferior + pr.superior)/2
    
    print(pr.final)
    
  }
  
}