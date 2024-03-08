# EXERCICIO 04
n4 = 100000

x = c()
for (i in 1:n4) {
  soma = 0
  contador = 0
  while (soma <  1) {
    u = runif(1, 0, 1)
    soma = soma + u
    contador = contador + 1
  }
  x[i] = contador
}

mean(x)
mean(x == 3)

# EXERCICIO 05
n5 = 100000

pA = pB = pC = pD = c()

# Verificar quantos valores n tem no vetor C
quantidade = function(n, C) {
  return (sum(C == n))
}

for (i in 1:n5) {
  # %% pra pegar os elementos do mesmo conjunto
  # + 1 -> facilitar a visualização, se não 0 seria às, 1 seria 2...
  cartas_tiradas = (sample(1:52, 5, replace=FALSE) %% 13) + 1
  C = c()
  for (j in 1:5) {
    # Irá formar um vetor com números de 1 à 4
    # Se for 1 -> elemento único
    # Se for 2 -> elemento repetido 2 vezes
    # Se for 3 -> elemento repetido 3 vezes
    # Se for 4 -> elemento repetido 4 vezes
    check = cartas_tiradas[j] == cartas_tiradas
    C[j] = sum(check == TRUE)
  }
  pA[i] = (quantidade(2, C) == 2 && quantidade(1, C) == 3)
  pB[i] = (quantidade(2, C) == 4)
  pC[i] = (quantidade(3, C) == 3 && quantidade(2, C) == 2)
  pD[i] = (quantidade(4, C) == 4 && quantidade(1, C) == 1)
    
}

mean(pA) * 100
mean(pB) * 100
mean(pC) * 100
mean(pD) * 100









