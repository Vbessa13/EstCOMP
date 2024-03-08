# EXERCICIO 01

# e^(2x + x^2) de -1 até 2

n1 = 10000

densidade = function(x) {
  return(exp(2 * x + x^2))
}

EstimativaIntegral = function (a, b) {
  x = runif(n1, a, b)
  g = densidade(x)
  esperanca = mean(g)
  return ((b - a) * esperanca)
}
EstimativaIntegral(-1, 2)

# EXERCICIO 02
n2 = 10000

nf = 20

figurinhas = function(N) {
  todas_figurinhas = c()
  sucessos = 0
  tentativas = 0
  while (sucessos < N) {
    figurinha_tirada = sample(1:N, 1, replace = TRUE)
    if (!(figurinha_tirada %in% todas_figurinhas)) {
      sucessos = sucessos + 1
    }
    tentativas = tentativas + 1
    todas_figurinhas = c(todas_figurinhas, figurinha_tirada)
  }
  return(tentativas)
}

# Para completar o album de 20 figurinhas, serão necessárias x figurinhas:
x = c()
for (i in 1:n2) {
  x[i] = figurinhas(nf)
}
mean(x)
# A probabilidade de ser menor ou igual à 30:
mean(x <= 30)

estimativa_esperanca = function (numero) {
  x = c()
  for (i in 1:n2) {
    x[i] = figurinhas(numero)
  }
  mean(x)
}

# EXERCICIO 03
n3 = 10000

inicio = sample(1:19, size = 1, replace = TRUE)

precipicio = function(L) {
  andadas = c()
  for (i in 1:n3) {
    ponto_atual = L
    while (TRUE) {
      ponto_atual <- ponto_atual + sample(c(-1, 1), 1) # Sorteia se vai pra direita ou esquerda
      if (ponto_atual == 20) {
        andadas[i] = FALSE
        break
        # Se chegar em 20, estará em casa e o programa para
      }
      else if (ponto_atual == 0) {
        andadas[i] = TRUE
        break
        # Se chegar em 0, cairá no precipicio e o programa para
      }
    }
  }
  return(mean(andadas)) 
}

valores_y = c()
for (i in 1:19) {
  valores_y[i] = precipicio(i)
}

plot(
  x = c(1:19), 
  y = valores_y, 
  ylab = "Probabilidade de cair no Precipicio", 
  xlab = "Posição Inicial",
  xaxp = c(1,19,9),
  pch = 16,
  col = "orange"
)
axis(side=1, at=c(1:19))

# Eu tentei deixar o gráfico o mais bonito que pude ;-;










