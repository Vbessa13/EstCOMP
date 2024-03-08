# NOME: GUSTAVO VINÍCIUS ALBA
# MATRICULA: 11911BCC016

# EXERCICIO 1
# A
aves = read.table("aves.txt", header = TRUE, sep = ",")
head(aves)
tail(aves)
str(aves)
summary(aves)

# B
aves = aves[sample(1:nrow(aves)),]

# C
dinamarca = aves[aves$local == "dinamarca",]
noruega = aves[aves$local == "noruega",]
islandia = aves[aves$local == "islandia",]

## Média
media_asa_dinamarca = mean(dinamarca$comprimento_asa)
media_asa_noruega = mean(noruega$comprimento_asa)
media_asa_islandia = mean(islandia$comprimento_asa)
### O local com maior comprimento de asa médio é a dinamarca(37.73) e o menor é a islandia(22.10)

media_peso_dinamarca = mean(dinamarca$peso)
media_peso_noruega = mean(noruega$peso)
media_peso_islandia = mean(islandia$peso)
### A dinamarca tem um valor médio de peso muito superior aos outros(193.38), enquanto islandia tem
### o menor valor(90.49)


## Desvio Padrão
desvio_asa_dinamarca = sd(dinamarca$comprimento_asa)
desvio_asa_noruega = sd(noruega$comprimento_asa)
desvio_asa_islandia = sd(islandia$comprimento_asa)
### A noruega possui um maior desvio padrão para o comprimento de asa(2.24), indicando que os valores
### obtidos são mais irregulares. Já a dinamarca tem o menor valor(1.83) indicando que há mais regularidade
### dentre os dados obtidos

desvio_peso_dinamarca = sd(dinamarca$peso)
desvio_peso_noruega = sd(noruega$peso)
desvio_peso_islandia = sd(islandia$peso)
### Os valores de desvio obtidos para dinarmarca e noruega são bem próximos(~= 10) e são um pouco
### maiores que o da islandia(8.34), que se mostra a ter um peso mais regular entre as aves

# D
plot(
  aves$comprimento_asa, 
  aves$peso, 
  col = ifelse(aves$local == "dinamarca", "red", ifelse(aves$local == "noruega", "blue", "green")),
  pch = 20,
  cex = 1.5
)

# E
treino = aves[1:120,] # 150 x 0.8 = 120
testes = aves[121:150,]

# F
arvore = function(asa, peso) {
  if (peso >= 167) {
    return("dinamarca")
  }
  else if (asa < 27) {
    return("islandia")
  }
  else {
    return ("noruega")
  }
}

# G 
resultado = c()
for (i in 1:nrow(testes)) {
  resultado[i] = arvore(testes$comprimento_asa[i], testes$peso[i])
}

mean(testes$local == resultado)


# EXERCICIO 2
# A
dinamarca = aves[aves$local == "dinamarca",]
noruega = aves[aves$local == "noruega",]
islandia = aves[aves$local == "islandia",]

cor_dinamarca = cor(dinamarca$comprimento_asa, dinamarca$peso)
cor_noruega = cor(noruega$comprimento_asa, noruega$peso)
cor_islandia = cor(islandia$comprimento_asa, islandia$peso)
### Dinamarca = 0.9397
### Noruega = 0.9212
### Islandia = 0.9769
### O local com as variáveis mais correlacionadas é a Islandia

# B

reta = function(x, y) {
  n = length(x)
  somatorio1 = 0
  somatorio2 = 0
  somatorio3 = 0
  somatorio4 = 0
  for (i in 1:n) {
    somatorio1 = somatorio1 + (x[i] * y[i])
    somatorio2 = somatorio2 + (x[i])
    somatorio3 = somatorio3 + (y[i])
    somatorio4 = somatorio4 + (x[i] * x[i])
  }
  m = ((n * somatorio1) - (somatorio2 * somatorio3))/ ((n * somatorio4) - (somatorio1 * somatorio1))
  b = mean(y) - (m * mean(x))
  return(c(m, b))
}


# C
modelo = function(x) {
  m_b = reta(islandia$comprimento_asa, islandia$peso)
  y = m_b[2] + (x * m_b[1]);
  return (y);
}

# D
### Para uma variação de 0.5cm no comprimento de asa, haverá uma variação de 3.912302e-06 gramas no peso da ave


# E
### O Excelentíssimo Senhor David Attenborough está certo, já que o valor de comprimento de asa de 23cm está
### dentro do intervalo de dados usado para criar a regressão. Assim, por meio da equação da reta de regressão
### é possível estimar o peso da ave, que será de 90.495 gramas
modelo(23)














