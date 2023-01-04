# Trabalho 01 - ANALISANDO O INTERVALO DE CONFIANÇA DE UM BANCO DE DADOS EM R
#
# Universidade Federal do Amazonas
# Engenharia da Computação
# Faculdade de Tecnologia 
# IEE312 - Inferência Estatística I
#
# Equipe:
#   Clarissa Thaís Nascimento de Oliveira - 21952382
#   Fabrício da Costa Guimarães - 21950515
#   Laura Aguiar Martinho - 21952064
#   Lorena Bastos Amazonas - 21952638


# ------------------------------------------------------------------------------
# Limpando a memória

rm(list=ls())


# ------------------------------------------------------------------------------
# Importando a base de dados, previamente limpa, com dados padronizados
# e anomalias removidas
# ------------------------------------------------------------------------------

df <- read.csv(file='clean_data.csv')



# ------------------------------------------------------------------------------
# Intervalo de Confiança por Média e Desvio Padrão
# Variáveis Quantitativas
# ------------------------------------------------------------------------------

# Função para o cálculo
IC.media <- function(media, tam, sigma, conf=0.95){
  quantis <- qnorm(c((1-conf)/2, 1 - (1-conf)/2))
  ic <- media + quantis * sigma/sqrt(tam)
  return(ic)
}


# Média das Variáveis Quantitativas
# media_Idade <- mean(df$IDADE)
# media_INR <- mean(df$INR)
# media_BILIR <- mean(df$BILIR..TOTAL)
# media_ALBUMINA <- mean(df$Albumina)
# media_CHILDPUGH1 <- mean(df$CHILD.PUGH..)

# Variáveis para a tabela
variavel_quant <- c('Idade', 'INR', 'BILIR Total','Albumina', 'CHILD-PUGH.')

# Média de todas
media_quant <- c(mean(df$IDADE), mean(df$INR), mean(df$BILIR..TOTAL), mean(df$Albumina), mean(df$CHILD.PUGH..))

# Desvio Padrão de todas
desvio_padrao_quant <- c(sd(df$IDADE), sd(df$INR), sd(df$BILIR..TOTAL) ,sd(df$Albumina), sd(df$CHILD.PUGH..))

# Limite inferior do intervalo de confiança
intervalo_confianca_quant_inf <- c(IC.media(media=media_Idade, tam=1308,sigma=sd(df$IDADE), conf=0.95)[1],
                               IC.media(media=media_INR, tam=1308,sigma=sd(df$INR), conf=0.95)[1],
                               IC.media(media=media_BILIR, tam=1308,sigma=sd(df$BILIR..TOTAL), conf=0.95)[1],
                               IC.media(media=media_ALBUMINA, tam=1308,sigma=sd(df$Albumina), conf=0.95)[1],
                               IC.media(media=media_CHILDPUGH1, tam=1308,sigma=sd(df$CHILD.PUGH..), conf=0.95)[1])


#intervalo_confianca_quant_inf2 <- c(IC.media(media=media_quant, tam=1308,sigma=sd(desvio_padrao_quant), conf=0.95)[1])

# Limite superior do intervalo de confiança
intervalo_confianca_quant_sup <- c(IC.media(media=media_Idade, tam=1308,sigma=sd(df$IDADE), conf=0.95)[2],
                               IC.media(media=media_INR, tam=1308,sigma=sd(df$INR), conf=0.95)[2],
                               IC.media(media=media_BILIR, tam=1308,sigma=sd(df$BILIR..TOTAL), conf=0.95)[2],
                               IC.media(media=media_ALBUMINA, tam=1308,sigma=sd(df$Albumina), conf=0.95)[2],
                               IC.media(media=media_CHILDPUGH1, tam=1308,sigma=sd(df$CHILD.PUGH..), conf=0.95)[2])

# Tabela Final das Variáveis Quantitativas
tabela_intervalo_confianca_quantitativas <- data.frame(variavel_quant,media_quant,desvio_padrao_quant,
                                                       intervalo_confianca_quant_inf,intervalo_confianca_quant_sup)


# ------------------------------------------------------------------------------
# Intervalo de Confiança por Proporção
# Variáveis Qualitativas
# ------------------------------------------------------------------------------

# Função para o cálculo
IC.proporcao <- function(qtde, total, conf=0.95){
  inter <- prop.test(qtde, total, conf.level = conf)
  inter <- inter[6]$conf.int
  return(inter[1:2])
}

# Obtento a tag e a frequência de cara variável
vars_quali <- data.frame(df$SEXO, df$Encefalopatia, df$Ascite ,df$CHILD.PUGH, df$HBeAg, df$Anti.HBe, df$a.HD.TOTAL)
tag <- c()
frequencia <- c()

for (x in 1:length(vars_quali)) {
  aux <- data.frame(table(vars_quali[x]))
  
  for (y in aux[1]) {
    tag <- append(tag, y)
  }
  
  for (z in aux[2]) {
    frequencia <- append(frequencia, z)
  }
}

# Variáveis para a tabela
variavel_quali <- c('Sexo','Sexo',
                    'Encefalopatia','Encefalopatia','Encefalopatia',
                    'Ascite','Ascite','Ascite',
                    'CHILP-PUGH','CHILP-PUGH','CHILP-PUGH',
                    'HBeAg','HBeAg',
                    'AntiHBe','AntiHBe',
                    'aHD_Total','aHD_Total')

# Limite inferior e superior do intervalo de confiança
intervalo_confianca_quali_inf <- c()
intervalo_confianca_quali_sup <- c()
for (x in frequencia) {
  intervalo_confianca_quali_inf <- append(intervalo_confianca_quali_inf, IC.proporcao(x, 1308, conf=0.95)[1])
  intervalo_confianca_quali_sup <- append(intervalo_confianca_quali_sup, IC.proporcao(x, 1308, conf=0.95)[2])
}

# Tabela Final das Variáveis Qualitativas
tabela_intervalo_confianca_qualitativas <- data.frame(variavel_quali,tag,frequencia,
                                                      intervalo_confianca_quali_inf,intervalo_confianca_quali_sup)

