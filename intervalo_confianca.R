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
media_Idade <- mean(df$IDADE)
media_INR <- mean(df$INR)
media_BILIR <- mean(df$BILIR..TOTAL)
media_ALBUMINA <- mean(df$Albumina)
media_CHILDPUGH1 <- mean(df$CHILD.PUGH..)

# Variáveis para a tabela
variavel_quant <- c('Idade', 'INR', 'BILIR Total','Albumina', 'CHILD-PUGH.')

# Média de todas
media_quant <- c(media_Idade, media_INR, media_BILIR, media_ALBUMINA , media_CHILDPUGH1) 

# Desvio Padrão de todas
desvio_padrao_quant <- c(sd(df$IDADE),sd(df$INR),sd(df$BILIR..TOTAL),sd(df$Albumina),sd(df$CHILD.PUGH..))

# Limite inferior do intervalo de confiança
intervalo_confianca_quant_inf <- c(IC.media(media=media_Idade, tam=1308,sigma=sd(df$IDADE), conf=0.95)[1],
                               IC.media(media=media_INR, tam=1308,sigma=sd(df$INR), conf=0.95)[1],
                               IC.media(media=media_BILIR, tam=1308,sigma=sd(df$BILIR..TOTAL), conf=0.95)[1],
                               IC.media(media=media_ALBUMINA, tam=1308,sigma=sd(df$Albumina), conf=0.95)[1],
                               IC.media(media=media_CHILDPUGH1, tam=1308,sigma=sd(df$CHILD.PUGH..), conf=0.95)[1])

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

# Obtento a frequência de cara variável
tags_sexo <- data.frame(table(df$SEXO))$Var1
freq_sexo <- data.frame(table(df$SEXO))$Freq

tags_encefalopatia <- data.frame(table(df$Encefalopatia))$Var1
freq_encefalopatia <- data.frame(table(df$Encefalopatia))$Freq

tags_ascite <- data.frame(table(df$Ascite))$Var1
freq_ascite <- data.frame(table(df$Ascite))$Freq

tags_CHILD_PUGH <- data.frame(table(df$CHILD.PUGH))$Var1
freq_CHILD_PUGH <- data.frame(table(df$CHILD.PUGH))$Freq

tags_HBeAg <- data.frame(table(df$HBeAg))$Var1
freq_HBeAg <- data.frame(table(df$HBeAg))$Freq

tags_AntiHBe <- data.frame(table(df$Anti.HBe))$Var1
freq_AntiHBe <- data.frame(table(df$Anti.HBe))$Freq

tags_aHD <- data.frame(table(df$a.HD.TOTAL))$Var1
freq_aHD <- data.frame(table(df$a.HD.TOTAL))$Freq

# Variáveis para a tabela
variavel_quali <- c('Sexo','Sexo', 
                    'Encefalopatia','Encefalopatia','Encefalopatia', 
                    'Ascite','Ascite','Ascite',
                    'CHILP-PUGH','CHILP-PUGH','CHILP-PUGH', 
                    'HBeAg','HBeAg', 
                    'AntiHBe','AntiHBe',
                    'aHD_Total','aHD_Total')

# Etiqueta de cada variável
tag <- c(tags_sexo[1],tags_sexo[2],
         tags_encefalopatia[1], tags_encefalopatia[2], tags_encefalopatia[3],
         tags_ascite[1],tags_ascite[2],tags_ascite[3],
         tags_CHILD_PUGH[1],tags_CHILD_PUGH[2],tags_CHILD_PUGH[3],
         tags_HBeAg[1],tags_HBeAg[2],
         tags_AntiHBe[1],tags_AntiHBe[2],
         tags_aHD[1],tags_aHD[2])

# Frequência de todas
frequencia <- c(freq_sexo[1],freq_sexo[2],
                freq_encefalopatia[1], freq_encefalopatia[2], freq_encefalopatia[3],
                freq_ascite[1],freq_ascite[2],freq_ascite[3],
                freq_CHILD_PUGH[1],freq_CHILD_PUGH[2],freq_CHILD_PUGH[3],
                freq_HBeAg[1],freq_HBeAg[2],
                freq_AntiHBe[1],freq_AntiHBe[2],
                freq_aHD[1],freq_aHD[2])

# Limite inferior do intervalo de confiança
intervalo_confianca_quali_inf <- c(IC.proporcao(frequencia[1], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[2], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[3], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[4], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[5], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[6], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[7], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[8], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[9], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[10], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[11], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[12], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[13], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[14], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[15], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[16], 1308, conf=0.95)[1],
                                   IC.proporcao(frequencia[17], 1308, conf=0.95)[1])

# Limite superior do intervalo de confiança
intervalo_confianca_quali_sup <- c(IC.proporcao(frequencia[1], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[2], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[3], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[4], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[5], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[6], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[7], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[8], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[9], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[10], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[11], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[12], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[13], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[14], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[15], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[16], 1308, conf=0.95)[2],
                                   IC.proporcao(frequencia[17], 1308, conf=0.95)[2])

# Tabela Final das Variáveis Qualitativas
tabela_intervalo_confianca_qualitativas <- data.frame(variavel_quali,tag,frequencia,
                                                      intervalo_confianca_quali_inf,intervalo_confianca_quali_sup)

