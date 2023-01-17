#install.packages("dplyr")

### pacotes utilizados
library(dplyr)
library(ggplot2)

### Leitura de dados
dados = read.csv("dados_shiny_2022081822.csv", sep = ",", dec = ".", 
                 check.names = F)
colnames(dados)

###### Filtros ######
# - Modelo
# - Ano
# - UF
# - Acessorios: 
# "AIR BAG"               "ALARME"               
# "AR CONDICIONADO"       "BLINDADO"             
# "CÂMERA DE RÉ"          "DIREÇÃO HIDRÁULICA"   
# "SENSOR DE RÉ"          "SOM"                  
# "TRAVA ELÉTRICA"        "VIDRO ELÉTRICO" 

modelo = 'corolla xei 16v'
uf = c('mt', 'se')

dados_select = dados %>% filter(MODELO == modelo & UF %in% uf)


#valor medio ao logo do tempo
mediana_data = dados_select %>%
  group_by(DATA_COLETA_METADADOS, UF) %>%
  summarise(mediaValor = median(VALOR))

mediana_data %>% 
  ggplot() +
    geom_line(aes(x =  DATA_COLETA_METADADOS, y = mediaValor,
                  group = UF, color = UF), size = 1) + 
  ggtitle('Média de Valor')

# variacao de valores por uf
# valor minimo, maximo, media, mediana, variacao(amplitude) e outline
dados_select %>%
ggplot() +
  geom_boxplot(aes(x = UF, y = VALOR, fill = UF)) +
  ggtitle("Variação de preço por UF")

# Variacao de Km, valor e UF
dados_select %>%
  ggplot() + 
  geom_point(aes(x = QUILOMETRAGEM, y = VALOR, color = UF)) +
  ggtitle("Distribuição do valor e KM por UF")

# Frequencia dos cambio dos automaveis
# grafico de barras vertical ou horizontal e pizza
freq_cambio = dados_select %>%
  group_by(CÂMBIO) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) * 100) %>%
  mutate(ypos = cumsum (prop) - 0.5 * prop) ## angulo do gráfico

freq_cambio %>%
  ggplot(aes(x = '', y = prop, fill = CÂMBIO)) +
  geom_bar(stat = 'identity' ) +
  coord_polar('y', start = 0) +
  theme_void() + theme(legend.position = 'none') + 
  geom_text(aes(y = ypos, label = paste(CÂMBIO, '\n', round(prop, 2), '%')),
            color ='white', size = 6) +
  ggtitle('Quantidade por Câmbio')

# Variacao de preco e valor por tipo de anuncio
# Frequencia pro direcao (hidraulica, eletrica, macanica, etc)

#Frequecia por cor
dados_select %>%
  group_by(COR) %>%
  summarise(QTD = n()) %>%
  ggplot() + 
    geom_bar(aes(x = reorder(COR, QTD), y = QTD), stat =  'identity') +
    xlab('Cor') + ylab("Quantidade") + ggtitle('Quantidade por Cor')
