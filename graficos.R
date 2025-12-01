###bbibliometria graficos personalizados ####
library(ggplot2)
library(readr)


### graficos unificados de produção ######

library(ggplot2)
library(dplyr)
library(ggpmisc)
# Pacotes necessários
library(ggplot2)
library(ggpmisc)


# Data
data <- data.frame(
  Year = 2015:2025,
  annual_production = c(8, 8, 14, 17, 25, 19, 30, 35, 42, 64, 86),
  cumulative_frequency = c(8, 16, 30, 47, 72, 91, 121, 156, 198, 262, 348)
)

# Pearson correlation coefficients (R)
R_annual <- cor(data$Year, data$annual_production)
R_cumulative <- cor(data$Year, data$cumulative_frequency)

# Plot
ggplot(data, aes(x = Year)) +
  # Annual production line
  geom_line(aes(y = annual_production, color = "Annual Production"), size = 1.2) +
  geom_point(aes(y = annual_production, color = "Annual Production"), size = 2.5) +
  
  # Cumulative frequency line (rescaled)
  geom_line(aes(y = cumulative_frequency / max(cumulative_frequency) * max(annual_production),
                color = "Cumulative Frequency"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = cumulative_frequency / max(cumulative_frequency) * max(annual_production),
                 color = "Cumulative Frequency"), size = 2.5) +
  
  # Add correlation text (R values)
  annotate("text", x = 2015, y = 85,
           label = paste0("R² = ", round(R_annual, 3)),
           color = "steelblue", hjust = 0, size = 5) +
  annotate("text", x = 2015, y = 80,
           label = paste0("R² = ", round(R_cumulative, 3)),
           color = "darkblue", hjust = 0, size = 5) +
  
  # Axis scales
  scale_y_continuous(
    name = "Annual number of articles",
    sec.axis = sec_axis(~ . * max(data$cumulative_frequency) / max(data$annual_production),
                        name = "Cumulative number of articles")
  ) +
  scale_x_continuous(breaks = seq(2015, 2025, 1)) +
  
  # Colors
  scale_color_manual(
    values = c("Annual Production" = "steelblue", "Cumulative Frequency" = "darkblue"),
    name = ""
  ) +
  
  # Labels and theme
  labs(title = "",
       x = "Year") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "steelblue", face = "bold"),
    axis.title.y.right = element_text(color = "darkblue", face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )



############ dados de média de citação por ano ####
##importando banco de dados 

dados_citacao<-dados_finais_R_31_10
# Plot básico com ggplot2
ggplot(dados_citacao, aes(x = Year, y = MeanTCperYear)) +
  geom_line(color = "steelblue", size = 1) + 
  scale_x_continuous(breaks = breaks_width(1), 
                     labels = label_number(accuracy = 1))+
  scale_y_continuous(breaks = breaks_width(1), 
                     labels = label_number(accuracy = 1))+ # linha
  geom_point(aes(size = N, color = CitableYears), alpha = 0.8) +  # pontos
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  scale_size_continuous(range = c(2,6)) +
  labs(
    title = "",
    x = "Year",
    y = "Total average number of citations per year",
    color = "Citable Years",
    size = "Number of Articles"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


############### dados de produção anual###########
## importando dados

##### produção anual ###
dados_anos<-dados_finais_R_31_10

#### produção anual citação ###
ggplot(dados_anos, aes(x = Year, y = Articles)) +
  geom_line(color = "steelblue", size = 1) +
  scale_x_continuous(breaks = breaks_width(1), 
                     labels = label_number(accuracy = 1))+
  scale_y_continuous(breaks = breaks_width(10), 
                     labels = label_number(accuracy = 1))+
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  scale_size_continuous(range = c(2,6)) +
  labs(
    title = "",
    x = "Year",
    y = "Total number of articles per year",
    color = "Citable Years",
    size = "Number of Articles"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

#### revistas mais relevantes por total de artigos ####
library("forcats")
revistas<-dados_bibliometria_R
revistas <- revistas %>%
  mutate(Sources = factor(Sources, levels = Sources[order(Articles, decreasing = F)]))

# Criando o gráfico
g1<-ggplot(revistas, aes(x = Articles, y = Sources, fill = Articles)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Number of Articles", y = "Sources") +
  geom_text(aes(label = Articles), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal()


############### revista mais relevante por fator H ######
revistas_H<-dados_finais_R_31_10
revistas_H <- revistas_H %>%
  mutate(Source = factor(Source, levels = Source[order(h_index, decreasing = F)]))

# Criando o gráfico
g2<-ggplot(revistas_H, aes(x = h_index, y = Source, fill = h_index)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "h index", y = "Sources") +
  geom_text(aes(label = h_index), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal()

######### produção por revista ao longo dos anos ############
library(reshape2)
revista_anos<- dados_finais_R_31_10
dados_long <- melt(revista_anos, id.vars = "Year", variable.name = "Source", value.name = "Articles")

ggplot(dados_long, aes(x = Year, y = Articles, color = Source)) +
  geom_line(size = 1.5) +  # linha mais grossa
  scale_color_brewer(palette = "Dark2") +  # cores diferentes por Source
  scale_x_continuous(breaks = seq(min(dados_long$Year), max(dados_long$Year), by = 1)) +  # apenas inteiros
  labs(
    title = "",
    x = "Year",
    y = "Cumulative frequency of articles",
    color = "Source"  # usa 'color' no lugar de 'fill' já que usamos geom_line
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

####
autor_relevante_n<-dados_bibliometria_R
autor_relevante_n <- autor_relevante_n %>%
  mutate(Authors = factor(Authors, levels = Authors[order(Articles, decreasing = F)]))

ggplot(autor_relevante_n, aes(x = Articles, y = Authors, fill = Articles)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Number of Articles", y = "Authors") +
  geom_text(aes(label = Articles), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal()

### produção de autor ao longo do tempo ####
library(patchwork)
autor_tempo<-dados_finais_R_31_10

ggplot(autor_tempo, aes(x = year, y = reorder(Author, desc(Author)))) +
  geom_line(aes(group = Author), color = "gray80", size = 0.6) +
  geom_point(aes(size = TC, color = TCpY), alpha = 0.8) +
  scale_size_continuous(name = "TC", range = c(3, 10)) +
  scale_color_gradient(low = "#9ecae1", high = "#08306b", name = "TCpY") +
  theme_minimal(base_size = 12) +
  labs(
    title = "",
    x = "Year",
    y = "Author"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#### autor H ###
autor_H<-dados_bibliometria_R
autor_H <- autor_H %>%
  mutate(Author = factor(Author, levels = Author[order(h_index, decreasing = F)]))

# Criando o gráfico
ggplot(autor_H, aes(x = h_index, y = Author, fill = h_index)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "h index", y = "Sources") +
  geom_text(aes(label = h_index), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal()
### AFILIAÇÃO ####
afiliacao<-dados_bibliometria_R
afiliacao <- afiliacao %>%
  mutate(Affiliation = factor(Affiliation, levels = Affiliation[order(Articles, decreasing = F)]))

# Criando o gráfico
ggplot(afiliacao, aes(x = Articles, y = Affiliation, fill = Articles)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Total of articles", y = "Affiliation") +
  geom_text(aes(label = Articles), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal()

### afiliação  tempo ####
library("reshape2")
afiliação_tempo<-dados_finais_R_31_10
ggplot(afiliação_tempo, aes(x = Year, y = Articles, color = Affiliation)) +
  geom_line(size = 1.5) +  # linha mais grossa
  scale_color_brewer(palette = "Paired") +  # cores diferentes por Source
  scale_x_continuous(breaks = seq(min(dados_long$Year), max(dados_long$Year), by = 1)) +  # apenas inteiros
  labs(
    title = "",
    x = "Year",
    y = "Cumulative frequency of articles",
    color = "Affiliation"  # usa 'color' no lugar de 'fill' já que usamos geom_line
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#### grafico de n de artigos atraves dos anos 2 ####
dados_df <- afiliação_tempo %>%
  mutate(
    Year = as.integer(Year),
    Articles = as.integer(Articles),
    Affiliation = as.factor(Affiliation)
  )
# A linha abaixo (e a próxima) não é estritamente necessária para o gráfico, 
# mas é útil para a análise.
num_afiliacoes <- n_distinct(dados_df$Affiliation)
nomes_afiliacoes <- levels(dados_df$Affiliation)

grafico_producao <- ggplot(dados_df, aes(x = Year, y = Articles, color = Affiliation, group = Affiliation)) +
  
  # Adicionar as linhas
  geom_line(linewidth = 1) +
  
  # Adicionar os pontos
  geom_point(size = 2) +
  
  # 🎯 CORREÇÃO CRÍTICA AQUI: 
  # Use scale_color_discrete() ou scale_color_manual() (se cores definidas) 
  # Se quiser cores bonitas e escaláveis, use scale_color_viridis_d()
  scale_color_discrete() + # Garante que haja cores suficientes para todas as afiliações
  # OU:
  # scale_color_viridis_d(option = "D") + 
  
  # Configurar os rótulos e título
  labs(
    title = "", # Sugestão de título
    x = "Year",
    y = "Total number of Published Articles",
    color = "Affiliation"
  ) +
  
  # Garantir que o eixo X exiba todos os anos inteiros
  scale_x_continuous(breaks = unique(dados_df$Year)) +
  
  # Melhorar a aparência geral do gráfico
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centralizado
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotação dos anos para melhor leitura
  )




####
# 6. Exibir o gráfico
library(ggplot2)
library(tidyr)
library(dplyr)



#########corespondente_pais###########
dados_long_pais <- dados_finais_R_31_10 %>%
  pivot_longer(
    cols = c(SCP, MCP), # Colunas a serem pivotadas
    names_to = "Type",   # Novo nome para a coluna de categorias (SCP/MCP)
    values_to = "Articles" # Novo nome para a coluna de valores
  )

####

country_totals <- dados_long_pais %>%
  group_by(Country) %>%
  summarise(Total_Articles = sum(Articles)) %>%
  arrange(desc(Total_Articles)) # Ordena do maior total para o menor

# 2. Extrair a lista de países na ordem correta
ordered_countries <- country_totals$Country

# 3. Aplicar a ordem correta ao fator 'Country' no DataFrame longo
dados_ordenados <- dados_long %>%
  mutate(Country = factor(Country, levels = ordered_countries))

# --- Seção 5: Criar o Gráfico de Barras Empilhadas (Inalterada) ---
grafico_barras_empilhadas <- ggplot(dados_ordenados, aes(x = Country, y = Articles, fill = Type)) +
  
  geom_bar(stat = "identity", position = "stack") +
  
  geom_text(
    aes(label = Articles), 
    position = position_stack(vjust = 0.3), 
    color = "white",                       
    size = 3
  ) +
  
  labs(
    title = "",
    x = "País",
    y = "Nº of articles",
    fill = "Type of Collaboration"
  ) +
  
  scale_fill_manual(values = c("SCP" = "darkblue", "MCP" = "lightblue")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 1, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  text(grafico_barras_empilhadas, dados_ordenados$Articles+0.4 , paste("n: ", dados_ordenados$Articles, sep="") ,cex=1)

# 6. Exibir o gráfico
print(grafico_barras_empilhadas)

#### #####  pais tempo ############## ###
pais_tempo<-dados_bibliometria_R

# 3. Pré-processamento dos dados
# Garantir que 'Year' seja numérico (ou inteiro) e 'Affiliation' seja um fator.
dados_df <- pais_tempo %>%
  mutate(
    Year = as.integer(Year),
    Articles = as.integer(Articles),
    Affiliation = as.factor(Country)
  )

nomes_afiliacoes <- levels(dados_df$Country)
# 5. Gerar o Gráfico de Linha com ggplot2
grafico_pais_tempo <- ggplot(dados_df, aes(x = Year, y = Articles, color = Country, group = Country)) +
  
  # Adicionar as linhas
  geom_line(linewidth = 1) +
  
  # Adicionar os pontos
  geom_point(size = 2) +
  
  # Aplicar a paleta de cores personalizada
  scale_color_discrete() +
  
  # Configurar os rótulos e título
  labs(
    title = "",
    x = "Year",
    y = "Total number of Published Articles",
    color = "Country"
  ) +
  
  # Garantir que o eixo X exiba todos os anos inteiros
  scale_x_continuous(breaks = unique(dados_df$Year)) +
  
  # Melhorar a aparência geral do gráfico
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centralizado
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotação dos anos para melhor leitura
  )

# 6. Exibir o gráfico
print(grafico_pais_tempo)

### total e media de citação por pais #####

citacao_pais<-dados_finais_R_31_10
ggplot(citacao_pais, aes(x=TC, y=Average_Article_Citations, label=Country)) +
  geom_point(aes(size=TC, color=Country), alpha=0.7) +
  geom_text(vjust=-1, size=3) +
  scale_size_continuous(range=c(3,10)) +
  labs(x="Total Citations", y="Average Citations per Article", 
       title="Citações por País", size="Total Citations") +
  theme_minimal()


#### linhas ###

# Dados
data <- data.frame(
  Country = c("CHINA","USA","THAILAND","INDIA","EGYPT","MALAYSIA","MEXICO","BRAZIL","KOREA","AUSTRALIA"),
  TC = c(2865,701,593,513,342,251,235,224,210,163),
  Average_Citations = c(13.8,25,15.6,9.5,12.7,15.7,9.8,10.7,21,12.5)
)
library(ggplot2)
# Criar gráfico com dois eixos
ggplot(citacao_pais, aes(x = reorder(Country, -TC))) +
  geom_line(aes(y = TC, group=1, color="TC"), size=1.2) +
  geom_point(aes(y = TC, color="TC"), size=3) +
  geom_line(aes(y = Average_Article_Citations * 100, group=1, color="Average_Article_Citations"), size=1.2) +
  geom_point(aes(y = Average_Article_Citations * 100, color="Average_Article_Citations"), size=3) +
  scale_y_continuous(
    name = "Total Citations (TC)",
    sec.axis = sec_axis(~./100, name="Average Citations per Article")
  ) +
  scale_color_manual(values = c("TC" = "darkblue", "Average Citations" = "lightblue")) +
  labs(x="Country", color="Metric",
       title="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



#### palavras todas ###
palavras_todas<-dados_finais_R_31_10
palavras_todas <- palavras_todas %>%
  mutate(Words = factor(Words, levels = Words[order(Occurrences, decreasing = F)]))

ggplot(palavras_todas, aes(x = Occurrences, y = Words, fill = Occurrences)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Number of Occurrences", y = " All keyWords") +
  geom_text(aes(label = Occurrences), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal()


#########frequencia de ocorrencia de palavras por ano #####
palavras_tempo<-dados_finais_R_31_10
dados_long <- melt(palavras_tempo, id.vars = "Year", variable.name = "Word", value.name = "Articles")

ggplot(dados_long, aes(x = Year, y = Articles, color = Word)) +
  geom_line(size = 1.5) +  # linha mais grossa
  scale_color_brewer(palette = "Paired") +  # cores diferentes por Source
  scale_x_continuous(breaks = seq(min(dados_long$Year), max(dados_long$Year), by = 1)) +  # apenas inteiros
  labs(
    title = "",
    x = "Year",
    y = "Frequency of occurrence of words over time",
    color = "Word"  # usa 'color' no lugar de 'fill' já que usamos geom_line
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

##### frenquencia cumulativa de palavras ao longo dos anos
palavras_anos<-dados_finais_R_31_10


dados_long <- melt(palavras_anos, id.vars = "Year", variable.name = "Word", value.name = "Articles")

ggplot(dados_long, aes(x = Year, y = Articles, color = Word)) +
  geom_line(size = 1.5) +  # linha mais grossa
  scale_color_brewer(palette = "Paired") +  # cores diferentes por Source
  scale_x_continuous(breaks = seq(min(dados_long$Year), max(dados_long$Year), by = 1)) +  # apenas inteiros
  labs(
    title = "",
    x = "Year",
    y = "Frequency of occurrence of accumulated words",
    color = "Word"  # usa 'color' no lugar de 'fill' já que usamos geom_line
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




#### trend topics palavras ####

trend_palavras<-dados_finais_R_31_10




# 3. Preparação de dados para plotagem
# Ordenar os termos pela Mediana (os mais antigos no topo)
datos <- trend_palavras%>%
  arrange(desc(Median), desc(Frequency))%>%
  mutate(Term = factor(Term, levels = Term))

# Pivotear (transformar) os dados para o formato 'longo' para usar ggplot2
datos_largo <- datos %>%
  pivot_longer(
    cols = c(Q1, Median, Q3),
    names_to = "Quartile",
    values_to = "Year"
  ) %>%
  mutate(
    Quartile = factor(
      Quartile,
      levels = c("Q1", "Median", "Q3"),
      labels = c("Q1 (25%)", "Median (50%)", "Q3 (75%)")
    )
  )
# 3. Criação do gráfico (Dot Plot de Rango Horizontal)
grafico_final <- ggplot(datos_largo, aes(y = Term, x = Year)) +
  
  # A. Linhas conectando Q1 a Q3
  geom_segment(
    data = datos,
    aes(x = Q1, xend = Q3, y = Term, yend = Term),
    color = "gray60", linewidth = 1.2, lineend = "round"
  ) +
  
  # B. Pontos para Q1 e Q3
  geom_point(
    data = datos_largo %>% filter(Quartile %in% c("Q1 (25%)", "Q3 (75%)")),
    aes(color = Quartile),
    shape = 18, size = 3
  ) +
  
  # C. Ponto para a Mediana (tendência principal)
  geom_point(
    data = datos_largo %>% filter(Quartile == "Median (50%)"),
    aes(size = Frequency, color = Quartile),
    shape = 16
  ) +
  
  # 4. Escalas e cores
  scale_color_manual(
    values = c("Q1 (25%)" = "#0072B2", "Median (50%)" = "lightblue", "Q3 (75%)" = "#0072B2"),
    name = "Year Percentile"
  ) +
  
  scale_size_continuous(
    range = c(3, 10),
    name = "Total Frequency (Volum)",
    breaks = c(1, 10, 50, 100, 161)
  ) +
  
  scale_x_continuous(
    breaks = seq(2016, 2025, by = 1),
    limits = c(2016, 2025.5)
  ) +
  
  # 5. Rótulos e tema
  labs(
    title = "",
    subtitle = "",
    x = "Year",
    y = "Term"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(linetype = "dotted", color = "gray90"),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(hjust = 0),
    plot.title.position = "plot"
  )

# 6. Mostrar o gráfico
print(grafico_final)

############grafico trend dois ######
ggplot(datos_largo, aes(x = Year, y = Frequency, color = Term)) +
  geom_line(size = 1.2) +
  geom_smooth(se = FALSE, linetype = "dotted") +
  theme_minimal() +
  labs(x = "Year", y = "Frequency", title = "")




