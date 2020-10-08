library(readr)
library(dplyr)
library(ggplot2)

base = read_csv("dados_enem_filtrado.csv") %>% 
  select(-X1) 


SAL_MIN = 880

base = base %>% 
  mutate(Q006_MEAN = case_when(
    Q006 == "A" ~ 0, 
    Q006 == "B" ~ mean(0.1, 880),
    Q006 == "C" ~ mean(880.01, 1320),
    Q006 == "D" ~ mean (1320.01, 1760),
    Q006 == "E" ~ mean (1760.01, 2200),
    Q006 == "F" ~ mean (2200.01, 2640),
    Q006 == "G" ~ mean (2640.01, 3520),
    Q006 == "H" ~ mean (3520.01, 4400),
    Q006 == "I" ~ mean (4400.01, 5280),
    Q006 == "J" ~ mean (5280.01, 6160),
    Q006 == "K" ~ mean (6160.01, 7040),
    Q006 == "L" ~ mean (7040.01, 7920),
    Q006 == "M" ~ mean (7920.01, 8800),
    Q006 == "N" ~ mean (8800.01, 10560),
    Q006 == "O" ~ mean (10560.01, 13200),
    Q006 == "P" ~ mean (13200.01, 17600),
    Q006 == "Q" ~ 17600.01
  )) %>% 
  mutate(RENDA_PER_CAPITA = round(Q006_MEAN/Q005, 2)) %>% 
  mutate(IN_RENDA_BAIXA = ifelse(RENDA_PER_CAPITA <= 1.5 * SAL_MIN, 1, 0),
         IN_RACA = ifelse(TP_COR_RACA == 2 |
                            TP_COR_RACA == 3 |
                            TP_COR_RACA == 5, 1, 0)) %>% 
  mutate(MODALIDADE = case_when(
    IN_RENDA_BAIXA == 1 & Q047 == "A" & IN_RACA == 1 ~ "L2",
    IN_RENDA_BAIXA == 1 & Q047 == "A" ~ "L1",
    Q047 == "A" & IN_RACA == 1 ~ "L6",
    Q047 == "A" ~ "L5",
    TRUE ~ "A0"
   ))


base = base %>% mutate(NOTA_MEDICINA = (NU_NOTA_REDACAO * 2 +
                                          NU_NOTA_CN * 3 + 
                                          NU_NOTA_CH * 2 + 
                                          NU_NOTA_LC + 
                                          NU_NOTA_MT * 2)/10) %>% 
  mutate(IN_APROVADO_A0 = ifelse(NOTA_MEDICINA >= 805.15 & MODALIDADE == "A0", 1, 0),
         IN_APROVADO_L1 = ifelse(NOTA_MEDICINA >= 761.49 & MODALIDADE == "L1", 1, 0),
         IN_APROVADO_L2 = ifelse(NOTA_MEDICINA >= 749.83 & MODALIDADE == "L2", 1, 0),
         IN_APROVADO_L5 = ifelse(NOTA_MEDICINA >= 785.38 & MODALIDADE == "L5", 1, 0),
         IN_APROVADO_L6 = ifelse(NOTA_MEDICINA >= 760.53 & MODALIDADE == "L6", 1, 0)) %>% 
  mutate(IN_APROVADO = case_when(
    IN_APROVADO_A0 == 1 ~ "Aprovado",
    IN_APROVADO_L1 == 1 ~ "Aprovado",
    IN_APROVADO_L2 == 1 ~ "Aprovado",
    IN_APROVADO_L5 == 1 ~ "Aprovado",
    IN_APROVADO_L6 == 1 ~ "Aprovado",
    TRUE ~ "Reprovado"
  ))

#etnia,sexo, tipo de escola (q047), in_renda_baixa (1- menos, 2- mais de 1,5) nota que n?o ? informado 
#a renda percapta, a variavel foi criada com base na variavel salario e total de pessoas morando na casa
#numero de aprovados por grupo, numero de aprovodaos de cada sexo por grupo e numero de aprovados por cada ra?a

#tratamento e graficos para sexo

#grafico de barra para sexo

tema = 
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15)
  )

sexo_data = base %>% 
  group_by(TP_SEXO) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(percent = n/sum(n))


sexo_data %>% 
  ggplot(aes(x = TP_SEXO, y = percent)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = percent, 
                label = scales::percent(percent)),
            vjust = -1,
            size = 10) + 
  ylim(0, 0.7) +
  theme(legend.position = "none") +
  scale_x_discrete(breaks = c("F", "M"), 
                   labels = c("Feminino", "Masculino")) +
  xlab('') + ylab('') +
  tema

#tratamento e graficos para etnia

etnia_data = base %>% 
  mutate(TP_COR_RACA = case_when(TP_COR_RACA == 0 ~ "Não informado",
                                 TP_COR_RACA == 1 ~ "Branco",
                                 TP_COR_RACA == 2 ~ "Preto",
                                 TP_COR_RACA == 3 ~ "Pardo",
                                 TP_COR_RACA == 4 ~ "Amarelo",
                                 TP_COR_RACA == 5 ~ "Indígena",
                                 TP_COR_RACA == 6 ~ "Não informado")) %>% 
  group_by(TP_COR_RACA) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(percent = n/sum(n)) 

#grafico de barra para etnia
etnia_data %>%
  ggplot(aes(x=reorder(TP_COR_RACA, -percent), y = percent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = percent, 
                label = scales::percent(percent, accuracy = 1)),
            vjust = -1,
            size = 10) +
  ylim(0, 0.7) +
  xlab('') + ylab('') +
  tema

#escola

tipo_escola_data = base %>% 
  mutate(Q047 = case_when(Q047=="A" ~ "Publica",
                          Q047=="B" ~ "Pública/Privada s/ Bolsa",
                          Q047=="C" ~ "Pública/Privada c/ Bolsa",
                          Q047=="D" ~ "Privada s/ Bolsa",
                          Q047=="E" ~ "Privada c/ Bolsa")) %>% 
  group_by(Q047) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(percent = n/sum(n)) 

tipo_escola_data %>%
  ggplot(aes(x=reorder(Q047, -percent), y = percent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = percent, 
                label = scales::percent(percent, accuracy = 1)),
            vjust = -1,
            size = 10) +
  ylim(0, 1) +
  xlab('') + ylab('') +
  tema

#renda
renda_categoria_data = base %>% 
  mutate(Q006 = ordered(Q006, levels = c("A", 
                                         "B", 
                                         "C", 
                                         "D", 
                                         "E", 
                                         "F", 
                                         "G",
                                         "H",
                                         "I",
                                         "J", 
                                         "K", 
                                         "L",
                                         "M",
                                         "N",
                                         "O",
                                         "P",
                                         "Q"),
                        labels = c("Sem renda",
                                   "R$880,00 - R$0,01",    
                                   "R$1320,00 - R$880,01",
                                   "R$1760,00 - R$1320,01",
                                   "R$2200,00 - R$1760,01",
                                   "R$2640,00 - R$2200,01",
                                   "R$3520,00 - R$2640,01",
                                   "R$4400,00 - R$3520,01",
                                   "R$5280,00 - R$4400,01",
                                   "R$6160,00 - R$5280,01",
                                   "R$7040,00 - R$6160,01",
                                   "R$7920,00 - R$7040,01",
                                   "R$8800,00 - R$7920,01",
                                   "R$10560,00 - R$8800,01",
                                   "R$13200,00 - R$10560,01",
                                   "R$17600,00 - R$13200,01",
                                   "R$17600,01 +"))) %>% 
  group_by(Q006) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(percent = n/sum(n))

renda_categoria_data %>%
  ggplot(aes(x = Q006, y = percent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = percent, 
                label = ifelse(percent > .01, 
                               scales::percent(percent, accuracy = 1),
                               scales::percent(percent, accuracy = .1)
                               )),
            hjust = -.1,
            size = 8) +
  ylim(0, .5) +
  coord_flip() +
  xlab('') + ylab('') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15)
  )

base %>% select_at(vars(starts_with("IN_APROVADO")))

table(base$IN_APROVADO)
paste0(100 * prop.table(table(base$IN_APROVADO)), "%")

base %>% 
  group_by(MODALIDADE, IN_APROVADO) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(IN_APROVADO)


base %>% 
  group_by(TP_SEXO, IN_APROVADO) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(IN_APROVADO)


base %>% 
  group_by(MODALIDADE) %>% 
  summarise(total_modalidade = n(), .groups = "drop") %>% 
  (function(x) {
    base %>% 
      group_by(MODALIDADE, IN_APROVADO, TP_SEXO) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      left_join(x, by="MODALIDADE")
  }) %>% 
  mutate(perc_modalidade = n/total_modalidade) %>% 
  select(-total_modalidade) %>% 
  filter(IN_APROVADO == "Aprovado")

base %>% 
  group_by(MODALIDADE) %>% 
  summarise(total_modalidade = n(), .groups = "drop") %>% 
  (function(x) {
    base %>% 
      group_by(MODALIDADE, IN_APROVADO, TP_SEXO) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      left_join(x, by="MODALIDADE")
  }) %>% 
  mutate(perc_modalidade = n/total_modalidade) %>% 
  select(-total_modalidade) %>% 
  filter(IN_APROVADO == "Aprovado")


etnia_data = 
  base %>% 
    mutate(TP_COR_RACA = case_when(TP_COR_RACA == 0 ~ "Não informado",
                                   TP_COR_RACA == 1 ~ "Branco",
                                   TP_COR_RACA == 2 ~ "Preto",
                                   TP_COR_RACA == 3 ~ "Pardo",
                                   TP_COR_RACA == 4 ~ "Amarelo",
                                   TP_COR_RACA == 5 ~ "Indígena",
                                   TP_COR_RACA == 6 ~ "Não informado")) %>% 
    group_by(MODALIDADE, IN_APROVADO, TP_COR_RACA) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    mutate(percent = paste0(round(100 * n/sum(n), 3), "%")) %>% 
    filter(IN_APROVADO == "Aprovado") %>% 
    select(-IN_APROVADO) %>% 
    group_by(MODALIDADE) %>% 
    summarise(modalidade_n = sum(n), .groups = "drop") %>% 
    (function(x){
      base %>% 
        mutate(TP_COR_RACA = case_when(TP_COR_RACA == 0 ~ "Não informado",
                                       TP_COR_RACA == 1 ~ "Branco",
                                       TP_COR_RACA == 2 ~ "Preto",
                                       TP_COR_RACA == 3 ~ "Pardo",
                                       TP_COR_RACA == 4 ~ "Amarelo",
                                       TP_COR_RACA == 5 ~ "Indígena",
                                       TP_COR_RACA == 6 ~ "Não informado")) %>%  
        group_by(MODALIDADE, IN_APROVADO, TP_COR_RACA) %>% 
        summarise(n = n(), .groups = "drop") %>%
        filter(IN_APROVADO == "Aprovado") %>% 
        left_join(x, by ="MODALIDADE") %>% 
        mutate(percent = n/modalidade_n) %>% 
        select(-modalidade_n)
    }) %>% 
    mutate(TP_COR_RACA = as.factor(TP_COR_RACA))

  
etnia_data %>% 
  ggplot(aes(x = MODALIDADE, 
             y = percent, 
             fill = reorder(TP_COR_RACA, -percent))) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), 
            position = position_dodge2(width = 0.9, preserve = "single"),
            vjust = -0.8,
            size = 4) +
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Set2")) +
  theme_minimal() +
  tema +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_text(vjust = 2, size = 15),
  ) +
  xlab("") + ylab("")

etnia_data %>% 
  ggplot(aes(x = MODALIDADE, 
             y = n, 
             fill = reorder(TP_COR_RACA, -n))) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label = n), 
            position = position_dodge2(width = 0.9, preserve = "single"),
            vjust = -0.8,
            size = 4) +
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Set2")) +
  theme_minimal() +
  tema +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_text(vjust = 2, size = 15),
  ) +
  xlab("") + ylab("")


genero_data = 
  base %>% 
  mutate(TP_SEXO = case_when(TP_SEXO == "F" ~ "Feminino",
                             TP_SEXO == "M" ~ "Masculino")) %>% 
  group_by(MODALIDADE, IN_APROVADO, TP_SEXO) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(percent = paste0(round(100 * n/sum(n), 3), "%")) %>% 
  filter(IN_APROVADO == "Aprovado") %>% 
  select(-IN_APROVADO) %>% 
  group_by(MODALIDADE) %>% 
  summarise(modalidade_n = sum(n), .groups = "drop") %>% 
  (function(x){
    base %>% 
      mutate(TP_SEXO = case_when(TP_SEXO == "F" ~ "Feminino",
                                 TP_SEXO == "M" ~ "Masculino")) %>%  
      group_by(MODALIDADE, IN_APROVADO, TP_SEXO) %>% 
      summarise(n = n(), .groups = "drop") %>%
      filter(IN_APROVADO == "Aprovado") %>% 
      left_join(x, by ="MODALIDADE") %>% 
      mutate(percent = n/modalidade_n) %>% 
      select(-modalidade_n)
  }) %>% 
  mutate(TP_COR_RACA = as.factor(TP_SEXO))

genero_data %>% 
  ggplot(aes(x = MODALIDADE, 
             y = n, 
             fill = reorder(TP_SEXO, -n))) +
  geom_col(position ="dodge") +
  geom_text(aes(label = n), 
            position = position_dodge(width = 1),
            vjust = -0.8,
            hjust = .5,
            size = 10) +
  scale_y_continuous(breaks = seq(0, 300, 50), limits = c(0, 300)) +
  scale_fill_manual(values = c("#595959", "#212121")) +
  theme_minimal() +
  tema +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_text(vjust = 2, size = 15),
  ) +
  xlab("") + ylab("")

genero_data %>% 
  ggplot(aes(x = MODALIDADE, 
             y = percent, 
             fill = reorder(TP_SEXO, -percent))) +
  geom_col(position ="dodge") +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), 
            position = position_dodge(width = 1),
            vjust = -0.8,
            hjust = .5,
            size = 10) +
  scale_fill_manual(values = c("#595959", "#212121")) +
  theme_minimal() +
  tema +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_text(vjust = 2, size = 15),
  ) +
  xlab("") + ylab("") + ylim(0, .8)

options(scipen = 999)
base %>% 
  ggplot(aes(x = NOTA_MEDICINA)) +
  geom_histogram(bins = 32) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  scale_y_continuous(breaks = seq(0, 300000, 25000)) +
  theme_minimal() +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_text(vjust = 2, size = 15)
  ) +
  xlab('Nota Medicina') + ylab('Frequência')

base %>% 
  mutate(TP_COR_RACA = case_when(TP_COR_RACA == 0 ~ "Não informado",
                                 TP_COR_RACA == 1 ~ "Branco",
                                 TP_COR_RACA == 2 ~ "Preto",
                                 TP_COR_RACA == 3 ~ "Pardo",
                                 TP_COR_RACA == 4 ~ "Amarelo",
                                 TP_COR_RACA == 5 ~ "Indígena",
                                 TP_COR_RACA == 6 ~ "Não informado")) %>% 
  mutate(TP_SEXO = case_when(
    TP_SEXO == "F" ~ "Feminino",
    TP_SEXO == "M" ~ "Masculino"
  )) %>% 
  filter(TP_COR_RACA != "Não informado") %>% 
  group_by(TP_SEXO, TP_COR_RACA) %>% 
  summarise(media = mean(NOTA_MEDICINA), .groups = "drop") %>% 
    ggplot(aes(media, reorder(TP_COR_RACA, -media))) +
  geom_line(aes(group = TP_COR_RACA), size = 1.5) +
  geom_point(aes(color = TP_SEXO), size = 4) +
  theme_minimal() +
  theme(text=element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        panel.grid.major.y = element_line(size = 1, linetype = "longdash"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position="bottom",
        legend.direction="horizontal") +
  xlab("Nota média - Medicina")


base %>% 
  filter(IN_APROVADO == "Aprovado") %>% 
  ggplot(aes(x = MODALIDADE, y = ..count..)) +
  geom_bar() +
  geom_text(aes(y = ..count.., label = ..count..), stat='count',
            vjust = -0.8,
            hjust = .5,
            size = 10) +
  theme_minimal() +
  tema +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_text(vjust = 2, size = 15),
  ) + 
  ylim(0, 350) + ylab('') + xlab('')
  
  
