library(dplyr)
library(psych)
library(car)
library(MASS)
library(DescTools)
library(QuantPsyc)
library(ggplot2)
library(roadtrafficdeaths)
library(onsvplot)

dados <- rtdeaths

dados <- dados %>% 
  filter(
    dados$modal_vitima != "Pedestre" &
      dados$modal_vitima != "Bicicleta"&
      dados$modal_vitima != "Outros"
  )

dados$faixa_etaria_vitima %>% table()
dados <- dados %>% 
  mutate(faixa_etaria_vitima = dplyr::recode(faixa_etaria_vitima,
                                      "0 a 4 anos" = "0-9", 
                                      "5 a 9 anos" = "0-9", 
                                      "10 a 14 anos" = "10-19", 
                                      "15 a 19 anos" = "10-19", 
                                      "20 a 24 anos" = "20-29", 
                                      "25 a 29 anos" = "20-29", 
                                      "30 a 34 anos" = "30-39", 
                                      "35 a 39 anos" = "30-39", 
                                      "40 a 44 anos" = "40-49", 
                                      "45 a 49 anos" = "40-49", 
                                      "50 a 54 anos" = "50-59", 
                                      "55 a 59 anos" = "50-59", 
                                      "60 a 64 anos" = "60 ou mais", 
                                      "65 a 69 anos" = "60 ou mais", 
                                      "70 a 74 anos" = "60 ou mais", 
                                      "75 a 79 anos" = "60 ou mais", 
                                      "Mais de 80 anos" = "60 ou mais"))


dados$ocupante_motocicleta <- ifelse(
  dados$modal_vitima == "Motocicleta",
  "ocupante",
  "nao_ocupante")

dados <- dados %>% dplyr::select(faixa_etaria_vitima, sexo_vitima, escolaridade_vitima,
                           nome_regiao_res, raca_vitima, estado_civil_vitima, ocupante_motocicleta)


dados$ocupante_motocicleta <- 
  relevel(as.factor(dados$ocupante_motocicleta), ref = "nao_ocupante")

dados$raca_vitima <- as.factor(dados$raca_vitima)   
dados$raca_vitima <- relevel(dados$raca_vitima, ref = "Branca")  

dados$escolaridade_vitima <- as.factor(dados$escolaridade_vitima)  
dados$escolaridade_vitima <- relevel(dados$escolaridade_vitima, ref = "12 anos ou mais")  

dados$nome_regiao_res <- as.factor(dados$nome_regiao_res) 
dados$nome_regiao_res <- relevel(dados$nome_regiao_res, ref = "Sul")

dados$faixa_etaria_vitima <- as.factor(dados$faixa_etaria_vitima) 
dados$faixa_etaria_vitima <- relevel(dados$faixa_etaria_vitima, ref = "60 ou mais")

dados$estado_civil_vitima <- as.factor(dados$estado_civil_vitima) 
dados$estado_civil_vitima <- relevel(dados$estado_civil_vitima, ref = "casado")

dados$sexo_vitima <- as.factor(dados$sexo_vitima) 
dados$sexo_vitima <- relevel(dados$sexo_vitima, ref = "Feminino")

# Construção do modelo
mod <- glm(ocupante_motocicleta ~ faixa_etaria_vitima + sexo_vitima + 
                                  raca_vitima + escolaridade_vitima + 
                                  nome_regiao_res + estado_civil_vitima, 
           family = binomial(link = 'logit'),
           data = dados)


vif(mod) # Multicolinearidade se VIF>10

# Overall effects

Anova(mod, type = "II", test = "Wald" ) #Todos p < 0.05 (todos são previsores)

summary(mod)

## razão odds

odds <- exp(cbind(OR = coef(mod), confint.default(mod)))

tab <- as.data.frame(odds)
tab <- tab[-1,]
tab <- tab %>% 
  rename( "Limite Inferior" = "2.5 %",
          "Limite Superior" = "97.5 %")
tab <- tab %>% 
  mutate( OR = round(OR, 2),
          `Limite Inferior` = round(`Limite Inferior`, 2),
          `Limite Superior` = round(`Limite Superior`, 2))
tab$variavel <- c(rep("Faixa etária", 6), "Sexo", rep("Raça", 4), rep("Escolaridade", 4), rep("Região", 4), rep("Estado civil", 4))
tab$categoria <- c("0-9","10-19","20-29","30-39","40-49","50-59","Masculino",
                   "Amarela","Indígena","Parda","Preta","1 a 3 anos","4 a 7 anos",
                   "8 a 11 anos","Nenhuma", "Centro-Oeste","Nordeste","Norte","Sudeste",
                   "Separado","Solteiro","União","Viúvo")
tab <- tab[,c(4,5,1,2,3)]
rownames(tab) <- NULL

# ex de interpretação: a chance do sexo masculino e de ser 
#condutor de motocicleta é 2.054 vezes maior que o sexo feminino (que foi o de ref)

#Gráficos ----
faixa_etaria <- as.data.frame(odds[c(2:7),])

faixa_etaria[7,] <- c("1","1","1")
faixa_etaria$valor <-  c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "Acima 60")
faixa_etaria$valor <- relevel(as.factor(faixa_etaria$valor), ref = "Acima 60")

faixa_etaria$OR <- round(as.numeric(faixa_etaria$OR), 3)
faixa_etaria$`2.5 %` <- round(as.numeric(faixa_etaria$`2.5 %`), 3)
faixa_etaria$`97.5 %` <- round(as.numeric(faixa_etaria$`97.5 %`), 3)

gf <- faixa_etaria %>% 
  ggplot(aes(x = valor, y = OR))+
  geom_col(fill = "orange2")+
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.6)+
  theme_classic()+
  labs(y = "Razão de ODDS",
       x = NULL)


sexo <- as.data.frame(odds[c(8,9),])
sexo[3,] <- c("1","1","1")
sexo <- sexo[-2,] 
sexo$valor <- c("Masculino", "Feminino")
sexo$valor <- relevel(as.factor(sexo$valor), ref = "Feminino")
sexo$OR <- round(as.numeric(sexo$OR), 3)
sexo$`2.5 %` <- round(as.numeric(sexo$`2.5 %`), 3)
sexo$`97.5 %` <- round(as.numeric(sexo$`97.5 %`), 3)

gs <- sexo %>% 
  ggplot(aes(x = valor, y = OR))+
  geom_col(fill = "orange2")+
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.6)+
  theme_classic()+
  labs(y = "Razão de ODDS",
       x = NULL)



raca <- as.data.frame(odds[c(9:12),])
raca[5,] <- c("1","1","1")
raca$valor <- c("Amarela", "Indígena", "Parda", "Preta", "Branca")
raca$valor <- relevel(as.factor(raca$valor), ref = "Branca")
raca$OR <- round(as.numeric(raca$OR), 3)
raca$`2.5 %` <- round(as.numeric(raca$`2.5 %`), 3)
raca$`97.5 %` <- round(as.numeric(raca$`97.5 %`), 3)

gr <- raca %>% 
  ggplot(aes(x = valor, y = OR))+
  geom_col(fill = "orange3")+
  geom_col(fill = "orange2")+
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.6)+
  theme_classic()+
  labs(y = "Razão de ODDS",
       x = NULL)

escola <- as.data.frame(odds[c(13:16),])
escola[5,] <- c("1","1","1")
escola$valor <- c("1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "Nenhuma", "12 anos ou mais")
escola$valor <- relevel(as.factor(escola$valor), ref = "12 anos ou mais")
escola$OR <- round(as.numeric(escola$OR), 3)
escola$`2.5 %` <- round(as.numeric(escola$`2.5 %`), 3)
escola$`97.5 %` <- round(as.numeric(escola$`97.5 %`), 3)

ge <- escola %>% 
  ggplot(aes(x = valor, y = OR))+
  geom_col(fill = "orange3")+
  geom_col(fill = "orange2")+
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.6)+
  theme_classic()+
  labs(y = "Razão de ODDS",
       x = NULL)

res <- as.data.frame(odds[c(17:20),])
res[5,] <- c("1","1","1")
res$valor <- c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
res$valor <- relevel(as.factor(res$valor), ref = "Sul")
res$OR <- round(as.numeric(res$OR), 3)
res$`2.5 %` <- round(as.numeric(res$`2.5 %`), 3)
res$`97.5 %` <- round(as.numeric(res$`97.5 %`), 3)

gres <- res %>% 
  ggplot(aes(x = valor, y = OR))+
  geom_col(fill = "orange3")+
  geom_col(fill = "orange2")+
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.6)+
  theme_classic()+
  labs(y = "Razão de ODDS",
       x = NULL)

ec <- as.data.frame(odds[c(21:24),])
ec[5,] <- c("1","1","1")
ec$valor <- c("Separado", "Solteiro", "União", "Viúvo", "Casado")
ec$valor <- relevel(as.factor(ec$valor), ref = "Casado")
ec$OR <- round(as.numeric(ec$OR), 3)
ec$`2.5 %` <- round(as.numeric(ec$`2.5 %`), 3)
ec$`97.5 %` <- round(as.numeric(ec$`97.5 %`), 3)

gec <- ec %>% 
  ggplot(aes(x = valor, y = OR))+
  geom_col(fill = "orange3")+
  geom_col(fill = "orange2")+
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.6)+
  theme_classic()+
  labs(y = "Razão de ODDS",
       x = NULL)
