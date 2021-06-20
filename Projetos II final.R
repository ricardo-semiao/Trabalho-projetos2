########### PACOTES E FUNÇÕES   ##########
library(tidyr) # Manipulação de data frame
library(ggplot2) # Gráficos
library(stargazer) # Tabela do latex
library(lmtest) # Testes pro lm
library(car) # Testes pro lm

# Gráfico customizado:
theme_update(
  panel.grid = element_line(color="darkgrey"),
  plot.background =  element_rect(fill="azure3"),
  panel.background = element_rect(fill="azure3"),
  legend.background =element_rect(fill="azure3", color="darkgrey"),
  legend.title = element_text(face="bold"),
  plot.title = element_text(face="bold"),
  axis.ticks = element_blank())

pal = wesanderson::wes_palettes$BottleRocket1


########### DADOS               ##########
#======== Ler e criar dummies ==========
censo = as.matrix(read.delim("m:/Users/Marcus/Downloads/Censo 2010/Amostra_Pessoas_35_RMSP.txt", header=FALSE))
censo = censo[-which(substr(censo, 219, 224)%in%c("      ","000000"))]

#"SALÁRIO PRINCIPAL"=c(219, 224),"RENDIMENTO TOTAL"=c(263, 269)), "SALÁRIO TOTAL"=C(247, 253)
Y = log(as.numeric(substr(censo, 219, 224)))
n = length(Y)

Dicio = as.data.frame(readxl::read_excel("m:/Users/Marcus/Downloads/Censo 2010/Dicionário.xls", sheet="X1")[,c(3:5,9:10)])

Vars = as.data.frame(matrix(nrow=length(censo),ncol=nrow(Dicio)))
for(i in 1:nrow(Dicio)){
  col = as.numeric(substr(censo, Dicio[i,2], Dicio[i,3]))
  if(Dicio$DUMMIE[i]=="S"){col=as.factor(col)}
  Vars[,i] = col}
colnames(Vars) = Dicio$`NOME CURTO`

#remove(censo, col)

#======== Tratar manualmente  ==========
for(i in 1:ncol(Vars)){
  soma = sum(is.na(Vars[,i]))
  if(soma>=1){
    print(paste(soma, "-", i, colnames(Vars)[i]))}}

#NACIONALIDADE -> 1 (BR)
#$FAM e NFAM -> 0
#IDADE FILHO -> 0 e dummie
#N nascidos e N vivos -> 0
#tempo deslocamento -> 0 e dummie
#N trabalh0s -> 1
#Retornar -> 1 (sim)
#ATIVIDADE e RELIGIÃO com poucos -> 0
#BOLSA, TRANSFERENCIAS e OUTROS 9 -> 0 

#Dummies de NA
Vars$`IDADE FILHO NA` = as.numeric(is.na(Vars$`IDADE FILHO`))
Vars$`TEMPO DESLOCAMENTO NA` = as.numeric(is.na(Vars$`TEMPO DESLOCAMENTO`))

#Mudar valores
na = c("$ FAMILIAR/C", "N FAMÍLIA", "N FILHOS VIVOS", "IDADE FILHO", "TEMPO DESLOCAMENTO")
Vars[,na][is.na(Vars[,na])] = 0

na = c("NACIONALIDADE", "RETORNAR", "N TRABALHOS")
Vars[,na][is.na(Vars[,na])] = 1

na = c("BOLSA-FAMÍLIA", "TRANSFERÊNCIAS", "OUTROS RENDIMENTOS")
Vars[,na][Vars[,na]==9] = 1

#Diminuir quantidade de dummies de atividade e religião
vc = c("ATIVIDADE"=4000,"RELIGIÃO"=4000)
for(j in names(vc)){
  for(i in unique(Vars[,j])){
  index = Vars[,j]==i
  if(sum(index) <= vc[j]){
    Vars[index,j] = "Outros"}}}

#Idade ao quadrado:
Vars$IDADE2 = Vars$IDADE^2

#Dummies de deficiência
for(i in c("VISÃO","AUDIÇÃO","LOCOMOÇÃO")){
  Vars[[paste0(i,2)]] = as.factor(as.numeric(Vars[,i]%in%1:2))
  Vars[,i] = NULL}
Vars$INTELECTO2 = as.factor(ifelse(Vars$INTELECTO==2,0,2))
Vars$INTELECTO = NULL

#Criar leva para as novas variáveis
names = c("IDADE FILHO NA","TEMPO DESLOCAMENTO NA","IDADE2","VISÃO2","AUDIÇÃO2","LOCOMOÇÃO2","INTELECTO2")
leva = c("F", "R", "C", "D", "D", "D", "D")
for(i in 1:length(names)){
  Dicio = rbind(Dicio, c(names[i], rep(0,3), leva[i]))}

Dicio = Dicio[-which(Dicio[,1]%in%c("VISÃO","AUDIÇÃO","LOCOMOÇÃO","INTELECTO")),]

Vars = Vars[-which(Vars$RAÇA==9 | Vars$INTELECTO2==9),]

remove(na, i, j, index, soma, names, leva)

#save(Vars, file="m:/Users/Marcus/Downloads/Vars.RData")
#load("m:/Users/Marcus/Downloads/Vars.RData")
size = sample(1:n, 30000)
size = 1:n
g.size = sample(1:n, 10000)

########### ANÁLISE INICIAL     ##########
#Relação quadrática com idade:
ggplot(mapping=aes(y=exp(Y[g.size]), x=Vars$IDADE[g.size])) +
  geom_point(color=pal[4], alpha=0.3) + ylim(0,12500) +
  geom_smooth(aes(color="Y ~ X + X^2"), method="lm", formula=y~poly(x, 1)) +
  geom_smooth(aes(color="Y ~ X"), method="lm", formula=y~poly(x, 2)) +
  #annotate(geom="label", x=95, y=1500, label="R²=0.00", size=3) +
  #annotate(geom="label", x=95, y=3800, label="R²=0.00", size=3) +
  scale_color_manual(values=pal[1:2], name="Modelo") +
  labs(title="Relação salário-idade") + xlab("Idade") + ylab("Salário")

#Relação pequena com horas trabalhadas
ggplot(mapping=aes(y=exp(Y[size]), x=Vars$`HORAS TRABALHADAS`[size])) +
  geom_point(color=pal[4], alpha=0.3) +
  geom_smooth(color=pal[1], method="lm", formula=y~poly(x,2)) + ylim(0,12500) + xlim(0,15)

#Relação decrescente com tamanho da família
ggplot(mapping=aes(y=Y[size], x=Vars$`N FAMÍLIA`[size])) +
  geom_point(color="#202547", alpha=0.3) +
  geom_smooth(color="#53354A", method="lm") + ylim(0,12500)

#Alta relação com renda familiar
ggplot(mapping=aes(y=Y[size], x=Vars$`$ FAMILIAR/C`[size])) +
  geom_point() + geom_smooth(method="lm") + ylim(0,12500) + xlim(0,600000)

#Médias condicioal às variáveis categóricas mais importantes
for(j in Dicio[which(Dicio$DUMMIE=="S"),1]){
  for(i in levels(Vars[,j])){
    print(paste(round(exp(mean(Y[which(Vars[,j]==i)])),2), i, j))
  }
}




########### ANÁLISE EFEITOS     ##########
Yt = Y[size]
Xt = Vars[size,]

#========== Sem interações    ==========
leva = character()
mods = list()
for(i in c("D", "C","R","F")){
  leva = c(leva,i)
  names = Dicio[Dicio$LEVA%in%leva,1]
  mods[[i]] = lm(Yt[-outliers] ~ ., Xt[-outliers,names])
  print(summary(mods[[i]]))
  print("=============================================")}

stargazer(mods$D, mods$C, mods$R, mods$F, single.row=TRUE, omit=c("RELIGIÃO", "ATIVIDADE"))

#========== Interações        ==========
formula = "Yt[-outliers] ~ ."
for(i in c("RAÇA", "SEXO", "EDUCAÇÃO", "`HORAS TRABALHADAS`", "`OUTROS TOTAL`")){
  for(j in c("INTELECTO2", "VISÃO2", "AUDIÇÃO2", "LOCOMOÇÃO2")){
    formula = paste(formula, "+", paste0(i, "*", j))}}
formula = paste(formula, "+ RAÇA*SEXO + `OUTROS TOTAL`*`$ FAMILIAR/C` + RAÇA*EDUCAÇÃO + SEXO*EDUCAÇÃO")

mods$I = lm(formula, Xt[-outliers,])

linearHypothesis(mods$I, singular.ok=TRUE,
                 c("EDUCAÇÃO2:VISÃO2","EDUCAÇÃO3:VISÃO2","EDUCAÇÃO4:VISÃO2"))

linearHypothesis(mods$I, singular.ok=TRUE,
                 c("`OUTROS TOTAL`:INTELECTO22",
                   "`OUTROS TOTAL`:VISÃO2",
                   "`OUTROS TOTAL`:AUDIÇÃO2",
                   "`OUTROS TOTAL`:LOCOMOÇÃO2"))

linearHypothesis(mods$I, singular.ok=TRUE,
                 c("RAÇA2:INTELECTO22",
                   "RAÇA3:INTELECTO22",
                   "RAÇA4:INTELECTO22",
                   "RAÇA5:INTELECTO22",
                   "RAÇA2:VISÃO2",
                   "RAÇA3:VISÃO2",
                   "RAÇA4:VISÃO2",
                   "RAÇA5:VISÃO2",
                   "RAÇA2:AUDIÇÃO2",
                   "RAÇA3:AUDIÇÃO2",
                   "RAÇA4:AUDIÇÃO2",
                   "RAÇA5:AUDIÇÃO2", 
                   "RAÇA2:LOCOMOÇÃO2",
                   "RAÇA3:LOCOMOÇÃO2",
                   "RAÇA4:LOCOMOÇÃO2",
                   "RAÇA5:LOCOMOÇÃO2"))

linearHypothesis(mods$I, singular.ok=TRUE,
                 c("EDUCAÇÃO2:AUDIÇÃO2",
                   "EDUCAÇÃO3:AUDIÇÃO2",
                   "EDUCAÇÃO4:AUDIÇÃO2",
                   "EDUCAÇÃO5:AUDIÇÃO2",
                   "EDUCAÇÃO2:LOCOMOÇÃO2",
                   "EDUCAÇÃO3:LOCOMOÇÃO2",
                   "EDUCAÇÃO4:LOCOMOÇÃO2",
                   "EDUCAÇÃO5:LOCOMOÇÃO2"))

linearHypothesis(mods$I, singular.ok=TRUE,
                 c("SEXO2:RAÇA2",
                   "SEXO2:RAÇA3",
                   "SEXO2:RAÇA4",
                   "RAÇA2:EDUCAÇÃO2",
                   "RAÇA3:EDUCAÇÃO2",
                   "RAÇA4:EDUCAÇÃO2",
                   "RAÇA2:EDUCAÇÃO3",
                   "RAÇA3:EDUCAÇÃO3",
                   "RAÇA4:EDUCAÇÃO3",
                   "RAÇA2:EDUCAÇÃO4",
                   "RAÇA3:EDUCAÇÃO4",
                   "RAÇA4:EDUCAÇÃO4",
                   "SEXO2:EDUCAÇÃO2",
                   "SEXO2:EDUCAÇÃO3",
                   "SEXO2:EDUCAÇÃO4"))

#---------- Outliers        ----------
outliers = which(abs(mods$I$residuals)>3*summary(mods$I)$sigma)
outliers = max(size) + 1 #Ignorar outliers


#========== TESTES            ==========
#---------- VIF             ----------
#- `BOLSA-FAMÍLIA` - `OUTROS RENDIMENTOS`- TRANSFERÊNCIAS
car::vif(mods$F)

#---------- Distribuição    ----------
# Densidades
g1 = ggplot(mapping=aes(x=mods$I$residuals/sd(mods$I$residuals))) +
  stat_density(aes(color="Resíduos"), size=1, geom="line") +
  stat_density(aes(color="Normal", x=rnorm(100000)), size=1, geom="line") +
  ylab("Densidade") + xlab("Resíduos padronizados") + labs(title="Densidade dos resíduos") + xlim(-3.5,3.5) +
  scale_color_manual(values=pal[1:2], name="Densidades")

# QQ plot
g2 = ggplot(mapping=aes(y=quantile(mods$I$residuals/sd(mods$I$residuals), seq(0,1,0.01)),
                   x=qnorm(seq(0,1,0.01)))) +
  geom_point(color=pal[4], alpha=0.3, size=2.5) +
  geom_line(aes(x=quantile(mods$I$residuals/sd(mods$I$residuals), seq(0,1,0.01)), color="y=x"),
            slope=1, intercept=0, size=1) +
  geom_smooth(aes(color="Fit"), method="lm", size=1) +
  scale_color_manual(values=pal[c(1,5)], name="Retas") + xlim(-1,1) + ylim(-1,1) +
  labs(title="QQ plot") + xlab("Quantis teóricos") + ylab("Quantis dos resíduos padronizados")

gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(2, 1.7))

# Teste de normal
shapiro.test(sample(mods$I$residuals, 5000))

#---------- Heterocedas.    ----------
# Resíduos vs fit
ggplot(mapping=aes(x=mods$F$fitted.value, y=mods$F$residuals)) +
  geom_point(color=pal[4], alpha=0.3) +
  geom_hline(yintercept=0) +
  ylab("Resíduos") + xlab("Valores fitados") + labs(title="Resíduos vs fit") +
  xlim(5,15) + ylim(-5,5)

bptest(mods$F) #BP
bptest(mods$F, ~ poly(fitted(mods$F),2)) #white

# Correção
robustSE = list()
for(i in names(mods)){
  robustSE[[i]] = coeftest(mods[[i]], vcov=sandwich::vcovHC(mods[[i]], "HC1"))[,2]}

stargazer(mods$D, mods$C, mods$R, mods$F,
          single.row=TRUE, omit=c("RELIGIÃO", "ATIVIDADE"),
          se=robustSE)

########### PREVISÃO            ##########
nt = round(0.9*n)

Yt = Y[1:nt]
Xt = Vars[1:nt,]

Yr = Y[(nt+1):n]
Xr = Vars[(nt+1):n,]


#========== STEPWISE          ==========
for(i in c("both", "backward", "forward")){
  mods[[i]] = MASS::stepAIC(lm(Yt ~ ., Xt), direction=i)
  print(summary(mods[[i]]))
  print("=============================================")}

# Tudo igual:
names(mods$both$coefficients)
names(mods$backward$coefficients)
names(mods$forward$coefficients)

names(mods$forward$coefficients)[-which(names(mods$forward$coefficients) %in% names(mods$backward$coefficients))]
names(mods$backward$coefficients)[-which(names(mods$backward$coefficients) %in% names(mods$F$coefficients))]


pred = predict(mods$forward, newdata=Xr[-which(Xr$RAÇA==9),])
sum((Yr[-which(Xr$RAÇA==9)] - pred)^2)/(n-nt)


pred = predict(modelo, newdata=Xr)
sum((Yr - pred)^2)/(n-nt)
#0.2994626 back
#0.2994862 forw

stargazer(mods$both, mods$forward, single.row=TRUE, report="vc")

stargazer
#========== GLMNET            ==========
nt = round(0.9*n)

Yt = Y[1:nt]
Xt = Vars2[1:nt,]

Yr = Y[(nt+1):n]
Xr = Vars2[(nt+1):n,]

Dicio$DUMMIE[21:22] = c("S", "S")

Vars2 = numeric(566603)
for(i in 1:nrow(Dicio)){
  if(Dicio$DUMMIE[i]=="S"){
    col = fastDummies::dummy_cols(Vars[,i])[,-c(1:2)]
    if(!is.null(dim(col))){colnames(col)[1]=Dicio[i,1]}}
  else{col = Vars[,i]}
  Vars2 = data.frame(Vars2, col)}
Vars2 = Vars2[,-1]

Vars2 = apply(Vars2, 2, as.numeric)

library(glmnet)
for(i in seq(0,1,0.2)){
  cv = cv.glmnet(y=Yt, x=as.matrix(Xt), alpha=i, nfolds=5)
  l.min = cv$lambda.min
  
  mod.a1 = glmnet(y=Yt, x=as.matrix(Xt), lambda=l.min)
  mod.a1$beta
  
  pred = predict(mod.a1, newx=Xr)
  
  print(i)
  print(l.min)
  print(sum((Yr - pred)^2)/(n-nt))
  print(mod.a1$a0)
  print(mod.a1$beta)
  print("=================")}



  