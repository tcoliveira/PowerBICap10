getwd()

##pacotes do R
install.packages(c("Amelia",'caret','ggplot2','dplyr','reshape','randomForest'))
#carregando os pacotes, tem de ser um por um ¬¬
library(Amelia)#, caret, ggplot2, dplyr, reshape, randomForest))
library(caret)
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)
#Carregando o dataset, usando a função read.csv
dt<-read.csv("credit-card.csv")

View(dt)
str(dt)
head(dt)
tail(dt)

#############################COmeçando o ETL, importante é onde esta o diferencial.

#convertendo idade, sexo, escolaridade e estado civil para fatores(ou categorias)

#Idade

head(dt$AGE)
#Converter para categoria, não mais numerico;
dt$Age<-cut(dt$AGE, c(0,30,50,100), labels=c('Kid', 'Adult', 'Old'))
head(dt$Age)

#Sexo
dt$SEX<-cut(dt$SEX, c(0,1,2), labels=c('Male', 'Female'))
head(dt$SEX)

#Education
dt$EDUCATION<-cut(dt$EDUCATION, c(0,1,2,3,4), labels=c('Grad', 'Undergrad','High School','Others'))

#Marriage status
dt$MARRIAGE<-cut(dt$MARRIAGE, c(-1,0,1,2,3),
                 labels = c('Unknown','Married','Single','Others'))
#Payments
dt$PAY_0<-as.factor(dt$PAY_0)
dt$PAY_2<-as.factor(dt$PAY_2)
dt$PAY_3<-as.factor(dt$PAY_3)
dt$PAY_4<-as.factor(dt$PAY_4)
dt$PAY_5<-as.factor(dt$PAY_5)
dt$PAY_6<-as.factor(dt$PAY_6)
#Changing my target variable as factor
dt$default.payment.next.month <-as.factor(dt$default.payment.next.month)
head(dt)

#renaming column
colnames(dt)
colnames(dt)[25][1]<-'Overdue'

#Checking missing values
sapply(dt, function(x) sum(is.na(x)))
missmap(dt, main = 'Checked missing values')
dt<-na.omit(dt)

#Removing ID
dt$ID <-NULL
table(dt$Overdue)

#Plot with ggplot2
qplot(Overdue, data = dt, geom = 'bar') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Set seed
set.seed(12345)
#Amostragem estratificado, seleciona as linhas de acorda com a variavel default.payment.month como estratificad
TrainingDataIndex<-createDataPartition(dt$Overdue, p = 0.45, list = FALSE)
TrainingDataIndex

#Traingin Data as a subset with index line
trainData<-dt[TrainingDataIndex,]
table(trainData$Overdue)

#percentage
prop.table(table(trainData$Overdue))

#Numero de linhas
nrow(trainData)
#Compare percentage between class e traing data
DCOmpare<-cbind(prop.table(table(trainData$Overdue)), prop.table(table(dt$Overdue)))
colnames(DCOmpare)<-c('Train','Original')
DCOmpare

#Melt Data
meltedDcomp<-melt(DCOmpare)
meltedDcomp
#plot to compare original and train
ggplot(meltedDcomp, aes(x= X1, y = value)) + geom_bar(aes(fill = X2),stat = 'identity', position = 'dodge')

#Test data
testData<-dt[-TrainingDataIndex,]
#Using cross validation
TrainingParameters<-trainControl(method='cv', number = 10)
##########Random Forest Classification Model

#Building model with random Forest
rf_model<-randomForest(Overdue ~.,data=trainData)
rf_model
#Checking model error
plot(rf_model, ylim = c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col = 1:3, fill = 1:3)

varImpPlot(rf_model)

#Checking more important variables
importance <- importance(rf_model)
varImportance<-data.frame(Variables = row.names(importance), Importance = round(importance[,'MeanDecreaseGini']))

#Ranking variables
rankImportance<-varImportance%>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

#usando ggplot2 para visualizar a importancia relativa das variaveis
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat='identity') +
  geom_text(aes(x=Variables, y =0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = 'red') +
  labs(x = 'Variables')+
  coord_flip()
#Prediction
predictorf<-predict(rf_model, testData)

#COnfusion Mtraix
install.packages('e1071')
library(e1071)
cmrf<-confusionMatrix(predictorf, testData$Overdue, positive = '1')
cmrf

saveRDS(rf_model, file = 'rf_model.rds')

#Carregando o modelo
modelo<-readRDS('rf_model.rds')
