library(gmodels)
library(class)
datafertility=read.csv("fertility_Diagnosis.txt", header=FALSE,sep=",")
names(datafertility)=c("season","age","children_diseases","accident/trauma","surgical","fever","alcohol","smoke","sitting","diagnosis")
str(datafertility)
View(datafertility)
summary(datafertility)
#labels
table(datafertility$diagnosis)
datafertility$diagnosis=factor(datafertility$diagnosis,levels = c("N","O"),labels=c("Normal","Altered"))
class(datafertility)
prop.table(table(datafertility$diagnosis))
datafertility_n=as.data.frame(datafertility[1:9])
#training and testing
datafertility_train = datafertility_n[1:70,]
datafertility_test = datafertility_n[71:100,]
# get labels for training and testing 
datafertility_train_labels=datafertility[1:70, 10]
datafertility_test_labels=datafertility[71:100, 10]
#predict1 
predictions1=knn(train=datafertility_train,test=datafertility_test,
                 cl=datafertility_train_labels,k=1)
#predict2 
predictions2=knn(train = datafertility_train, test = datafertility_test, 
                   cl = datafertility_train_labels, k=2)
#predict3 27/30 is good
predictions3=knn(train = datafertility_train, test = datafertility_test, 
                 cl = datafertility_train_labels, k=3)
#predict4 
predictions4=knn(train = datafertility_train, test = datafertility_test, 
                 cl = datafertility_train_labels, k=4)

CrossTable(predictions3, datafertility_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
cm=table(predictions3,datafertility_test_labels)
sum(diag(cm))/sum(cm)

