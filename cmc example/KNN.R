library(gmodels)
library(class)
data=read.csv("./cmc.data",header=FALSE,sep=",")
names(data)=c("wife_age","wife_education","husband_education","childrenNumber","wife_religion","wife_work","husband_occupation","livingStandard","mediaExposure","contraceptiveMethod")
str(data)
View(data)
table(data$contraceptiveMethod)
data$contraceptiveMethod=factor(data$contraceptiveMethod,levels = c("1","2","3"),labels=c("No-use","Long-term","Short-term"))
class(data)
prop.table(table(data$contraceptiveMethod))
data_n=as.data.frame(data[1:9])
#training and testing
data_train = data_n[1:1000,]
data_test = data_n[1001:1473,]
# get labels for training and testing 
data_train_labels=data[1:1000, 10]
data_test_labels=data[1001:1473, 10]
#predict2
predictions2=knn(train = data_train, test = data_test, 
                 cl = data_train_labels, k=2)
#predict3 is good
predictions3=knn(train = data_train, test = data_test, 
                 cl = data_train_labels, k=3)

CrossTable(predictions3, data_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
cm=table(predictions2,data_test_labels)
sum(diag(cm))/sum(cm)

