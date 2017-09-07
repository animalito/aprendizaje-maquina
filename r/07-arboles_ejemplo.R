###################################################
### chunk number 1: 
###################################################
options(digits=4)


###################################################
### chunk number 2: 
###################################################

library(rpart)
library(maptree)
library(ROCR)
library(xtable)
spam.train<-read.table("./datos/spam.train",sep=",")
names(spam.train) <- c("wfmake", "wfaddress", "wfall", "wf3d", "wfour",
	"wfover", "wfremove", "wfinternet", "wforder", "wfmail", 
	"wfreceive", "wfwill", "wfpeople", "wfreport", "wfaddresses", 
	"wffree", "wfbusiness", "wfemail", "wfyou", "wfcredit", "wfyour", 
	"wffont", "wf000", "wfmoney", "wfhp", "wfhpl", "wfgeorge", "wf650", 
	"wflab", "wflabs", "wftelnet", "wf857", "wfdata", "wf415", "wf85", 
	"wftechnology", "wf1999", "wfparts", "wfpm", "wfdirect", "wfcs", 
	"wfmeeting", "wforiginal", "wfproject", "wfre", "wfedu", "wftable", 
	"wfconference", "cfsc", "cfpar", "cfbrack", "cfexc", "cfdollar", 
	"cfpound", "crlaverage", "crllongest", "crltotal", "spam")
spam.train$spam<-as.factor(spam.train$spam)


###################################################
### chunk number 3: 
###################################################
table(spam.train$spam)
table(spam.train$spam)/length(spam.train$spam)


###################################################
### chunk number 4: spam_arbol_completo
###################################################
set.seed(22)
control.completo<-rpart.control(cp=0, minsplit=10,
        minbucket=1, xval=10, maxdepth=30)
#spam.train$spam <- factor(spam.train$spam, levels=c(0,1), labels=c('not spam','spam'))
spam_tree_completo<-rpart(spam~.,data = spam.train, method="class",
    control = control.completo)
#print(spam_tree_completo)
#print(plot(spam_tree_completo,compress=TRUE,margin=0.1))
#print(text(spam_tree_completo))

## es mejor no correr desde Rstudio?
par(mar=c(1,1,1,1))
par(lwd=0.3)
plot(spam_tree_completo,uniform=T)
text(spam_tree_completo,use.n=T,cex=0.2)
#print(draw.tree(spam_tree_completo,nodeinfo=TRUE,cex=0.2,print.levels=TRUE))


# Indicadores de ajuste para el árbol gigante
predicted <- predict(spam_tree_completo, type="class")
1-mean(predicted==spam.train$spam)
table(predicted,spam.train$spam)
prop.table(table(predicted,spam.train$spam),2)



#### Ahora podamos el arbol gigante


printcp(spam_tree_completo)
plotcp(spam_tree_completo)
## podamos al tamaño que está a una desviación estándar del mínimo parámetro de complejidad (cp)
spam_tree_podado <- prune(spam_tree_completo,cp=0.0028)

print(spam_tree_podado)


## Graficar árbol podado
## usando draw.tree de maptree


library(rpart.plot)
prp(spam_tree_podado, type=4)
prp(spam_tree_podado, type=2)
prp(spam_tree_podado, type=2, extra=1) # obs en cada nodo por clase
prp(spam_tree_podado, type=2, extra=2) # tasa de correctos
prp(spam_tree_podado, type=2, extra=4, branch=1) # proporciones de clase

## para árboles chicos:
library(rattle)
fancyRpartPlot(prune(spam_tree_podado,cp=0.01))
###################################################
### chunk number 9: 
###################################################

predicted<-predict(spam_tree_podado,type="class")
print(1-mean(predicted==spam.train$spam),digits=2)
table(predicted,spam.train$spam,deparse.level=2)
print(prop.table(table(predicted,spam.train$spam,deparse.level=2),2),digits=2)

predicted.prob <- predict(spam_tree_podado)




###################################################
### chunk number 11: 
###################################################
printcp(spam_tree_podado)

###Interpretar
summary(spam_tree_podado)

###################################################
### chunk number 12: leerprueba
###################################################
spam.test<-read.table("./data/spam.test",sep=",")
names(spam.test) <- c("wfmake", "wfaddress", "wfall", "wf3d", "wfour",
	"wfover", "wfremove", "wfinternet", "wforder", "wfmail", 
	"wfreceive", "wfwill", "wfpeople", "wfreport", "wfaddresses", 
	"wffree", "wfbusiness", "wfemail", "wfyou", "wfcredit", "wfyour", 
	"wffont", "wf000", "wfmoney", "wfhp", "wfhpl", "wfgeorge", "wf650", 
	"wflab", "wflabs", "wftelnet", "wf857", "wfdata", "wf415", "wf85", 
	"wftechnology", "wf1999", "wfparts", "wfpm", "wfdirect", "wfcs", 
	"wfmeeting", "wforiginal", "wfproject", "wfre", "wfedu", "wftable", 
	"wfconference", "cfsc", "cfpar", "cfbrack", "cfexc", "cfdollar", 
	"cfpound", "crlaverage", "crllongest", "crltotal", "spam")
spam.test$spam<-as.factor(spam.test$spam)


###################################################
### chunk number 13: descr
###################################################
table(spam.train$spam)
table(spam.test$spam)/length(spam.test$spam)

pob.pred <- predict(spam_tree_podado, spam.test)
clase.pred <- predict(spam_tree_podado, spam.test, type='class')
table(clase.pred, spam.test$spam)
prop.table(table(clase.pred, spam.test$spam),2)
prop.table(table(clase.pred==spam.test$spam))

    

