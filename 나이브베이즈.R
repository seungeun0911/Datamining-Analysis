# NaiveBayes

nb_model <- naiveBayes(dong.training.norm, dong.training.norm$label, laplace = 1) 
nb_pred<-predict(nb_model, dong.testing.norm, type="class") 

result<-cbind(dong.testing.norm,dong.testing.norm$label,nb_pred)

acc<-table(nb_pred, dong.testing.norm$label) 
acc<-prop.table(table(nb_pred, dong.testing.norm$label))

CrossTable(x=dong.testing.norm$label, y=nb_pred, prop.chisq=FALSE)


table(nb_pred, subway.testLabels)