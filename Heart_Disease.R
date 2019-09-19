#Loading the Dataset
hd_data=read.csv('Cleveland_hd.csv')
print(head(hd_data))

library(tidyverse)

hd_data$hd=ifelse(hd_data$class>0,1,0)
hd_data$sex=factor(hd_data$sex,levels=c(0,1),labels=c('Female','Male'))


#To Find the relation between sex and class using Chi-Square
hd_Sex=chisq.test(hd_data$sex,hd)
hd_Sex

#To Find the relation between age and class
hd_age=t.test(hd_data$age~hd)
print(hd_age)


#To Find the relation between age and class
hd_heartrate=t.test(hd_data$thalach~hd)
print(hd_heartrate)

hd_data$hd_label=ifelse(hd==0,'No Disease','Disease')
hd_label

# age vs hd
ggplot(hd_data,aes(x=hd_label,y=age))+geom_boxplot()

#sex vs hd
ggplot(hd_data,aes(x=hd_label,fill=sex))+geom_bar(position = 'fill',ylab='Sex%')


#Heartrate vs hd
ggplot(hd_data,aes(x=hd_label,y=thalach))+geom_boxplot()


#Training the model
model=glm(data=hd_data,hd ~ age+sex+thalach,family='binomial')
summary(model)

library(broom)

#Summarizing the model
tidy_m=tidy(model)
tidy_m

#Calculating ODD Ratios
tidy_m$OR=exp(tidy_m$estimate)
tidy_m

#95% CI and save as lower CI and upper CI
tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96 * tidy_m$std.error)
tidy_m$upper_CI <- exp(tidy_m$estimate - 0.95 * tidy_m$std.error)

tidy_m


#Prediction
pred_prob=predict(model,hd_data,type='response')
pred_prob

hd_data$prediction=ifelse(pred_prob>0.5,1,0)

#To Calculate prediction for specific data change the parametres
newdata <- data.frame(age = 45, sex = "Female", thalach = 150)
pred_prob=predict(model,newdata,type='response')
pred_prob


