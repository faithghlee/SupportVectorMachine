source('SupportVectorFunctions.R')
library(gridExtra) 

#EDA
plot_1<-generate_plot(linear_train, type = "Linear", label = "Training")
plot_2<-generate_plot(noisy_train, type  = "Noisy", label = "Training")
plot_3<-generate_plot(quadratic_train, type = "Quadratic", label = "Training")
grid.arrange(plot_1, plot_2, plot_3)

plot_1<-generate_plot(linear_test, type = "Linear", label = "Testing")
plot_2<-generate_plot(noisy_test, type = "Noisy", label = "Testing")
plot_3<-generate_plot(quadratic_test, type = "Quadratic", label = "Testing")
grid.arrange(plot_1, plot_2, plot_3)


#Generate Plots 
#HARD MARGIN 
p1<-results(linear_train[,-3], linear_train[,3], type = "Linear", label = "Training")
p2<-results(noisy_train[,-3], noisy_train[,3], type = "Noisy Linear", label = "Training")
p3<-results(quadratic_train[,-3], quadratic_train[,3], type = "Quadratic", label = "Training")

p1a<-results(linear_test[,-3], linear_test[,3], type = "Linear", label = "Test")
p2a<-results(noisy_test[,-3], noisy_test[,3], type = "Noisy Linear", label = "Test")
p3a<-results(quadratic_test[,-3], quadratic_test[,3], type = "Quadratic", label = "Test")

grid.arrange(p1, p1a, p2,  p2a,p3, p3a, nrow = 3, ncol = 2)

#SOFT MARGIN

p1<-results(linear_train[,-3], linear_train[,3], gamma = 0.5, type = "Linear", label = "Training")
p2<-results(noisy_train[,-3], noisy_train[,3], gamma= 0.5, type = "Noisy Linear", label = "Training")
p3<-results(quadratic_train[,-3], quadratic_train[,3], gamma = 0.5, type = "Quadratic", label = "Training")

p1a<-results(linear_test[,-3], linear_test[,3], gamma = 0.5, type = "Linear", label = "Test")
p2a<-results(noisy_test[,-3], noisy_test[,3], gamma = 0.5, type = "Noisy Linear", label = "Test")
p3a<-results(quadratic_test[,-3], quadratic_test[,3], gamma = 0.5, type = "Quadratic", label = "Test")

grid.arrange(p1, p1a, p2,  p2a,p3, p3a, nrow = 3, ncol = 2)

