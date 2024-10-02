source('models.r')

library(tidyverse)
library(gridExtra)

#create an average value from the 3 ML models
test.set_A$avg <- as.factor(round((as.numeric(test.set_A$LR) + as.numeric(test.set_A$RF)  + as.numeric(test.set_A$SVML))/3-1,0))


test.set_B$avg <- as.factor(round((as.numeric(test.set_B$LR) + as.numeric(test.set_B$RF)  + as.numeric(test.set_B$SVML))/3-1,0))

robert <- read.csv('datasets/ADAB_90_test_No_PFI.csv')


#tabulate confusion matrices values into vectors for each ML model

#indexes: 1. true positive, 2. false negative, 3. false positive, 4. true negative
LR_A <- c(table(test.set_A$cond,test.set_A$LR)[1],
          table(test.set_A$cond,test.set_A$LR)[2],
          table(test.set_A$cond,test.set_A$LR)[3],
          table(test.set_A$cond,test.set_A$LR)[4])

RF_A <- c(table(test.set_A$cond,test.set_A$RF)[1],
          table(test.set_A$cond,test.set_A$RF)[2],
          table(test.set_A$cond,test.set_A$RF)[3],
          table(test.set_A$cond,test.set_A$RF)[4])

SVML_A <- c(table(test.set_A$cond,test.set_A$SVML)[1],
          table(test.set_A$cond,test.set_A$SVML)[2],
          table(test.set_A$cond,test.set_A$SVML)[3],
          table(test.set_A$cond,test.set_A$SVML)[4])

avg_A <- c(table(test.set_A$cond,test.set_A$avg)[1],
            table(test.set_A$cond,test.set_A$avg)[2],
            table(test.set_A$cond,test.set_A$avg)[3],
            table(test.set_A$cond,test.set_A$avg)[4])


LR_B <- c(table(test.set_B$cond,test.set_B$LR)[1],
          table(test.set_B$cond,test.set_B$LR)[2],
          table(test.set_B$cond,test.set_B$LR)[3],
          table(test.set_B$cond,test.set_B$LR)[4])

RF_B <- c(table(test.set_B$cond,test.set_B$RF)[1],
          table(test.set_B$cond,test.set_B$RF)[2],
          table(test.set_B$cond,test.set_B$RF)[3],
          table(test.set_B$cond,test.set_B$RF)[4])

SVML_B <- c(table(test.set_B$cond,test.set_B$SVML)[1],
            table(test.set_B$cond,test.set_B$SVML)[2],
            table(test.set_B$cond,test.set_B$SVML)[3],
            table(test.set_B$cond,test.set_B$SVML)[4])

avg_B <- c(table(test.set_B$cond,test.set_B$avg)[1],
           table(test.set_B$cond,test.set_B$avg)[2],
           table(test.set_B$cond,test.set_B$avg)[3],
           table(test.set_B$cond,test.set_B$avg)[4])

rob <- c(table(robert$response,robert$response_pred)[1],
         table(robert$response,robert$response_pred)[2],
         table(robert$response,robert$response_pred)[3],
         table(robert$response,robert$response_pred)[4])


#create a dataframe with confusion matrix values
conf_A <- data.frame('result' = c('True Positive', 'False Negative', 'False Positive', 'True Negative'),
                        'LR' = LR_A, 
                        'RF' = RF_A,
                        'SVML' = SVML_A,
                        'avg' = avg_A,
                        'robert' = rob)
write.csv(conf_A, file = 'confusionmatrix_acids.csv')

#calculate accuracy, sensitivity, and specificity of each value
acc_A <- c()
sens_A <- c()
spec_A <- c()
for(i in 2:ncol(conf_A)){
  acc_A <- c(acc_A,(conf_A[1,i]+conf_A[4,i])/sum(conf_A[,i]))
  sens_A <- c(sens_A,(conf_A[4,i])/(conf_A[4,i]+conf_A[3,i]))
  spec_A <- c(spec_A,(conf_A[1,i])/(conf_A[1,i]+conf_A[2,i]))
}


res_A <- data.frame('ML' = c('LR', 'RF', 'SVML', 'average','ROBERT'),
                    'Accuracy' = acc_A, 
                    'Sensitivity' = sens_A, 
                    'Specificity' = spec_A)


results_A <- res_A %>% pivot_longer(cols = c('Accuracy', 'Sensitivity', 'Specificity'), names_to = 'Result', values_to = 'Values')




conf_B <- data.frame('result' = c('True Positive', 'False Negative', 'False Positive', 'True Negative'),
                     'LR' = LR_B, 
                     'RF' = RF_B,
                     'SVML' = SVML_B,
                     'avg' = avg_B,
                     'robert' = rob)

write.csv(conf_B, file = 'confusionmatrix_bases.csv')

acc_B <- c()
sens_B <- c()
spec_B <- c()
for(i in 2:ncol(conf_B)){
  acc_B <- c(acc_B,(conf_B[1,i]+conf_B[4,i])/sum(conf_B[,i]))
  sens_B <- c(sens_B,(conf_B[4,i])/(conf_B[4,i]+conf_B[3,i]))
  spec_B <- c(spec_B,(conf_B[1,i])/(conf_B[1,i]+conf_B[2,i]))
}


res_B <- data.frame('ML' = c('LR', 'RF', 'SVML', 'average', 'ROBERT'),
                    'Accuracy' = acc_B, 
                    'Sensitivity' = sens_B, 
                    'Specificity' = spec_B)


results_B <- res_B %>% pivot_longer(cols = c('Accuracy', 'Sensitivity', 'Specificity'), names_to = 'Result', values_to = 'Values')






#plot confusion matrix values



#acids
p1 <- ggplot(results_A) + 
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(5:10) * 0.1),
    color = "lightgrey"
  ) + 
  geom_bar( aes(fill=ML,x=Result,y=Values),stat = "identity",colour='black',alpha=0.7,position = position_dodge(),width=0.7) +
  geom_segment(aes(x=Result,y=0,xend=Result,yend=1),linetype='dashed',size=1) +
  theme(panel.background = element_blank(), panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.position = 'bottom',
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
  labs(y="Value",x="Confusion Matrix Results", title = 'Acids') + 
  scale_fill_manual(values=c('red','blue4','darkgreen','gold3','purple')) + 
  coord_polar() + 
  annotate(geom='text',label='0.5',x=0.5,y=0.5,size=3.6,alpha=0.5) +
  annotate(geom='text',label='0.6',x=0.5,y=0.6,size=3.6,alpha=0.5) +
  annotate(geom='text',label='0.7',x=0.5,y=0.7,size=3.6,alpha=0.5) +
  annotate(geom='text',label='0.8',x=0.5,y=0.8,size=3.6,alpha=0.5) +
  annotate(geom='text',label='0.9',x=0.5,y=0.9,size=3.6,alpha=0.5) +
  annotate(geom='text',label='1',x=0.5,y=1,size=3.6,alpha=0.5)




#bases
p2 <- ggplot(results_B) + 
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(5:10) * 0.1),
    color = "lightgrey"
  ) + 
  geom_bar( aes(fill=ML,x=Result,y=Values),stat = "identity",colour='black',alpha=0.7,position = position_dodge(),width=0.7) +
  geom_segment(aes(x=Result,y=0,xend=Result,yend=1),linetype='dashed',size=1) +
  theme(panel.background = element_blank(), panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.position = 'bottom',
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
  labs(y="Value",x="Confusion Matrix Results", title = 'Bases') + 
  scale_fill_manual(values=c('red','blue4','darkgreen','gold3','purple')) + 
  coord_polar() + 
  annotate(geom='text',label='0.5',x=0.5,y=0.5,size=3.6,alpha=0.5) +
  annotate(geom='text',label='0.6',x=0.5,y=0.6,size=3.6,alpha=0.5) +
  annotate(geom='text',label='0.7',x=0.5,y=0.7,size=3.6,alpha=0.5) +
  annotate(geom='text',label='0.8',x=0.5,y=0.8,size=3.6,alpha=0.5) +
  annotate(geom='text',label='0.9',x=0.5,y=0.9,size=3.6,alpha=0.5) +
  annotate(geom='text',label='1',x=0.5,y=1,size=3.6,alpha=0.5)


plots <- list(p1,p2)


plots_grid <- grid.arrange(do.call(arrangeGrob,c(plots, ncol = 2, nrow = 1)))

ggsave('plots/confusion_res.pdf', plot = plots_grid, width = 10, height = 6, units = 'in')
ggsave('plots/confusion_res.png', plot = plots_grid, width = 10, height = 6, units = 'in')
