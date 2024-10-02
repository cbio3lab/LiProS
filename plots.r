source('models.r')


library(ggplot2)
library(ggcorrplot)
library(gridExtra)


#correlation plots of the descriptors after the feature selection
corr <- round(cor(acids_d[,-ncol(acids_d)]),2)
p <- ggcorrplot(corr,show.diag = F,type='lower', lab = TRUE, lab_size=3) + 
  scale_fill_gradient2(low='blue4',high='red4',breaks=c(-1,-0.5,0,0.5,1),limit=c(-1,1),name='Correlation')
ggsave(p,file='plots/corr_A.pdf',width = 4000, height = 3734,units = 'px')
ggsave(p,file='plots/corr_A.png',width = 4000, height = 3734,units = 'px')



corr <- round(cor(bases_d[,-ncol(bases_d)]),2)
p <- ggcorrplot(corr,show.diag = F,type='lower', lab = TRUE, lab_size=3) + 
  scale_fill_gradient2(low='blue4',high='red4',breaks=c(-1,-0.5,0,0.5,1),limit=c(-1,1),name='Correlation')
ggsave(p,file='plots/corr_B.pdf',width = 4000, height = 3734,units = 'px')
ggsave(p,file='plots/corr_B.png',width = 4000, height = 3734,units = 'px')





#violin plots for acid and basic descriptors that had p < 0.05

acids_violins <- list() #save every plot in this list

#automate plotting into the list
for (i in 1:(ncol(acids_d)-1)) {
  col_name <- colnames(acids_d)[i]
  
  p <- ggplot(acids_d) + 
    geom_violin(aes_string(x = "cond", y = col_name),fill='gray',color="black", size=0.4, alpha=0.5) +
    geom_boxplot(aes_string(x = "cond", y = col_name, fill = "cond"),width=0.8) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill=NA,colour="black",size=1),
          axis.title.x = element_text(size = 10),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.title = element_text(size=10),
          legend.text = element_text(size=8),
          legend.position = 'none') + 
    labs(y=col_name,x='Better Fit') +
    scale_fill_manual(name="Better fit",
                      values=c("seagreen4", "red"),
                      labels=c('Eq. 2','Both or Eq. 1')) + 
    scale_x_discrete(labels=c("0" = "Eq. 2", "1" = "Both or Eq. 1"))
  
  acids_violins[[i]] <- p #se pone cada grafica en la lista
}



ncol <- 5
nrow <- 5

#grid plot creation
grid_acids <- grid.arrange(do.call(arrangeGrob, c(acids_violins,ncol = ncol, nrow = nrow)))
ggsave('plots/acids_violins.pdf', plot = grid_acids, width = 11, height = 11, units = 'in')
ggsave('plots/acids_violins.png', plot = grid_acids, width = 11, height = 11, units = 'in')






bases_violins <- list() #save every plot in this list

#automate plotting into the list
for (i in 1:(ncol(bases_d)-1)) {
  col_name <- colnames(bases_d)[i]
  
  p <- ggplot(bases_d) + 
    geom_violin(aes_string(x = "cond", y = col_name),fill='gray',color="black", size=0.4, alpha=0.5) +
    geom_boxplot(aes_string(x = "cond", y = col_name, fill = "cond"),width=0.8) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill=NA,colour="black",size=1),
          axis.title.x = element_text(size = 10),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.title = element_text(size=10),
          legend.text = element_text(size=8),
          legend.position = 'none') + 
    labs(y=col_name,x='Better Fit') +
    scale_fill_manual(name="Better fit",
                      values=c("seagreen4", "red"),
                      labels=c('Eq. 2','Both or Eq. 1')) + 
    scale_x_discrete(labels=c("0" = "Eq. 2", "1" = "Both or Eq. 1"))
  
  bases_violins[[i]] <- p #se pone cada grafica en la lista
}



ncol <- 5
nrow <- 7

#grid plot creation
grid_bases <- grid.arrange(do.call(arrangeGrob, c(bases_violins,ncol = ncol, nrow = nrow)))
ggsave('plots/bases_violins.pdf', plot = grid_bases, width = 11, height = 11, units = 'in')
ggsave('plots/bases_violins.png', plot = grid_bases, width = 11, height = 11, units = 'in')







#descriptor importance plot (from Random Forest models)

#acids

ImpData_A <- as.data.frame(importance(model_RF_A))
ImpData_A$Var.Names <- row.names(ImpData_A)

p1 <- ggplot(ImpData_A, aes(x=Var.Names, y=MeanDecreaseGini)) +
  labs(x="", y = "MeanDecreaseGini") + 
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=MeanDecreaseGini), color="gray") +
  geom_point(aes(size = MeanDecreaseGini), color="blue4", alpha=0.8) +
  coord_flip() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA,colour="black",size=1),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.position='none')



#bases
ImpData_B <- as.data.frame(importance(model_RF_B))
ImpData_B$Var.Names <- row.names(ImpData_B)

p2 <- ggplot(ImpData_B, aes(x=Var.Names, y=MeanDecreaseGini)) +
  labs(x="", y = "MeanDecreaseGini") + 
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=MeanDecreaseGini), color="gray") +
  geom_point(aes(size = MeanDecreaseGini), color="red4", alpha=0.8) +
  coord_flip() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA,colour="black",size=1),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.position='none')



plots <- list(p1,p2)


plots_grid <- grid.arrange(do.call(arrangeGrob,c(plots, ncol = 2, nrow = 1)))

ggsave('plots/importance.pdf', plot = plots_grid, width = 15, height = 6, units = 'in')
ggsave('plots/importance.png', plot = plots_grid, width = 15, height = 6, units = 'in')




library(readxl)
library(svglite)
naprore <- read_excel('NAPRORE_lipros/naprore_sample.xlsx') 
naprore$NPC_PATHWAY_1 <- ifelse(is.na(naprore$NPC_PATHWAY_1), "No label", naprore$NPC_PATHWAY_1)
naprore$Prediction <- ifelse(is.na(naprore$Prediction), "No prediction", naprore$Prediction)


n <- table(naprore$NPC_PATHWAY_1)


p3 <- ggplot(naprore) + 
  geom_bar(aes(y = NPC_PATHWAY_1, fill = Prediction), 
           position = 'fill', 
           color = 'black') + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = 'black', linewidth = 1), 
        legend.direction = 'horizontal', 
        legend.position = 'bottom', 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)) + 
  labs(x = "Amount", y = "") + 
  scale_fill_manual("", values = c('gray', 'magenta4', 'red4')) + 
  geom_label(aes(x = 0.8, y = 1, label = paste('n =', n[[1]])), size = 6) + 
  geom_label(aes(x = 0.8, y = 2, label = paste('n =', n[[2]])), size = 6) + 
  geom_label(aes(x = 0.8, y = 3, label = paste('n =', n[[3]])), size = 6) + 
  geom_label(aes(x = 0.8, y = 4, label = paste('n =', n[[4]])), size = 6) + 
  geom_label(aes(x = 0.8, y = 5, label = paste('n =', n[[5]])), size = 6) + 
  geom_label(aes(x = 0.8, y = 6, label = paste('n =', n[[6]])), size = 6) + 
  geom_label(aes(x = 0.8, y = 7, label = paste('n =', n[[7]])), size = 6) 

ggsave('plots/naprore.pdf', plot = p3, width = 8, height = 6, units = 'in')
ggsave('plots/naprore.png', plot = p3, width = 8, height = 6, units = 'in')






####plot OUTLIERS NAPRORE

outliers_naprore <- read.csv('NAPRORE_lipros/outliers.csv')
acids_norm <- acids_d[,-ncol(acids_d)]

for (i in 1:ncol(acids_norm)){
  acids_norm[,i] <- abs(acids_norm[,i])/max(acids_norm[,i])
  outliers_naprore[,i] <- abs(outliers_naprore[,i])/max(acids_norm[,i])
}

acids_norm <- pivot_longer(acids_norm, 
                           cols = everything()
                           ,names_to = "Descriptor", 
                           values_to = "rel_value")


outliers_naprore <- pivot_longer(outliers_naprore, 
                           cols = everything()
                           ,names_to = "Descriptor", 
                           values_to = "rel_value")

outliers_naprore$mol <- c(rep('Gemin D', 21), rep('Borucoside', 21))




p4 <- ggplot() + 
  geom_violin(aes(x = acids_norm$rel_value, y = acids_norm$Descriptor, fill = acids_norm$Descriptor), width = 2) + 
  geom_point(aes(x = outliers_naprore$rel_value[-c(1,22)], y = outliers_naprore$Descriptor[-c(1,22)], colour = outliers_naprore$mol[-c(1,22)]),
             size = 4) + 
  theme_minimal() + 
  theme(panel.border = element_rect(fill = NA, color = 'black', linewidth = 1), 
        legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16)) + 
  guides(fill = 'none') + 
  scale_color_manual("", values = c("red4", "purple4")) + 
  labs(x = "Relative value", y = "Descriptor")


ggsave('plots/outliers.pdf', plot = p4, width = 8.5, height = 11, units = 'in')
ggsave('plots/outliers.png', plot = p4, width = 8.5, height = 11, units = 'in')




#chemical space mapping of naprore
naprore$NPC_PATHWAY_1 <- gsub("Amino acids and Peptides", "Amino acids \nand Peptides", naprore$NPC_PATHWAY_1)
naprore$NPC_PATHWAY_1 <- gsub("Shikimates and Phenylpropanoids", "Shikimates and \nPhenylpropanoids",naprore$NPC_PATHWAY_1)

naprore_violins <- list()

for (i in 24:29) {
  col_name <- colnames(naprore)[i]
  
p <- ggplot(naprore) + 
    geom_violin(aes_string(y = "NPC_PATHWAY_1", fill = "NPC_PATHWAY_1", x = col_name),color="black", size=0.4) +
    geom_jitter(aes_string(y = "NPC_PATHWAY_1", color = "NPC_PATHWAY_1", x = col_name),width=0.8, alpha = 0.5) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill=NA,colour="black",size=1),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          legend.position = 'none') + 
    labs(x=col_name,y='Pathway') 
  
  naprore_violins[[i-23]] <- p #se pone cada grafica en la lista
}
  
plots_grid <- grid.arrange(do.call(arrangeGrob,c(naprore_violins, ncol = 3, nrow = 2)))
ggsave('plots/naprore_cs.pdf', plot = plots_grid, width = 14, height = 8, units = 'in')
ggsave('plots/naprore_cs.png', plot = plots_grid, width = 14, height = 8, units = 'in')




  