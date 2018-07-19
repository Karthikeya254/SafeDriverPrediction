require(ggplot2)

df <- data.frame(Algo = c(0.2275614, 0.2274766, 0.2317357,0.2227995, 0.2343704)
                 ,Smote = c(0.2172669, 0.2169862, 0.2228861, 0.2252394,0.2225442)
                 ,Case = 1:5
                 ,Algorithm = 'Naive Bayes')

df <- rbind(df,data.frame(Algo = c(0.2539336, 0.2539953, 0.2582782, 0.2582789,0.2583550 )
                          ,Smote = c(0.2310864, 0.2310313, 0.2394668, 0.2456241,0.2455170 )
                          ,Case = 1:5
                          ,Algorithm = 'Logistic'))

df <- rbind(df,data.frame(Algo = c(0.1333116, 0.1353205, 0.1451361, 0.1451321,0.1420495)
                          ,Smote = c(0.1065553,0.1014633, 0.1044463, 0.109784,0.1047654)
                          ,Case = 1:5
                          ,Algorithm = 'Decision Tree'))

df <- rbind(df,data.frame(Algo = c(0.2759901, 0.2785579, 0.2798754, 0.2799664,0.2799788)
                 ,Smote = c(0.2702259, 0.2696111, 0.2686191, 0.2720016,0.2707761)
                 ,Case = 1:5
                 ,Algorithm = 'XGBoost'))





P <- ggplot(data = df) +
  geom_line(aes(Case,Algo, col = Algorithm), size = 1.5) +
  geom_point(aes(Case,Algo), size = 3, alpha = .5) +
  geom_line(aes(Case,Smote, col = Algorithm), size = 1.5, linetype = "dotdash") +
  geom_point(aes(Case,Smote), size = 3, alpha = .5) +
  theme(text = element_text(size=15)) +
  ylab('Gini Score') + xlab('Trials') +
  scale_y_continuous(breaks=seq(.1,.3,.005)) +
  scale_x_continuous(breaks=c(1:5), labels=c(paste('Case',1:5))) +
  coord_fixed(ratio = 50,expand = T) +
  ggtitle('Gini Score -- Algorithm Comparision') +
  
  geom_point(aes(y = df[which.max(df$Smote),"Smote"], x = df[which.max(df$Smote),"Case"]), size = 5, col = 'cyan') + 
  geom_hline(yintercept = df[which.max(df$Smote),"Smote"], linetype = "dashed", size = .5, col = 'cyan') + 
  geom_vline(xintercept = df[which.max(df$Smote),"Case"], linetype = "dashed", size = .5, col = 'cyan') +
  
  geom_point(aes(y = df[which.max(df$Algo),"Algo"], x = df[which.max(df$Algo),"Case"]), size = 5, col = 'red') + 
  geom_hline(yintercept = df[which.max(df$Algo),"Algo"], linetype = "dashed", size = .5, col = 'red') + 
  geom_vline(xintercept = df[which.max(df$Algo),"Case"], linetype = "dashed", size = .5, col = 'red')

ggsave(plot = P,filename = 'Allin1.png',width = 9,height = 16)
 