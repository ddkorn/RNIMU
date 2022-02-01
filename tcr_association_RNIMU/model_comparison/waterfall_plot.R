setwd("/home/daria/github/tcr-association/model_comparison/")


data=c("adaptive_set1_fmba_set2", "adaptive_set2_fmba_set2", "fmba_set1_fmba_set2",
  "adaptive_set1_fmba_set1", "adaptive_set2_fmba_set1", "fmba_set2_fmba_set1",
  "adaptive_set1_adaptive_set2","fmba_set1_adaptive_set2", "fmba_set2_adaptive_set2",
  "adaptive_set2_adaptive_set1", "fmba_set1_adaptive_set1", "fmba_set2_adaptive_set1")


install.packages('rmarkdown')
tinytex::install_tinytex()  # install TinyTeX

l=list()
i <- 1
while(i<=12) {
  options(repr.plot.width = 1, repr.plot.height = 0.75)
  dataset=data[i]
  file_name=sprintf("plots/hist/data_SVM_3/%s.tsv", dataset)
  
  a=read.csv(file_name, sep="\t",header=TRUE)
  a$label=factor(a$label, levels = c("COVID", "healthy"))
  bp=ggplot(a,aes(x=index,y=healthy_prob,fill=label))+geom_col()+theme_bw()+scale_fill_manual(values=c("#F85C70", "#41729F"))+
    xlab("Index") +
    ylab("Healthy probability") +
    ylim(-0.5, 0.5) +
    theme(legend.position = "none",text=element_text(size=12,  family="Comic Sans MS"), 
          axis.title = element_blank(),
          axis.text.x = element_blank()) 
  
  l[[i]] <- bp
  i <- i + 1
}


grid.arrange(grobs = l,
             layout_matrix = rbind(c(1,2,3,NA),
                                   c(4,5,NA,6),
                                   c(7,NA,8,9),
                                   c(NA,10,11,12))
)


data=read.csv('best_estimator/svm_4_predict_proba_gkb23_Tcell.tsv', sep="\t", header=TRUE)
data$label=factor(data$Tcell_response_level, levels = c("0-1 (no response)", "3-6", "10-20", ">100"))
pred=c("adaptive_set1.gkb23", 
       "adaptive_set2.gkb23",
       "fmba_set1.gkb23", 
       "fmba_set2.gkb23")
l=list()
i <- 1
while(i<=4) {
  dataset=pred[i]
  proba=sprintf("%s_prob_healthy", dataset) 
  data['prob_0.5']=data[proba]-0.5
  data=data[order(data['prob_0.5']),]
  data$index=seq(nrow(data))
  bp=ggplot(data,aes(x=index,y=prob_0.5,fill=label))+geom_col()+theme_bw()+scale_fill_manual(values=c("#5AB25B", "#6E8541", "#825726", "#962A0C"))+
    xlab("Index") +
    ylab("Healthy probability") +
    ylim(-0.5, 0.5) +
    ggtitle(dataset)+
    theme(legend.position = "right",text=element_text(size=12,  family="Comic Sans MS"), 
          axis.title = element_blank(),
          axis.text.x = element_blank()) 
  
  l[[i]] <- bp
  i <- i + 1
}


grid.arrange(grobs = l,
             layout_matrix = rbind(c(1,2),
                                   c(3,4))

)



