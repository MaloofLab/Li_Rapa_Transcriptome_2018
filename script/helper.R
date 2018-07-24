# 1) plot pheno data 
pheno.plot <- function(data, trait){
  pl <- ggplot(data = data, aes(x=trait, fill=trt))
  pl <- pl + geom_density()
  pl <- pl + facet_grid(gt ~ trt)
  pl
  return(pl)
} 

# 2) lmer result summary 
pheno.plot.result.GC <- function(data, data2){
  tmp <- as.data.frame(data[,c("Estimate", "Std. Error", "Pr(>|t|)")])
  colnames(tmp) <- c("mean", "SE", "pvalue")
  
  tmp$gt <- rep(c("R500", "IMB211"), 2)
  tmp$trt <- c("SUN","SUN","SHADE","SHADE")
  
  tmp$mean[4] = sum(tmp$mean)
  tmp$mean[2] = tmp$mean[1]+tmp$mean[2]
  tmp$mean[3] = tmp$mean[1]+tmp$mean[3]
  
  tmp$ymin <- tmp$mean - tmp$SE
  tmp$ymax <- tmp$mean + tmp$SE
  
  tmp$trt <- factor(tmp$trt, c("SUN", "SHADE"))
  tmp$gt <- factor(tmp$gt, c("R500", "IMB211"))
  
  # add significance level
  tmp$significance[1] <- ""
  if (tmp$pvalue[3] < 0.05){tmp$significance[2] <- "*"}
  if (tmp$pvalue[2] < 0.05){tmp$significance[3] <- "*"}
  # if (data2[,"Pr(>|t|)"][3] < 0.05 && tmp$pvalue[4] < 0.05){tmp$significance[4] <- "*&"}
  # if (data2[,"Pr(>|t|)"][3] < 0.05 && tmp$pvalue[4] > 0.05){tmp$significance[4] <- "*"}
  if (tmp$pvalue[4] < 0.05){tmp$significance[4] <- "*"}
  
  pl <- ggplot(data=tmp)
  pl <- pl + geom_bar(mapping=aes(fill=trt,x=trt,y=mean),stat="identity") + theme(axis.line.x=element_blank(), axis.ticks.x=element_blank())
  pl <- pl + scale_fill_manual(values = c("SUN" = "red","SHADE" = "darkred")) 
  pl <- pl + facet_wrap(facets=~gt)
  pl <- pl + geom_errorbar(mapping=aes(x=trt,ymin=ymin,ymax=ymax), position=position_dodge(width=0.9),width=0.5)
  pl <- pl + geom_text(data=tmp,aes(x=trt,y=ymax*1.05), label=factor(tmp$significance), size=5)
  pl <- pl + labs(list(x=" ", y="mean"))
  return(pl) 
} 

pheno.plot.result.GH <- function(data, data2){
  tmp <- as.data.frame(data[,c("Estimate", "Std. Error", "Pr(>|t|)")])
  colnames(tmp) <- c("mean", "SE", "pvalue")
  
  tmp$gt <- rep(c("R500", "IMB211"), 2)
  tmp$trt <- c("uncrowded","uncrowded","crowded","crowded")
  
  tmp$mean[4] = sum(tmp$mean)
  tmp$mean[2] = tmp$mean[1]+tmp$mean[2]
  tmp$mean[3] = tmp$mean[1]+tmp$mean[3]
  
  tmp$ymin <- tmp$mean - tmp$SE
  tmp$ymax <- tmp$mean + tmp$SE
  
  tmp$trt <- factor(tmp$trt, c("uncrowded", "crowded"))
  tmp$gt <- factor(tmp$gt, c("R500", "IMB211"))
  
  # add significance level
  tmp$significance[1] <- ""
  if (tmp$pvalue[3] < 0.05){tmp$significance[2] <- "*"}
  if (tmp$pvalue[2] < 0.05){tmp$significance[3] <- "*"}
  # if (data2[,"Pr(>|t|)"][3] < 0.05 && tmp$pvalue[4] < 0.05){tmp$significance[4] <- "*&"}
  # if (data2[,"Pr(>|t|)"][3] < 0.05 && tmp$pvalue[4] > 0.05){tmp$significance[4] <- "*"}
  if (tmp$pvalue[4] < 0.05){tmp$significance[4] <- "*"}
  
  pl <- ggplot(data=tmp)
  pl <- pl + geom_bar(mapping=aes(fill=trt,x=trt,y=mean),stat="identity") + theme(axis.line.x=element_blank(), axis.ticks.x=element_blank())
  pl <- pl + scale_fill_manual(values = c("uncrowded" = "red","crowded" = "darkred"))  
  pl <- pl + facet_wrap(facets=~gt)
  pl <- pl + geom_errorbar(mapping=aes(x=trt,ymin=ymin,ymax=ymax), position=position_dodge(width=0.9),width=0.5)
  pl <- pl + geom_text(data=tmp,aes(x=trt,y=ymax*1.05), label=factor(tmp$significance), size=5) 
  pl <- pl + labs(list(x=" ", y="mean"))
  return(pl)  
} 

# get legend 
get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)} 
