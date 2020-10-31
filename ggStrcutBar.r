library(ggplot2)
library(reshape2)

args <- commandArgs(TRUE)

args.len <- length(args)

rank.file <- args[1]
out.plot <- args[2]
qfiles <- args[3:args.len]

row.rank <- read.delim(rank.file,header=F)

qfiles.count <- length(qfiles)
for(i in 1:qfiles.count){
  qfile <- qfiles[i]
  data <- read.delim(qfile,header=F,sep=" ")
  data$K <- paste("K=",length(colnames(data)),sep="")
  data$ID <- rownames(data)
  data.melt <- melt(data,id.vars=c("ID","K"))
  if(i == 1){
    data.bind <- data.melt
  }else{
    data.bind <- rbind(data.bind,data.melt)
  }
}

data.bind$ID <- factor(data.bind$ID,levels=row.rank$V1)

plot.w <- 10
plot.h <- 0.6 * qfiles.count
pdf(out.plot,width=plot.w,height=plot.h)

ggplot(data.bind) +
  geom_bar(aes(x=ID,y=value,fill=variable),position = "fill",stat = "identity") +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  theme(
    axis.text.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  facet_wrap(~ K, ncol=1, scales = "free",strip.position = "right")

dev.off()
