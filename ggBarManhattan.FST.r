library(ggplot2)
library(reshape2)

args <- commandArgs(TRUE)

file <- args[1]
out.plot <- args[2]
col.rank <- as.numeric(args[3])
sig.value <- as.numeric(args[4])

#limit <- 3
# top.pct <- 5

data <- read.delim(file,header=T)

#if(limit > max(data[,col.rank])){
#  limit <- max(data[,col.rank])
#}
#sig.value <- as.numeric(quantile(data[,col.rank],prob=1-top.pct/100))
#limit <- max(data[,col.rank])

# For FST
data[data[,col.rank] < 0,col.rank] <- 0
# for FST end

data$sig <- "NA"
data[data[,col.rank] >= sig.value,]$sig <- "top"
data$sig <- as.factor(data$sig)
data$mask <- data[,col.rank]
data[data[,col.rank] >= sig.value,]$mask <- sig.value
data[data[,col.rank] < sig.value,col.rank] <- NA

data$chr <- as.factor(gsub("Chr","",data[,1]))
chrs <- as.character(sort(as.numeric(levels(data$chr))))
data$chr <- factor(data$chr, levels=chrs)

# 设置染色体长度、数量、中位断点（breakpoint）作为x轴的tick
chrcount <- length(chrs)
breakpoints <- levels(data$chr)

# 设置新坐标（累加坐标）
data$POS <- data[,2]
genome.len <- 0

# 循环计算，获取每一条染色体的长度、中间点，同时计算累加坐标
for(i in 1:chrcount){
  chrname <- chrs[i]
  chrlen <- max(data[data$chr==chrname,2])
  breakpoints[i] <- chrlen/2 + genome.len
  data[data$chr==chrname,]$POS <- data[data$chr==chrname,2] + genome.len
  genome.len <- genome.len + chrlen
}

# 设置染色体颜色，需要两套颜色，分别是染色体区分色以及高亮色
colorchr <- rep(c("orange","grey"),round(chrcount/2)+1)[1:chrcount]
colorhl <- c("grey","red")
colorset <- c(colorchr,colorhl)
colorset.rank <- colorset

color.ranks <- match(sort(c(chrs,"top","NA")),c(chrs,"NA","top"))

for(i in 1:length(color.ranks)){
  rank <- color.ranks[i]
  colorset.rank[i] <- colorset[rank]
}

pdf(out.plot,width=8,height=2)

ggplot(data) + 
  geom_bar(aes(x=POS, y=data[,col.rank], color=sig),stat="identity", position="identity",size=0.5) + 
  geom_bar(aes(x=POS, y=mask,color=data$chr),stat="identity",size=0.5) +
  scale_color_manual(values = colorset.rank) +
  #coord_cartesian(ylim=c(0,limit)) + 
  xlab("CHROMOSOME") +
  ylab("value") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept=sig.value, linetype="dashed", color = "red") +
# 设置阈值线
  scale_x_continuous(breaks=as.numeric(breakpoints),labels=chrs) 
# 关键语句，设置断点

dev.off()
