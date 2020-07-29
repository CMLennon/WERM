library(ggplot2)
library(cowplot)

# df.result = read.csv('200531-1745-napkin-time.csv')
# df.result = read.csv('200531-1745-mediator-time.csv')
df.result = read.csv('200531-1745-double-time.csv')
# df.result$adjtime.50 = df.result$adjtime.50 + rnorm(length(df.result$adjtime.50),mean=0,sd = 1e-1)
# df.result$naivetime.50 = df.result$naivetime.50 + abs(rnorm(length(df.result$naivetime.50),mean=0,sd = 1e-1))

regmethod = 'loess'
ylimits = c(0,50)
Dlist = df.result$Dlist
gg.time = ggplot(data = df.result, aes(x=Dlist))
gg.time = gg.time + geom_smooth(data = df.result, aes(x=Dlist,y=adjtime.50,colour="adjtime.50"),size=0.5,method=regmethod,alpha=0.5,linetype='dashed',se=F,spanval=0.5)
gg.time = gg.time + geom_point(data=df.result,aes(x=Dlist,y=adjtime.50,colour='adjtime.50'),size=4,alpha=1)
# gg.time = gg.time + geom_ribbon(data=df.result, aes(x=Dlist, ymin=adjtime.25,ymax=adjtime.75),alpha=0.25, fill="blue")
gg.time = gg.time + geom_smooth(data = df.result, aes(x=Dlist,y=naivetime.50,colour="naivetime.50"),size=0.5,method=regmethod,alpha=0.5,linetype='dashed',se=F,,spanval=0.5)
gg.time = gg.time + geom_point(data=df.result,aes(x=Dlist,y=naivetime.50,colour='naivetime.50'),size=4,alpha=1)
# gg.time = gg.time + geom_ribbon(data=df.result, aes(x=Dlist, ymin=naivetime.25,ymax=naivetime.75),alpha=0.25, fill="red")
# gg.time = gg.time + geom_smooth(data = df.result, aes(x=Dlist,y=Y.truth.time.50,colour="Y.truth.time.50"),size=1.5)
# gg.time = gg.time + geom_ribbon(data=df.result, aes(x=Dlist, ymin=Y.truth.time.25,ymax=Y.truth.time.75),alpha=0.25, fill="green")
gg.time = gg.time + scale_color_manual("",breaks=c("adjtime.50","naivetime.50"),values = c("blue", "red"),labels=c("Weight","Naive")) 
gg.time = gg.time + theme_bw()
gg.time = gg.time + coord_cartesian(ylim=ylimits)
gg.time = gg.time + scale_x_continuous(name = "Dimension",breaks = Dlist) + scale_y_continuous(name = "CPU time (sec)")
gg.time = gg.time + scale_fill_continuous(guide = guide_legend())
gg.time = gg.time + theme(axis.line.x = element_line(size = 0.5, colour = "black"),
                          axis.line.y = element_line(size = 0.5, colour = "black"),
                          axis.line = element_line(size=1, colour = "black"),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          legend.text = element_text(size=15),
                          plot.title=element_text(size = 40),
                          axis.text.x = element_text(size = 20),
                          axis.text.y = element_text(size = 20),
                          axis.title.y = element_text(size=25),
                          axis.title.x = element_text(size=20)
)
gg.time

