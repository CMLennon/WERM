library(ggplot2)
library(cowplot)
library(mise)

################################################################################################
# For NeurIPS2020 
################################################################################################
# mise()
# ylimits = c(0,0.2)
# df.result = read.csv('exact_mediator-200530-2300-5-50-10-10.csv'); Dtitle = "Mediator D5"
# df.result = read.csv('exact_mediator-200530-2300-10-50-10-10.csv'); Dtitle = "Mediator D10"
# df.result = read.csv('exact_mediator-200530-2300-15-50-10-10.csv'); Dtitle = 'Mediator D15'
# df.result = read.csv('exact_mediator-200530-2300-20-20-10-10.csv'); Dtitle = 'Mediator D20'

# df.result = read.csv('exact_double-200530-2300-5-50-10-10.csv'); Dtitle = 'Double D5'
# df.result = read.csv('exact_double-200530-2300-10-50-10-10.csv'); Dtitle= 'Double D10'
# df.result = read.csv('exact_double-200530-2300-15-50-10-10.csv'); Dtitle = 'Double D15'
# df.result = read.csv('exact_double-200530-2300-20-50-10-10.csv'); Dtitle = 'Double D20'


# df.result = read.csv('exact_napkin-200531-0200-5-50-10-10.csv'); Dtitle = 'Napkin D5'
# # df.result = read.csv('exact_napkin-200531-0200-10-50-10-10.csv'); Dtitle = 'Napkin D10'
# # df.result = read.csv('exact_napkin-200531-0200-15-50-10-10.csv'); Dtitle = 'Napkin D15'

# mise() 
# df.result = read.csv('exact2_napkin-200531-1100-5-50-10-10.csv'); Dtitle = 'Napkin-Compare with Exact'
# df.result = read.csv('exact2_napkin-200531-1100-10-50-10-10.csv'); Dtitle = 'Napkin-Compare with Exact'
# df.result = read.csv('exact2_napkin-200531-1100-15-20-10-10.csv'); Dtitle = 'Napkin-Compare with Exact'
# df.result = read.csv('exact2_napkin-200531-1100-20-20-10-10.csv'); Dtitle = 'Napkin-Compare with Exact'

# df.result = read.csv('exact2_mediator-200531-1100-5-50-10-10.csv'); Dtitle = 'Mediator-Compare with Exact'
# df.result = read.csv('exact2_mediator-200531-1100-10-20-10-10.csv'); Dtitle = 'Mediator-Compare with Exact'
# df.result = read.csv('exact2_mediator-200531-1100-15-20-10-10.csv'); Dtitle = 'Mediator-Compare with Exact'

# df.result = read.csv('exact2_double-200531-1100-5-50-10-10.csv'); Dtitle = 'Double-Compare with Exact'
# df.result = read.csv('exact2_double-200531-1100-10-50-10-10.csv'); Dtitle = 'Double-Compare with Exact'
# df.result = read.csv('exact2_double-200531-1100-15-50-10-10.csv'); Dtitle = 'Double-Compare with Exact'

# ylimits = c(0.01,0.05)
# df.result = read.csv('exact_napkin-200601-0200-5.csv'); Dtitle = "New Napkin Exact D5"
# df.result = read.csv('exact_napkin-200601-0200-10.csv'); Dtitle = "New Napkin Exact D10"
# df.result = read.csv('exact_napkin-200601-0200-15.csv'); Dtitle = "New Napkin Exact D20"
  
# ylimits = c(0,0.1)
# df.result = read.csv('exact_napkin-10-100-200601-0200-5-50.csv'); Dtitle = "New Napkin Exact 10 100 D5"
# df.result = read.csv('exact_napkin-10-100-200601-0200-D10-50.csv'); Dtitle = "New Napkin Exact 10 100 D10"
# df.result = read.csv('exact_napkin-10-100-200601-0200-D15-50.csv'); Dtitle = "New Napkin Exact 10 100 D10"

# ylimits = c(0.01,0.05)
# df.result = read.csv('exact_mediator-200601-0140-D5-50.csv'); Dtitle = "New Mediator Exact D5"
# df.result = read.csv('exact_mediator-200601-0140-D10-50.csv'); Dtitle = "New Mediator Exact D10"
# df.result = read.csv('exact_mediator-200601-0140-D15-50.csv'); Dtitle = "New Mediator Exact D15"

# mise()
# ylimits = c(0,0.15)
# df.result = read.csv('exact_double-200601-0330-D5-50.csv'); Dtitle = "New Double Exact D5"
# df.result = read.csv('exact_double-200601-0330-D10-50.csv'); Dtitle = "New Double Exact D10"
# df.result = read.csv('exact_double-200601-0330-D15-50.csv'); Dtitle = "New Double Exact D15"

# mise()
# ylimits = c(0,0.05)
# df.result = read.csv('exact_napkin-200601-0930-D5-50.csv'); Dtitle = "New Napkin Exact 5 D5"
# df.result = read.csv('exact_napkin-200601-0930-D10-50.csv'); Dtitle = "New Napkin Exact 5 D10"
# df.result = read.csv('exact_napkin-200601-0930-D15-50.csv'); Dtitle = "New Napkin Exact 5 D15"

# mise()
# ylimits = c(0,0.05)
# df.result = read.csv('exact_mediator-200601-1030-D5-50.csv'); Dtitle = "New Mediator Exact D5"
# df.result = read.csv('exact_mediator-200601-1030-D10-50.csv'); Dtitle = "New Mediator Exact D10"
# df.result = read.csv('exact_mediator-200601-1030-D15-50.csv'); Dtitle = "New Mediator Exact D15"

# ylimits = c(0,0.05)
# df.result = read.csv('exact_mediator-200601-1200-D5-50.csv'); Dtitle = "New Mediator Exact D5"
# df.result = read.csv('exact_mediator-200601-1200-D10-50.csv'); Dtitle = "New Mediator Exact D5"

# ylimits = c(0,0.2)
# df.result = read.csv('exact_napkin-naive-200601-1630-D5-50.csv'); Dtitle = "New Napkin Naive Exact D5"
# df.result = read.csv('exact_napkin-naive-200601-1630-D10-50.csv'); Dtitle = "New Napkin Naive Exact D10"
# df.result = read.csv('exact_napkin-naive-200601-1630-D15-50.csv'); Dtitle = "New Napkin Naive Exact D15"

# ylimits = c(0,0.5)
# df.result = read.csv('exact_mediator-naive-200601-1630-D5-50.csv'); Dtitle = "New Mediator Naive Exact D5"
# df.result = read.csv('exact_mediator-naive-200601-1630-D10-50.csv'); Dtitle = "New Mediator Naive Exact D10"
# df.result = read.csv('exact_mediator-naive-200601-1630-D15-50.csv'); Dtitle = "New Mediator Naive Exact D15"

# ylimits = c(0,0.5)
# df.result = read.csv('exact_double-naive-200601-1630-D5-50.csv'); Dtitle = "New Double Naive Exact D5"
# df.result = read.csv('exact_double-naive-200601-1630-D10-50.csv'); Dtitle = "New Double Naive Exact D10"
# df.result = read.csv('exact_double-naive-200601-1630-D15-50.csv'); Dtitle = "New Double Naive Exact D15"

# ylimits = c(0,0.18)
# df.result = read.csv('exact_napkin-10-100-200601-0200-D15-50.csv'); Dtitle = "Small Napkin D15"
# df.result = read.csv('exact_mediator-200602-0400-D15-50-1-100.csv'); Dtitle = "Small Mediator D15"
# df.result = read.csv('exact_double-200602-0400-D15-50-1-100.csv'); Dtitle = "Small Double D15"


# ylimits = c(0,0.05)
# df.result = read.csv('exact_mediator-200604-0100-D15-50-summary.csv'); Dtitle = "Mediator D15"
# df.result = read.csv('exact_mediator-200604-0400-D15-50-summary.csv'); Dtitle = "Mediator D15"


############ FINAL PLOT ############
# mise()
# D = 20
# ylimits = c(0,0.05)
# df.result = read.csv('exact_napkin-200601-0930-D20-50.csv'); Dtitle = "New Napkin Exact D20"
# df.result = read.csv('exact_mediator-200601-1030-D20-50.csv'); Dtitle = "New Mediator Exact D20"
# df.result15 = read.csv('exact_double-200601-0330-D20-50.csv'); Dtitle = "New Double Exact D20"

# mise()
# D = 15
# ylimits = c(0,0.15)
# df.result = read.csv('exact_napkin-200601-0930-D15-50.csv'); Dtitle = "New Napkin Exact 5 D15"
# df.result = read.csv('exact_mediator-200601-1030-D15-50.csv'); Dtitle = "New Mediator Exact D15"
# df.result = read.csv('exact_double-200601-0330-D15-50.csv'); Dtitle = "New Double Exact D15"

# D = 10 
# ylimits = c(0,0.15)
# df.result = read.csv('exact_napkin-200601-0930-D10-50.csv'); Dtitle = "New Napkin Exact 5 D10"
# df.result = read.csv('exact_mediator-200601-1030-D10-50.csv'); Dtitle = "New Mediator Exact D10"
# df.result = read.csv('exact_double-200601-0330-D10-50.csv'); Dtitle = "New Double Exact D10"

# D = 5
# ylimits = c(0,0.15)
# df.result = read.csv('exact_napkin-200601-0930-D5-50.csv'); Dtitle = "New Napkin Exact 5 D5"
# df.result = read.csv('exact_mediator-200601-1030-D5-50.csv'); Dtitle = "New Mediator Exact D5"
# df.result = read.csv('exact_double-200601-0330-D5-50.csv'); Dtitle = "New Double Exact D5"

# mise()
# ylimits = c(0,0.05)
# df.result = read.csv('napkin-200608-2300-D15-50-summary.csv'); Dtitle = 'Napkin D15'
# df.result = read.csv('mediator-200608-2300-D15-50-summary.csv'); Dtitle = 'Mediator D15'
# df.result = read.csv('doubleeffect-200608-2300-D15-50-summary.csv'); Dtitle = 'Double D15'

mise()
ylimits = c(0,0.05)
# df.result = read.csv('napkin-200609-1500-D15-50-summary.csv'); Dtitle = 'Napkin D15'
# df.result = read.csv('mediator-200609-2100-D15-50-summary.csv'); Dtitle = 'Mediator D15'
# df.result = read.csv('doubleeffect-200609-2100-D15-50-summary.csv'); Dtitle = 'Double D15'
# df.result = read.csv('doubleeffect-200610-1130-D15-50-summary.csv'); Dtitle = 'Double D15'
# df.result = read.csv('doubleeffect-200613-1130-D15-50-summary.csv'); Dtitle = 'Double D15'

# df.result = read.csv('doubleeffect-200614-0200-D20-50-summary.csv'); Dtitle = 'Double D20'
# df.result = read.csv('napkin-200616-0200-D20-50-summary.csv'); Dtitle = 'Napkin D20'
# df.result = read.csv('mediator-200617-0100-D20-50-summary.csv'); Dtitle = 'Mediator D20'
# df.result = read.csv('Result/napkin_temp-summary.csv'); Dtitle = 'Napkin'
df.result = read.csv('Result/mediator_temp-summary.csv'); Dtitle = 'Mediator'


# General 
regmethod = 'loess'
# ylimits = c(0.0,0.02)
# xlimits = c(0,5000)
xlimits = c(0,max(df.result$Nlist))
spanval = 1.0
point_size = 3
alpha_point = 1

twoD = T
medianTF = T

# df.result = df.result[1:160,]
# df.result = read.csv('genFD-0307-0323-1-40.csv')
# df.result = df.result[1:40,]

gg = ggplot(data = df.result, aes(x=Nlist))

if(medianTF == T){
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.global.50,colour="Y.global.50"),size=1,method=regmethod,se=F, span=spanval)  
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.plugin.50,colour="Y.plugin.50"),size=1.5,method=regmethod,se=F, span=spanval,linetype='dashed')
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.heuristic.50,colour="Y.heuristic.50"),size=2,method=regmethod,se=F, span=spanval,linetype='dotdash')
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.id.50,colour="Y.id.50"),size=3,method=regmethod,se=F, span=spanval,linetype='dotted')
  # gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.multi.25,ymax=Y.multi.75),alpha=0.2, fill="red")
  # gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.naive.25,ymax=Y.naive.75),alpha=0.1, fill="blue")
  # gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.exact.25,ymax=Y.exact.75),alpha=0.2, fill="seagreen")
  # gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.plugin.50,colour="Y.plugin.50"),size=1.5,method='gam',se=F,formula=y~s(x,k=10),)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.plugin.50,colour='Y.plugin.50'),size=point_size,alpha=alpha_point,shape=4)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.global.50,colour='Y.global.50'),size=point_size,alpha=alpha_point,shape=16)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.heuristic.50,colour='Y.heuristic.50'),size=point_size,alpha=alpha_point,shape=8)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.id.50,colour='Y.id.50'),size=point_size,alpha=alpha_point,shape=9)
  if(twoD == F){
    gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.scale.50,colour="Y.scale.50"),size=1.5,method=regmethod,se=F,span=spanval)
    gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.scale.50"),values = c("gold", "red","seagreen"),labels=c("CWO","Naive","Weight-HD"))
    # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.scale.50,colour='Y.scale.50'),size=1.5,alpha=0.2)
  }else{
    gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.heuristic.50","Y.id.50"),values = c("blue", "firebrick2","orange","purple"),labels=c("WERM-Global","Plug-In","WERM-ID-R","WERM-ID"))
    # gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.heuristic.50","Y.id.50"),values = c("blue", "firebrick2","orange","darkgreen"),labels=c("WERM-ID-R-Global","Plug-in","WERM-ID-R-Heuristic","WERM-ID"))   
  }
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.global.50,colour='Y.global.50'),size=1.5,alpha=0.2)
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.plugin.50,colour='Y.plugin.50'),size=1.5,alpha=0.2)
}else{
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.multi.mean,colour="Y.multi.mean"),size=1.5,method=regmethod,se=F, span=spanval)
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.naive.mean,colour="Y.naive.mean"),size=1.5,method=regmethod,se=F, span=spanval)
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.multi.25,ymax=Y.multi.75),alpha=0.25, fill="blue")
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.naive.25,ymax=Y.naive.75),alpha=0.25, fill="red")
  # gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.plugin.50,colour="Y.naive.mean"),size=1.5,method='lm',se=F,formula=y~splines::bs(x,5),span=spanval)
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.multi.mean,colour='Y.multi.mean'),size=1.5,alpha=0.2)
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.naive.mean,colour='Y.naive.mean'),size=1.5,alpha=0.2)
  if(twoD == F){
    gg = gg + scale_color_manual("",breaks=c("Y.multi.mean","Y.naive.mean","Y.scale.mean"),values = c("blue", "red","green"),labels=c("CWO","Naive","Weight-HD"))   
    gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.scale.mean,colour="Y.scale.mean"),size=1.5,method=regmethod,se=F,span=2)
    # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.scale.mean,colour='Y.scale.mean'),size=1.5,alpha=0.2)
  }else{
    gg = gg + scale_color_manual("",breaks=c("Y.multi.mean","Y.naive.mean"),values = c("blue", "red"),labels=c("CWO","Naive"))   
  }
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.multi.mean,colour='Y.multi.mean'),size=1.5,alpha=0.2)
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.naive.mean,colour='Y.naive.mean'),size=1.5,alpha=0.2)
}

# gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.multi.25,ymax=Y.multi.75),alpha=0.25, fill="blue")
# gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.naive.25,ymax=Y.naive.75),alpha=0.25, fill="red")
# gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.truth.x0.25,ymax=Y.truth.x0.75),alpha=0.25, fill="green")

# gg = gg + ggtitle(Dtitle) 
gg = gg + coord_cartesian(ylim=ylimits)
gg = gg + theme_bw()
gg = gg + scale_x_continuous(name = "m", limits=xlimits) + scale_y_continuous(name = "MAAE")
gg = gg + theme(axis.line.x = element_line(size = 0.5, colour = "black"),
                axis.line.y = element_line(size = 0.5, colour = "black"),
                axis.line = element_line(size=1, colour = "black"),
                panel.border = element_blank(),
                panel.background = element_blank(),
                legend.text = element_text(size=15),
                plot.title=element_text(size = 40),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.title.y = element_text(size=25),
                axis.title.x = element_text(size=30)
)
gg