library(ggplot2)
library(cowplot)
library(mise)
library(mgcv)

mise()

ComputeSD = function(df.result){
  df.result = t(df.result)
  df.result = df.result[c(2:nrow(df.result)),]
  SDarray = rep(0,ncol(df.result))
  for (colidx in 1:ncol(df.result)){
    SDarray[colidx] = sd(df.result[,colidx],na.rm=T)
  }
  return(SDarray)
}

computeColMeans = function(df.result){
  tmp = t(df.result)
  return(colMeans(tmp[c(2:nrow(tmp)),],na.rm=T))
}

ConstructDFPlot = function(instancename,mean_median){
  df.result.summary = read.csv(paste(instancename,'-summary.csv',sep=""))
  df.result.global = read.csv(paste(instancename,'-heuristic.csv',sep=""))
  df.result.id = read.csv(paste(instancename,'-ipw.csv',sep=""))
  df.result.plugin = read.csv(paste(instancename,'-ipw.csv',sep=""))
  # df.result.IPW = read.csv(paste(instancename,'-ipw.csv',sep=""))
  
  confidence_coef = 1/5
  
  if (mean_median == "median"){
    globalCenter = df.result.summary$Y.heuristic.50
    idCenter = df.result.summary$Y.ipw.50
    pluginCenter = df.result.summary$Y.ipw.50
    # ipwCenter = df.result.summary$Y.ipw.50
  }else{
    globalCenter = df.result.summary$Y.heuristic.mean
    idCenter = df.result.summary$Y.ipw.mean
    # pluginCenter = computeColMeans(df.result.ipw)
    pluginCenter = df.result.summary$Y.ipw.mean
    # ipwCenter = df.result.summary$Y.ipw.mean
  }
  globalSD = ComputeSD(df.result.global)
  globalLow = globalCenter - confidence_coef*globalSD
  globalHigh = globalCenter + confidence_coef*globalSD
  
  idSD = ComputeSD(df.result.id)
  idLow = idCenter - confidence_coef*idSD
  idHigh = idCenter + confidence_coef*idSD
  
  pluginSD = ComputeSD(df.result.plugin)
  pluginLow = pluginCenter - confidence_coef*pluginSD
  pluginHigh = pluginCenter + confidence_coef*pluginSD
  
  # ipwSD = ComputeSD(df.result.IPW)
  # ipwLow = ipwCenter - confidence_coef*ipwSD
  # ipwHigh = ipwCenter + confidence_coef*ipwSD
  
  df.Plot = data.frame(Nlist = df.result.summary$Nlist, 
                       Y.global.50 = globalCenter,
                       Y.global.25 = globalLow,
                       Y.global.75 = globalHigh,
                       Y.plugin.50 = pluginCenter,
                       Y.plugin.25 = pluginLow,
                       Y.plugin.75 = pluginHigh,
                       Y.id.50 = idCenter,
                       Y.id.25 = idLow,
                       Y.id.75 = idHigh
                       # Y.ipw.50 = ipwCenter,
                       # Y.ipw.25 = ipwLow,
                       # Y.ipw.75 = ipwHigh
  )
  return(df.Plot)
}


# instancename = 'Result/napkin-0802-1030-D20'
# instancename = 'Result/mediator-0804-0000-D20'
# instancename = 'Result/doubleeffect-0804-0000-D20'
# instancename = 'Result/doubleeffect-0811-0200-D15'
# instancename = 'Result/doubleeffect-0811-1300-D15'
# instancename = 'Result/doubleeffect-0813-0400-D2'
instancename = 'Result/doubleeffect-0813-0400-D5'

df.result = ConstructDFPlot(instancename,'median')
# df.result = ConstructDFPlot('Result/mediator-0804-0000-D20','mean')
# df.result = ConstructDFPlot('Result/doubleeffect-0804-0000-D20','mean')

# General 
regmethod = 'auto'
ylimits = c(0.0,0.1)
# xlimits = c(0,5000)
# xlimits = c(15000,max(df.result$Nlist))
xlimits = c(15000,200000)
spanval = 1
point_size = 3
alpha_point = 1

twoD = T
medianTF = T

gg = ggplot(data = df.result, aes(x=Nlist))


if(medianTF == T){
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.global.50,colour="Y.global.50"),size=1.5,method=regmethod,se=F, span=spanval)  
  # gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.plugin.50,colour="Y.plugin.50"),size=1.5,method=regmethod,se=F, span=spanval,linetype='dashed')
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.plugin.50,colour="Y.plugin.50"),size=1.5,method=regmethod,se=F, span=spanval)
  # gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.ipw.50,colour="Y.ipw.50"),size=2,method=regmethod,se=F, span=spanval,linetype='dotdash')
  # gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.id.50,colour="Y.id.50"),size=1.5,method=regmethod,se=F, span=spanval,linetype='longdash')
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.global.25,ymax=Y.global.75),alpha=0.2, fill="dodgerblue")
  # gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.ipw.25,ymax=Y.ipw.75),alpha=0.1, fill="orange")
  # gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.id.25,ymax=Y.id.75),alpha=0.2, fill="seagreen")
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.plugin.25,ymax=Y.plugin.75),alpha=0.2, fill="firebrick1")
  # gg = gg + geom_smooth(data=df.result, aes(x=Nlist,y=Y.plugin.50,colour="Y.plugin.50"),size=1.5,method='gam',se=F,formula=y~s(x,k=10),)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.plugin.50,colour='Y.plugin.50'),size=point_size,alpha=alpha_point,shape=4)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.global.50,colour='Y.global.50'),size=point_size,alpha=alpha_point,shape=16)
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.ipw.50,colour='Y.ipw.50'),size=point_size,alpha=alpha_point,shape=6)
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.id.50,colour='Y.id.50'),size=point_size,alpha=alpha_point,shape=8)
  if(twoD == F){
    gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.scale.50,colour="Y.scale.50"),size=1.5,method=regmethod,se=F,span=spanval)
    gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.scale.50"),values = c("gold", "red","seagreen"),labels=c("CWO","Naive","Weight-HD"))
    # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.scale.50,colour='Y.scale.50'),size=1.5,alpha=0.2)
  }else{
    # gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.id.50","Y.ipw.50"),values = c("blue", "firebrick2","seagreen","orange"),labels=c("WERM-ID-R","Plug-In","PO-Regression","PO-IPW"))
    # gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.id.50"),values = c("blue", "firebrick2","seagreen"),labels=c("WERM-ID-R","Plug-In","PO-framework"))
    gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50"),values = c("blue", "firebrick2"),labels=c("WERM-ID-R","PO-framework"))
  }
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.global.50,colour='Y.global.50'),size=1.5,alpha=0.2)
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.plugin.50,colour='Y.plugin.50'),size=1.5,alpha=0.2)
}else{
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.multi.mean,colour="Y.multi.mean"),size=1.5,method=regmethod,se=F, span=spanval)
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.naive.mean,colour="Y.naive.mean"),size=1.5,method=regmethod,se=F, span=spanval)
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.multi.25,ymax=Y.multi.75),alpha=, fill="blue")
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.naive.25,ymax=Y.naive.75),alpha=0.05, fill="red")
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