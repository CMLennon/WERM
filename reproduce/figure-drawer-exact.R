library(ggplot2)
library(cowplot)
library(mise)
library(mgcv)

mise()

returnPred = function(df.result,modelmode,mean_median,myNlist){
  if (modelmode == "global"){
    if (mean_median == "mean"){
      smoothModel = gam(Y.global.mean~s(Nlist),data=df.result)   
    }else{
      smoothModel = gam(Y.global.50~s(Nlist),data=df.result)   
    }
  }
  else if (modelmode == "plugin"){
    if (mean_median == "mean"){
      smoothModel = gam(Y.plugin.mean~s(Nlist),data=df.result)   
    }else{
      smoothModel = gam(Y.plugin.50~s(Nlist),data=df.result)   
    }
  }
  else if (modelmode == "id"){
    if (mean_median == "mean"){
      smoothModel = gam(Y.id.mean~s(Nlist),data=df.result)   
    }else{
      smoothModel = gam(Y.id.50~s(Nlist),data=df.result)   
    }
  }
  # myNlist = seq(0,1000,length.out = 100)
  pred.smoothModel = predict(smoothModel,newdata = data.frame(Nlist=myNlist),se=TRUE)
  mfit.smoothModel = pred.smoothModel$fit
  low.smoothModel = mfit.smoothModel - 1.96 * pred.smoothModel$se.fit
  high.smoothModel = mfit.smoothModel + 1.96 * pred.smoothModel$se.fit
  return(list(mfit.smoothModel,low.smoothModel,high.smoothModel))
}
ConstructDFPlot1 = function(df.result){
  myNlist = seq(0,1000,length.out = 100)
  mean_median = "mean"
  globalModel = returnPred(df.result,"global",mean_median,myNlist)
  idModel = returnPred(df.result,"id",mean_median,myNlist)
  pluginModel = returnPred(df.result,"plugin",mean_median,myNlist)
  
  df.Plot = data.frame(Nlist = myNlist, 
                       Y.global.50 = globalModel[[1]], 
                       Y.global.25 = globalModel[[2]], 
                       Y.global.75 = globalModel[[3]],
                       Y.plugin.50 = pluginModel[[1]], 
                       Y.plugin.25 = pluginModel[[2]], 
                       Y.plugin.75 = pluginModel[[3]],
                       Y.id.50 = idModel[[1]], 
                       Y.id.25 = idModel[[2]], 
                       Y.id.75 = idModel[[3]]
                       )
  return(df.Plot)
}

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


ConstructDFPlot2 = function(instancename, mean_median){
  df.result.summary = read.csv(paste(instancename,'-summary.csv',sep=""))
  df.result.global = read.csv(paste(instancename,'-global.csv',sep=""))
  df.result.id = read.csv(paste(instancename,'-id.csv',sep=""))
  df.result.plugin = read.csv(paste(instancename,'-param.csv',sep=""))
  
  confidence_coef = 1/5
  
  if (mean_median == "median"){
    globalCenter = df.result.summary$Y.global.50
    idCenter = df.result.summary$Y.id.50
    pluginCenter = df.result.summary$Y.plugin.50
  }else{
    globalCenter = df.result.summary$Y.global.mean
    idCenter = df.result.summary$Y.id.mean
    pluginCenter = computeColMeans(df.result.plugin)
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
  )
  return(df.Plot)
  
  
}

# df.result = read.csv('Result/napkin-0802-1030-D20-summary.csv')
# df.result = read.csv('Result/mediator-0804-0000-D20-summary.csv')
# df.result = read.csv('Result/doubleeffect-0804-0000-D20-summary.csv')

# df.result = ConstructDFPlot2('Result/napkin-0802-1030-D20','mean')
# df.result = ConstructDFPlot2('Result/mediator-0804-0000-D20','mean')
df.result = ConstructDFPlot2('Result/doubleeffect-0804-0000-D20','mean')


# General 
regmethod = 'auto'
ylimits = c(0.0,0.1)
# xlimits = c(0,5000)
xlimits = c(0,max(df.result$Nlist))
spanval = 1
point_size = 3
alpha_point = 1

twoD = T
medianTF = T



gg = ggplot(data = df.result, aes(x=Nlist))


if(medianTF == T){
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.global.50,colour="Y.global.50"),size=1,method=regmethod,se=F, span=spanval)  
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.plugin.50,colour="Y.plugin.50"),size=1.5,method=regmethod,se=F, span=spanval,linetype='dashed')
  # gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.heuristic.50,colour="Y.heuristic.50"),size=2,method=regmethod,se=F, span=spanval,linetype='dotdash')
  gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.id.50,colour="Y.id.50"),size=3,method=regmethod,se=F, span=spanval,linetype='dotted')
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.global.25,ymax=Y.global.75),alpha=0.2, fill="dodgerblue")
  # gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.naive.25,ymax=Y.naive.75),alpha=0.1, fill="blue")
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.id.25,ymax=Y.id.75),alpha=0.2, fill="orange")
  gg = gg + geom_ribbon(data=df.result, aes(x=Nlist, ymin=Y.plugin.25,ymax=Y.plugin.75),alpha=0.2, fill="firebrick1")
  # gg = gg + geom_smooth(data=df.result, aes(x=Nlist,y=Y.plugin.50,colour="Y.plugin.50"),size=1.5,method='gam',se=F,formula=y~s(x,k=10),)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.plugin.50,colour='Y.plugin.50'),size=point_size,alpha=alpha_point,shape=4)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.global.50,colour='Y.global.50'),size=point_size,alpha=alpha_point,shape=16)
  # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.heuristic.50,colour='Y.heuristic.50'),size=point_size,alpha=alpha_point,shape=8)
  gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.id.50,colour='Y.id.50'),size=point_size,alpha=alpha_point,shape=9)
  if(twoD == F){
    gg = gg + geom_smooth(data = df.result, aes(x=Nlist,y=Y.scale.50,colour="Y.scale.50"),size=1.5,method=regmethod,se=F,span=spanval)
    gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.scale.50"),values = c("gold", "red","seagreen"),labels=c("CWO","Naive","Weight-HD"))
    # gg = gg + geom_point(data=df.result,aes(x=Nlist,y=Y.scale.50,colour='Y.scale.50'),size=1.5,alpha=0.2)
  }else{
    gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.id.50"),values = c("blue", "firebrick2","orange"),labels=c("WERM-ID-R","Plug-In","WERM-ID"))
    # gg = gg + scale_color_manual("",breaks=c("Y.global.50","Y.plugin.50","Y.heuristic.50","Y.id.50"),values = c("blue", "firebrick2","orange","darkgreen"),labels=c("WERM-ID-R-Global","Plug-in","WERM-ID-R-Heuristic","WERM-ID"))   
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