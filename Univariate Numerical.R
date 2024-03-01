rm(list = ls()) 
m_n <-(read.csv("owid-covid-data.csv"))

summary(m_n,total_deaths_per_million)
base= ggplot(m_n,aes(y = total_deaths_per_million))  
base + geom_boxplot()

(statVals=summary(m_n$total_deaths_per_million,digits = 3)[1:6])

# the summary values as vector
statVals=statVals%>%as.vector() 
base= ggplot(m_n,aes(y = total_deaths_per_million))  
b1= base + geom_boxplot() 
b1=b1+ scale_y_continuous(breaks = statVals) 
b1

b1=b1 +coord_flip()
b1

(upperT=ggplot_build(b1)$data[[1]]$ymax)
(numOutliers=sum(m_n$total_deaths_per_million>upperT,na.rm = T))

txtOutliers=paste0('#Outlying deaths: ',numOutliers)
txtUpper=paste0('Threshold:',upperT)


b1_vertical = b1 + geom_hline(yintercept = upperT,
                              color='red',
                              linetype="dotted",
                              size=2) 
b1_annot=b1_vertical + annotate(geom = 'text',
                                label=txtUpper,
                                y = upperT+5,
                                x=0.2,
                                angle=90)

b1_annot=b1_annot + annotate(geom = 'text',
                             label=txtOutliers,
                             y = upperT+60,
                             x=0.1,
                             angle=0)
b1_annot
b1_annot_noX = b1_annot + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
b1_annot_noX

b1_newGrid=b1_annot_noX +  theme_classic()
b1_newGrid

b1_better_axisText = b1_newGrid+ theme(axis.text.x = element_text(angle = 60,
                                                                  size = 7,
                                                                  vjust = 0.5))
b1_better_axisText
library(DescTools)


cv=CoefVar(m_n$total_deaths_per_million,na.rm = T)
sd=SD(m_n$total_deaths_per_million,na.rm = T)
md=Median(m_n$total_deaths_per_million,na.rm = T)
mn=Mean(m_n$total_deaths_per_million,na.rm = T)
mn.low=MeanCI(m_n$total_deaths_per_million,
              na.rm = T)[['lwr.ci']]
mn.up=MeanCI(m_n$total_deaths_per_million,
             na.rm = T)[['upr.ci']]
sk=Skew(m_n$total_deaths_per_million,
        na.rm = T)




barWIDTH=10
library(ggplot2)
base= ggplot(m_n)  
h1= base + geom_histogram(aes(x = total_deaths_per_million),
                          binwidth = barWIDTH,
                          fill='black') 
h1=h1 + labs(y="count")
h1


txtMean=paste0('Mean:',round(mn))
txtSkew=paste0('Skeness:',round(sk,2))


h1+ geom_vline(xintercept = mn,color='red') + 

  annotate(geom = 'text',color='red',
           label=txtMean, 
           y = 400,
           x=mn+5,
           angle=90) + 
  # about the skewness
  annotate(geom = 'text', color='blue',
           label=txtSkew, 
           y = 50,
           x=upperT+170,
      
                angle=0) 



base=ggplot(m_n) + theme_light()
box2=base + geom_boxplot(aes(y=total_deaths_per_million)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
box2=box2 + coord_flip()
box2

hist2=base + geom_histogram(aes(x=total_deaths_per_million))
hist2=hist2 + labs(y='count')
hist2

ggplot_build(hist2)$data[[1]]%>%head()
(fromHist=ggplot_build(hist2)$data[[1]][,c('count','x','xmin','xmax')])
(modeClassInfo=round(fromHist[which.max(fromHist$count),],2))

count for modal class


ModeCountY=modeClassInfo$count

PositionCountX=modeClassInfo$x

txtMode=paste0("<- Count of Modal Class [",
               modeClassInfo$xmin,' - ',
               modeClassInfo$xmax,']')

hist2ann=hist2 + geom_hline(yintercept =ModeCountY,
                            linetype="dotted") +
  annotate(geom = 'text',
           label=ModeCountY,
           y = ModeCountY+40,
           x=PositionCountX,
           color='red',
           angle=0) +
  annotate(geom = 'text',
           label=txtMode,
           y = ModeCountY+40,
           x=PositionCountX+90,
           color='red',
           angle=0)
hist2ann


library(ggpubr)
ggarrange(hist2ann,box2,align='v',ncol = 1,heights = 2:1)