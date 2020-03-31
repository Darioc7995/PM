#############################################
#SISTEMO LA DATA NEL DATASET E ORDINO PER DATA
#############################################
datiOK<-function(dataset){
library(readxl)
library(caret)
library(stringr)


colnames(dataset)<-gsub(" ", "_", colnames(dataset))


#sistemo WET_CLEAN_TYPE--> KIT_Acg in KIT_ACg
dataset$WET_CLEAN_TYPE[which(dataset$WET_CLEAN_TYPE=="KIT_Acg")]="KIT_ACg"


#chr in factor
dataset$WET_CLEAN_WEEK<-as.factor(dataset$WET_CLEAN_WEEK)
dataset$WET_CLEAN_YEAR<-as.factor(dataset$WET_CLEAN_YEAR)
dataset$WET_CLEAN_TYPE<-as.factor(dataset$WET_CLEAN_TYPE)


dataset$ETCH_TIME<-dataset$Start_Time
#####
#GESTIRE LA DATA
mm<-str_sub(dataset$ETCH_TIME, start= 6,end=7)

#GIORNO
gg<-str_sub(dataset$ETCH_TIME, start= 9,end=10)

#ANNO
yy<-str_sub(dataset$ETCH_TIME, start= 1,end=4)

date<-paste(gg,mm,yy,sep="/")


#ORA
ora<-substring(dataset$ETCH_TIME, regexpr(" ", dataset$ETCH_TIME) + 1)


###
dataset$gg<-gg
dataset$mm<-mm
dataset$anno<-yy
dataset$date<-date
dataset$ora<-ora

dataset$gg<-as.factor(dataset$gg)
dataset$mm<-as.factor(dataset$mm)
dataset$anno<-as.factor(dataset$anno)
dataset$date<-as.factor(dataset$date)
dataset$ora<-as.factor(dataset$ora)
#########################################################


dataset$date<-as.Date(dataset$date,"%d/%m/%Y")
dataset_sort<-dataset[order(dataset$date,dataset$ora),]

dataset_sort<-dataset_sort[405:nrow(dataset_sort),]
dataset_sort<-dataset_sort[1:2874,]


#NA
miss<-which(is.na(dataset_sort$PM_ANNUALE_ORE_RF)==T)
dataset_sort$PM_ANNUALE_ORE_RF[miss]<- sum(dataset_sort$PM_ANNUALE_ORE_RF[miss-1],dataset_sort$PM_ANNUALE_ORE_RF[miss+1])/2
dataset_sort$WET_CLEAN_ORE_RF[miss]<- sum(dataset_sort$WET_CLEAN_ORE_RF[miss-1],dataset_sort$WET_CLEAN_ORE_RF[miss+1])/2
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="Mean_STEP_04_TOP_RF_Reflected_power")]


#NZV
nzv<-nearZeroVar(dataset_sort, 
                 freqCut = 95/5, 
                 uniqueCut = 10)

dataset_sort<-dataset_sort[,-nzv]
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="ETCH_TIME")]
#variabili correlate a 1--> Max_RAP_cycles_completed e Max_Total_RAP_cycles
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="Max_Total_RAP_cycles")]
#Max_STEP_05_06_Recipe_Step_Elapsed_Time e Mean_ENDPT_STEP_05_ProcChm_EndPt_Step_FloatTime
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="Mean_ENDPT_STEP_05_ProcChm_EndPt_Step_FloatTime")]
#TEMP_CHUCK_DELTA
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="TEMP_CHUCK_DELTA")]
#Max_TCP_Window_Temperature e Mean_TCP_Window_Temperature
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="Max_TCP_Window_Temperature")]


return(dataset_sort)

}


tempi<-function(dati,ampiezza,passo){
  amp<-ampiezza
  k<-passo
  data.q<-dati
  finestra<- ceiling( amp*nrow(data.q)/100 ) #575 obs per finestra
  passo<- ceiling( k*nrow(data.q)/100 )
  n.finestre<-( (100-amp)/k )+1
  
  
  ###CREO LA MATRICE DEI TEMPI
  
  t1<-1:finestra
  TE<-matrix(NA,finestra,n.finestre)
  TE[,1]<-t1
  for (i in 2:(n.finestre-1)) {
    nam<-paste("t", i, sep = "")
    assign(nam,t1+passo*(i-1))
    TE[,i]<-(t1+passo*(i-1))
  }
  last<-paste("t", n.finestre, sep = "")
  assign(last,(1+passo*(n.finestre-1)):nrow(data.q))
  TE[1:length((1+passo*(n.finestre-1)):nrow(data.q)),n.finestre]<-  (1+passo*(n.finestre-1)):nrow(data.q)
  return(TE)
}



tempi1<-function(dati,ampiezza.stima,ampiezza.prev,passo){
  amp.s<-ampiezza.stima
  amp.p<-ampiezza.prev
  passo<-passo
  
  #amp.s<-5
  #amp.p<-3
  #passo<-5
  
  
  finestra.s<-ceiling( amp.s*nrow(dati)/100 ) 
  finestra.p<-ceiling( amp.p*nrow(dati)/100 ) 
  
  finestra.s*(100/amp.s)
  finestra.p*(100/amp.p)
  
  walk<- ceiling( passo*nrow(dati)/100 )
  n.finestre<-( (100-amp.s)/passo )+1
  
  
  
  
  ###CREO LA MATRICE DEI TEMPI
  
  t1<-1:finestra.s
  TE.s<-matrix(NA,finestra.s,n.finestre)
  TE.s[,1]<-t1
  for (i in 2:(n.finestre-1)) {
    nam<-paste("t", i, sep = "")
    assign(nam,t1+walk*(i-1))
    TE.s[,i]<-(t1+walk*(i-1))
  }
  last<-paste("t", n.finestre, sep = "")
  assign(last,(1+walk*(n.finestre-1)):nrow(dati))
  TE.s[1:length((1+walk*(n.finestre-1)):nrow(dati)),n.finestre]<-  (1+walk*(n.finestre-1)):nrow(dati)
  
  
  colonne<-0
  for (k in 1:ncol(TE.s)) {
    ifelse( (TE.s[nrow(TE.s),k]+finestra.p) < nrow(dati),
            colonne<-colonne+1,
            colonne<-colonne+0)
  }
  
  ifelse(amp.p<passo,
         colonne<-colonne-1,
         colonne<-colonne
         )
  
  
  TE.p<-matrix(NA,finestra.p,colonne+1 )
  
  #tp1<-( TE[nrow(TE),1]+1): (TE[nrow(TE),1]+finestra.p )
  #TE.p[,1]<-tp1
  
  for (i in 1:colonne )  {
    TE.p[,i]<-( TE.s[nrow(TE.s),i]+1): (TE.s[nrow(TE.s),i]+finestra.p )
  }
  
  
  #dif<-(ncol(TE.s)-1) - colonne
  
  
  d<-1:length( (TE.s[nrow(TE.s),colonne+1]+1) : nrow(dati) )
  ifelse(length(d)>nrow(TE.p),
         TE.p[,colonne+1]<- ( TE.s[nrow(TE.s),colonne+1]+1  ) : ( ( TE.s[nrow(TE.s),colonne+1]  )+nrow(TE.p)),
         TE.p[d,colonne+1]<- ( TE.s[nrow(TE.s),colonne+1]+1  ) : nrow(dati)
  )
  
  
  TE<-list(TE.s,TE.p)
  
  return(TE)
  
}





calibro<-function(lista){
  for (i in 1:length(lista)) {
    Errs<-lista[[i]]
    quar<-length(which(Errs$ampiezza=="40%"))-1
    
    cinque<-which(Errs$ampiezza=="05%")
    cinque<-cinque[(length(cinque)-quar):length(cinque)]
    
    dieci<-which(Errs$ampiezza=="10%")
    dieci<-dieci[(length(dieci)-quar):length(dieci)]
    
    quindici<-which(Errs$ampiezza=="15%")
    quindici<-quindici[(length(quindici)-quar):length(quindici)]
    
    venti<-which(Errs$ampiezza=="20%")
    venti<-venti[(length(venti)-quar):length(venti)]
    
    venticinque<-which(Errs$ampiezza=="25%")
    venticinque<-venticinque[(length(venticinque)-quar):length(venticinque)]
    
    trenta<-which(Errs$ampiezza=="30%")
    trenta<-trenta[(length(trenta)-quar):length(trenta)]
    
    trentacinque<-which(Errs$ampiezza=="35%")
    trentacinque<-trentacinque[(length(trentacinque)-quar):length(trentacinque)]
    
    quaranta<-which(Errs$ampiezza=="40%")
    
    length(cinque);length(dieci);length(quindici);length(venti);length(venticinque)
    length(trenta);length(trentacinque);length(quaranta)
    
    
    Errs<-Errs[c(cinque,dieci,quindici,venti,venticinque,trenta,trentacinque,quaranta),]
    
    
    assign(paste("RMSE.bis",i,sep=""),Errs)
  }
  tot<-list(RMSE.bis1,RMSE.bis2,RMSE.bis3,RMSE.bis4,RMSE.bis5)
  return(tot)
}



















