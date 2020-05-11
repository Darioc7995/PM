#############################################
#SISTEMO LA DATA NEL DATASET E ORDINO PER DATA
#############################################
datiOK<-function(dataset){
library(readxl)
library(caret)
library(stringr)

#Variabili hanno nome con spazio--> metto trattino basso al posto dello spazio
colnames(dataset)<-gsub(" ", "_", colnames(dataset))

#Un'osservazione in "WET_CLEAN_TYPE" (tipo di kit applicato) aveva coma valore
#"KIT_Acg" al posto di "KIT_ACg".
dataset$WET_CLEAN_TYPE[which(dataset$WET_CLEAN_TYPE=="KIT_Acg")]="KIT_ACg"


#chr in factor
dataset$WET_CLEAN_WEEK<-as.factor(dataset$WET_CLEAN_WEEK)
dataset$WET_CLEAN_YEAR<-as.factor(dataset$WET_CLEAN_YEAR)
dataset$WET_CLEAN_TYPE<-as.factor(dataset$WET_CLEAN_TYPE)


## LAVORO SULLA VARIABILE CHE DESCRIVE LA DATA DI MISURAZIONE DEL DATO
#OBIETTIVO--> ORDINARE I DATI

#####
#GESTIRE LA DATA
mm<-str_sub(dataset$Start_Time, start= 6,end=7)

#GIORNO
gg<-str_sub(dataset$Start_Time, start= 9,end=10)

#ANNO
yy<-str_sub(dataset$Start_Time, start= 1,end=4)

date<-paste(gg,mm,yy,sep="/")


#ORA
ora<-substring(dataset$Start_Time, regexpr(" ", dataset$Start_Time) + 1)


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


#SPEZZO IL DATASET PRENDENDO LA PORZIONE DI RIFLESSA CRESCENTE
dataset_sort<-dataset_sort[405:nrow(dataset_sort),]
dataset_sort<-dataset_sort[1:2874,]


#PM E WET HANNO 1 "NA"--> RIMPIAZZO CON VALOR MEDIO 
#TRA IL PRECEDENTE ED IL SUCCESSIVO
miss<-which(is.na(dataset_sort$PM_ANNUALE_ORE_RF)==T)
dataset_sort$PM_ANNUALE_ORE_RF[miss]<- sum(dataset_sort$PM_ANNUALE_ORE_RF[miss-1],dataset_sort$PM_ANNUALE_ORE_RF[miss+1])/2
dataset_sort$WET_CLEAN_ORE_RF[miss]<- sum(dataset_sort$WET_CLEAN_ORE_RF[miss-1],dataset_sort$WET_CLEAN_ORE_RF[miss+1])/2
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="Mean_STEP_04_TOP_RF_Reflected_power")]


#MOLTE VARIABILI HANNO VARIABILITà NULLA (STESSO VALORE PER OGNI MISURAZIONE)
nzv<-nearZeroVar(dataset_sort, 
                 freqCut = 95/5, 
                 uniqueCut = 10)

#TOLGO VARIABILI A VARIANZA NULLA E QUELLA CHE SI RIFERISCE ALLA DATA
dataset_sort<-dataset_sort[,-nzv]

#ALCUNE COPPIE DI VARIABILI HANNO CORRELAZIONE RECIPROCA PARI A 1 (NE TOLGO UNA)
#variabili correlate a 1--> Max_RAP_cycles_completed e Max_Total_RAP_cycles
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="Max_Total_RAP_cycles")]
#Max_STEP_05_06_Recipe_Step_Elapsed_Time e Mean_ENDPT_STEP_05_ProcChm_EndPt_Step_FloatTime
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="Mean_ENDPT_STEP_05_ProcChm_EndPt_Step_FloatTime")]
#TEMP_CHUCK_DELTA,Max_TCP_Window_Temperature
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="TEMP_CHUCK_DELTA")]
#Max_TCP_Window_Temperature e Mean_TCP_Window_Temperature
dataset_sort<-dataset_sort[,-which(colnames(dataset_sort)=="Max_TCP_Window_Temperature")]

#############
#EVIDENTI OUTLIERS (4) NELLA VARIABILE "Min_DECLAMP_ESC_Voltage_In"
#FACCIO MEDIA TRA LE 5 RILEVAZIONI PRECEDENTI
out<-which(dataset_sort$Min_DECLAMP_ESC_Voltage_In>-1100)
dataset_sort$Min_DECLAMP_ESC_Voltage_In[out[1]]<- 
  mean( dataset_sort$Min_DECLAMP_ESC_Voltage_In[(out[1]-5):(out[1]-1)]    )

dataset_sort$Min_DECLAMP_ESC_Voltage_In[out[2]]<- 
  mean( dataset_sort$Min_DECLAMP_ESC_Voltage_In[(out[2]-5):(out[2]-1)]    )

dataset_sort$Min_DECLAMP_ESC_Voltage_In[out[3]]<- 
  mean( dataset_sort$Min_DECLAMP_ESC_Voltage_In[(out[3]-5):(out[3]-1)]    )

dataset_sort$Min_DECLAMP_ESC_Voltage_In[out[4]]<- 
  mean( dataset_sort$Min_DECLAMP_ESC_Voltage_In[(out[4]-5):(out[4]-1)]    )


return(dataset_sort)

}


#tempi<-function(dati,ampiezza,passo){
#  amp<-ampiezza
#  k<-passo
#  data.q<-dati
#  finestra<- ceiling( amp*nrow(data.q)/100 ) #575 obs per finestra
#  passo<- ceiling( k*nrow(data.q)/100 )
#  n.finestre<-( (100-amp)/k )+1
  
  
  ###CREO LA MATRICE DEI TEMPI
  
#  t1<-1:finestra
#  TE<-matrix(NA,finestra,n.finestre)
#  TE[,1]<-t1
#  for (i in 2:(n.finestre-1)) {
#    nam<-paste("t", i, sep = "")
#    assign(nam,t1+passo*(i-1))
#    TE[,i]<-(t1+passo*(i-1))
#  }
#  last<-paste("t", n.finestre, sep = "")
#  assign(last,(1+passo*(n.finestre-1)):nrow(data.q))
#  TE[1:length((1+passo*(n.finestre-1)):nrow(data.q)),n.finestre]<-  (1+passo*(n.finestre-1)):nrow(data.q)
#  return(TE)
#}


#FUNZIONE CHE MI SPEZZA IL DATASET IN PORZIONI DI DATI DI TRAIN E DI TEST
#IN BASE AI VALORI DI AMPIEZZA E PASSO CHE INSERISCO
tempi1<-function(dati,ampiezza.stima,ampiezza.prev,passo){
  amp.s<-ampiezza.stima
  amp.p<-ampiezza.prev
  passo<-passo
  
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




#FUNZIONE CHE MI PERMETTE DI CONFRONTARE AMPIEZZE DI STIMA DIFFERENTI
#(FATTA PER CONFRONTARE AMPIEZZE DI STIMA PARI A: 5,10,15,20,25,30,35,40)
#TOLGO GLI ERRORI COMMESSI DAL MODELLO NELLA PARTE INIZIALE DEL CICLO PRODUTTIVO
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













#FUNZIONE CHE MI PERMETTE DI OTTENERE LA MATRICE FATTA DA DIEGO
#threshold= VALORE SOGLIA (ES: PER XGBOOST ERA 0.0225)
#n.smooth= NUMERO DI FINESTRE DI LISCIAMENTO (es: n.smooth=5 la funzione fa una media
#mobile di una matrice 5x5 di valori)
Smooth.matrix<-function(dataset,threshold,n.smooth,ordina=T){
  
  if(ordina) {
    #dicotomizzo
    matrice<-apply(dataset,1:2, function(x) ifelse(x<=threshold,0,1) )
    
    #somma cumulata di colonna 
    mat<-apply(matrice,2,cumsum)
    
    #indice
    n.zeri<-colSums(mat==0|mat==1)
    indice<-  as.numeric(  paste(n.zeri,".",1:length(n.zeri),sep="") )
    
    #ordino matrice 
    matrice<-matrice[,order(indice,decreasing = T)]
    
  } else {
    #dicotomizzo
    matrice<-apply(dataset,1:2, function(x) ifelse(x<=threshold,0,1) )
    
  }

  
  
  celle<-n.smooth
  p=ncol(matrice)
  n=nrow(matrice)
  lung.r<- n- (celle%/%2)*2
  lung.c<- p- (celle%/%2)*2
  
  x.ok<-matrix(0,nrow=lung.r,ncol= n)
  for (i in 1:lung.r) {
    x=c(rep(0,i-1),rep(1,3),rep(0,n-3- (i-1)))
    x.ok[i,]<-x
  }
  
  
  y.ok<-matrix(0,nrow=lung.c,ncol=p)
  for (i in 1:lung.c) {
    y=c(rep(0,i-1),rep(1,3),rep(0,p-3- (i-1)))
    y.ok[i,]<-y
  }
  
  
  lista<-list()
  for (i in 1:lung.r) {
    for (j in 1:lung.c) {
      M<-x.ok[i,]%o%y.ok[j,]
      MM<-M*matrice
      lista<-c(lista,list(MM))
    }
  }
  
  
  full<-matrix(sapply(lista,sum)/celle^2,nrow = lung.r,ncol=lung.c,byrow = T)
  empty.r<- matrix(0,nrow = nrow(full),ncol=celle%/%2) 
  full<-cbind( empty.r,full,empty.r     )
  empty.c<- matrix(0,nrow = celle%/%2 ,ncol=ncol(full)) 
  full<- rbind(empty.c,full,empty.c)
  colnames(full)<-colnames(matrice)
  
  nome.var<-c('StdDev_STEP_08_Esc_Current_1','StdDev_STEP_08_Bottom_RF_Forward_Power',
              'StdDev_STEP_08_Bottom_Peak_Voltage','StdDev_STEP_05_06_TOP_RF_Forward_power',
              'StdDev_STEP_05_06_Process_Gas_08_Flow','Max_CLAMP_Esc_Current_1',
              'StdDev_STEP_05_06_Process_Gas_01_Flow','StdDev_STEP_05_06_Esc_Current_1',
              'StdDev_STEP_05_06_Chamber_Pressure','StdDev_STEP_05_06_Bottom_Peak_Voltage',
              'StdDev_STEP_02_TOP_RF_Forward_power','StdDev_STEP_02_Process_Gas_08_Flow',
              'StdDev_STEP_02_Process_Gas_05_Flow','StdDev_STEP_02_Process_Gas_03_Flow',
              'StdDev_STEP_02_Esc_Current_1','StdDev_STEP_02_Bottom_RF_Forward_Power',
              'StdDev_STEP_02_Bottom_Peak_Voltage','StdDev_CLAMP_Chiller_Line_Out_Temperature',
              'Min_DECLAMP_ESC_Voltage_In','Mean_STEP_RAP_TOP_RF_Reflected_power',
              'Mean_STEP_RAP_Esc_Current_1','Mean_STEP_RAP_Bottom_RF_Reflected_Power',
              'Mean_STEP_11_Esc_Current_1','Mean_STEP_09_TOP_RF_Reflected_power',
              'Mean_STEP_09_Esc_Current_1','Mean_STEP_09_Bottom_RF_Reflected_Power',
              'Mean_STEP_09_Bottom_RF_Forward_Power','Mean_STEP_08_Chamber_Pressure',
              'Mean_STEP_08_Bottom_RF_Reflected_Power','Mean_STEP_08_Bottom_RF_Forward_Power',
              'Mean_STEP_05_06_TOP_RF_Reflected_power','Mean_STEP_05_06_TOP_RF_Forward_power',
              'Mean_STEP_05_06_Chamber_Pressure','Mean_STEP_02_TOP_RF_Forward_power',
              'Mean_STEP_02_Bottom_RF_Reflected_Power','Mean_STEP_02_Bottom_RF_Forward_Power',
              'Mean_STEP_02_Bottom_Peak_Voltage','Mean_ENDPT_STEP_08_ProcChm_EndPt_Step_FloatTime',
              'Mean_CLAMP_Post_Align_Error','Mean_CLAMP_Middle_Chamber_Temperature','Max_STEP_RAP_Process_Gas_07_Flow',
              'Max_STEP_RAP_Process_Gas_02_Flow','Max_STEP_RAP_Process_Gas_01_Flow','Max_STEP_08_Recipe_Step_Elapsed_Time',
              'Max_RAP_cycles_completed','TimeCount_DECLAMP_ESC_Voltage_In','StdDev_STEP_RAP_TOP_RF_Forward_power',
              'StdDev_STEP_RAP_Process_Gas_07_Flow','StdDev_STEP_RAP_Process_Gas_02_Flow','StdDev_STEP_RAP_Process_Gas_01_Flow',
              'StdDev_STEP_RAP_Bottom_Peak_Voltage','StdDev_STEP_13_Process_Gas_04_Flow','StdDev_STEP_13_Esc_Current_1',
              'Max_DECLAMP_Backside_Helium_Flow','StdDev_STEP_11_Process_Gas_02_Flow','StdDev_STEP_11_Esc_Current_1',
              'StdDev_STEP_09_TOP_RF_Forward_power','StdDev_STEP_09_Process_Gas_04_Flow',
              'StdDev_STEP_09_Esc_Current_1','StdDev_STEP_09_Bottom_RF_Forward_Power',
              'Max_CLAMP_START_ESC_Voltage_In','StdDev_STEP_09_Bottom_Peak_Voltage',
              'StdDev_STEP_05_06_Process_Gas_05_Flow','Mean_STEP_13_TOP_RF_Reflected_power',
              'Mean_STEP_11_Chamber_Pressure','Mean_STEP_09_Bottom_Peak_Voltage',
              'Mean_CLAMP_ESC_Voltage_In','StdDev_STEP_13_Chamber_Pressure','StdDev_STEP_13_Bottom_RF_Forward_Power',
              'Mean_STEP_13_TOP_RF_Forward_power','Mean_STEP_13_Esc_Current_1','Mean_STEP_08_Esc_Current_1',
              'Mean_STEP_RAP_VAT_position','Mean_STEP_05_06_Bottom_RF_Forward_Power','Max_STEP_05_06_Recipe_Step_Elapsed_Time',
              'Mean_STEP_09_TOP_RF_Forward_power','Mean_STEP_08_VAT_position','Mean_CLAMP_Bottom_Electrode_Temperature',
              'Max__STEP_RAP_Bottom_Peak_Voltage','StdDev_STEP_13_Bottom_Peak_Voltage','CHECK_TOP_RF_05_06',
              'Mean_STEP_02_VAT_position','Mean_STEP_09_VAT_position','Mean_STEP_05_06_Esc_Current_1','Mean_TCP_Window_Temperature',
              'Range_TCP_Window_Temperature','Mean_STEP_13_VAT_position','Mean_CLAMP_Chiller_Line_Out_Temperature',
              'StdDev_STEP_08_TOP_RF_Forward_power','Mean_STEP_08_TOP_RF_Reflected_power','Mean_STEP_05_06_Bottom_RF_Reflected_Power',
              'Max_DECLAMP_ESC_Current_1','Mean_STEP_05_06_Bottom_Peak_Voltage','StdDev_STEP_08_Process_Gas_04_Flow',
              'CHECK_TOP_RF_09','CHECK_TOP_RF_02','Mean_STEP_13_Chamber_Pressure','StdDev_STEP_11_Chamber_Pressure',
              'Min_STEP_RAP_Bottom_Peak_Voltage','Mean_STEP_08_Bottom_Peak_Voltage','Mean_STEP_RAP_Bottom_Peak_Voltage',
              'Mean_STEP_13_Bottom_RF_Reflected_Power','StdDev_STEP_11_TOP_RF_Forward_power','Mean_STEP_11_VAT_position',
              'StdDev_STEP_08_Chamber_Pressure','CHECK_TOP_RF_08','Mean_STEP_RAP_Chamber_Pressure','Mean_STEP_02_Esc_Current_1',
              'Mean_STEP_RAP_TOP_RF_Forward_power','Mean_STEP_RAP_Gas_Flow_Ratio','CHECK_TOP_RF_13','Mean_STEP_13_Bottom_RF_Forward_Power',
              'Mean_STEP_08_TOP_RF_Forward_power','Mean_CLAMP_Backside_Helium_Flow','StdDev_STEP_13_TOP_RF_Forward_power',
              'Mean_STEP_11_TOP_RF_Forward_power','Mean_STEP_05_06_VAT_position','Mean_STEP_13_Bottom_Peak_Voltage','CHECK_TOP_RF_11','DCQV')
  
  
  
  return(list(full,nome.var))
}




smooth.matrix2<-function(dataset,threshold,ordina=T){
  
  if(ordina) {
    #dicotomizzo
    matrice<-apply(dataset,1:2, function(x) ifelse(x<=threshold,0,1) )
    
    #somma cumulata di colonna 
    mat<-apply(matrice,2,cumsum)
    
    #indice
    n.zeri<-colSums(mat==0|mat==1)
    indice<-  as.numeric(  paste(n.zeri,".",1:length(n.zeri),sep="") )
    
    #ordino matrice 
    matrice<-matrice[,order(indice,decreasing = T)]
    
  } else {
    #dicotomizzo
    matrice<-apply(dataset,1:2, function(x) ifelse(x<=threshold,0,1) )
    
  }
  
  
  
  return(matrice)
  
}


