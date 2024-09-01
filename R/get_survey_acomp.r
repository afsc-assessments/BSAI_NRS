GET_ACOMP<-function(vast=VAST,species=srv_sp_str,start_yr=srv_start_yr,area=fsh_sp_area,Vmax_age=vmax_age,Seas=1,FLT=2,Gender=1,Part=0,Ageerr=0,Lgin_lo=1,Lgin_hi=120){
  ## create sql query
  


  if(area == 'GOA') { survey="47"
                        area_id ="99903"
                        }
    if(area == 'AI') { survey="52"
                       area_id= "99904"
                       }
    if(area == 'BS') {survey= c(98,143)
                      area_id = c(99900,99902)
                      }
    if(area == 'SLOPE') { survey = 78
                          print ("There are no size comp. worked up for the slope in the database")
                          area_id = c(99905)
                          stop()
                      }
  
  Count = readLines('sql/count_AKFIN.sql')
  Count = sql_filter(sql_precode = "IN", x =species , sql_code = Count, flag = '-- insert species')
  Count = sql_filter(sql_precode = "IN", x =survey , sql_code = Count, flag = '-- insert survey')
  Count = sql_run(afsc, Count) %>% data.table() %>%
    dplyr::rename_all(toupper)

     
	if(!vast) {
	
    Age = readLines('sql/survey_agecomp.sql')
    Age = sql_filter(sql_precode = "in", x =area_id , sql_code = Age, flag = '-- insert area_id')
    Age = sql_filter(sql_precode = "=", x =species , sql_code = Age, flag = '-- insert species')
    Age = sql_filter(sql_precode = ">=", x = start_yr , sql_code = Age, flag = '-- insert start_year')
    Acomp <- sql_run(afsc, Age) %>% dplyr::rename_all(toupper)%>%data.table()

    if(area=='BS'){
      Age = readLines('sql/survey_agecomp.sql')
      Age = sql_filter(sql_precode = "in", x =99901 , sql_code = Age, flag = '-- insert area_id')
      Age = sql_filter(sql_precode = "=", x =species , sql_code = Age, flag = '-- insert species')
      Age = sql_filter(sql_precode = "<=", x = 1986 , sql_code = Age, flag = '-- insert start_year')
  
      Acomp2 <- sql_run(afsc, Age) %>% dplyr::rename_all(toupper)%>%data.table()
      
      if(nrow(Acomp2>0)){
        Acomp=data.table(rbind(Acomp2,Acomp))
      }}


    Acomp1<-Acomp[AGE < Vmax_age]
    Acomp2<-Acomp[AGE >= Vmax_age]

    Acomp2[Acomp2$AGE >= Vmax_age]$AGE=Vmax_age

    if(nrow(Acomp2)>0){
      Acomp3<-Acomp2[,list(AGEPOP=sum(AGEPOP)),by=c("AGE","YEAR")]
      Acomp<-rbind(Acomp1,Acomp3)
      Acomp<-Acomp[order(YEAR,AGE)]
    }

   ## run database query

  	YR<-unique(sort(Acomp$YEAR))
   	grid=expand.grid(AGE=c(0:Vmax_age),YEAR=YR)
  	
    Acomp<-merge(grid,Acomp,all=T)
    Acomp$AGEPOP[is.na(Acomp$AGEPOP)==T]<-0
	}

	if(vast){
    
    Proportions<-data.table(VAST_AGECOMP)
   	Proportions<-Proportions[Region=='Both'& Year!=2020]
    YR<-unique(sort(Proportions$Year))
    n=ncol(Proportions)-2
	  AGECOMP<-Proportions[,2:n]
    
    if(max_age > ncol(AGECOMP)-1){ max_age <- ncol(AGECOMP)-1}
    
    MAGE=max_age+1
    if(ncol(AGECOMP) > MAGE){
      AGECOMP1<-AGECOMP[,1:MAGE]
      AGECOMP1_plus<-AGECOMP[,(MAGE+1):ncol(AGECOMP1)]
      AGEP<-rowSums(AGECOMP1_plus)
      AGECOMP1[,MAGE:MAGE]<-AGECOMP1[,MAGE:MAGE]+AGEP
      AGECOMP<-AGECOMP1
    }
      AGECOMP$YEAR<-Proportions$Year
      names(AGECOMP)<-c(0:max_age,"YEAR")
      Acomp<-melt(AGECOMP,"YEAR")
      names(Acomp)<-c("YEAR","AGE","AGEPOP")
      Acomp<-data.table(Acomp)[order(YEAR,AGE)]
  }
  
  FIN<-MAGE+9 
  Nsamp<-Count[YEAR%in%YR]$HAULS
  y<-matrix(ncol=FIN,nrow=length(YR))
  SS_out<-data.frame(y)
  names(SS_out)<-c("YEAR","Seas","FltSvy","Gender","Part","Ageerr","Lgin_lo","Lgin_hi","Nsamp",paste0("F",c(0:max_age)))
  SS_out$YEAR=YR
  SS_out$Seas=Seas
  SS_out$FltSvy=FLT
  SS_out$Gender=Gender
  SS_out$Part=Part
  SS_out$Ageerr=Ageerr
  SS_out$Lgin_lo=Lgin_lo
  SS_out$Lgin_hi=Lgin_hi
  SS_out$Nsamp=Nsamp
  
  for (i in 1:length(YR)){
    SS_out[i,10:FIN]<-Acomp$AGEPOP[Acomp$YEAR==YR[i]]
  }
    
  SS_out
}
   
   
   
   
   