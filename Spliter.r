Spliter<-function(TrainDataBase,TestDataBase,Vars,Target,IDs_Var){
  Split_Out<-matrix(0,ncol=2,nrow=length(Vars))
  colnames(Split_Out)<-c('Vars','Split')
  formulas<-paste(Target,Vars,sep ='~')
  for(i in 1:length(Vars))
  {
    Tree.Vars<-rpart(formulas[i],
                    weights = Weights,
                    data=TrainDataBase,
                    method = "class",
                    parms = list(split='information'),
                    control=list(maxdepth = 2))
    
    Splits<-Tree.Vars$splits[,4] %>% sort
    if(is.null(Splits)) Splits<-TrainDataBase%>%select(Vars[i])%>%as.matrix()%>%median(na.rm=T)
    Split_Out[i,]<-c(Vars[i],paste(Splits,collapse = " - "))
    if (length(Splits)==1) {
      conds<-paste(Vars[i],Splits,sep = '<')
      #IfElse<-paste(paste('if_else(is.na(',Vars[i],"),0,",sep=""),paste('if_else(',paste(eval(parse(text=conds)),"1","2",sep=','),')',sep=""),sep="")
      IfElse<-paste(paste('if_else(is.na(',Vars[i],"),0,",sep=""),paste('if_else(',conds,",1,2))",sep=''),sep='')
      Mutate<-paste("factor(",IfElse,",levels=",paste(0,2,sep = ':'),")",sep="")
      } else {
        cond_begin<-paste(Vars[i],Splits[1],sep = '<')
        for(j in 2:length(Splits))
          {
          assign(paste('cond',j,sep='_'),paste(paste(Vars[i],Splits[j-1],sep = '>='),
                                               paste(Vars[i],Splits[j],sep = '<'),sep=' & '))
          }
        cond_end<-paste(Vars[i],Splits[length(Splits)],sep = '>=')
        conds<-ls()[str_detect(ls(),'cond_')]
        conds <- c('cond_begin',conds[!conds%in%c('cond_begin','cond_end')],'cond_end')
        IfElse [1]<- paste('if_else(is.na(',Vars[i],"),0",sep="")
        for(j in 1:(length(conds)-1))
          {
          IfElse [j+1]<- paste('if_else(',paste(eval(parse(text = conds[j])),j,sep = ','),sep = "")
          }
        IfElse <- paste(paste(IfElse,collapse  =','),paste(length(conds),paste(rep(")",times=length(conds)),collapse = ''),sep = ''),sep=',')
        Mutate<-paste("factor(",IfElse,",levels=",paste(0,length(conds),sep = ':'),")",sep="")
        }
    TrainDataBase%>%
      mutate(eval(parse(text = Mutate)))->TrainDataBase
    names(TrainDataBase)[names(TrainDataBase)=="eval(parse(text = Mutate))"] <- paste("Cat",Vars[i],sep="_")
    TestDataBase%>%
      mutate(eval(parse(text = Mutate)))->TestDataBase
    names(TestDataBase)[names(TestDataBase)=="eval(parse(text = Mutate))"] <- paste("Cat",Vars[i],sep="_")
  }
  TrainDataBase%>%select(c(IDs_Var,Target,starts_with('Cat')))->TrainDataBase
  TestDataBase%>%select(c(IDs_Var,Target,starts_with('Cat')))->TestDataBase
  return(list(Train=TrainDataBase,Test=TestDataBase,Splits=Split_Out))
}
