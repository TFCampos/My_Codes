Spliter<-function(DataBase,Vars,Target,Weights){
  
  formulas<-paste(Target,Vars,sep ='~')
  for(i in 1:length(Vars))
  {
    Tree.Vars<-rpart(formulas[i],
                    weights = vars(Weights),
                    data=DataBase,
                    method = "class",
                    parms = list(split='information'),
                    control=list(maxdepth = 2))
    
    Splits<-Tree.Vars$splits[,4] %>% sort
    if (length(Splits)==1) {
      cond<-paste(Vars[i],Splits,sep = '<')
      IfElse<-paste(paste('if_else(is.na(',Vars[i],"),0,",sep=""),paste('if_else(',paste(eval(parse(text=conds)),"1","2",sep=','),')',sep=""),sep="")
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
        }
    Mutate<-paste("factor(",IfElse,",levels=",paste(0,length(conds),sep = ':'),")",sep="")
    DataBase%>%
      mutate(eval(parse(text = Mutate)))->DataBase
    names(DataBase)[names(DataBase)=="eval(parse(text = Mutate))"] <- paste("Cat",Vars[i],sep="_")
  }
  return(DataBase)
}
