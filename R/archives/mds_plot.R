# mdsAcc=function(data,formula,measure){
#   data.molten=melt(species,id=all.vars(formula),measure=measure);
#   composition=dcast(data.molten,formula,sum);
#   sites.2.removed=rowSums(composition[-1])==0;
#   print("Sites with zero abundance not included in the mds plot:");
#   print(composition$Location[sites.2.removed]);
#   composition=composition[!sites.2.removed,];
#   
#   d=vegdist(composition[-1],method="bray");
# 
#   res.isoMDS=isoMDS(d,k=2);
#   print(paste("Stress value:",res.isoMDS$stress));
#   composition.red=data.frame(Location=composition[1],res.isoMDS$points);
# 
#   gp=ggplot(data=composition.red,aes(x=X1,y=X2))+geom_point()+geom_text(aes(label=Location),hjust=-0.2)+
#     coord_equal()+labs(title=paste("Non-metric MDS"));
#   return(gp);
# }


#' @title Multidimensional scaling
#' @rdname mds
#' @export
mds=function(data,id,mapping.point,mapping.path,with.label,...){
  UseMethod("mds",data)
}

#' @rdname mds
#' @S3method mds dist
#' @method mds dist
mds.dist=function(data,id,...){
  res.isoMDS=isoMDS(data,...);
  if(missing(id)) id=data.frame()[seq(attr(data,"Size")),]
  
  res=merge(id,res.isoMDS$points,by=0,all.x=F,all.y=T)
  res=list(data=res,stress=res.isoMDS$stress)
  class(res)="mds"
  res
}

# BE CAREFULL NA BEHAVIOUR AND NON NUMERIC DATA
#' @rdname mds
#' @S3method mds data.frame
#' @method mds data.frame
mds.data.frame=function(data,...){
  id=attr(data,"keys")
  
  data.num=as.matrix(data[!(names(data)%in%id)])
  mode(data.num)="numeric"
  
  null.rows=rowSums(data.num)==0
  rows.removed=data[null.rows,id]
  
  d=vegdist(data.num[!null.rows,],method="bray")  
  mds.dist(d,id=data[names(data)%in%id],...)
}

#' @S3method plot mds
#' @method plot mds
plot.mds=function(x,with.stress=F,with.label=F,...){
  data=x$data

  args=list(...)
  if("mapping"%in%names(args)){
    mapping=args[["mapping"==names(args)]]
  }
  else{
    id.vars=setdiff(names(data),c("Row.names","V1","V2"))
    if(length(id.vars)==0){
      mapping=NULL
    }
    else{
      mapping=aes_string(color=id.vars[1])
    }
  }
  g=ggplot(data=data,aes(x=V1,y=V2))+geom_point(mapping=mapping,...)+coord_equal()

  if(with.label){
    g=g+geom_text(aes(label=as.character(Row.names)),...)
  }
  if(with.stress){
    g=g+ggtitle(paste("The stress value is",stress(x)))
  }
  
  g
}

#' @rdname mds
#' @export
stress=function(mds){
  mds$stress
}

#' @title Add "keys" attribute to a data frame
#' @export
setKeys=function(data,keys.var=NA){
  attr(data,"keys")=keys.var
  data
}