source("/Users/janelange/Documents/Work/Stan/Code/simulatedata.R")

rate_mat=matrix(0,nrow=5,ncol=5)
rate_mat[1,2]=.034
rate_mat[1,5]=.06
rate_mat[2,3]=.1
rate_mat[2,5]=.06
rate_mat[3,4]=.3
rate_mat[3,5]=.06
diag(rate_mat)=(-1)*apply(rate_mat,1,"sum")


out=get.obs.data.many(rate.matrix=rate_mat,obs.times=seq(0,10,.5),end.time=100, start.time=0,start.state=1,
                      absorbing.state=c(4,5),num.individuals=500,simplify=T)
simdata=out[[2]];
outdata=lapply(simdata,FUN="process_data")
simdata=do.call(rbind, outdata)


