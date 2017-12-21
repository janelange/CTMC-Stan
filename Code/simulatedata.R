get.obs.data.many<-function(rate.matrix,obs.times=seq(1,30,2),end.time=30, start.time=0,start.state=1,absorbing.state=1,num.individuals,simplify=F){
  ###########################################################################
  # author: JL 2/2/2011
  # This function obtains simulated data from an CTMC at discrete observation times for multiple subjects
  # INPUTS: rate.matrix=transition intensity matrix for underlying states, emission.matrix=emissin matrix for observed states
  #         obs.times=discrete observation times; start.time=the start time for the CTMC simulation, start.state, the start state (single or vector)
  #         for the CTMC, num.individuals=number of individuals to simulate data for
  # OUTPUTS: a list with num.individual elements, and 2 sub-elements; obs.times, and obs.data
  #
  ############################################################################
  if(length(start.state)>1){
    trajectory.list<-lapply(start.state, FUN="sim.ctmc", rate.matrix=rate.matrix, end.time=end.time, start.time=0,
                            absorbing.state=absorbing.state)
  }else{
    trajectory.list<-replicate(n=num.individuals, sim.ctmc(rate.matrix=rate.matrix, start.state=start.state, end.time=end.time,start.time=0,
                                                           absorbing.state=absorbing.state),simplify=F)
  }
  if(is.list(obs.times)){
    underlying.data.list<-mapply(lapply(trajectory.list,"[[",c("times")),lapply(trajectory.list,"[[",c("states")),
                                 obs.times=obs.times,FUN="discrete.ctmc",SIMPLIFY=F)
  }else{
    underlying.data.list<-mapply(lapply(trajectory.list,"[[",c("times")),lapply(trajectory.list,"[[",c("states")),
                                 FUN="discrete.ctmc",MoreArgs=list(obs.times=obs.times),SIMPLIFY=F)
  }
  return(list(trajectory.list,underlying.data.list)) 
 
}

sim.ctmc<-function(start.state,rate.matrix, end.time,start.time=0,absorbing.state){
  ########################################################################################
  #Author: JL, 2/7/2011
  #This function simulates from a homogeneous CTMC characterized by rate.matrix
  #INPUTS: rate.matrix=rate matrix, start.state=starting state for CTMC, end.time=time
  #         to stop data simulations; start.time= time to start data simulations
  #OUTPUTS: a list with two objects: "times"=transition times and "states"=transition states
  #
  #
  ##########################################################################################
  state.space<-seq(1:dim(rate.matrix)[1])
  size<-dim(rate.matrix)[1]
  cur.state<-start.state
  times<-vector()
  states<-vector()
  
  times[1]<-start.time
  states[1]<-cur.state
  cur.time<-start.time
  k<-2
 # browser()
  while((cur.time<end.time)&(!(cur.state%in% absorbing.state))){
    exp.rate<-(-1)*rate.matrix[cur.state,cur.state]
    if(exp.rate==0){
      cur.time<-end.time
    } else{
      cur.time<-cur.time+rexp(n=1,rate=exp.rate)
      if(cur.time<end.time){
        times[k]<-cur.time
        if(size==2){
          cur.state=as.numeric(state.space[-cur.state])
        }else{
          cur.state<-sample(state.space[-cur.state],size=1,prob=rate.matrix[cur.state,-cur.state])
        }
        states[k]<-cur.state
        k<-k+1
        print(cur.state)
      }
    }
  }
  return.list<-list(times,states)
  names(return.list)<-c("times","states")
  return(return.list)
}

process_data<-function(simdata){
  temp1=head(cbind(simdata$states, simdata$states[-1]),-1)
  temp2=head(cbind(simdata$obs.times,simdata$obs.times[-1]),-1)
  out=data.frame(cbind(temp1,temp2))
  # browser()
  names(out)=c("startstate","endstate","starttime","endtime")
  out$dur=out$endtime-out$starttime
  return(out)
}


