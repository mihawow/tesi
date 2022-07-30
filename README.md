set.seed(333)#######
t=sample(1:4000,1) ###############
f=10
distanzeprov=distanze
for(i in 1:f){
  for (j in 1:i){
    distanze[i,j]= distanze[j,i]
  }
}
prezzog=matrix(0,f,f)
prezzoe=matrix(0,f,f)
prezzob=matrix(0,f,f)
for(i in 1:f){
  for (j in 1:f){
    if(i==j){
      prezzog[i,j]=0
      prezzoe[i,j]=0
      prezzob[i,j]=0
      
    } else {
      prezzog[i,j]=4+0.02*as.numeric(unlist(distanze[i,j]))
      prezzoe[i,j]=5+0.04*as.numeric(unlist(distanze[i,j]))
      prezzob[i,j]=10+0.09*as.numeric(unlist(distanze[i,j]))
    }
  }
}

incidenze=matrix(0,t,f)
for(i in 1:t){
  for(j in 1:f){
   incidenze[i,j]=sample(c(0,1),1) 
  }
}
progressivo=matrix(0,t,f)
for(i in 1:t){
  p=1
  for(j in 1:f){
    if(incidenze[i,j]==0){
      progressivo[i,j]==0
    } else {
      progressivo[i,j]=p
      p=p+1
    }
  }
}

giovani=matrix(0,t,f^2)
for(i in 1:t){
  for(j in 1:f){
    pr=0
    if(progressivo[i,j]>=1){
      v=j
      s=progressivo[i,j]
      for(k in 1:f){
        if(progressivo[i,k]==s+1){
          pr=pr+as.numeric(unlist(prezzog[v,k]))
          c=f*(j-1)+k
          giovani[i,c]=pr
          s=s+1
          v=k
        }
      }
    }
  }
}
economy=matrix(0,t,f^2)
for(i in 1:t){
  for(j in 1:f){
    pr=0
    if(progressivo[i,j]>=1){
      v=j
      s=progressivo[i,j]
      for(k in 1:f){
        if(progressivo[i,k]==s+1){
          pr=pr+as.numeric(unlist(prezzoe[v,k]))
          c=f*(j-1)+k
          economy[i,c]=pr
          s=s+1
          v=k
        }
      }
    }
  }
}
business=matrix(0,t,f^2)
for(i in 1:t){
  for(j in 1:f){
    pr=0
    if(progressivo[i,j]>=1){
      v=j
      s=progressivo[i,j]
      for(k in 1:f){
        if(progressivo[i,k]==s+1){
          pr=pr+as.numeric(unlist(prezzob[v,k]))
          c=f*(j-1)+k
          business[i,c]=pr
          s=s+1
          v=k
        }
      }
    }
  }
}
tempi=matrix(0,f,f)
for(i in 1:f){
  for (j in 1:f){
    if(i==j){
      tempi[i,j]=0} else{
        tempi[i,j]=round(as.numeric(unlist(distanze[i,j]))/4,0)
        
      }
  }
}
Partenze=matrix(0,t,f)
Arrivi=matrix(0,t,f)
for(i in 1:t){
  m=max(progressivo[i,])
  v=1
  time=sample(360:1100,1)#####
  for (j in 1:f){
    if(progressivo[i,j]<m){
    if(progressivo[i,j]==1){
      Partenze[i,j]=time
      v=v+1
      l=j
    } else {
      if(progressivo[i,j]==v){
        time=time+tempi[l,j]+10
        Partenze[i,j]=time
         v=v+1
         l=j
      }
    }
  }
  }
}
for(i in 1:t){
  v=2
  for (j in 1:f){
    if(progressivo[i,j]==1){
      l=j
      time=Partenze[i,j]
    }else{
        if(progressivo[i,j]==v){
          time=time+tempi[l,j]
          Arrivi[i,j]=time
          time=time+10
          v=v+1
          l=j
        }
    }
  }
 }


#----------------------------------------------------------#


t2=4000-t ######
distanze2=distanzeprov
for(i in 1:round((f/2-1)+0.1)){
  for(j in i:f+1-i){
    s=distanze2[i,j]
    distanze2[i,j]=distanze2[f+1-j,f+1-i]
    distanze2[f+1-j,f+1-i]=s
  }
}
for(i in 1:f){
  for (j in 1:i){
    distanze2[i,j]= distanze2[j,i]
  }
}
prezzog2=matrix(0,f,f)
prezzoe2=matrix(0,f,f)
prezzob2=matrix(0,f,f)
for(i in 1:f){
  for (j in 1:f){
    if(i==j){
      prezzog2[i,j]=0
      prezzoe2[i,j]=0
      prezzob2[i,j]=0
      
    } else {
      prezzog2[i,j]=4+0.02*as.numeric(unlist(distanze2[i,j]))
      prezzoe2[i,j]=5+0.04*as.numeric(unlist(distanze2[i,j]))
      prezzob2[i,j]=10+0.09*as.numeric(unlist(distanze2[i,j]))
    }
  }
}

incidenze2=matrix(0,t2,f)
for(i in 1:t2){
  for(j in 1:f){
    incidenze2[i,j]=sample(c(0,1),1) 
  }
}
progressivo2=matrix(0,t2,f)
for(i in 1:t2){
  p=1
  for(j in 1:f){
    if(incidenze2[i,j]==0){
      progressivo2[i,j]==0
    } else {
      progressivo2[i,j]=p
      p=p+1
    }
  }
}

giovani2=matrix(0,t2,f^2)
for(i in 1:t2){
  for(j in 1:f){
    pr=0
    if(progressivo2[i,j]>=1){
      v=j
      s=progressivo2[i,j]
      for(k in 1:f){
        if(progressivo2[i,k]==s+1){
          pr=pr+as.numeric(unlist(prezzog2[v,k]))
          c=f*(j-1)+k
          giovani2[i,c]=pr
          s=s+1
          v=k
        }
      }
    }
  }
}
economy2=matrix(0,t2,f^2)
for(i in 1:t2){
  for(j in 1:f){
    pr=0
    if(progressivo2[i,j]>=1){
      v=j
      s=progressivo2[i,j]
      for(k in 1:f){
        if(progressivo2[i,k]==s+1){
          pr=pr+as.numeric(unlist(prezzoe2[v,k]))
          c=f*(j-1)+k
          economy2[i,c]=pr
          s=s+1
          v=k
        }
      }
    }
  }
}
business2=matrix(0,t2,f^2)
for(i in 1:t2){
  for(j in 1:f){
    pr=0
    if(progressivo2[i,j]>=1){
      v=j
      s=progressivo2[i,j]
      for(k in 1:f){
        if(progressivo2[i,k]==s+1){
          pr=pr+as.numeric(unlist(prezzob2[v,k]))
          c=f*(j-1)+k
          business2[i,c]=pr
          s=s+1
          v=k
        }
      }
    }
  }
}
tempi2=matrix(0,f,f)
for(i in 1:f){
  for (j in 1:f){
    if(i==j){
      tempi2[i,j]=0} else{
        tempi2[i,j]=round(as.numeric(unlist(distanze2[i,j]))/4,0)
        
      }
  }
}
Partenze2=matrix(0,t2,f)
Arrivi2=matrix(0,t2,f)
for(i in 1:t2){
  m=max(progressivo2[i,])
  v=1
  time=sample(360:1100,1)#####
  for (j in 1:f){
    if(progressivo2[i,j]<m){
      if(progressivo2[i,j]==1){
        Partenze2[i,j]=time
        v=v+1
        l=j
      } else {
        if(progressivo2[i,j]==v){
          time=time+tempi2[l,j]+10
          Partenze2[i,j]=time
          v=v+1
          l=j
        }
      }
    }
  }
}
for(i in 1:t2){
  v=2
  for (j in 1:f){
    if(progressivo2[i,j]==1){
      l=j
      time=Partenze2[i,j]
    }else{
      if(progressivo2[i,j]==v){
        time=time+tempi2[l,j]
        Arrivi2[i,j]=time
        time=time+10
        v=v+1
        l=j
      }
    }
  }
}
for(i in 1:t2){
  for(j in 1:round(f/2-0.1)){
    s=Arrivi2[i,j]
    Arrivi2[i,j]=Arrivi2[i,f+1-j]
    Arrivi2[i,f+1-j]=s
    s=Partenze2[i,j]
    Partenze2[i,j]=Partenze2[i,f+1-j]
    Partenze2[i,f+1-j]=s
  }
}
Arrivitot=matrix(0,t+t2,f)
Partenzetot=matrix(0,t+t2,f)
for(i in 1:t){
  for (j in 1:f){
    Arrivitot[i,j]=Arrivi[i,j]
    Partenzetot[i,j]=Partenze[i,j]
  }
}

for(i in 1:t2){
  for(j in 1:f){
    Arrivitot[t+i,j]=Arrivi2[i,j]
    Partenzetot[t+i,j]=Partenze2[i,j]
  }
}
