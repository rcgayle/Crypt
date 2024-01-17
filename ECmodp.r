
#finds square roots of a modulo n if they exist, else returns error

mod<-function(a,n){
      return(a-floor(a/n)*n)}

powermod<-function(a,m,n){
        b=1
        while (m>0){
                if (m-floor(m/2)*2==1){
                    b=b*a-floor((b*a)/n)*n
                                    }
                else
                    {b=b}
                a<-a^2-floor(a^2/n)*n
                m<-floor(m/2)
                    }
        return(b)
                          }
                          

    
sqroot<-function(a, n){
        sqr<-c()
        x<-seq(0,n-1,1)
        sqr<-x[mod(x^2, n)==a]
        return(sqr)
                }
                
#computes the points on elliptic specified by f=x^3+Ax+B over Z_p

Ecurve<-function(A,B,p){
         if (4*A^3+27*B^2==0){
             return(data.frame('oops'))}
         else{
         f<-function(x){
               mod(powermod(x, 3,p)+mod(A*x,p)+B, p)
                              }
         E<-as.data.frame(matrix(rep(0, 2), nrow=2))
         x<-seq(0, p-1, 1)
         for (i in 1:length(x)){
              if (mod(f(x[i]),p)==0){
                      E<-cbind(E, c(x[i], 0))
                                    }
              else if (length(sqroot(f(x[i]), p))!=0){
                  for (j in 1:length(sqroot(f(x[i]),p))){
                      E<-cbind(E, c(x[i], sqroot(f(x[i]),p)[j]))
                                                        }
                                                 }
                else {E<-E}                                
                                }
          pts<-rep('', dim(E)[2])
          for (k in 1:dim(E)[2]){
                pts[k]<-paste0('P',k)
                                }
          names(E)<-pts
          return(E)}}                            
                    
                    
ECPrint<-function(A,B,p,GO){
        EC<-Ecurve(A, B, p)
        pts<-c('O')
        for (i in 2:dim(EC)[2]){
               pts<-paste0(pts,', (',EC[1,i],',', EC[2,i],')')
                               }
          if (GO==FALSE){return(paste0(
                 'Enter parameters for E'))}
          else if (4*A^3+27*B^2==0){return(
                 (paste0('The curve is singular')))
                            }
          else {
             return(paste0('The elliptic curve is {',pts,'}'))
               }
                        }
                        
Inv<-function(a,p){
         if (mod(a,p)==0){return(NA)}
         else {return(powermod(a, p-2, p))}
                  }
                  
checkpt<-function(P, A, B, p){
         E<-Ecurve(A,B,p)
         i<-1
         T<-FALSE
         if (is.numeric(E[1,1])==TRUE){
            while (i<=dim(E)[2] & T==FALSE){
                if (mod(P[1],p)==E[1, i] &  mod(P[2],p)==E[2, i])
                     {T<-TRUE}
                else {i<-i+1}
                                           }
                                  }
        else {T<-T}
        return(T)}                 

RuleI<-function(P,A,p){
          L<-mod(mod(3*P[1]^2+A, p)*Inv(2*P[2], p),p)
          X<-mod(powermod(L, 2, p)-2*P[1],p)
          Y<-mod(L*(P[1]-X)-P[2], p)
          return(c(X,Y))}
          
RuleII<-function(P,Q,p){
          L<-mod(P[2]-Q[2],p)*Inv(P[1]-Q[1],p)
          X<-mod(powermod(L, 2, p)-P[1]-Q[1],p)
          Y<-mod(L*(P[1]-X)-P[2], p)
          return(c(X,Y))}                   

                        
ECAdd<-function(P,Q,A,B,p){
          O<-c(0,0)
          if (checkpt(P,A,B,p)==FALSE | checkpt(Q, A, B,p)==FALSE)
              {return(NA)}
          else if (mod(P[1],p)==0 & mod(P[2],p)==0)
              {return(Q)}
          else if (mod(Q[1],p)==0 & mod(Q[2],p)==0)
              {return(P)}
          else if (mod(P[1],p)==mod(Q[1],p) & mod(P[2]+Q[2],p)==0)
              {return(O)}
          else if (mod(P[1],p)==mod(Q[1],p)&mod(P[2],p)==mod(Q[2],p))
              {return(RuleI(P, A, p))}
          else if (mod(P[1],p)==mod(Q[1],p)&
                        mod(P[2],p)==mod(Q[2],p)& 
                           mod(Q[2],p)==0)
               {return(O)}
          else {return(RuleII(P,Q,p))}
                       }
 
ECAddPrint<-function(P,Q,A,B,p, GO){ 
          if (GO==FALSE)
            {return(print('Enter Values for P and Q'))}     
          else if (is.numeric(ECAdd(P,Q,A,B, p))==FALSE){
            return(print('One of P or Q is not on E or E is singular.'))}
          else if (ECAdd(P,Q,A,B,p)[1]==0 &
                     ECAdd(P,Q,A,B,p)[2]==0)
             {return(print('O'))}       
          else {
             return(print(paste0('(',ECAdd(P,Q,A,B,p)[1],',',
                         ECAdd(P,Q,A,B,p)[2],')')))
                }
                               }
                               
