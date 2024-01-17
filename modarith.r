#reduces a (mod n)

mod<-function(a,n){
      return(a-floor(a/n)*n)}

#finds gcd

gcd<-function(a,b){
       if (a==0 | b==0)
            {return(max(abs(a), abs(b)))}
       else {
          a<-abs(a)
          b<-abs(b) 
          while (b>0){
              r<-mod(a,b)      
              a<-b
              b<-r}
             }
        return(a)}
        
#finds elements a of x with gcd(a,n)=1
        
relprime<- function(x, n){
        s<-rep(0, length(x))
        for (i in 1:length(x)){
            if (gcd(x[i], n)==1){
                 s[i]<-x[i]}
                           }
     return(s[s!=0])}
     
RPList<-function(AS,x,n){
            y<-seq(1, n-1,1)
            out<-c()
            if (AS=='a'){out<-relprime(y,n)}
            else  {out<-relprime(unique(x),n)}
            return(setlist(out))
                         }
                                  
     
notrelprime<- function(x, n){
        s<-rep(0, length(x))
        for (i in 1:length(x)){
            if (gcd(x[i], n)!=1){
                 s[i]<-x[i]}
                           }
     return(s[s!=0])}
     
#find if gcd(a,n)=1, finds <a> in (Z/nZ)^* and ord(a) 
     
cycgr<-function(a,n){
        if (gcd(a,n)!=1)
           print(paste0(
    'Since ',a,' and ',n,' are not co-prime, ', a, ' generates no subgroup.'))
        else {
        H<-c(1)
        i<-1
        while (powermod(a, i, n)!=1 & i<n){
             H<-c(H,powermod(a, i, n))
             i<-i+1
                                             }                    
        out<-list(sort(H), length(H))
        names(out)<-c('group', 'order')
        return(out)}
                      }
                      
setlist<-function(x){
        if (length(x)==0){
            out<-paste0('The set is empty.')}
        else if (length(x)==1){
            out<-paste0('{',x[1],'}')}
        else{
          out<-paste0('{',x[1])
          for (i in 2:length(x)){
          out<-paste(out, x[i], sep=',')
                             }
          out<-paste(out,'}',sep='')}
        return(out)}                      

GRPPRINT<-function(a,n){
            if (EEA(a,n)[1]!=1 & a!=1){
                return(cycgr(a,n))}
            else {
             print(paste0('H: ', setlist(cycgr(a,n)$group))) 
             print(paste0('ord(a)= ',cycgr(a,n)$order,'.'))                     
                  }
                        }
                 
                  
#finds square roots of a modulo n if they exist, else returns error
     
sqroot<-function(a, n){
        sqr<-c()
        x<-seq(1,n-1,1)
        sqr<-x[mod(x^2, n)==a]
        return(sqr)
                }

SQRPRINT<-function(a,n){
            if (length(sqroot(a,n))==0){
                 print(paste0(a,' has no square roots mod(',n,')'))}
            else 
               {print(paste0('Square Roots: ', setlist(sqroot(a,n))))}
                       }               
#finds the gcd amongst elements of x
        
GCD<-function(x){
        g<-abs(x[1])
        for (i in 2:length(x)){
             g<-gcd(g, x[i])
             i<-i+1}
        return(g)}

#applies the extended Euclidean Algorithm to a, b and returns gcd, Bezout coefs          
             
EEA<-function(a,b){
        s<-0
        t<-1
        os<-1
        ot<-0
        r<-b
        olr<-a
        q<-1
        while (r!=0){
                q<-floor(olr/r)
                stemp<-s
                s<-os-q*s
                os<-stemp
                ttemp<-t
                t<-ot-q*t
                ot<-ttemp
                rtemp<-r
                r<-olr-q*r
                olr<-rtemp}
        if (os<0){
                os<-os+b/olr
                ot<-ot-a/olr
                  }
        return(c(olr, os, ot))
                    }
                    
EEAPRINT<-function(a,b){
   print(paste0('The gcd of ',a,' and ',
                  b,' is ', EEA(a,b)[1]))
    print(paste0(EEA(a,b)[1],'=',EEA(a,b)[2],'*',a,'+('
                  , EEA(a,b)[3],')*',b))
                  } 
                  
DEKEYPRINT<-function(p, q, e){
   phi<-(p-1)*(q-1)
   k<-phi/gcd(p-1, q-1)
   if (gcd(e, phi)!=1){
       print(paste0('gcd(e,phi(N)) is not 1'))}
   else{
       print(paste0('d = ',EEA(e,k)[2]))}
                  }
                    
      
MODPRINT<-function(a,n){
   print(paste0(mod(a,n)))}
                    
 #computes  and reduces a^m (mod n)
                    
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
                    
PWRMODPRINT<-function(a,m, n){
   print(paste0(powermod(a,m,n)))}   
   
CRTtable<-function(n){
   df<-data.frame(M<-matrix(rep(0, 2*n), nrow=2))
        row.names(M)<-c('Values','Moduli')
        L<-rep('', n)
        for (i in 1:n){L[i]<-paste0('EQ',i)}
        colnames(M)<-L
   return(M)}
   
PWRP<-function(x){
          m<-1
          n<-length(x)
          for (i in 1:n){
                j<-i+1
                while (j<=n){
                    if (gcd(x[i], x[j])>1){
                            m<-max(m, gcd(x[i], x[j]))
                                       }
                    else {}
                    j<-j+1
                            }
                         }
           return(m==1)
                   }
   
CRTsolve<-function(M){
          if (PWRP(as.numeric(M[2,]))==FALSE){
                return(print(
                "The pairwise GCD of the moduli isn't 1 so there is no solution."))
                  }
          else {
                N<-1
                n<-dim(M)[2]
                for (i in 1:n){
                        N<-N*abs(M[2,i])}
                NL<-rep(0, n)
                for (i in 1:n){
                   NL[i]<-N/abs(M[2,i])}
                ML<-rep(0, n)
                for (i in 1:n){
                   ML[i]<-EEA(NL[i], M[2, i])[2]}
                T<-rep(0, n)
                for (i in 1:n){
                   T[i]<-M[1,i]*ML[i]*NL[i]}
                x<-mod(sum(T),N)
                }
                return(c(x,N))
                     }
                     
CRTPRINT<-function(M){
          if (PWRP(as.numeric(M[2,]))==FALSE){
                return(print(
                "The pairwise GCD of the moduli isn't 1 so there is no solution."))
                  }
          else {
            print(paste0(
           'The solution is ', CRTsolve(M)[1],' mod(',CRTsolve(M)[2],')'
                            ))
                }
                
                       }
                       
#implements Pollards algorithm to attempt to find factors of n (might fail
# even for composite n)
                            
PLLRD<-function(n){
             v<-seq(1, n-1,1)
             a<-sample(v,1)
             y<-sample(v,1)
             z<-y
             i<-1
             d<-1
             x<-1
             f<-function(t){t^2+a}
             while(d==1 & i<max(log(n),200) & x!=0)
                   {
                   y<-mod(f(y),n)
                   z<-mod(f(mod(f(z),n)),n)
                   x<-mod(y-z,n)
                   if (x!=0)
                      {d<-gcd(x,n)}
                   else {d<-d}
                   i<-i+1
                   }
             print(paste0(n,'=',d,'*',n/d))
             #return(c(d, n/d))
             }                            
 
              
              
