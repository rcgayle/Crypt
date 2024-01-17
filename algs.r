mod<-function(a,n){
      return(a-floor(a/n)*n)}

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
                