
mod<-function(a,n){
      return(a-floor(a/n)*n)}
      
#with p's coeff listed in descending degree order, divides p by x-a

syndiv<-function(a,p){
        Coefs<-rep(0, length(p))
        Coefs[1]<-p[1]
        for (i in 2:length(p)){
            Coefs[i]<-a*Coefs[i-1]+p[i]}
        return(Coefs)}
        
        
#with p as above, produces coefs of p(-x)  
        
pminx<-function(p){
          n<-length(p)
          q<-rep(0, n)
          for (i in 1:length(p))
              {
              if (mod(i, 2)==1)
                   {q[i] <- -p[i]}
              else 
                    {q[i]<-p[i]}
              }
         return(q)
                     }
                     
#find upper-bound for pos. roots of p
              
UB<-function(p, maxiter){
        if (p[1]<0)
            {p<--p}
        else {p<-p}
        i<-1
        GO<-TRUE
        while (GO==TRUE & i<=maxiter)
             {
             Coefs<-syndiv(i, p)
             if (length(Coefs[Coefs<=0])>0)
                   {
                    GO<-TRUE
                    i<-i+1
                    }
             else  {GO<-FALSE}
             }
        return(i)
                         }   
                         
#for elliptic curve in weierstrass form (y^2=x^3+ax+b as c(a,b)) returns 
# discriminant


disc<-function(ep){
        return(4*ep[1]^3+27*ep[2]^2)} 
        
#for q, coefs of elliptic curve in Weierstrass form, descending order
# produces evaluatable function

PF<-function(q, x){
       return(g<-function(x){
                q[1]*x^3+q[3]*x+q[4]})}
                
PF2<-function(q, x){
       return(g<-function(x){
                sqrt(q[1]*x^3+q[3]*x+q[4])})}
                
PFD<-function(q,x){
       return(g<-function(x){
                3*x^2+q[3]+q[3]})}
                

                

#for EC in Weierstrass form specified as coeffs (x^3+4x+3<-> c(1,0,4,3))
#returns a list of EC's real zeros. Requires allzerosB.py using
#R library 'reticulate'.

ZEROS<-function(q){
    if(length(allzeros(PF(q), -UB(pminx(q),2000), 
                  UB(q,2000), 0.0001,as.integer(20)))!=0){
    return(round(allzeros(PF(q), -UB(pminx(q),2000), 
                  UB(q,2000), 0.0001,as.integer(20)), 5))
                          }
    else {return(c())}
                   }  
    
#renders a plot of elliptic curve in Weierstrass form as specified in ZEROS (above) 

   

ECPlot<-function(q){
        Z<-ZEROS(q)
        g<-PF(q)
        h<-PF2(q)
        k<-PFD(q)
        qp<-c(3, 0, q[3])
        if (length(Z)==1){
            ZD<-allzeros(k, -UB(pminx(qp), 200), UB(qp, 200), 
                   0.0001, as.integer(20))
            if (length(ZD)!=0){
                 ZM<-ZD[2]-Z}
            else {ZM<-8*abs(Z)}
            x<-seq(Z, ZM, 0.0001)
            x<-x[g(x)>=0]
                        }
        else {
            x1<-seq(Z[1], Z[2], 0.0001)
            x1<-x1[g(x1)>=0]
            x2<-seq(Z[3], Z[3]+1.5*(Z[2]-Z[1]), 0.0001)
            x2<-x2[g(x2)>=0]}
        if (length(Z)==1){
           M<-round(max(h(x)))
           I1<-round(Z-1)
           I2<-round(ZM)
           I<-seq(I1, I2, 1)
           J<-seq(-M, M, 1)
           P<-{ 
           plot(x, h(x), type='l', col='red', axes=F, 
                   xlim=c(I1,I2),ylim=c(-M,M),xlab='x',ylab='y',lwd=2)
           axis(1, at=I, labels=c(I[1], rep('', length(I)-2), I[length(I)]),
                      pos=0)
           axis(2, at=J, labels=c(J[1], rep('', length(J)-2), J[length(J)]),
                      pos=0)
           lines(x,-h(x), col='red', lwd=2)
                   }
                          }
         else {
            M<-round(max(max(h(x1)), max(h(x2))))
            I1<-round(Z[1]-2)
            I2<-round(Z[3]+1.5*(Z[2]-Z[1]))
            I<-seq(I1, I2, 1)
            J<-seq(-M, M, 1)
            P<-{ 
            plot(x1, h(x1), type='l', col='red', axes=F, xlim=c(I1, I2),
                  ylim=c(-M,M), xlab='x', ylab='y', lwd=2)
            axis(1, at=I, labels=c(I[1], rep('', length(I)-2), I[length(I)]),
                      pos=0)
            axis(2, at=J, labels=c(J[1], rep('', length(J)-2), J[length(J)]),
                      pos=0)
            lines(x1,-h(x1), col='red', lwd=2)
            lines(x2,h(x2), col='red', lwd=2)
            lines(x2,-h(x2), col='red', lwd=2)
                    }
                }
            return(P)
                       }
                       

                       
#check that pt P is on curve q within vertical distance eps

OnCurve<-function(P, q, eps){
            g<-PF2(q)
            x1<-as.numeric(P[1])
            y1<-as.numeric(P[2])
            if (abs(abs(y1)-g(x1))<eps){
                return(TRUE)}
            else {return(FALSE)}}
            
Vert<-function(P,Q,eps){
            x1<-as.numeric(P[1])
            x2<-as.numeric(Q[1])
            y1<-as.numeric(P[2])
            y2<-as.numeric(Q[2])
            if (abs(x1-x2)<eps & abs(y1+y2)<eps){
                return(TRUE)}
            else {return(FALSE)}}
            
#add P=c(x1,y1), Q=c(x2, y2) on elliptic curve q=c(1, 0, A, B)

            
AddPts<-function(P, Q, q, eps){
            g<-PF(q)
            A<-q[3]
            x1<-as.numeric(P[1])
            x2<-as.numeric(Q[1])
            y1<-as.numeric(P[2])
            y2<-as.numeric(Q[2])
            if (OnCurve(P, q, eps)==F | OnCurve(Q, q, eps)==F)
              {return('Either P or Q is not on the curve')}
            else if (Vert(P,Q,eps)==T)
              {return('The sum is O, the point at infinity')}
            else {
               if (abs(x1-x2)<eps & abs(y1-y2)<eps){
                   lambda<-(3*x1^2+A)/(2*y1)}
               else {
                   lambda<-(y2-y1)/(x2-x1)}
            x3<-lambda^2-x1-x2
            y3<-lambda*(x1-x3)-y1}
            return(c(x3, y3))}
            
AddLine<-function(P,Q, q){
            eps<-max((par('usr')[4]-par('usr')[3])/50, 
                          (par('usr')[2]-par('usr')[1])/50)    
            g<-PF2(q)
            A<-q[3]
            x1<-as.numeric(P[1])
            x2<-as.numeric(Q[1])
            y1<-as.numeric(P[2])
            y2<-as.numeric(Q[2])
            if (OnCurve(P, q, eps)==F | OnCurve(Q, q, eps)==F)
              {text((par('usr')[2]+par('usr')[1])/2, par('usr')[4]-3,
                  'Either P or Q is not on the curve!', cex=2, font=2,
                         col='red')}
            else if (Vert(P,Q, eps)==T)
              {par('usr')[3]->ymin
              par('usr')[4]->ymax
              Y<-seq(ymin, ymax, 0.001)
              X<-rep(x1, length(Y))
              lines(X,Y, col='magenta', lwd=1.5)
              if (y1>0){Y1<-y1+2.5} else{Y1<-y1-2.5}
              if (y2>0){Y2<-y2+2.5} else{Y2<-y2-2.5}
              text(x1-0.5, Y1, 'P', cex=1.25, font=2, col='blue')
              text(x2-0.5, Y2, 'Q', cex=1.25, font=2, col='blue')
              text(x1+1, 1.75*Y1, 'P+Q=O', cex=1.25, font=2, col='darkgreen')}
            else{
              x3<-AddPts(P, Q, q, eps)[1]
              y3<-AddPts(P, Q, q, eps)[2]
              segments(x1,y1, x2, y2,lwd=1.5, col='magenta')
              segments(x3,-y3, x2, y2,lwd=1.5, col='magenta')
              segments(x3, y3, x3, -y3, lwd=1.5, lty=2, col='magenta')
              points(x3, -y3, pch=19, col='orange')
              points(x3, y3, pch=19, col='darkgreen')
              if (y1>0){Y1<-y1+2.5} else{Y1<-y1-2.5}
              if (y2>0){Y2<-y2+2.5} else{Y2<-y2-2.5}
              if (y3>0){Y3<-y3+2.5} else{Y3<-y3-2.5}
              if (abs(x1-x2)<eps & abs(y2-y1)<eps){
                  text(x1, Y1, 'P=Q', cex=1.25, font=2, col='blue')
                  text(x3, Y3, 'P+Q=2P=2Q', cex=1.25, font=2, col='darkgreen')}
               else {
                  {text(x1, Y1, 'P', cex=1.25, font=2, col='blue')
                  text(x2, Y2, 'Q', cex=1.25, font=2, col='blue')
                  text(x3, Y3, 'P+Q', cex=1.25, font=2, col='darkgreen')}
                    }             
                       }
                       }
              
                       
GetPlot<-function(){
            x=c()
            y=c()
            plot(x, y, axes=F, xlim=c(-10,10), 
                  ylim=c(-10,10), xlab='',ylab='')
            text(0, 0, expression(paste(
                 'Select Parameters A and B')), cex=2, col='red')
                 }          
             
            
     
         



        
        
        
        
        
        
        
        
         

                              
                                        
          
         