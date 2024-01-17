def xint(f,a,b):#finds zero of line through (a,f(a)) and (b,f(b))
	#f is entered as def f(x): float(formula in x)
    return float(a+(f(a)*(a-b))/(f(b)-f(a)))
    
def secnewt(f,I,tol):
	#secant newton's method assuming opposite signs for f(a),f(b)on I=[a,b]
	#f is entered as def f(x): float(formula in x): count is iterations required
	a=float(I[0])
	b=float(I[1])
	count=0
	x=[a,b]
	jump=b-a
	while jump>tol:
		if (f(a)>0 and f(xint(f, a,b))>0) or (f(a)<0 and f(xint(f, a,b))<0):
			a=xint(f,a,b)
			b=b
		else:
			a=a
			b=xint(f,a,b)
		count+=1
		x[1]=x[0]
		x[0]=xint(f,a,b)
		jump=abs(x[1]-x[0])
	return xint(f,a,b)

#def allzeros(f, a,b, tol, n):
    #combines above functions to find and print allzeros on the interval [a,b]
	#for i in range(0, len(sectzeros(f,a,b,n))):
		#print secnewt(f,sectzeros(f,a,b,n)[i], tol)

def allzeros(f, a,b, tol, n):
    #combines above to find all zeros
	zeros=[]
	for i in range(0, len(sectzeros(f,a,b,n))):
		zeros.append(secnewt(f,sectzeros(f,a,b,n)[i], tol))
	return zeros
		
def sectzeros(f,a,b, n):
	#divides [a,b] into n subintervals, returns list of subintervals on which f      changes sign
	y=[]
	incr=(b-a)/float(n)
	eps=incr/float(5)
	x=[a+i*incr for i in range(0,n+1)]
	if abs(f(a))<=eps/float(10):
		x[0]=a-eps
	if abs(f(b))<=eps/float(10):
		x[n]=b+eps
	for i in range(1, len(x)-1):
		if abs(f(x[i]))<=eps/float(10):
		     x[i]+=eps
	for i in range(0, n):
		if (f(x[i])>0 and f(x[i+1])<0) or (f(x[i])<0 and f(x[i+1])>0):
			y.append([round(x[i],4), round(x[i+1],4)])
	return y
