import numpy as np

#%%

Y0=122
B0=25.884
rD=0.03
rY=0.01
rB=0.13
sigmaY=0.1
sigmaB=0.1
rho=0.2
T=1
M=10000
Y=np.zeros(M)
for i in range(M):
    x = np.random.randn(2)
    e1 = x[0]
    e2 = rho*x[0]+x[1]*np.sqrt(1-rho**2)
    
    YT = Y0/np.exp((rD-rY-(sigmaY**2)/2)*T+sigmaY*e1*np.sqrt(T))
    BT = B0/np.exp((rD-rB-(sigmaB**2)/2)*T+sigmaB*e2*np.sqrt(T))
    
    alpha = max(0,(YT-Y0)/YT) + min(1,5*((B0-BT)/BT))
    payment = 5300*(1-0.03-alpha)
    
    Y[i] = payment * np.exp(-rD*T)
      
value=np.mean(Y)
err=np.std(Y)/np.sqrt(M)
print('value = {:.2f}, err = {:.2f}'.format(value,err))

#%%

def diamond(sigmaY,sigmaB):
    Y0=122
    B0=25.884
    rD=0.03
    rY=0.01
    rB=0.13
    rho=0.2
    T=1
    M=10000
    Y=np.zeros(M)
    for i in range(M):
        x = np.random.randn(2)
        e1 = x[0]
        e2 = rho*x[0]+x[1]*np.sqrt(1-rho**2)
    
        YT = Y0/np.exp((rD-rY-(sigmaY**2)/2)*T+sigmaY*e1*np.sqrt(T))
        BT = B0/np.exp((rD-rB-(sigmaB**2)/2)*T+sigmaB*e2*np.sqrt(T))
    
        alpha = max(0,(YT-Y0)/YT) + min(1,5*((B0-BT)/BT))
        payment = 5300*(1-0.03-alpha)
        
        Y[i] = payment * np.exp(-rD*T)
        
    value=np.mean(Y)
    err=np.std(Y)/np.sqrt(M)
    return value,err

sigmaY=[0.01,0.05,0.1,0.2,0.3,0.5]
sigmaB=[0.01,0.05,0.1,0.2,0.3,0.5]
n=len(sigmaY)
value=np.zeros((n,n))
print('sigmaB ',end=' ')
for i in range(n):
    print('{:{width}.2f} '.format(sigmaB[i],width=6),end=' ')
print()
print('sigmaY')
for i in range(n):
    print('{:{width}.2f} '.format(sigmaY[i],width=6),end=' ')
    for j in range(n):
        value[i,j],err=diamond(sigmaY[i],sigmaB[j])
        print('{:{width}.0f} '.format(value[i,j],width=6),end=' ')
    print()
