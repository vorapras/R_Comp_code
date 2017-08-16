clear all
close all

S=100;
%finite difference schemes for heat equation u_t=u_{xx}.
n=500; %number of x values at which u is computed
x=linspace(-4,4,n+2); %range of x values
K=S./exp(-x(end:-1:1));
T=1;
m=1000; %number of time discretizations
r=0.0;d=0.0;%risk-free interest rate - divident yield
rd=r-d; 
sigma=0.2; s2=sigma^2; %volatility
tau = 0.5*T*s2;
t=linspace(0,tau,m+2);
a = -0.5*(-1+(2*rd/s2) );
b = -0.25*((-1+(2*rd/s2) )^2) -(2*r/s2);
dt=t(2)-t(1); %delta t
dx=x(2)-x(1); %delta x
lambda=0.5*dt/(dx*dx);

u=zeros(m+2,n+2);
u(1,:)=exp(0.5*(-1+(2*rd/s2))*x).*max(exp(x)-1,0); %initial condition call
u(:,1) = 0;
u(:,n+2) = 0;

%Explicit Scheme
A=(1-2*lambda)*eye(n,n); %initialize A matrix
A(1,2)=lambda;
A(n-1,n)=lambda;
for i=2:n-1
    A(i,i-1)=lambda;
    A(i,i+1)=lambda;
end
for j=1:m+1
    u(j+1,2:n+1)=(A*u(j,2:n+1)')';
end

figure(1);plot(x,u(m+2,:))

%Implicit Scheme
A=(1+2*lambda)*eye(n,n); %initialize A matrix
A(1,2)=-lambda;
A(n-1,n)=-lambda;
for i=2:n-1
    A(i,i-1)=-lambda;
    A(i,i+1)=-lambda;
end
Ainv=(A\eye(n));
for j=1:m+1
    u(j+1,2:n+1)=(Ainv*u(j,2:n+1)')';
end

figure(1);plot(x,u(m+2,:))


%CN Scheme
A=(1+lambda)*eye(n,n); %initialize A matrix
A(1,2)=-lambda/2;
A(n-1,n)=-lambda/2;
for i=2:n-1
    A(i,i-1)=-lambda/2;
    A(i,i+1)=-lambda/2;
end
%do the same for B
B=(1-lambda)*eye(n,n); 
B(1,2)=lambda;
B(n-1,n)=lambda;
for i=2:n-1
    B(i,i-1)=lambda/2;
    B(i,i+1)=lambda/2;
end
C=(A\B);
for j=1:m+1
    u(j+1,2:n+1)=(C*u(j,2:n+1)')';
end

figure(1);plot(x,u(m+2,:))

%Calculate BS formula
sst = sigma*sqrt(tau);
d1 = (log(S./K) + rd + tau )/sst;
d2 = d1 - sst;
AnPrice = ( S*exp(-d*T)*normcdf(d1) ) - (K*exp(-r*T).*normcdf(d2)  );

V = exp(a*x+b*tau).*u(m+2,:).*K;
inds=round(n/2)+1:n-100;
figure(2);plot(x(inds),V(inds),x(inds),AnPrice(inds))