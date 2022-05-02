#include "stdafx.h"
#include "iostream" 
#include "conio.h"
#include "math.h"	
#include "fstream"  
#include "stdlib.h" 
#include "time.h"


#define MAXLINE 1000 
#define SHORTLINE 20
#define MAXSDVIG 5
#define VIDFUNC 3
#define KOLOPUT 20  //Numner of Mote-Carlo experiments
#define KOLMONTKARL 100//Number of regression tests
#define CONSTMK 1   //Constant of random chose(-0,5;0,5)*CONSTMK

using namespace std; 

long double srdis (long double *s , int n ,short j)
{
	long double sum=0;
	for (int i = 0; i < n; i++)
	{
		sum+=s[i+j];
	} 
	sum/=n;
	return sum;  
}

long double dis(long double *a , int n , short j)
{
	long double sum=0;
	for (int i = 0; i < n; i++)
	{
		sum+=a[i+j]*a[i+j];
	} 
	sum/=n;  
	sum-=(srdis(a,n,j))*(srdis(a,n,j));
	return sum;
}

long double covar ( long double *a,long double *b ,int n,short j)
{
	long double sum=0;
	for (int i = 0; i < n; i++)
	{
		sum+=b[i+j]*a[i];  
	} 
	sum/=n;
	sum-=(srdis(a,n,0))*(srdis(b,n,j));
	return sum;
}

long double korel (long double *a,long double *b ,int n ,short j)
{
	long double sum;
	sum=covar(a,b,n,j)/(sqrt(dis(a,n,0)*dis(b,n,j))); 
	return sum;
}
long double poiskb2 (long double *a,long double *b ,int n,short j)
{
	long double b2;
	b2=covar(a,b,n,j)/dis(a,n,0);
	return b2;
}
long double poiskb1 (long double *a,long double *b ,int n,short j)  
{
	long double b1;
	b1=srdis(b,n,j)-poiskb2(a,b,n,j)*srdis(a,n,0);
	return b1;
}

long double sravbestkor(long double *a,long double b)
{
	long double ret,c;
	c=*a;
	ret=0;
	if (c<b)
	{
		*a=b;
		ret=1;
	}
	return ret;
}
long double stringtolongdouble(char *a)
{
	long double sum=0,k=10;
	int i=0,sign=1;
	if (a[i]=='-')
	{
		i++;
		sign=-1;
	}
	else if (a[i]=='+')
		i++;
	while (((a[i])!=',')&&((a[i])!=0))
	{
		sum=sum*10+(a[i]-'0');
		i++;
	}
	if (a[i]==',')
	{
		i++;
		while (a[i]!=0)
		{
			sum=sum+(a[i]-'0')/k;
			k=k*10;
			i++;
		}
	}
return sum*sign;
}

long double srkvad(long double *a,long double *b,long double bb1, long double bb2,int n, short j, short con)
{
	long double ret=0,yx;
	if (con==1)
	{
		for (int i=0;i<n;i++)
		{
			yx=bb1+bb2*a[i];
			ret+=(b[i+j]-yx)*(b[i+j]-yx);
		}
	}
	if (con==2)
	{
		for (int i=0;i<n;i++)
		{
			yx=bb1+bb2*a[i]*a[i];
			ret+=(b[i+j]-yx)*(b[i+j]-yx);
		}
	}
	if (con==3)
	{
		for (int i=0;i<n;i++)
		{
			yx=bb1+bb2*log(a[i]);
			ret+=(b[i+j]-yx)*(b[i+j]-yx);
		}
	}
	ret=sqrt(ret/n);
	return ret;
}

long double elastich(long double bb2,long double *w, long double *q,int n,short j)
{
	long double ret=0;
	ret=bb2*srdis(w,n,0)/srdis(q,n,j);
	return ret;	
}

long double aprox(long double *a,long double *b,long double bb1, long double bb2,int n, short j, short con)
{
	return srkvad(a,b,bb1,bb2,n,j,con)/srdis(b,n,j)*100;
}

void montekarlo(long double b1,long double b2,long double *b1n,long double *b2n)
{
	int q;
	long double t=0;
long double zna4Xnew[KOLOPUT],  zna4Ynew[KOLOPUT], *ykzna4Xnew,    *ykzna4Ynew, u;
	for (int i=0;i<KOLOPUT;i++)
	{
		zna4Xnew[i]=t;
		u=((1.0*rand()/RAND_MAX)-0.5)*CONSTMK; 
		/*cout<<"\n"<<"u="<<u<<"\n";*/
		zna4Ynew[i]=b1+b2*t+u;
		t++;
		q=i+1;
	}
	ykzna4Xnew=zna4Xnew;
	ykzna4Ynew=zna4Ynew;
	*b2n=poiskb2(ykzna4Xnew,ykzna4Ynew,q,0);
	*b1n=poiskb1(ykzna4Xnew,ykzna4Ynew,q,0);
}

void vuvodfunc(long double b1,long double b2,short n)
{
	cout<<"y="<<b1<<"+"<<b2<<"*";
	if (n==1)
		cout<<"X\n";
	else if (n==2)
			cout<<"X*X\n";
		else if (n==3)
			cout<<"Ln(X)\n";
	cout<<"\n";
	return ;
}

void vuvodmatr(long double *mas,short a,short b)
{
	for (int j=0;j<b+1;j++)
	{   
		cout<<j<<"-aya nedelya sdviga\n";
		for (int i=0;i<a;i++)
			cout<<mas[j+i*(b+1)]<<"\t";
		cout<<"\n";
	}
	return ; 
}

void revers(char *s)
{
	int i=0,j=0;
	int a[SHORTLINE];
	if (s[i]=='-')
	{
		a[i]='-';
		i++;
	}
	else if (s[i]=='+')
		{
		a[i]='+';
		i++;
		}
	do
	{
		a[i]=s[i];
		i++;
	}
	while (s[i]!=0);
	if ((a[j]=='+')||(a[j]=='-'))
	{
		s[j]=a[j];
		j++;
		for(j;j<i;j++)
			s[j]=a[i-j];
	}
	else 
		for(j;j<i;j++)
		s[j]=a[i-j-1];
}

void ldtochar (long double a,char *s)
{	
	int i=0,k,count=0;
	if (a<0)
	{
		s[i++]='-';
		a=-a;
	}
	k=(int)(1000*(a));
	do 
	{
		if (count==3)
			s[i++]='.';
		s[i++]=k%10+'0';
		k/=10;
		count++;
	}
	while (k>0);
	s[i]=0;
	revers(&s[0]);
	return ;
}

int _tmain(int argc, _TCHAR* argv[])
{

	short ned, con;//the weeks shifts con=1,2,3
	long double a[VIDFUNC][MAXSDVIG+1],bestkor=0,bb1,bb2,elast;
	int n;//кількість спостережень
	long double e[MAXLINE],logo[MAXLINE],q[MAXLINE],w[MAXLINE]
	char s[SHORTLINE];//input numbers

////Input the number of observations

	cout << "Input number period\n";
	cin>>n;
	ifstream inX("Prices.txt");//File of prices
	inX>>s;
	//read the data Х(Х is earlier then Y from the start)
	w[0]=stringtolongdouble(&s[0]);   
	
	for (int i=1;i<n+MAXSDVIG+1;i++)
		{
			inX>>s;
			w[i]=stringtolongdouble(&s[0]);  
			   //make averade Х
                      //( previos + current week)/2
			w[i-1]=(w[i]+w[i-1])/2;               
			e[i-1]=w[i-1]*w[i-1];
			logo[i-1]=log(w[i-1]);
		}
		inX.close(); // 

		////Read sales file Y

		ifstream inY("sales.txt");// read salse data
		for (int i=0;i<n+MAXSDVIG;i++)
		{
			inY>>s;
			q[i]=stringtolongdouble(&s[0]);
		}
		inY.close(); // eof
		
           //iterate through weeks
		for (short j=0;j<MAXSDVIG+1;j++)    		{
		////corellation calculs

			a[0][j]=korel(&w[0],&q[0],n,j);
			a[1][j]=korel(&e[0],&q[0],n,j);
			a[2][j]=korel(&logo[0],&q[0],n,j);
	
		//// find the best correclation coefficient

			if (sravbestkor(&bestkor,a[0][j]))
			{
				bb1=poiskb1(&w[0],&q[0],n,j);
				bb2=poiskb2(&w[0],&q[0],n,j);
				con=1;
				ned=j;
				elast=elastich(bb2,&w[0],&q[0],n,ned);
			}
			if (sravbestkor(&bestkor,a[1][j]))
			{
				bb1=poiskb1(&e[0],&q[0],n,j);
				bb2=poiskb2(&e[0],&q[0],n,j);
				con=2;
				ned=j;
				elast=elastich(bb2,&e[0],&q[0],n,ned);
			}
			if (sravbestkor(&bestkor,a[2][j]))
			{
				bb1=poiskb1(&logo[0],&q[0],n,j);
				bb2=poiskb2(&logo[0],&q[0],n,j);
				con=3;
				ned=j;
				elast=elastich(bb2,&logo[0],&q[0],n,ned);
			}
	}

	//output
	//elasticity
	cout<<"elasticity="<<elast<<"\n";
	//mean sqr error
	cout<<"standfalse="<<srkvad(&w[0],&q[0],bb1,bb2,n,ned,con)<<"\n";
	//approximation coeficient
	cout<<"aprox="<<aprox(&w[0],&q[0],bb1,bb2,n,ned,con)<<"\n";
	//correlation matrix output
	cout<<"matrix kor\n";
	cout<<"y=b+ax\t"<<"\t"<<"y=b+a*x*x\t"<<"y=b+a*ln(x)\n"<<"\n";
	//thebest correlation cofficient
        vuvodmatr(&a[0][0],VIDFUNC,MAXSDVIG);	
        
	cout<<"\n"<<bestkor<<"=best korrelation coefficient on "<<ned<<" week \n"<<"\n";
	vuvodfunc(bb1,bb2,con);//best corellation in work

	////Monte-Carlo implementation

	long double monteb1[KOLMONTKARL],monteb2[KOLMONTKARL],b1n=0,b2n=0;

	srand((unsigned)time(NULL));// initialize rand()
	for (int i=0;i<KOLMONTKARL;i++)
	{
		montekarlo(bb1,bb2,&b1n,&b2n);
		monteb1[i]=b1n;
		monteb2[i]=b2n;
	}
	cout<<"\n";
	b1n=srdis(monteb1,KOLMONTKARL,0);
	b2n=srdis(monteb2,KOLMONTKARL,0);
	cout<<"the equation after "<<KOLMONTKARL<<" tests of Monte Carlo\n";
	vuvodfunc(b1n,b2n,con);
	cout<<"real equation\n";
	vuvodfunc(bb1,bb2,con);
	cout<<"dispertion b1="<<dis(&monteb1[0],KOLMONTKARL,0)<<"\n";
	cout<<"dispertion b2="<<dis(&monteb2[0],KOLMONTKARL,0)<<"\n";
	cout<<"error b1="<<dis(&monteb1[0],KOLMONTKARL,0)/bb1<<"\n";
	cout<<"error b2="<<dis(&monteb2[0],KOLMONTKARL,0)/bb2<<"\n";
	
////затримка
    _getch();
	return 0;
}

Додаток 1.2
#include "stdafx.h"
#include "iostream" //
#include "conio.h"
#include "math.h"	//
#include "fstream"  //
#include "stdlib.h" //rand()
#include "time.h"


#define MAXT 2
#define a1 1.32  //y=a1+xb1-  linear function of prices1 функ реакции выробнк.
#define b1 0.026  //y=a1+xb1- linear function of prices2 функ реакции выробнк.
#define c1 -0,3  //inflya=arcsin(c1+c2*t) 
#define c2 0.01  //inflya=arcsin(c1+c2*t) 
#define a2 0,63  //y=a2+b2x  prediction of sales
#define b2 0.35  //y=a2+b2x prediction of sales 

using namespace std; 

double firstpart(double x,double y)
{
	double ret,c3;
	c3=c1+c2*x;
	ret=-(a1+b1*asin(c3))*y;
	return ret;
}

double secondpart(double x,double y)
{
	double ret,c3;
	c3=c1+c2*x;
	ret=a2*(a1+b1*asin(c3))*y/(asin(c3));
	ret+=b2*y*(a1+b1*asin(c3));
	return ret;
}
int _tmain(int argc, _TCHAR* argv[])
{
	double t=0,k, dt =0.1,k1[2],k2[2],k3[2],k4[2];  
	double shock[2],lyab=10;
	shock[0]=1;
	cout<<shock[0]<<"\n";
	shock[1]=1;
	cout<<shock[1]<<"\n";
    while(t < MAXT)
    {
		k1[0] = firstpart(t , shock[0]);
        k2[0] = firstpart(t+0.5*dt , shock[0] + 0.5*k1[0]);
        k3[0] = firstpart(t+0.5*dt , shock[0] + 0.5*k2[0]);
        k4[0] = firstpart(t+dt , shock[0] + k3[0]);
		k1[1] = secondpart(t , shock[1]);
        k2[1] = secondpart(t+0.5*dt , shock[1] + 0.5*k1[1]);
        k3[1] = secondpart(t+0.5*dt , shock[1] + 0.5*k2[1]);
        k4[1] = secondpart(t+dt , shock[1] + k3[1]);
		shock[0]=shock[0]+(k1[0]+2*k2[0]+2*k3[0]+k4[0])/6;
		shock[1]=shock[1]+(k1[1]+2*k2[1]+2*k3[1]+k4[1])/6;
		//lyab=-shock[1]/shock[0];
		k=shock[1]+shock[0];
		shock[1]=k;
		shock[0]=k;
        t += dt;       
		cout<<"t="<<t<<"\n"<<"lyab="<<k<<"\n";
    }
	_getch();
	return 0;
}




