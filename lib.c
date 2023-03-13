#include <stdio.h>

void print(double x) 
{
	printf("%f\n", x);
}

double scan() 
{
	double a;
	printf("Input> ");
    fflush(stdout);
	scanf("%lf",&a);
	return a;
}