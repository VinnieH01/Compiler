//Library with useful functions that match the possible signatures in the language (only doubles)

#include <stdio.h>

void print(double x) 
{
	printf("%f", x);
}

double scan() 
{
	double a;
	printf("Input> ");
    fflush(stdout);
	scanf("%lf",&a);
	return a;
}