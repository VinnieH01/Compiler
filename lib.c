#include <stdio.h>
#include <stdlib.h>

static double* ptr;

void store(double x) 
{
	if (ptr != NULL) free(ptr);
	ptr = malloc(sizeof(double));
	(*ptr) = x;
}

double load() 
{
	return *ptr;
}

void printf64(double x) 
{
	printf("%f\n", x);
}

void printi32(int x) 
{
	printf("%d\n", x);
}

double scanf64() 
{
	double a;
	printf("Input> ");
    fflush(stdout);
	scanf("%lf",&a);
	return a;
}

int scani32() 
{
	int a;
	printf("Input> ");
    fflush(stdout);
	scanf("%d",&a);
	return a;
}