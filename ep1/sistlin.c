#include <stdio.h>
#include <stdlib.h>

#define abs(n) ((n)<0?-(n):(n))

#define NMAX 100
#define EPSILON 0.0001

int lucol(int n, double A[][NMAX], int p[]) {

}

int lurow(int n, double A[][NMAX], int p[]) {
	int i, j, temp, max;

	for(k=0;k<n;++k) {
		max = k;
		for(i=k+1;i<n;++i) 
			if(abs(A[i][k]) > (A[max][k]))
				max = i;
		if(abs(A[max][k] <= EPSILON))
			return -1;
		if(max != k)
			for(j=0;j<n;++j) {
				temp = A[max][j];
				A[max][j] = A[k][j];
				A[k][j] = temp;
			}
		p[k] = max;
		for(i=k+1;i<n;++i) {
			A[i][k] /= A[k][k];
			for(j=k+1;j<n;++j)
				A[i][j] = A[i][j] - A[i][k]*A[k][j];
		}
	}
}

int sscol(int n, double A[][NMAX], int p[], double b[]) {
	int i, j, temp;

	for(i=0;i<n;++i) {
		temp = b[i];
		b[i] = b[p[i]];
		b[p[i]] = temp;
	}

	for(i=0;i<n;++i)
		for(j=0;j<n;++j)
			b[i] = b[i] - b[j]*A[i][j];

	for(j=n;j>-1;--j) {
		if(A[j][j] == 0)
			return -1;
		b[j] /= a[j][j];
		for(i=0;i<j-1;++i)
			b[i] = b[i] - b[j]*A[i][j];
	}

	return 0;
}

int ssrow(int n, double A[][NMAX], int p[], double b[]) {
	int i, j, temp;

	for(i=0;i<n;++i) {
		temp = b[i];
		b[i] = b[p[i]];
		b[p[i]] = temp;
	}

	for(i=0;i<n;++i)
		for(j=0;j<n;++j)
			b[i] = b[i] - b[j]*A[i][j];

	for(i=n-1;i>-1;--i) {
		for(j=i+1;j<n;++j)
			b[i] = b[i] - b[j]*A[i][j];
		if(A[i][i] == 0)
			return -1;
		b[i] /= a[i][i];
	}

	return 0;
}

int main() {

	return 0;
}