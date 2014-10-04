#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define abs(n) ((n)<0?-(n):(n))

#define NMAX 100
#define EPSILON 0.0001

#define SWARN(s) if((s) < 0) { puts("A e' provavelmente singular."); fclose(f); continue; } 

int lucol(int n, double A[][NMAX], int p[]) {
	return 0;
}

int lurow(int n, double A[][NMAX], int p[]) {
	int i, j, k, temp, max;

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

	return 0;
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
		b[j] /= A[j][j];
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
		b[i] /= A[i][i];
	}

	return 0;
}

int main(int argc, char* args[]) {
	FILE* f;
	int i, j, k, n, x, y;
	double A[NMAX][NMAX];
	double b[NMAX];
	int p[NMAX];
	time_t now, then;

	/* Ax=b */

	for(i=1;i<=argc;++i) {
		f = fopen(args[i], "r");

		fscanf(f, "%d", &n);

		for(j=0;j<n;++j)
			for(k=0;k<n;++k) {
				fscanf(f, "%d %d", &x, &y);
				fscanf(f, "%le", &A[x][y]);
			}

		for(j=0;j<n;++j) {
			fscanf(f, "%d", &x);
			fscanf(f, "%le", &b[x]);
		}

		puts("Solucionando o sistema: Ax=b");

		puts("==================\nPelo metodo orientado a linha:");
		time(&now);
		SWARN(lurow(n, A, p));
		time(&then);
		printf("Tempo de execucao do pivoteamento e decomposicao em LU: %.5e\n", difftime(now, then));
		time(&now);
		SWARN(ssrow(n, A, p, b));
		time(&then);
		printf("Tempo de execucao para solucao do sistema por LUP: %.5e\n", difftime(now, then));

		puts("==================\nPelo metodo orientado a coluna:");
		time(&now);
		SWARN(lucol(n, A, p));
		time(&then);
		printf("Tempo de execucao do pivoteamento e decomposicao em LU: %.5e\n", difftime(now, then));
		time(&now);
		SWARN(sscol(n, A, p, b));
		time(&then);
		printf("Tempo de execucao para solucao do sistema por LUP: %.5e\n", difftime(now, then));
		puts("==================");

		fclose(f);
	}

	return 0;
}