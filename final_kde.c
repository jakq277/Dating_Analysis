#include <stdio.h>
#include <math.h>

#include <R.h>
#include <Rmath.h>
#define bandwidth 0.5232409
#define sqrt2pi 2.50662827

void kde(int*n, int*m, double *g,double *x, double *y){
  for (int i = 0; i < *m; i++) {
    y[i] = 1.0;
    for (int j = 0; j < *n; j++) {
      y[i] += exp(-0.5 * pow((g[i] - x[j]) / bandwidth, 2)) / (bandwidth * sqrt2pi);
    }
    y[i] /= *n;
    
  }
}


