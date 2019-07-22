/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon 
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/
 
#include "kernels/LimiterProjectionMatrices.h"
#include <cmath>



double** kernels::uh2lim;
double** kernels::uh2lob;
double** kernels::lim2uh;

void kernels::freeLimiterProjectionMatrices(const std::set<int>& orders) {
  // @todo The argument is not used yet.
  constexpr int MAX_ORDER=9;

  for (int i = 0; i < MAX_ORDER + 1; i++) {
    delete [] uh2lim[i];
    delete [] uh2lob[i];
    delete [] lim2uh[i];
  }

  delete [] uh2lim;
  delete [] uh2lob;
  delete [] lim2uh;
  
}

void kernels::initLimiterProjectionMatrices(const std::set<int>& orders) {
  // @todo The argument is not used yet.
  constexpr int MAX_ORDER=9;

  uh2lim = new double* [MAX_ORDER + 1];
  uh2lob = new double* [MAX_ORDER + 1];
  lim2uh = new double* [MAX_ORDER + 1];

  //TODO JMG, use generated values
  int basisSize, basisSizeLim;
  int i,j,k;
  for (int n = 0; n < MAX_ORDER + 1; n++) {
    basisSize = n+1;
    basisSizeLim = 2*n+1;
    uh2lim[n] = new double[basisSize*basisSizeLim]();
    uh2lob[n] = new double[basisSize*basisSize]();
    lim2uh[n] = new double[basisSizeLim*basisSize]();
  
    double* phi = new double[basisSize]();
    
    {
      idx2 idx(basisSize, basisSizeLim);
      
      const double dxi = 1. / basisSizeLim;
      double xLeft, xi;
      for(i=0; i<basisSizeLim; i++) {
        xLeft = i*dxi;
        for(j=0; j<basisSize; j++) {
          xi = xLeft + dxi*gaussLegendreNodes[basisSize-1][j];
          BaseFunc1D(phi, xi, basisSize);
          for(k=0; k<basisSize; k++) { //
            uh2lim[n][idx(k,i)] += gaussLegendreWeights[basisSize-1][j]*phi[k];
          }
        }
      }
    }
    
    {
      idx2 idx(basisSize, basisSize);
      
      for(i=0; i<basisSize; i++) {
        BaseFunc1D(phi, gaussLobattoNodes[basisSize-1][i], basisSize);
        for(j=0; j<basisSize; j++) {
          uh2lob[n][idx(j,i)] = phi[j]; //Fortran: uh2lob(ii,:) = phi(:) 
        }
      }
    }
    
    {
      idx2 idx(basisSizeLim, basisSize);
      
      double* lsqm = new double[(basisSize+1)*(basisSize+1)];
      double* lsqrhs = new double[basisSizeLim*(basisSize+1)];
      idx2 idxLSQM((basisSize+1),(basisSize+1));
      idx2 idxLSQrhs(basisSizeLim,(basisSize+1));
      idx2 idxUh2Lim(basisSize, basisSizeLim);
      const double dxi = 1.0 / basisSizeLim;
      for(i=0; i<basisSize; i++) {
        for(j=0; j<basisSize; j++) {
          lsqm[idxLSQM(i,j)] = 0.;
          for(k=0; k<basisSizeLim; k++) {
            lsqm[idxLSQM(i,j)] += 2* uh2lim[n][idxUh2Lim(i,k)] * uh2lim[n][idxUh2Lim(j,k)];
          }
        }
        lsqm[idxLSQM(i,basisSize)] = gaussLegendreWeights[basisSize-1][i];
      }
      for(i=0; i<basisSize; i++) {
        lsqm[idxLSQM(basisSize,i)] = -gaussLegendreWeights[basisSize-1][i];
      }
      lsqm[idxLSQM(basisSize,basisSize)] = 0.;
      
      double* ilsqm = matrixInverse(basisSize+1, lsqm);   
      
      for(i=0; i<basisSizeLim; i++) {
        for(j=0; j<basisSize; j++) {
          lsqrhs[idxLSQrhs(i,j)] = 2*uh2lim[n][idxUh2Lim(j,i)];
        }
        lsqrhs[idxLSQrhs(i,basisSize)] = dxi;
      }
      
      for(i=0; i<basisSizeLim; i++) {
        for(j=0; j<basisSize; j++) {
          lim2uh[n][idx(i,j)] = 0.;
          for(k=0; k<basisSize+1; k++) {
            lim2uh[n][idx(i,j)] += ilsqm[idxLSQM(k,j)] * lsqrhs[idxLSQrhs(i,k)];
          }
        }
      }
      
      delete[] lsqm;
      delete[] ilsqm;
      delete[] lsqrhs;
    }
    
    delete[] phi;

  }
}

//TODO JMG remove when generated value
void kernels::BaseFunc1D(double* phi, double xi, const int basisSize) {
  int i,j,m;
  for(i=0; i<basisSize; i++) {
    phi[i] = 1;
  }
  for(m=0; m<basisSize; m++) {
    for(j=0; j<basisSize; j++) {
      if(j == m) continue;
      phi[m] = phi[m]*(xi- gaussLegendreNodes[basisSize-1][j])/(gaussLegendreNodes[basisSize-1][m]-gaussLegendreNodes[basisSize-1][j]);
    }
  }
}

//TODO JMG remove when generated value
double* kernels::matrixInverse(int n, double* a) {
  
  double* ia = new double[n*n];
  double* c = new double[n*n*2];

  idx2 idx(n,n);
  idx2 idxC(n,2*n);
  
  int i,j,k,ml;
  double tmp, piv, mlV;
  
  for(i=0; i<n; i++) {
    for(j=0; j<n; j++) {
      c[idxC(i,j)] = a[idx(j,i)];
    }
    for(; j<2*n; j++) {
      c[idxC(i,j)] = 0.;
    }
    c[idxC(i,i+n)] = 1.;
  }

  //Forward elimination and row swapping (if necessary)
  for(i=0; i<n; i++) {
    ml = i;
    mlV = std::abs(static_cast<double>(c[idxC(i,i)]));
    for(j=i+1; j<n; j++) {
      if(std::abs(c[idxC(j,i)]) > mlV) {
        ml = j;
        mlV = c[idxC(j,i)];
      }
    }

    for(k=0; k<2*n; k++) {
      tmp = c[idxC(ml,k)];
      c[idxC(ml,k)] = c[idxC(i,k)];
      c[idxC(i,k)] = tmp;
    }
    if(c[idxC(i,i)] == 0) {
      //logError("matrixInverse()", "Matrix is singular" );
      return nullptr;
    }
    piv = 1. / c[idxC(i,i)];
    for(k=0; k<2*n; k++) {
      c[idxC(i,k)] *= piv;
    }
    for(j=i+1; j<n; j++) {
      tmp = c[idxC(j,i)];
      for(k=0; k<2*n; k++) {
        c[idxC(j,k)] -= tmp*c[idxC(i,k)];
      }
    }
  }
  
  //Back substitution
  for(i=n-1; i>=0; i--) {
    for(j=i-1; j>=0; j--) {
      tmp = c[idxC(j,i)];
      for(k=0; k<2*n; k++) {
        c[idxC(j,k)] -= tmp*c[idxC(i,k)];
      }
    }
  }
  
  for(i=0; i<n; i++) {
    for(j=0; j<n; j++) {
      ia[idx(j,i)] = c[idxC(i,j+n)];
    }
  }
  
  // clean up
  delete[] c;

  return ia;
}
