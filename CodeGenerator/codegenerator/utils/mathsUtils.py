##
# @file This file is part of the ExaHyPE project.
# @author ExaHyPE Group (exahype@lists.lrz.de)
#
# @section LICENSE
#
# Copyright (c) 2016  http://exahype.eu
# All rights reserved.
#
# The project has received funding from the European Union's Horizon 
# 2020 research and innovation programme under grant agreement
# No 671698. For copyrights and licensing, please consult the webpage.
#
# Released under the BSD 3 Open Source License.
# For the full license text, see LICENSE.txt


import math


class MathsUtils:
    #****************************************
    #****************************************
    #****** Matrix/Vector operations ********
    #****************************************
    #****************************************

    @staticmethod
    def matrixTranspose(M):
        """Transpose a matrix M"""
        return [[M[j][i] for j in range(len(M))] for i in range(len(M[0]))]


    @staticmethod
    def matrixDot(A,B):
        """A dot B"""
        return [[sum([A[n][k]*B[k][m] for k in range(len(B))]) for m in range(len(B[0]))] for n in range(len(A))]


    @staticmethod
    def matrixInverse(M):
        """M^-1"""
        return MathsUtils.matrixInverse_Pivot(M)


    @staticmethod
    def matrixInverse_Pivot(M):
        """Compute a matrix inverse using a pivot algorithm"""
        
        n = len(M)
        # c = (M|Id)
        c = [[0.0 for _ in range(2*n)] for _ in range(n)]
        for i in range(n):
            for j in range(n):
                c[i][j] = M[j][i]
                c[i][j+n] = 0.0
            c[i][i+n]  =1.0

        #Forward elimination and row swapping (if necessary)
        for i in range(n):
            ml = i
            mlV = math.fabs(c[i][i])
            for j in range(i+1, n):
                if(math.fabs(c[j][i]) > mlV):
                    ml = j
                    mlV = math.fabs(c[j][i])
            for k in range(0, 2*n):
                tmp = c[ml][k]
                c[ml][k] = c[i][k]
                c[i][k] = tmp

            if c[i][i] == 0.:
                raise ValueError("MatrixInverse: Matrix is singular")
            
            piv = 1. / c[i][i]
            for k in range(0, 2*n):
                c[i][k] *= piv
            for j in range(i+1, n):
                tmp = c[j][i]
                for k in range(0, 2*n):
                    c[j][k] -= tmp*c[i][k]
        
        #Back substitution
        for i in range(n-1,-1,-1):
            for j in range(i-1,-1,-1):
                tmp = c[j][i];
                for k in range(0, 2*n):
                    c[j][k] -= tmp*c[i][k]

        return [[c[i][j+n] for i in range(n)] for j in range(n)]


    @staticmethod
    def vectorPad(v,padSize):
        """zero-pad a vector"""
        if padSize <= 0:
            return v
        return v + [0. for _ in range(padSize)]


    @staticmethod
    def matrixPadAndFlatten_RowMajor(M, padSize):
        """ 
        a b c 
        d e f 
         => a b c 0 d e f 0 
        """
        result = []
        for i in range(len(M)):
            result += MathsUtils.vectorPad(M[i], padSize)
        return result


    @staticmethod
    def matrixPadAndFlatten_ColMajor(M, padSize):
        """ 
        a b c 
        d e f 
         => a d 0 b e 0 c f 0 
        """
        return MathsUtils.matrixPadAndFlatten_RowMajor(MathsUtils.matrixTranspose(M), padSize)


    #****************************************
    #****************************************
    #*********** Gauss-Legendre *************
    #****************************************
    #****************************************  
    @staticmethod
    def getGaussLegendre(nDof):
        """Return Gauss-Legendre weights, points"""
        if nDof < 1:
            raise ValueError("order must be positive")
            
        if nDof > 10:
            raise ValueError("order is currently limited to 9")
            
        if nDof == 1:
            return [1.0000000000000000], [0.5000000000000000]

        if nDof == 2:
            return  [0.5000000000000000, 0.5000000000000000], \
                    [0.2113248654051871, 0.7886751345948129]

        if nDof == 3:
            return  [0.2777777777777778, 0.4444444444444444, 0.2777777777777778], \
                    [0.1127016653792583, 0.5000000000000000, 0.8872983346207417]

        if nDof == 4:
            return  [0.1739274225687273, 0.3260725774312732, 0.3260725774312732, 0.1739274225687273], \
                    [0.0694318442029737, 0.3300094782075719, 0.6699905217924281, 0.9305681557970262]

        if nDof == 5:
            return  [0.1184634425280948, 0.239314335249683, 0.2844444444444443, 0.239314335249683, 0.1184634425280948], \
                    [0.04691007703066802, 0.2307653449471584, 0.5000000000000000, 0.7692346550528415, 0.9530899229693319]

        if nDof == 6:
            return  [0.0856622461895845, 0.1803807865240695, 0.2339569672863459, 0.2339569672863459, 0.1803807865240695, 0.0856622461895845], \
                    [0.03376524289842397, 0.1693953067668678, 0.3806904069584015, 0.6193095930415985, 0.8306046932331322, 0.966234757101576]

        if nDof == 7:
            return  [0.06474248308443538, 0.1398526957446382, 0.1909150252525592, 0.2089795918367344, 0.1909150252525592, 0.1398526957446382, 0.06474248308443538], \
                    [0.02544604382862076, 0.1292344072003028, 0.2970774243113014, 0.5000000000000000, 0.7029225756886985, 0.8707655927996972, 0.9745539561713792]

        if nDof == 8:
            return  [0.05061426814518821, 0.1111905172266871, 0.1568533229389437, 0.1813418916891809, 0.1813418916891809, 0.1568533229389437, 0.1111905172266871, 0.05061426814518821], \
                    [0.01985507175123186, 0.1016667612931866, 0.2372337950418355, 0.4082826787521751, 0.5917173212478249, 0.7627662049581645, 0.8983332387068134, 0.9801449282487682]

        if nDof == 9:
            return  [0.04063719418078751, 0.09032408034742861, 0.1303053482014677, 0.1561735385200013, 0.1651196775006297, 0.1561735385200013, 0.1303053482014677, 0.09032408034742861, 0.04063719418078751], \
                    [0.01591988024618696, 0.08198444633668212, 0.1933142836497048, 0.3378732882980955, 0.5000000000000000, 0.6621267117019045, 0.8066857163502952, 0.9180155536633179, 0.984080119753813]

        if nDof == 10:
            return  [0.03333567215434358, 0.07472567457529024, 0.1095431812579912, 0.1346333596549983, 0.1477621123573766, 0.1477621123573766, 0.1346333596549983, 0.1095431812579912, 0.07472567457529024, 0.03333567215434358], \
                    [0.01304673574141413, 0.06746831665550773, 0.1602952158504878, 0.2833023029353764, 0.4255628305091844, 0.5744371694908156, 0.7166976970646236, 0.8397047841495122, 0.9325316833444923, 0.9869532642585859]



    #****************************************
    #****************************************
    #*********** Gauss-Lobatto **************
    #****************************************
    #****************************************  
    @staticmethod
    def getGaussLobatto(nDof):
        """Return Gauss-Lobatto weights, points"""
        if nDof < 1:
            raise ValueError("order must be positive")
            
        if nDof > 10:
            raise ValueError("order is currently limited to 9")
            
        if nDof == 1:
            return [1.0000000000000000], [0.5000000000000000]

        if nDof == 2:
            return [0.5, 0.5,], \
                   [1.0, 0.0]
      
        if nDof == 3:
            return [0.1666666666666667, 0.6666666666666666, 0.1666666666666667], \
                   [1.0, 0.5, 0.0]  

        if nDof == 4:
            return [0.08333333333333333, 0.4166666666666667, 0.4166666666666667, 0.08333333333333333], \
                   [1.0, 0.7236067977499789, 0.2763932022500211, 0.0]
      
        if nDof == 5:
            return [0.05, 0.2722222222222221, 0.3555555555555556, 0.2722222222222221, 0.05], \
                   [1.0, 0.8273268353539885, 0.5, 0.1726731646460115, 0.0]  

        if nDof == 6:
            return [0.03333333333333333, 0.1892374781489235, 0.2774291885177432, 0.2774291885177432, 0.1892374781489235, 0.03333333333333333], \
                   [1.0, 0.8825276619647324, 0.6426157582403226, 0.3573842417596774, 0.1174723380352676, 0.0]

        if nDof == 7:
            return [0.02380952380952381, 0.138413023680783, 0.2158726906049313, 0.2438095238095238, 0.2158726906049313, 0.138413023680783, 0.02380952380952381], \
                   [1.0, 0.9151119481392835, 0.7344243967353571, 0.5, 0.2655756032646429, 0.08488805186071652, 0.0] 

        if nDof == 8:
            return [0.01785714285714286, 0.1053521135717531, 0.1705613462417522, 0.2062293973293519, 0.2062293973293519, 0.1705613462417522, 0.1053521135717531, 0.01785714285714286], \
                   [1.0, 0.9358700742548033, 0.7958500907165711, 0.6046496089512394, 0.3953503910487606, 0.2041499092834289, 0.06412992574519671, 0.0]  

        if nDof == 9:
            return [0.01388888888888889, 0.08274768078040276, 0.1372693562500808, 0.1732142554865232, 0.1857596371882086, 0.1732142554865232, 0.1372693562500808, 0.08274768078040276, 0.01388888888888889], \
                   [1.0, 0.94987899770573, 0.8385931397553689, 0.6815587319130891, 0.5, 0.3184412680869109, 0.1614068602446311, 0.05012100229426991, 0.0] 

        if nDof == 10:
            return [0.01111111111111111, 0.06665299542553503, 0.1124446710315632, 0.1460213418398419, 0.1637698805919487, 0.1637698805919487, 0.1460213418398419, 0.1124446710315632, 0.06665299542553503, 0.01111111111111111], \
                   [1.0, 0.9597669540832294, 0.8693869325527526, 0.7389624749052223, 0.5826394788331934, 0.4173605211668065, 0.2610375250947777, 0.1306130674472474, 0.04023304591677057, 0.0]


    #****************************************
    #****************************************
    #*************** ADERDG *****************
    #****************************************
    #****************************************

    @staticmethod
    def baseFunc1d(xi, xin, N):
        """
        Computes the ADER-DG basis functions and their first derivative.
        
        Args:
           xi:
              The reference element point the basis functions are evaluated at.
              Here, xi refers to the greek letter that is often used as a reference element coordinate.
           xin:
              The reference element nodes corresponding to the nodal basis functions.
           N:
              Number of nodal basis functions (=order+1).
        Returns:
           phi:
              Basis function values.
           phi_xi:
              First derivatives of the basis functions.
        """
        phi    = [1.]*N 
        phi_xi = [0.]*N
        for m in range(0,N):
            for j in range(0,N):
                if j == m:
                    continue 
                phi[m] = phi[m]*(xi-xin[j])/(xin[m]-xin[j])
            for i in range(0,N):
                if i == m:
                    continue
                tmp = 1.;
                for j in range(0,N):
                    if j == i:
                        continue
                    if j == m:
                        continue
                    tmp = tmp*(xi-xin[j])/(xin[m]-xin[j])
                phi_xi[m] += tmp/(xin[m]-xin[i])
        return phi, phi_xi    


    @staticmethod
    def assembleStiffnessMatrix(xGPN, wGPN, N):
        """
        Computes the (reference) element stiffness matrix for an approximation of
        order N.

        Args:
           xGPN:
              Gauss-Legendre nodes (N nodes).
           wGPN:
              Gauss-Legendre weights  (N weights).
           N:
              Number of nodal basis functions (=order+1).
        Returns:
           K_xi:
              The (reference) element stiffness matrix.
        """
        # init matrix with zero
        Kxi = [[0 for _ in range(N)] for _ in range(N)]
         
        for i in range(0,N):
            phi, phi_xi = MathsUtils.baseFunc1d(xGPN[i], xGPN, N)
            for k in range(0,N):
                for l in range(0,N):
                    Kxi[k][l] += wGPN[i]*phi_xi[k]*phi[l] 
            
        return Kxi


    @staticmethod
    def assembleK1(Kxi, xGPN, N):
        """
        Computes the difference between the reference element mass operator 
        evaluated at point xi=1.0 and the element stiffness matrix.
        
        Args:
           K_xi:
              The (reference) element stiffness matrix for a approximation of 
              order N.
           xGPN:
              Gauss-Legendre nodes (N nodes).
           N:
              Order of approximation corresponding to N+1 nodal basis functions.
        Returns:
           K1:
              <unknown>
        """
        phi1, _ = MathsUtils.baseFunc1d(1.0, xGPN, N)
        FRm = [[0 for _ in range(N)] for _ in range(N)]
        
        for k in range(0, N):
            for l in range(0, N):
                FRm[k][l] = phi1[k]*phi1[l] 
        
        return [[FRm[i][j] - Kxi[i][j] for j in range(N)] for i in range(N)]


    @staticmethod
    def assembleMassMatrix(xGPN, wGPN, N):
        """
        Computes the (reference) element mass matrix for an approximation of
        order N.

        Args:
           xGPN:
              Gauss-Legendre nodes (N nodes).
           wGPN:
              N Gauss-Legendre weights (N weights).
           N:
              Number of nodal basis functions (=order+1).
        Returns:
           M_xi:
              The (reference) element mass matrix.
        """
        # init matrix with zeros
        MM = [[0 for _ in range(N)] for _ in range(N)]
        
        for i in range(0,N):
            phi, _ = MathsUtils.baseFunc1d(xGPN[i], xGPN, N)
            for k in range(0,N):
                for l in range(0,N):
                    MM[k][l] += wGPN[i]*phi[k]*phi[l]
          
        return MM


    @staticmethod
    def assembleDiscreteDerivativeOperator(MM, Kxi):
        """
        Computes some derivative values for debugging purposes.

        Args:
           MM:
              The (reference) element mass matrix for a approximation of 
              order N.
           Kxi:
              The (reference) element stiffness matrix for a approximation of 
              order N.
           
        Returns:
           dudx:
              Derivative values for debugging purposes.
        """
        dudx = MathsUtils.matrixDot(MathsUtils.matrixInverse(MM),MathsUtils.matrixTranspose(Kxi))
        return dudx


    @staticmethod
    def assembleFineGridProjector1d(xGPN, j, N):
        """
        Transforms the degrees of freedom located on a coarse grid edge
        nodes to degrees of freedoms located on nodes of a fine grid edge.
        The difference in levels is 1.
        
        Let us denote by P the 1d fine grid projector (=1d equidistantGridProjector). The fine grid DoF 
        are computed according to:
        
        u^{fine;j}_i =  sum_{m} P^{j}_im u^{coarse}_m
        
        Args:
           xGPN:
              Gauss-Legendre nodes (N nodes).
           j:
              Index of one the three subintervals: 0,1, or 2.
           N:
              Number of nodal basis functions (=order+1).
        Returns:
           equidistantGridProjector:
              The corresponding degrees of freedom located at nodes of an equidistant grid over (0,1).
        """
        fineGridProjector1d = [[0 for _ in range(N)] for _ in range(N)]
        
        for i in range(0, N): # Eq. basis
            phi_i, _ = MathsUtils.baseFunc1d((xGPN[i]+j)/3.0, xGPN, N)
            for m in range(0, N): # DG basis
                fineGridProjector1d[m][i] = phi_i[m]
        return fineGridProjector1d


    #****************************************
    #****************************************
    #*************** Limiter ****************
    #****************************************
    #****************************************

    @staticmethod
    def assembleQuadratureConversion(fromQ, toQ, N):
        """Return base conversion matrix"""
        conversionMat = [[0.0 for _ in range(N)] for _ in range(N)]
        for i in range(0, N):
            phi, _ = MathsUtils.baseFunc1d(toQ[i], fromQ, N)
            for j in range(0, N):
                conversionMat[j][i] = phi[j] #check order
        return conversionMat


    @staticmethod
    def assembleDGToFV(nodes, weights, N, Nlim):
        """Return conversion matrix from DG grid to FV grid"""
        dg2fv = [[0.0 for _ in range(Nlim)] for _ in range(N)]
        dxi = 1.0 / float(Nlim)
        xLeft = 0.0
        xi = 0.0
        for i in range(0, Nlim):
            xLeft = i*dxi
            for j in range(0, N):
                xi = xLeft + dxi*nodes[j]
                phi, _ = MathsUtils.baseFunc1d(xi, nodes, N)
                for k in range(0, N):
                    dg2fv[k][i] += weights[j]*phi[k]
        return dg2fv


    @staticmethod
    def assembleFVToDG(dg2fv, weights, N, Nlim):
        """Return conversion matrix from FV grid to DG grid"""
        fv2dg = [[0.0 for _ in range(N)] for _ in range(Nlim)]
        lsqm = [[0.0 for _ in range(N+1)] for _ in range(N+1)]
        lsqrhs = [[0.0 for _ in range(Nlim)] for _ in range(N+1)]
        
        dxi = 1.0 / float(Nlim)
        
        for i in range(0, N):
            for j in range(0, N):
                for k in range(0, Nlim):
                    lsqm[j][i] += 2* dg2fv[i][k] * dg2fv[j][k]
            lsqm[N][i] = weights[i]
        for i in range(0, N):
            lsqm[i][N] = -weights[i]
        lsqm[N][N] = 0.0
        
        ilsqm = MathsUtils.matrixInverse(lsqm)
        
        for i in range(0, Nlim):
            for j in range(0, N):
                lsqrhs[j][i] = 2*dg2fv[j][i]
            lsqrhs[N][i] = dxi;
            
        for i in range(0, Nlim):
            for j in range(0, N):
                for k in range(0, N+1):
                    fv2dg[i][j] += ilsqm[j][k] * lsqrhs[k][i]
                    
        return fv2dg
