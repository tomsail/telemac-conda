#******************************************************************************
# Python script for the VNV of TELEMAC2D BUMP CASE
#******************************************************************************
#
#Purpose: compute the 1D stationnary analytical solution of the bump test case
#
#history  F.SOUILLE (LNHE)
#+        21/10/2019
#+        V8P1
#
#******************************************************************************
import os
import numpy as np

#------------------------
# PARAMETERS:
#------------------------
G = 9.81

#------------------------
# CLASSES:
#------------------------
class Bottom():
    """
    Bottom elevation class
    """
    def __init__(self, bottom_function, x, xb=10.):
        self.function = bottom_function

        if isinstance(x, (list, np.ndarray)):
            self.zb = np.empty(len(x))
            for i, x_i in enumerate(x):
                self.zb[i] = self.compute_bottom(x_i)
        else:
            self.zb = self.compute_bottom(x)

    def compute_bottom(self, x):

        if self.function == 'parabolic':
            return self.parabolic(x)
        if self.function == 'exponential':
            return self.exponential(x)

        raise ValueError("Unknown bottom_function")

    def parabolic(self, x, xb=10.):
        if 8. < x < 12.:
            return 0.2 - 0.05*(x - xb)**2
        return 0.

    def exponential(self, x, xb=10.):
        return 0.25*np.exp(-0.5*(x - xb)**2)


class BumpAnalyticSol():
    """
    Bumps 1d stationary analytical solution

    """
    def __init__(\
            self, flow='sub', Q=8.85, hl=1.8, length=20., width=2.,
            xb=10., bottom_function='parabolic', N=8001):

        # Physical properties
        self.flow = flow
        self.q0 = Q/width
        self.hc = (self.q0**2/G)**(1./3.)
        self.hl = hl

        # 1D mesh
        self.N = N
        self.x = np.linspace(0., length, N)

        # Bottom
        bottom = Bottom(bottom_function, self.x, xb=10.)
        self.zb = bottom.zb
        self.zm = max(self.zb)

        # Solution
        self.H = np.empty((N))
        self.E = np.empty((N))
        self.U = np.empty((N))
        self.V = np.empty((N))
        self.F = np.empty((N))

    def __call__(self):
        if self.flow == 'sub':
            self.compute_subcritical_solution()
        elif self.flow == 'cri':
            self.compute_critical_solution()
        elif self.flow == 'trans':
            self.compute_transcritical_solution()

        for i in range(self.N):
            self.E[i] = self.zb[i] + self.H[i]
            self.U[i] = self.q0 / self.H[i]
            self.V[i] = 0.0
            self.F[i] = abs(self.U[i])/np.sqrt(G*self.H[i])

    def compute_subcritical_solution(self):
        coeff = np.zeros((4), dtype='d')

        for i in range(self.N):
            polyd3_coef(coeff, self.zb[i], self.q0, self.hl, 0.)
            hsub = max(np.roots(coeff).real)
            # Subcritical flow for all i:
            self.H[i] = hsub

    def compute_critical_solution(self):
        coeff = np.zeros((4), dtype='d')

        for i in range(self.N):
            polyd3_coef(coeff, self.zb[i], self.q0, self.hc, self.zm)
            hroots = np.sort(np.roots(coeff).real)
            hsub = hroots[2]
            hsup = hroots[1]
            j = np.argmax(self.zb)
            if self.x[i] < self.x[j]:
                # Subcritical flow:
                self.H[i] = hsub
            elif self.x[i] > self.x[j]:
                # Supercritical flow:
                self.H[i] = hsup
            else:
                # Transition:
                self.H[i] = self.hc

    def compute_transcritical_solution(self):
        coeff = np.zeros((4), dtype='d')
        i = self.N - 1
        supercritical = False
        hjump = False
        h1d = np.empty(self.N, dtype='d')

        while i >= 0:
            while supercritical is False:
                j = np.argmax(self.zb)
                if self.x[i] > self.x[j]:
                    polyd3_coef(coeff, self.zb[i], self.q0, self.hl, 0.)
                    hroots = np.sort(np.roots(coeff).real)
                    h1d[i] = hroots[2]
                    if h1d[i] < self.hc:
                        supercritical = True
                i = i - 1
                if i < 0: break

            if supercritical is True:
                ic = np.argmax(self.zb)
                while hjump == False:
                    polyd3_coef(coeff, self.zb[i], self.q0, self.hc, self.zm)
                    hroots = np.sort(np.roots(coeff).real)
                    h1d[ic] = hroots[1]
                    if ic > i:
                        state_L = 2*self.q0**2/h1d[ic] + G*h1d[ic]**2
                        state_R = 2*self.q0**2/h1d[ic+1] + G*h1d[ic+1]**2
                        if state_L < state_R:
                            hjump = True
                            x_shock = self.x[ic]
                            i = -1
                    ic += 1

        if not hjump:
            raise ValueError("No jump, check flow type")

        for i in range(self.N):
            if self.x[i] > x_shock:
                polyd3_coef(coeff, self.zb[i], self.q0, self.hl, 0.)
                hsub = max(np.roots(coeff).real)
                # Subcritical flow:
                self.H[i] = hsub
            if self.x[i] <= x_shock:
                polyd3_coef(coeff, self.zb[i], self.q0, self.hc, self.zm)
                hroots = np.sort(np.roots(coeff).real)
                hsub = hroots[2]
                hsup = hroots[1]
                j = np.argmax(self.zb)
                if self.x[i] < self.x[j]:
                    # Subcritical flow:
                    self.H[i] = hsub
                elif self.x[i] > self.x[j]:
                    # Supercritical flow:
                    self.H[i] = hsup
                else:
                    # Transition:
                    self.H[i] = self.hc

    def savetxt(self, file_name='ANALYTIC_SOL.txt'):
        np.savetxt(file_name,\
            np.c_[self.x, self.H, self.U, self.E, self.F])

#------------------------
# FUNCTIONS:
#------------------------
def polyd3_coef(coef, z, q, h, zm):
    coef[0] = 1.0
    coef[1] = z - q**2/(2.*G*h**2) - h - zm
    coef[2] = 0.0
    coef[3] = q**2/(2.*G)
    return 0

#------------------------
# UNIT TESTS:
#------------------------
def test_bottom_class():
    import matplotlib.pylab as plt
    x = np.linspace(0., 20., 2001)
    bottom_functions = ['parabolic', 'exponential']
    for funct in bottom_functions:
        bottom = Bottom(funct, x, xb=10.)
        plt.plot(x, bottom.zb, label=funct)
    plt.legend()
    plt.show()

def test_solution():
    import matplotlib.pylab as plt
    bottom_function = 'exponential'
    solsub = BumpAnalyticSol(\
        flow='sub', Q=1.5, hl=0.8, bottom_function=bottom_function)
    solcri = BumpAnalyticSol(\
        flow='cri', Q=0.3, hl=0., bottom_function=bottom_function)
    soltrans = BumpAnalyticSol(\
        flow='trans', Q=0.45, hl=0.35, bottom_function=bottom_function)
    solsub()
    solcri()
    soltrans()
    plt.plot(solsub.x, solsub.zb, label='zb')
    plt.plot(solsub.x, solsub.E, label='sub')
    plt.plot(solsub.x, solcri.E, label='cri')
    plt.plot(solsub.x, soltrans.E, label='trans')
    plt.legend()
    plt.show()

def debug_solution_trans():
    import matplotlib.pylab as plt
    bottom_function = 'exponential'
    Qin = 0.45
    HL = .35
    solsub = BumpAnalyticSol(\
        flow='sub', Q=Qin, hl=HL, bottom_function=bottom_function)
    solcri = BumpAnalyticSol(\
        flow='cri', Q=Qin, hl=HL, bottom_function=bottom_function)
    soltrans = BumpAnalyticSol(\
        flow='trans', Q=Qin, hl=HL, bottom_function=bottom_function)
    solsub()
    solcri()
    soltrans()
    plt.plot(solsub.x, solsub.zb, label='zb')
    plt.plot(solsub.x, solsub.zb+solsub.hc, label='hc')
    plt.plot(solsub.x, solsub.E, label='sub')
    plt.plot(solsub.x, solcri.E, label='cri')
    plt.plot(solsub.x, soltrans.E, label='trans')
    plt.legend()
    plt.show()

#------------------------
# MAIN:
#------------------------
if __name__ == '__main__':

    # TEST BOTTOM CLASS:
    test_bottom_class()

    # TEST ANALYTIC SOLUTION:
    test_solution()
