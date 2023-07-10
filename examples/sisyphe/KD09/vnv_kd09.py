
"""
Validation script for kd09
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # kd09 telemac2d-sisyphe coupled scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_kd09_mak.cas')


        # kd09 telemac2d-sisyphe coupled scalar mode
        cas = TelemacCas('t2d_kd09_mak.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_kd09_mak_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_kd09.slf',
                            eps=[1e-11])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_kd09.slf',
                            eps=[1e-11])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[1e-11])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_kd09.slf',
                            eps=[1e-11])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_kd09.slf',
                            eps=[1e-11])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-11])


    def _post(self):
        """
        Post-treatment processes
        """

        # TODO: plot compariason with analitycal solution
        # Code from plot_kd09.py (not working) It is using two csv files that are not available
       #def get_data(filename):
       #    x = []
       #    g1 = []
       #    g2 = []

       #    tfile = open(filename, 'r')

       #    for line in tfile:
       #       if line.split()[0] != '#':
       #           lineparts = line.split(',')
       #           x.append(lineparts[0])
       #           g1.append(lineparts[1])
       #           g2.append(lineparts[2])
       #       else:
       #           continue

       #    tfile.close()
       #    return x, g1, g2

        #x,a1,n1 = get_data('kd09bed.csv')

        #fig = plt.figure()

        #plt.plot(x[2:],n1[2:],'r',label='Analytical KD09')
        #plt.plot(x[2:],a1[2:], 'b', label='Telemac-Sisyphe')
        #plt.xlabel('Distance(m)')
        #plt.ylabel('Bed level (m)')

        #plt.legend(loc='lower right')
        #plt.title('Comparison of Telemac-Sisyphe and KD09 analytical solution at t=1s' )
        #filename = 'KD09_Bed_profile_1s'+ '.png'
        #plt.savefig(filename, dpi=100)
        #print(' '*8+'~> '+fileename)
        #plt.close('all')


        ### Fig 2
        #x,a1,n1 = get_data('kd09dep.csv')

        #fig = plt.figure()

        #plt.plot(x[2:],n1[2:],'r',label='Analytical KD09')
        #plt.plot(x[2:],a1[2:], 'b', label='Telemac-Sisyphe')
        #plt.xlabel('Distance(m)')
        #plt.ylabel('Water Depth (m)')

        #plt.legend(loc='lower left')
        #plt.title('Comparison of Telemac-Sisyphe and KD09 analytical solution at t=1s' )
        #filename = 'KD09_Depth_profile_1s'+ '.png'
        #print(' '*8+'~> '+fileename)
        #plt.savefig(filename, dpi=100)
        #plt.close('all')
