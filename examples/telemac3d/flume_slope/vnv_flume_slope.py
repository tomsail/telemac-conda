
"""
Validation script for flume_slope
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
        self.rank = 3
        self.tags = ['telemac3d', 'postel3d']

    def _pre(self):
        """
        Defining the studies
        """

        # flume slope cohesive sediment scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_flume_slope.cas')


        # flume slope cohesive sediment parallel mode
        cas = TelemacCas('t3d_flume_slope.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_flume_slope_par.cas',
                       cas=cas)

        del cas

        # post-treatment
        self.add_study('p3d',
                       'postel3d',
                       'p3d_flume_slope.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_flume_slope_set1.slf',
                            eps=[1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_flume_slope_set1.slf',
                            eps=[1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-5])


    def _post(self):
        """
        Post-treatment processes
        """

