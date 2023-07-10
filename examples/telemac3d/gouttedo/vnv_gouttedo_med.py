
"""
Validation script for gouttedo_med
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
        self.rank = 0
        self.tags = ['telemac3d', 'med']

    def _pre(self):
        """
        Defining the studies
        """

        # gouttedo med scalar mode
        self.add_study('vnv_med_1',
                       'telemac3d',
                       't3d_gouttedo_med.cas')


        # gouttedo med parallel mode
        cas = TelemacCas('t3d_gouttedo_med.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_med_2',
                       'telemac3d',
                       't3d_gouttedo_med_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_med_1:T3DRES',
                            'f3d_gouttedo.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_med_2:T3DRES',
                            'f3d_gouttedo.slf',
                            eps=[1.E-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_med_1:T3DRES',
                            'vnv_med_2:T3DRES',
                            eps=[1.E-12])

    def _post(self):
        """
        Post-treatment processes
        """

