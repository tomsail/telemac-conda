
"""
Validation script for erosionflume
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # ErosionFlume parallel mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3dsed_erosionflume_mixte_05.cas')


        # ErosionFlume parallel mode
        cas = TelemacCas('t3dsed_erosionflume_mixte_05.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3dsed_erosionflume_mixte_05_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_erosionflume_mixte-05.slf',
                            eps=[1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_erosionflume_mixte-05.slf',
                            eps=[1.E-6, 1.E-5, 1.E-5, 2.E-4, 1.E-6, 0.002, 1.E-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-6, 1.E-5, 1.E-5, 2.E-4, 1.E-6, 0.002, 1.E-4])


    def _post(self):
        """
        Post-treatment processes
        """

