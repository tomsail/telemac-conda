
"""
Validation script for lazare-t2d
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
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # St-Lazare T2D+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_lazare.cas')


        # St-Lazare T2D+SIS scalar mode
        cas = TelemacCas('t2d_lazare.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_lazare_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_lazare.slf',
                            eps=[1.E-6, 1.E-6, 1.E-6, 4.E-5, 4.E-5, 1.E-5, 1.E-9, 1.E-6, 1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_lazare.slf',
                            eps=[1.E-6, 1.E-6, 1.E-6, 4.E-5, 4.E-5, 1.E-5, 1.E-9, 1.E-6, 1.E-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[1.E-6, 1.E-6, 1.E-6, 4.E-5, 4.E-5, 1.E-5, 1.E-9, 1.E-6, 1.3E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_lazare.slf',
                            eps=[1.E-6, 1.E-6, 1.E-6, 4.E-5, 4.E-5, 1.E-7, 1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_lazare.slf',
                            eps=[1.E-6, 1.E-6, 1.E-6, 4.E-5, 4.E-5, 1.E-7, 1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-6, 1.E-6, 1.E-6, 4.E-5, 4.E-5, 1.E-7, 1.E-5])


    def _post(self):
        """
        Post-treatment processes
        """

