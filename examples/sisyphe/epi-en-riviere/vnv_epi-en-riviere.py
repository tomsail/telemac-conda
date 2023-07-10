
"""
Validation script for epi-en-riviere
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

        # epi-en-riviere T2D+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_epi-en-riviere.cas')


        # epi-en-riviere T2D+SIS parallel mode
        cas = TelemacCas('t2d_epi-en-riviere.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_epi-en-riviere_par.cas',
                       cas=cas)

        del cas


        # epi-en-riviere T2D+SIS parallel mode
        cas = TelemacCas('t2d_epi-en-riviere_concat.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_concat',
                       'telemac2d',
                       't2d_epi-en-riviere_concat_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_epi-en-riviere.slf',
                            eps=[3e-0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_epi-en-riviere.slf',
                            eps=[666e-0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[666e-0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_epi-en-riviere.slf',
                            eps=[1e-0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_epi-en-riviere.slf',
                            eps=[1e-0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_2:T2DRES',
                            'vnv_concat:T2DRES',
                            eps=[1e-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_2:SISRES',
                            'vnv_concat:SISRES',
                            eps=[1e-12])


    def _post(self):
        """
        Post-treatment processes
        """

