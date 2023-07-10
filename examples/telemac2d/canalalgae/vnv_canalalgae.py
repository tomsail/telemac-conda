
"""
Validation script for canalalgae
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
        self.rank = 4
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # canalalgae scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_canal_algae.cas')


        # canalalgae 4 proc
        cas = TelemacCas('t2d_canal_algae.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_canal_algae_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_canalalgae.slf',
                            eps=[1e-2])


    def _post(self):
        """
        Post-treatment processes
        """
        from data_manip.conversion.dat2vtu import convert_drogues_file_to_vtu
        drogues_file = self.get_study_file('vnv_1:T2DFLO')
        vtu_file = 'drogues.vtu'

        convert_drogues_file_to_vtu(drogues_file, vtu_file)
