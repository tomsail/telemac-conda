
"""
Validation script for convergence
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # convergence scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_convergence.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_convergence.slf',
                            eps=[1e-4])


    def _post(self):
        """
        Post-treatment processes
        """
