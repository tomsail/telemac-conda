
"""
Validation script for Equilibrium channel
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of Equilibrium channel
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['mascaret','courlis']

    def _pre(self):
        """
        Defining the studies
        """
        # Soni test case permanent kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'sarap.xcas')

        # Soni test case non-permanent kernel
        self.add_study('vnv_2',
                       'mascaret',
                       'rezodt.xcas')

    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """
