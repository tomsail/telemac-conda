
"""
Validation script for test16_choc
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
        self.tags = ['mascaret']

    def _pre(self):
        """
        Defining the studies
        """

        # Test16_choc steady kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'sarap.xcas')


        # Test16_choc unsteady subcritical kernel
        self.add_study('vnv_2',
                       'mascaret',
                       'rezo.xcas')


        # Test16_choc transcritical kernel
        self.add_study('vnv_3',
                       'mascaret',
                       'mascaret.xcas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

