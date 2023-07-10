
"""
Validation script for selafin
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
        self.tags = ['stbtel']

    def _pre(self):
        """
        Defining the studies
        """

        # selafin scalar mode
        self.add_study('vnv_1',
                       'stbtel',
                       'stb_selafin.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

