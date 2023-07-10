
"""
Validation script for solitary
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
        self.tags = ['postel3d']

    def _pre(self):
        """
        Defining the studies
        """

        # solitary scalar mode
        self.add_study('vnv_1',
                       'postel3d',
                       'p3d.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """
        from os import path
        # Checking that all the horizontal slice where copied back
        for i in range(3):
            file_name = 'hor_{0:03d}'.format(i+1)
            print ("  ~> Checking that {} is there".format(file_name))
            assert(path.exists(path.join(self.get_vnv_working_dir('vnv_1'), file_name)))
        # Checking that all the vertical slice where copied back
        for i in range(2):
            for j in range(7):
                file_name = 'ver_{0:03d}-{0:03d}'.format(i+1, j+1)
                print ("  ~> Checking that {} is there".format(file_name))
                assert(path.exists(path.join(self.get_vnv_working_dir('vnv_1'), file_name)))


    def _post(self):
        """
        Post-treatment processes
        """

