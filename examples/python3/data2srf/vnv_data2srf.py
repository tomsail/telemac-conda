
"""
Validation script for data2srf
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
        self.tags = ['python3']

    def _pre(self):
        """
        Defining the studies
        """

        # Extracting ecmwf data into a Serafin file
        self.add_command('vnv_ecmwf2srf',
                         'converter.py ecmwf2srf --from "2017-08-01" --stop "2017-08-02" --bl "(20, -119)" --tr "(21,-118)" --dataset ecmwf_20170801_20170802_20-119_21-118.nc --stream oper ecmwf_20170801_20170802_20-119_21-118.slf')


        # Extracting gebco data into a Serafin file
        self.add_command('vnv_gebco2srf',
                         'converter.py gebco2srf GEBCO2014_0.0_0.0_10.0_10.0_30Sec_ESRIASCII.asc')


        # Extracting hycom data into a Serafin file
        self.add_command('hycom2srf-2d',
                         'converter.py hycom2srf --from 2017-08-01 --stop 2017-08-02 --bl 20,-119 --tr 21,-118 -r t2d_hycom-2m_2017-08-01_2017-08-02_20_21_-119-118.slf --2d')


        # Extracting hycom data into a Serafin file
        self.add_command('hycom2srf-3d',
                         'converter.py hycom2srf --from 2017-08-01 --stop 2017-08-02 --bl 20,-119 --tr 21,-118 -r t3d_hycom-2m_2017-08-01_2017-08-02_20_21_-119-118.slf')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

