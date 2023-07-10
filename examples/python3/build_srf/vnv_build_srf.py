
"""
Validation script for converter
"""
from vvytel.vnv_study import AbstractVnvStudy


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

        # Generation of a boundary SELAFIN file to set data on the boundary
        self.add_command('vnv_generate_bnd',
                         'converter.py generate_bnd geometry_bnd.cli geometry_bnd.slf t3d_hycom-2m_2017-08-01_2017-08-02_20_21_-119-118.slf out_BND.slf')

        # Generation of a boundary SELAFIN file to set data from tomawac on the boundary
        self.add_command('vnv_generate_bnd_2d',
                         "converter.py generate_bnd " +
                         "--varnames=\"WAVE HEIGHT HM0;MEAN DIRECTION;WATER DEPTH;FORCE FX;FORCE FY\" " +
                         "--varunits=\"M;DEG;M;M/S2;M/S2\" " +
                         "geo_beach.cli geo_beach.slf tom_plage_res.slf tom_plage_lqd.lqd")

        # Generation of an atmospheric file
        self.add_command('vnv_generate_atm',
                         'converter.py generate_atm geometry.slf ecmwf_20170801_20170802_20-119_21-118.slf out_ATM.slf')

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """
