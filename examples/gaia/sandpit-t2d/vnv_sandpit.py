
"""
Validation script for sandpit
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
        self.rank = 2
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # Run Sandpit with coupled gaia telemac
        self.add_study('vnv_scal',
                       'telemac2d',
                       't2d_sandpit.cas')


        # Run Sandpit with coupled gaia telemac
        cas = TelemacCas('t2d_sandpit.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_para',
                       'telemac2d',
                       't2d_sandpit_par.cas',
                       cas=cas)

        del cas


        # Run Sandpit with coupled gaia telemac
        self.add_study('vnv_bl8',
                       'telemac2d',
                       't2d_sandpit_bl8.cas')


        # Run Sandpit with coupled gaia telemac
        cas = TelemacCas('t2d_sandpit_bl8.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_bl8_para',
                       'telemac2d',
                       't2d_sandpit_bl8_par.cas',
                       cas=cas)

        del cas


        # Run Sandpit with coupled gaia telemac
        self.add_study('vnv_bl9',
                       'telemac2d',
                       't2d_sandpit_bl9.cas')


        # Run Sandpit with coupled gaia telemac
        cas = TelemacCas('t2d_sandpit_bl9.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_bl9_para',
                       'telemac2d',
                       't2d_sandpit_bl9_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_scal:GAIRES',
                            'gai_ref_sandpit.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_para:GAIRES',
                            'gai_ref_sandpit.slf',
                            eps=[1.3e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_scal:GAIRES',
                            'vnv_para:GAIRES',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_scal:T2DRES',
                            'f2d_sandpit.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_para:T2DRES',
                            'f2d_sandpit.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_scal:T2DRES',
                            'vnv_para:T2DRES',
                            eps=[1.3e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bl8:GAIRES',
                            'gai_ref_sandpit_bl8.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bl8_para:GAIRES',
                            'gai_ref_sandpit_bl8.slf',
                            eps=[1e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_bl8:GAIRES',
                            'vnv_bl8_para:GAIRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bl8:T2DRES',
                            'f2d_sandpit_bl8.slf',
                            eps=[1.3e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bl8_para:T2DRES',
                            'f2d_sandpit_bl8.slf',
                            eps=[1e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_bl8:T2DRES',
                            'vnv_bl8_para:T2DRES',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bl9:GAIRES',
                            'gai_ref_sandpit_bl9.slf',
                            eps=[1.1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bl9_para:GAIRES',
                            'gai_ref_sandpit_bl9.slf',
                            eps=[1e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_bl9:GAIRES',
                            'vnv_bl9_para:GAIRES',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bl9:T2DRES',
                            'f2d_sandpit_bl9.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bl9_para:T2DRES',
                            'f2d_sandpit_bl9.slf',
                            eps=[1e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_bl9:T2DRES',
                            'vnv_bl9_para:T2DRES',
                            eps=[1e-3])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d
        import matplotlib.pyplot as plt
        import numpy as np
        exp_data = np.loadtxt('experiment_data.csv',
                              delimiter=",",
                              comments="#",
                              skiprows=4)

        bed_init = exp_data[:, 1] * 500.
        bed_measured = exp_data[:, 2] * 500.
        exp_x = 10.*exp_data[:, 0] - 50.

        poly = [[50., 0.5], [130., 0.5]]
        res = TelemacFile(self.get_study_file('vnv_scal:GAIRES'))
        res_bl8 = TelemacFile(self.get_study_file('vnv_bl8:GAIRES'))
        res_bl9 = TelemacFile(self.get_study_file('vnv_bl9:GAIRES'))

        # Extraction result values
        _, abs_curv, values_poly = \
                res.get_timeseries_on_polyline(\
                'BOTTOM', poly)

        _, _, values_poly_bl8 = \
                res_bl8.get_timeseries_on_polyline(\
                'BOTTOM', poly)

        _, _, values_poly_bl9 = \
                res_bl9.get_timeseries_on_polyline(\
                'BOTTOM', poly)

        fig, ax = plt.subplots(figsize=(10, 7))

        plot1d(ax, exp_x, bed_init,
               plot_label='Initial bed', color='k')
        plot1d(ax, exp_x, bed_measured,
               plot_label='Measured', marker='*', color='b')
        plot1d(ax, abs_curv, 50.*values_poly[:, 20],
               plot_label='Modelled ref-conc 4', color='r')
        plot1d(ax, abs_curv, 50.*values_poly_bl8[:, 20],
               plot_label='Modelled Baillard', color='grey')
        plot1d(ax, abs_curv, 50.*values_poly_bl9[:, 20],
               plot_label='Modelled Dibajnia', color='brown')

        ax.set_xlabel('location(m)')
        ax.set_ylabel('Bed level (m)')
        ax.set_title('Comparison of model and experiment morphology')

        ax.legend(loc='lower right')

        fig_name = "img/Sandpit_2_profiles"
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        res.close()
        res_bl8.close()
        res_bl9.close()
