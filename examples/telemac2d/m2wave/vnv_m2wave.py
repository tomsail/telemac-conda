
"""
Validation script for m2wave
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from utils.exceptions import TelemacException

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of m2wave
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """
        # scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_m2wave.cas')
        # parallel mode
        cas = TelemacCas('t2d_m2wave.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_m2wave_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Check epsilons
        """
        import numpy as np
        from os import path
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_m2wave.slf',
                            eps=[1.0e-06])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_m2wave.slf',
                            eps=[1.0e-06])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.0e-06])

        # Check PHASE/AMPLITUDES:
        vnv_1_phase_file = self.get_study_file('vnv_1:T2DRF1')
        res_data = np.loadtxt(vnv_1_phase_file, skiprows=2, usecols=(1, 2))
        res_ampl = [float(res_data[i, 0]) for i in range(res_data.shape[0])]
        res_phas = [float(res_data[i, 1]) for i in range(res_data.shape[0])]

        ref_ampl = np.array([1.29, 2.23, 2.91, 3.1, 2.01, 3.84, 1.29, 1.99])
        ref_phas = np.array([15.3, 344.3, 335.6, 318.4, 228.2, 183, 105.8, 114.5])

        if not np.allclose(res_ampl, ref_ampl, rtol=.5, atol=.1):
            raise TelemacException("Epsilon reached in PHASE/AMPLITUDES")
        if not np.allclose(res_phas, ref_phas, rtol=.5, atol=.1):
            raise TelemacException("Epsilon reached in PHASE/AMPLITUDES")


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_history, vnv_plot2d
        # Getting files
        res_file = self.get_study_file('vnv_1:T2DRES')
        res = TelemacFile(res_file)

        # Plot 1d timeseries:
        vnv_plot1d_history(\
            'FREE SURFACE', res,
            fig_size=(12, 5),
            points=[[-280000, 200000]],
            x_label='time (s)',
            y_label='Elevation',
            fig_name='img/FreeSurfaceTimeSeries')

        # Plot mesh:
        vnv_plot2d(\
            '', res,
            fig_size=(6, 5),
            record=-1,
            x_factor=1/10000.,
            y_factor=1/10000.,
            x_label='$x/10^4$',
            y_label='$y/10^4$',
            fig_name='img/Mesh',
            plot_mesh=True)

        # Plot bottom:
        vnv_plot2d(\
            'BOTTOM', res,
            fig_size=(8, 6),
            record=-1,
            cbar_label='Bottom elevation (m)',
            x_factor=1/10000.,
            y_factor=1/10000.,
            x_label='$x/10^4$',
            y_label='$y/10^4$',
            fig_name='img/Bathy',
            filled_contours=True)

        # Plot free surface:
        vnv_plot2d(\
            'FREE SURFACE', res,
            fig_size=(8, 6),
            record=3,
            cbar_label='Free surface (m)',
            x_factor=1/10000.,
            y_factor=1/10000.,
            x_label='$x/10^4$',
            y_label='$y/10^4$',
            fig_name='img/FreeSurfacet3',
            filled_contours=True)

        vnv_plot2d(\
            'FREE SURFACE', res,
            fig_size=(8, 6),
            record=-1,
            cbar_label='Free surface (m)',
            x_factor=1/10000.,
            y_factor=1/10000.,
            x_label='$x/10^4$',
            y_label='$y/10^4$',
            fig_name='img/FreeSurfacetf',
            filled_contours=True)

        vnv_plot2d(\
            'AMPLI PERIOD  1', res,
            time=178800,
            cbar_label='Amplitude (m)',
            fig_title='Amplitude wave at t=178800s',
            fig_size=(8, 6),
            x_factor=1/10000.,
            y_factor=1/10000.,
            x_label='$x/10^4$',
            y_label='$y/10^4$',
                   vmin=0, vmax=4, nv=21,
            fig_name='img/ampli',
            filled_contours=True)
        
        vnv_plot2d(\
            'PHASE PERIOD  1', res,
            time=178800,
            cbar_label='Phase (°)',
            fig_title='Phase wave at t=178800s',
            fig_size=(8, 6),
            x_factor=1/10000.,
            y_factor=1/10000.,
            x_label='$x/10^4$',
            y_label='$y/10^4$',
                   vmin=0, vmax=360, nv=21,
            fig_name='img/phase',
            filled_contours=True)

        # Closing files
        res.close()
