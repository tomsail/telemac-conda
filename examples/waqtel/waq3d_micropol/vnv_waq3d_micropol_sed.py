
"""
Validation script for sedimentation in waqtel 3D
"""
import matplotlib.pyplot as plt
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from postel.plot_vnv import vnv_plot2d
from postel.plot1d import plot1d

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['telemac3d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # Sedimentation cas file

        cas = TelemacCas('t3d_waq3d_micropol_sed.cas', get_dico('telemac3d'))

        self.add_study('vnv_sed_seq',
                       'telemac3d',
                       't3d_waq3d_micropol_sed.cas',
                       cas=cas)

        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_sed_par',
                       'telemac3d',
                       't3d_waq3d_micropol_sed_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_seq:T3DRES',
                            'f3d_waq3d_micropol_sed.slf',
                            eps=[1.E-13])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_par:T3DRES',
                            'f3d_waq3d_micropol_sed.slf',
                            eps=[1.E-13])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_seq:T3DRES',
                            'vnv_sed_par:T3DRES',
                            eps=[1.E-13])

    def _post(self):
        """
        Post-treatment processes
        """

        t3dres = self.get_study_file('vnv_sed_seq:T3DRES')
        res = TelemacFile(t3dres)

        #Plotting horizontal mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   plot_mesh=True,
                   fig_size=(20, 5),
                   fig_name='img/sed/res_mesh')

        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[0., 0.0], [50., 0.]],
                   record=-1,
                   plot_mesh=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   fig_size=(20, 5),
                   fig_name='img/sed/res_mesh_section')

        # Plotting tracers at different times

        vnv_plot2d(\
                   'SUSPENDED LOAD',
                   res,
                   record=-1,
                   poly=[[0., 0.], [50, 0.]],
                   fig_size=(20, 5),
                   fig_name="img/sed/SS_f",
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='g/l',
                   filled_contours=True)

        vnv_plot2d(\
                   'BED SEDIMENTS',
                   res,
                   record=-1,
                   xlim=[0, 50],
                   fig_size=(20, 5),
                   fig_name="img/sed/SF_f",
                   x_label='$x$ (m)', y_label='$y$ (m)',
                   cbar_label='kg/m$^2$',
                   filled_contours=True)

        vnv_plot2d('SUSPENDED LOAD',
                   res,
                   poly=[[0., 0.], [50, 0.]],
                   record=0,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='g/l',
                   fig_size=(20, 5),
                   fig_name='img/sed/SS_i')

        # Comparison of tracers (1D slice):

        # Getting array of time values from file
        times = res.times

        # List of points we want to display
        points = [[25., 0., 0], [25., 0., 0.5], [25., 0., 1.]]

        data1 = res.get_timeseries_on_points('SUSPENDED LOAD', points)
        data2 = res.get_timeseries_on_points('BED SEDIMENTS', points)
        fig, ax = plt.subplots(figsize=(10, 5))

        # for each plot adding a history plot with a label node_(law)
        plot1d(ax, times, data1[2, :],
               color='lime',
               x_label='$t$(s)',
               y_label='tracer concentration',
               plot_label='SPM ($SS$) ($z$ = 1 m)')

        plot1d(ax, times, data1[1, :],
               color='green',
               x_label='$t$ (s)',
               y_label='tracer concentration',
               plot_label='SPM ($SS$) ($z$ = 0.5 m)')

        plot1d(ax, times, data1[0, :],
               color='olive',
               x_label='$t$ (s)',
               y_label='tracer concentration',
               plot_label='SPM ($SS$) ($z$ = 0 m)')

        plot1d(ax, times, data2[0, :],
               linewidth=2,
               color='red',
               x_label='$t$ (s)',
               y_label='tracer concentration',
               plot_label='Bed sediment ($SF$) ($z$ = 0 m)')

        plot1d(ax, [250, 250], [0, 1],
               linestyle='dashed',
               color='blue',
               x_label='$t$ (s)',
               y_label='tracer concentration',
               plot_label='$h/w$')

        ax.legend()
        fig_name = 'img/sed/res_sed'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')
