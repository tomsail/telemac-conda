
"""
Validation script for waq2d_o2
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
        self.rank = 1
        self.tags = ['telemac2d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # water quality- dissolved O2 process
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_waq2d_o2.cas')


        # water quality- dissolved O2 process
        cas = TelemacCas('t2d_waq2d_o2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_waq2d_o2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_waq2d_o2.slf',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_waq2d_o2.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-15])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        from postel.plot1d import plot1d

        # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res = TelemacFile(vnv_1_t2dres)

        #Plotting mesh
        vnv_plot2d('',
                   res,
                   plot_mesh=True,
                   fig_size=(7., 7.),
                   fig_name='img/res_mesh')

        # Plotting final condition for Traceur 1 (at -1)
#        vnv_plot2d('Traceur 1',
#                   res,
#                   poly=[[10, 110], [210, 110]],
#                   record=-1,
#                   filled_contours=True,
#                   x_label='$x$ (m)', y_label='$z$ (m)',
#                   cbar_label='Tracer 1',
#                   fig_size=(20, 5),
#                   fig_name='img/res_ta1')

        #----------------------------------------------------------------------
        # Comparison of tracers (1D slice):

        # Getting array of time values from file 
        times = res.times/3600.

        # List of points we want to display
        points = [[0., 0.]]

        # Getting tracer values over time for each point of extraction
        data1 = res.get_timeseries_on_points('DISSOLVED O2', points)
        data2 = res.get_timeseries_on_points('ORGANIC LOAD', points)
        data3 = res.get_timeseries_on_points('NH4 LOAD', points)

        #Initialising figure
        fig, ax = plt.subplots(figsize=(10,5))

        # for each plot adding a history plot with a label node_(law)
        plot1d(ax, times, data1[0, :],
               x_label='$t$ (h)',
               y_label='tracer concentration',
               plot_label='Dissolved O$_2$ in mg/l')

        # for each plot adding a history plot with a label node_(law)
        plot1d(ax, times, data2[0, :],
               x_label='$t$ (h)',
               y_label='tracer concentration',
               plot_label='Organic load in mg/l')

        # for each plot adding a history plot with a label node_(law)
        plot1d(ax, times, data3[0, :],
               x_label='$t$ (h)',
               y_label='tracer concentration',
               plot_label='NH$_4$ load in mg/l')

        # Displaying legend
        ax.legend()

        fig_name = 'img/res_tracers'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        res.close()
