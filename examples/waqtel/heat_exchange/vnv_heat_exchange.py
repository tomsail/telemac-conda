
"""
Validation script for heat_exchange
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
        self.tags = ['telemac3d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # heat exchange without rain nor wind has been deleted
        # as it does not work well with NON-HYDROSATIC VERSION = YES


        # heat exchange rain wind scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_heat_exchange_rain_wind.cas')


        # heat exchange rain wind parallel mode
        cas = TelemacCas('t3d_heat_exchange_rain_wind.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_heat_exchange_rain_wind_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_heat_exchange_rain_wind.slf',
                            eps=[8.E-5, 4.E-3, 5.E-3, 2.E-4, 0.048])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_heat_exchange_rain_wind.slf',
                            eps=[6.E-5, 0.01, 0.025, 7.E-4, 0.57])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[8.E-5, 0.011, 0.025, 7.E-4, 0.6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
        import matplotlib.pyplot as plt
        from postel.plot1d import plot1d
        from postel.plot2d import plot2d_triangle_mesh, plot2d_scalar_filled_contour
        import numpy as np

        # Getting files
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        res = TelemacFile(vnv_3_t3dres)

        node = res.get_closest_node([0, 0], plane=res.nplan-1)

        #Plotting Z on [0, 0] over records range(0, res.ntimestep)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res,
                'ELEVATION Z',
                nodes=[node],
                x_factor=1./86400.,
                fig_size=(12, 7),
                fig_name='img/res_timeseries',
                x_label='time (days)',
                y_label='$z$ (m)')

        #Plotting Z on [0, 0] over records range(0, res.ntimestep)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res,
                'ELEVATION Z',
                nodes=[node -i*res.npoin2 for i in range(res.nplan)],
                nodes_labels=['plane {}'.format(i) for i in range(res.nplan)],
                x_factor=1./86400.,
                fig_size=(12, 7),
                fig_name='img/res_timeseries_all_layers')


        #Plotting mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   plot_mesh=True,
                   fig_size=(7, 6.5),
                   fig_name='img/res_mesh')

        #Plotting 3D mesh section (vertical mesh)
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[-100, 0], [100, 0]],
                   record=0,
                   plot_mesh=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   fig_size=(12, 7),
                   fig_name='img/res_mesh_section')

        # Plotting VELOCITY U at -1
        vnv_plot2d('VELOCITY U',
                   res,
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/res_velocity')

        # Plotting final for temperature (at -1)
        vnv_plot2d('TEMPERATURE',
                   res,
                   poly=[[-100., 0.], [100, 0.]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='$T$ ($^{\circ}$C)',
                   fig_size=(20, 5),
                   fig_name='img/res_temp_section')

        #----------------------------------------------------------------------
        # Comparison of tracers (1D slice):

        # Getting array of time values from file (in days)
        times = res.times/86400.

        # List of points we what to display
        points = [[0., 0., 0.], [0., 0., 2.5], [0., 0., 4.9]]

        # Getting tracer values over time for each point of extraction
        data = res.get_timeseries_on_points('TEMPERATURE', points)

        #Initialising figure
        fig, ax = plt.subplots(figsize=(10,5))

        # for each plot adding a history plot with a label node_(law)
        plot1d(ax, times, data[0, :],
               x_label='$t$ (days)',
               y_label='$T$ ($^{\circ}$C)',
               plot_label='T bottom with rain and wind')

        # for each plot adding a history plot with a label node_(law)
        plot1d(ax, times, data[1, :],
               x_label='$t$ (days)',
               y_label='$T$ ($^{\circ}$C)',
               plot_label='T mid with rain and wind')

        # for each plot adding a history plot with a label node_(law)
        plot1d(ax, times, data[2, :],
               x_label='$t$ (days)',
               y_label='$T$ ($^{\circ}$C)',
               plot_label='T surface with rain and wind')

        # Displaying legend
        ax.legend()

        fig_name = 'img/res_temp_evol'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #----------------------------------------------------------------------
        # Comparison of tracers on vertical segment over time:

        # 2d point where you want the vertical extraction
#        points_to_extract=np.array([0., 0.])
        points_to_extract = [0., 0.]

        # time serie extraction of elevation variable
        timeseries_Z = \
        res.get_timeseries_on_vertical_segment('ELEVATION Z', points_to_extract)

        # time serie extraction of elevation variable
        timeseries_temp = \
        res.get_timeseries_on_vertical_segment('TEMPERATURE', points_to_extract)

#        data = timeseries_temp[0,:]

        # creation of a mesh from the elevation value and curvilinear coordinate of the polyline
        X, Y = np.meshgrid(res.times, timeseries_Z[:,0])

        # plot of the time serie extraction
        fig, axes = plt.subplots(nrows=1, ncols=1)
#        im = axes.pcolormesh(X/3600., Y, timeseries_vel,shading='gouraud')
        im = axes.pcolormesh(X/86400., timeseries_Z, timeseries_temp)
        axes.set_xlabel('$t$ (days)')
        axes.set_ylabel('$z$ (m)')
        fig.colorbar(im, ax=axes, label='$T$ ($^{\circ}$C)')

        # Displaying legend
#        axes.legend()

        fig_name = 'img/res_temp_segment'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Closing files
        res.close()
