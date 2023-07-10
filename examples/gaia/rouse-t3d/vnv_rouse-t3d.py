
"""
Validation script for rouse-t3d
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
        self.tags = ['telemac3d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # rouse-t3d scalar mode T3D+GAI
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_rouse-t3d.cas')


        # rouse-t3d parallel mode T3D+GAI
        cas = TelemacCas('t3d_rouse-t3d.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 2)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_rouse-t3d_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_rouse-t3d.slf',
                            eps=[1.e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_rouse-t3d.slf',
                            eps=[1.e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_rouse-t3d.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_rouse-t3d.slf',
                            eps=[1.e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.e-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        from postel.plot1d import plot1d
        import matplotlib.pyplot as plt
        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)


        points = [2000., 0.]
        points2 = [[2000.,0.]]

        timeseries_z = res_vnv_1_t3dres.get_timeseries_on_vertical_segment(\
                'ELEVATION Z', points)
        timeseries_vel = res_vnv_1_t3dres.get_timeseries_on_vertical_segment(\
                'VELOCITY U', points)

        fig, ax = plt.subplots(figsize=(12, 7))

        # Plotting vertical section
        plot1d(ax, timeseries_vel[:,-1], timeseries_z[:,-1],
               x_label='Velocity U [m/s]',
               y_label='Elevation Z [m]',
               plot_label=\
         'Velocity profile on the vertical at {} at the last time step'\
         .format(points))

        ax.legend()

        fig_name = 'img/velocityU_profile'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        timeseries_conc = res_vnv_1_t3dres.get_timeseries_on_vertical_segment(\
                'COH SEDIMENT1', points)
        ustar = res_vnv_1_t3dhyd.get_timeseries_on_points('FRICTION VELOCIT', points2)

        #Plotting Rouse profile against analytical solution

        ws = -1.e-3
        vkarman = 0.4

        z = timeseries_z[:,-1]
        z0 = z[0]
        h = z[-1]-z[0]
        zh = []
        for i in range(len(z)):
            z[i] = z[i] - z0
        for i in range(len(z)):
            zh.append(z[i] / h)
            z[i] = z[i] + (0.01/30.) / (z[-1]-z[0])

        ssc = timeseries_conc[:,-1]
        Ca = ssc[2]

        Rouse=[]
        for i in range(len(z)-1):
            ssc[i] = ssc[i]/Ca
            Rouse.append(( (z[i]/z[2]) * ( (h-z[2])/(h-z[i]) ) ) ** (ws/(vkarman*ustar[0][-1])))
        Rouse.append(0.)

        fig, ax = plt.subplots(figsize=(12, 7))

        # Plotting 3d points
        plot1d(ax, ssc, zh,
               plot_label=\
               'Simulated results at point {} at the last time step'\
               .format(points))
        plot1d(ax, Rouse, zh,
               x_label='Normalized suspended concentration [-]',
               y_label='Normalized elevation Z [-]',
               plot_label=\
               'Analytical solution at point {} at the last time step'\
               .format(points))

        ax.legend()

        fig_name = 'img/suspconc_profile'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   fig_size=(10,1),
                   fig_name='img/mesh')


        # Plotting COH SEDIMENT1 at -1
        vnv_plot2d('COH SEDIMENT1',
                   res_vnv_1_t3dhyd,
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(15, 3),
                   fig_name='img/sediment_plan_view')


        # TODO: change cmap
        # Plotting vertical split
        vnv_plot2d(\
                'COH SEDIMENT1',
                res_vnv_1_t3dres,
                poly=[[-2500, 0], [2500, 0]],
                record=-1,
                filled_contours=True,
                fig_size=(12, 7),
                x_label='X (m)',
                y_label='Z elevation (m)',
                fig_name='img/sediment_section')

        # TODO: change cmap
        # Plotting vertical split
        vnv_plot2d(\
                'VELOCITY U',
                res_vnv_1_t3dres,
                poly=[[-2500, 0], [2500, 0]],
                record=-1,
                filled_contours=True,
                fig_size=(12, 7),
                x_label='X (m)',
                y_label='Z elevation (m)',
                fig_name='img/velocityU_section')

        # Closing files
        res_vnv_1_t3dres.close()
        res_vnv_1_t3dhyd.close()
