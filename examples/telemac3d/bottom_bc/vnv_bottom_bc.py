
"""
Validation script for bottom_bc
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
        self.rank = 3
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # Bottom inlet scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_bottom_inlet.cas')


        # Bottom inlet parallel mode
        cas = TelemacCas('t3d_bottom_inlet.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_bottom_inlet_par.cas',
                       cas=cas)

        del cas


        # Bottom source scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_bottom_source.cas')


        # Bottom source parallel mode
        cas = TelemacCas('t3d_bottom_source.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_bottom_source_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_bottom_inlet.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_bottom_inlet.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_bottom_source.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_bottom_source.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """

        from data_manip.computation.triangulation import triangulation_from_data
        import matplotlib.pyplot as plt
        import numpy as np
        from postel.plot_actions import plot1d
        from postel.plot_vnv import vnv_plot2d
        from postel.plot2d import plot2d_triangle_mesh, plot2d_vectors, plot2d_scalar_filled_contour

        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        res_vnv_3_t3dres = TelemacFile(vnv_3_t3dres)
        vnv_3_t3dhyd = self.get_study_file('vnv_3:T3DHYD')
        res_vnv_3_t3dhyd = TelemacFile(vnv_3_t3dhyd)

        # Figures for inlet (vnv_1)

        # Plotting profile of water depth
        fig, ax = plt.subplots(1, 1, figsize=(12, 5))

        poly = [[0., 2000.], [4000., 2000.]]
        points = poly
        poly_number=[50]

        _, abs_curv, data = res_vnv_1_t3dhyd.get_timeseries_on_polyline('WATER DEPTH', poly)

        for i in range(0, res_vnv_1_t3dhyd.ntimestep):
            plot1d(ax, abs_curv, data[:, i],\
                   plot_label='$t$ = '+ str(res_vnv_1_t3dhyd.times[i]) +' s')
            ax.legend()
        fig_name = 'img/water_depth_inlet'
        print(" "*8+"~> Plotting "+fig_name)
        ax.set_xlabel('$x$ (m)')
        ax.set_ylabel('$h$ (m)')
        plt.savefig(fig_name)
        plt.close('all')

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dhyd,
                   plot_mesh=True,
                   fig_size=(12, 12),
                   fig_name='img/Mesh_inlet')

        #Plotting vertical mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   record=0,
                   y_label='z (m)',
                   plot_mesh=True,
                   fig_size=(12, 4),
                   fig_name='img/MeshV_inlet')

        # Plotting horizontal slice of vectors velocity
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 10),
                   fig_name='img/veloH_inlet')

        # Plotting vertical slice of vertical velocity at t = 180 s
        vnv_plot2d('VELOCITY W',
                   res_vnv_1_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   y_label='z (m)',
                   fig_title='$t$ = 180 s',
                   record=1,
                   cbar_label='Velocity along $z$ (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 4),
                   fig_name='img/veloW_inlet_180s')

        # Plotting vertical slice of vertical velocity at t = 540 s
        vnv_plot2d('VELOCITY W',
                   res_vnv_1_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   y_label='z (m)',
                   fig_title='$t$ = 540 s',
                   record=3,
                   cbar_label='Velocity along $z$ (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 4),
                   fig_name='img/veloW_inlet_540s')

        # Plotting vertical slice of vertical velocity at t = 900 s
        vnv_plot2d('VELOCITY W',
                   res_vnv_1_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   y_label='z (m)',
                   fig_title='$t$ = 900 s',
                   record=10,
                   cbar_label='Velocity along $z$ (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 4),
                   fig_name='img/veloW_inlet_900s')

        # Plotting vertical slice of vertical velocity at final time step t = 1800 s
        vnv_plot2d('VELOCITY W',
                   res_vnv_1_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   y_label='z (m)',
                   fig_title='$t$ = 1,800 s',
                   record=10,
                   cbar_label='Velocity along $z$ (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 4),
                   fig_name='img/veloW_inlet_1800s')

        _, abs_curv, values_poly_Z=\
        res_vnv_1_t3dres.get_data_on_vertical_plane('ELEVATION Z', -1, points, poly_number)
        mesh = triangulation_from_data(abs_curv, values_poly_Z)
        _, abs_curv2, velox=\
        res_vnv_1_t3dres.get_data_on_vertical_plane('VELOCITY U', -1, points, poly_number)
        _, abs_curv2, veloy=\
        res_vnv_1_t3dres.get_data_on_vertical_plane('VELOCITY V', -1, points, poly_number)
        _, abs_curv2, veloz=\
        res_vnv_1_t3dres.get_data_on_vertical_plane('VELOCITY W', -1, points, poly_number)
        fig2, axes2 = plt.subplots(figsize=(20, 8))
        velocity = np.sqrt(velox.flatten()**2 + veloy.flatten()**2 + veloz.flatten()**2)
        plot2d_scalar_filled_contour(fig2, axes2, mesh, velocity, data_name='Velocity (m/s)')
        plot2d_vectors(fig2, axes2, mesh, velox.flatten(), veloz.flatten(),
                       grid_resolution=[29, 29], data_name='Velocity', color='k',
                       grid_xlim=[1500, 2500], grid_ylim=[-100, 0], scale = 50)
        axes2.set(xlim=[1500, 2500], ylim=[-100, 0])
        plt.xlabel('$x$ (m)')
        plt.ylabel('$z$ (m)')

        filefig='img/vectorial_inlet'
        print('        ~> Plotting',filefig)
        plt.savefig(filefig)
        plt.close('all')

        points2 = [2000., 2000.]

        timeseries_z = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('ELEVATION Z', points2)
        timeseries_vel = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('VELOCITY W', points2)

        fig, ax = plt.subplots(figsize=(4, 7))

        # Plotting vertical section
        for i in range(1, res_vnv_3_t3dhyd.ntimestep):
            plot1d(ax, timeseries_vel[:,i], timeseries_z[:,i],
                   x_label='Velocity along $z$ (m/s)', y_label='Elevation $z$ (m)',
                   plot_label='$t$ = '+ str(res_vnv_3_t3dhyd.times[i]) +' s')
            ax.legend()

        fig_name = 'img/velocityW_profile_inlet'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #--------------------------------------------------
        # Figures for source (vnv_3)

        # Plotting profile of water depth
        fig, ax = plt.subplots(1, 1, figsize=(12, 5))

        poly = [[0., 2000.], [4000., 2000.]]

        _, abs_curv, data = res_vnv_3_t3dhyd.get_timeseries_on_polyline('WATER DEPTH', poly)

        for i in range(0, res_vnv_3_t3dhyd.ntimestep):
            plot1d(ax, abs_curv, data[:, i],\
                   plot_label='$t$ = '+ str(res_vnv_3_t3dhyd.times[i]) +' s')
            ax.legend()
        fig_name = 'img/water_depth_source'
        print(" "*8+"~> Plotting "+fig_name)
        ax.set_xlabel('$x$ (m)')
        ax.set_ylabel('$h$ (m)')
        plt.savefig(fig_name)
        plt.close('all')

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_3_t3dhyd,
                   plot_mesh=True,
                   fig_size=(12, 12),
                   fig_name='img/Mesh_source')

        #Plotting vertical mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_3_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   record=0,
                   y_label='z (m)',
                   plot_mesh=True,
                   fig_size=(12, 4),
                   fig_name='img/MeshV_source')

        # Plotting horizontal slice of vectors velocity
        vnv_plot2d('VELOCITY',
                   res_vnv_3_t3dres,
                   plane=res_vnv_3_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 10),
                   fig_name='img/veloH_source')

        # Plotting vertical slice of vertical velocity at t = 180 s
        vnv_plot2d('VELOCITY W',
                   res_vnv_3_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   y_label='z (m)',
                   fig_title='$t$ = 180 s',
                   record=1,
                   cbar_label='Velocity along $z$ (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 4),
                   fig_name='img/veloW_source_180s')

        # Plotting vertical slice of vertical velocity at t = 540 s
        vnv_plot2d('VELOCITY W',
                   res_vnv_3_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   y_label='z (m)',
                   fig_title='$t$ = 540 s',
                   record=3,
                   cbar_label='Velocity along $z$ (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 4),
                   fig_name='img/veloW_source_540s')

        # Plotting vertical slice of vertical velocity at t = 900 s
        vnv_plot2d('VELOCITY W',
                   res_vnv_3_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   y_label='z (m)',
                   fig_title='$t$ = 900 s',
                   record=10,
                   cbar_label='Velocity along $z$ (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 4),
                   fig_name='img/veloW_source_900s')

        # Plotting vertical slice of vertical velocity at final time step t = 1800 s
        vnv_plot2d('VELOCITY W',
                   res_vnv_3_t3dres,
                   poly=[[0., 2000.], [4000., 2000.]],
                   y_label='z (m)',
                   fig_title='$t$ = 1,800 s',
                   record=10,
                   cbar_label='Velocity along $z$ (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 4),
                   fig_name='img/veloW_source_1800s')

        _, abs_curv, values_poly_Z=\
        res_vnv_3_t3dres.get_data_on_vertical_plane('ELEVATION Z', -1, points, poly_number)
        mesh = triangulation_from_data(abs_curv, values_poly_Z)
        _, abs_curv2, velox=\
        res_vnv_3_t3dres.get_data_on_vertical_plane('VELOCITY U', -1, points, poly_number)
        _, abs_curv2, veloy=\
        res_vnv_3_t3dres.get_data_on_vertical_plane('VELOCITY V', -1, points, poly_number)
        _, abs_curv2, veloz=\
        res_vnv_3_t3dres.get_data_on_vertical_plane('VELOCITY W', -1, points, poly_number)
        fig2, axes2 = plt.subplots(figsize=(20, 8))
        velocity = np.sqrt(velox.flatten()**2 + veloy.flatten()**2 + veloz.flatten()**2)
        plot2d_scalar_filled_contour(fig2, axes2, mesh, velocity, data_name='Velocity (m/s)')
        plot2d_vectors(fig2, axes2, mesh, velox.flatten(), veloz.flatten(),
                       grid_resolution=[29, 29], data_name='Velocity', color='k',
                       grid_xlim=[1500, 2500], grid_ylim=[-100, 0], scale = 50)
        axes2.set(xlim=[1500, 2500], ylim=[-100, 0])
        plt.xlabel('$x$ (m)')
        plt.ylabel('$z$ (m)')

        filefig='img/vectorial_source'
        print('        ~> Plotting',filefig)
        plt.savefig(filefig)
        plt.close('all')

        points2 = [2000., 2000.]

        timeseries_z = res_vnv_3_t3dres.get_timeseries_on_vertical_segment('ELEVATION Z', points2)
        timeseries_vel = res_vnv_3_t3dres.get_timeseries_on_vertical_segment('VELOCITY W', points2)

        fig, ax = plt.subplots(figsize=(4, 7))

        # Plotting vertical section
        for i in range(1, res_vnv_3_t3dhyd.ntimestep):
            plot1d(ax, timeseries_vel[:,i], timeseries_z[:,i],
                   x_label='Velocity along $z$ (m/s)', y_label='Elevation $z$ (m)',
                   plot_label='$t$ = '+ str(res_vnv_3_t3dhyd.times[i]) +' s')
            ax.legend()

        fig_name = 'img/velocityW_profile_source'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        res_vnv_1_t3dres.close()
        res_vnv_1_t3dhyd.close()
        res_vnv_3_t3dres.close()
        res_vnv_3_t3dhyd.close()
