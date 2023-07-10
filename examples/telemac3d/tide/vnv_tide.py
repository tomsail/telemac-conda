
"""
Validation script for tide
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # tide scalar mode type
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_tide-jmj_type.cas')


        # tide parallel mode type
        cas = TelemacCas('t3d_tide-jmj_type.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_tide-jmj_type_par.cas',
                       cas=cas)

        del cas


        # tide scalar mode real gen
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_tide-jmj_real_gen.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_tide-jmj_type.slf',
                            eps=[8.E-3, 3.E-2, 2.E-2, 2.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_tide-jmj_type.slf',
                            eps=[8.E-3, 3.E-2, 2.E-2, 2.E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[8.E-3, 3.E-2, 2.E-2, 2.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_tide-jmj_real_gen.slf',
                            eps=[4.0E-1, 3.1E0, 1.1E0, 7.E-2])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)

        node = res.get_closest_node([195000, 150000], plane=res.nplan-1)

        #Plotting Z on a node at the surface (res.nplan-1)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res,
                'ELEVATION Z',
                nodes=[node],
                fig_size=(12, 7),
                fig_name='img/res_timeseries_surface')

        #Plotting Z on a node for every layer
        vnv_plot1d_history(\
                'ELEVATION Z',
                res,
                'ELEVATION Z',
                nodes=[node -i*res.npoin2 for i in range(res.nplan)],
                fig_size=(12, 7),
                fig_name='img/res_timeseries_all_layers')

        #Plotting mesh. No annotate_bnd=True with TelemacFile?
#        vnv_plot2d('',
#                   res,
#                   plot_mesh=True,
##                   annotate_bnd=True,
#                   fig_size=(7.5, 7),
#                   fig_name='img/res_mesh')

        # Plotting mesh section
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[185500, 150000], [200500, 150000]],
                   record=-1,
                   plot_mesh=True,
                   x_label='s (m)', y_label='z (m CD)',
                   fig_size=(12, 7),
                   fig_name='img/res_mesh_section')

        # Plotting free surface elevation Z at -1 (last time step)
        vnv_plot2d('ELEVATION Z',
                   res,
                   record=-1,
                   plane=res.nplan-1,
                   cbar_label='Z (m CD)',
                   filled_contours=True,
#                   cmap_name='viridis',
                   fig_size=(7.5, 7),
                   fig_name='img/res_z_map')

        # Plotting magnitude of VELOCITY at -1 (last time step)
        vnv_plot2d('VELOCITY',
                   res,
                   plane=res.nplan-1,
                   record=res.ntimestep-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_size=(7.5, 7),
                   fig_name='img/res_velocity_surf')
        # If willing to plot VELOCITY U for X component of velocity
        # at graphic printout end -5 + viridis cmap,
        # replace lines by the following lines:
#        vnv_plot2d('VELOCITY U',
#                   record=res.ntimestep-5,
#                   cmap_name='viridis',

        # Plotting magnitude of VELOCITY at -1 (last time step) + vectors
        vnv_plot2d('VELOCITY',
                   res,
                   plane=res.nplan-1,
                   record=res.ntimestep-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True, vectors_scale=20,
                   grid_resolution=[15, 15],
                   fig_size=(7.5, 7),
                   fig_name='img/res_velocity_surf_vectors')

        # Plotting magnitude of VELOCITY at -1 (last time step) + streamlines
        vnv_plot2d('VELOCITY',
                   res,
                   plane=res.nplan-1,
                   record=res.ntimestep-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   streamlines=True, streamlines_density=3,
                   grid_resolution=[50, 50],
                   fig_size=(7.5, 7),
                   fig_name='img/res_velocity_surf_streamlines')

        # Getting files
        # Boundary conditions annotations do not seem to be available with TelemacFile?
        # Mask tidal flats: only available if water depth is present (only for 2D result file)!
        geom, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")
        r2d, _ = self.get_study_res('vnv_1:T3DHYD')


        # Plot mesh + write boundary conditions notes
        vnv_plot2d('',
            geom,
            fig_size=(7.5, 7),
            fig_name='img/geom_mesh',
            annotate_bnd=True,
            plot_mesh=True)

        # Plot water depth + mask tidal flats:
        vnv_plot2d('WATER DEPTH', r2d,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/r2d_water_depth',
            cbar_label='H (m)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot free surface elevation + mask tidal flats:
        vnv_plot2d('FREE SURFACE', r2d,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/r2d_elevation',
                   cbar_label='Z (m CD)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot averaged velocity + mask tidal flats:
        vnv_plot2d('VELOCITY', r2d,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/r2d_velocity',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot averaged velocity vectors + mask tidal flats:
        vnv_plot2d('VELOCITY', r2d,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/r2d_velocity_vectors',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True,
            vectors=True, vectors_scale=20,
            grid_resolution=[15, 15])

        # Plot averaged velocity streamlines + mask tidal flats:
        vnv_plot2d('VELOCITY', r2d,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/r2d_velocity_streamlines',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True,
            streamlines=True, streamlines_density=3,
            grid_resolution=[50, 50])

        # Plot surface velocity from 2D RESULT FILE + mask tidal flats:
        vnv_plot2d('M SURFACE', r2d,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/r2d_surface_velocity',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Closing files
        geom.close()
        r2d.close()
        res.close()
