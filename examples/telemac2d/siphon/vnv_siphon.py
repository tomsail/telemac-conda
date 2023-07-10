
"""
Validation script for siphon
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # siphon scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_siphon.cas')


        # siphon parallel mode
        cas = TelemacCas('t2d_siphon.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_siphon_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_siphon.slf',
                            eps=[1e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_siphon.slf',
                            eps=[1e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
                # Getting files
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        bnd_file = self.get_study_file('vnv_1:T2DCLI')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo, bnd_file)
        
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        #TODO: Redo figures from documentation
        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   aspect_ratio='equal',
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(10, 4),
                   fig_name='img/Mesh')

        # Plotting TRACER 1 at 0
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=0,
                   aspect_ratio='equal',
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 3),
                   fig_name='img/tracor1_init')

        # Plotting tracer over polyline over records range(0, -1)
        vnv_plot1d_polylines(\
                'TRACER 1',
                res_vnv_1_t2dres,
                poly=[[0., 112.5], [550, 122.5]],
                record=[i for i  in range(0, res_vnv_1_t2dres.ntimestep, 4)],
                y_label='Tracer concentration (g/L)',
                fig_size=(12, 5),
                fig_title='Tracer concentration profil along (0.;112.5),(550.;112.5)',
                fig_name='img/profile_tracer')

        # Plotting tracer over polyline over records range(0, -1)
        vnv_plot1d_polylines(\
                'TRACER 2',
                res_vnv_1_t2dres,
                poly=[[0., 112.5], [550, 122.5]],
                record=[i for i  in range(0, res_vnv_1_t2dres.ntimestep, 4)],
                y_label='Tracer concentration (g/L)',
                fig_size=(12, 5),
                fig_title='Tracer concentration profil along (0.;112.5),(550.;112.5)',
                fig_name='img/profile_tracer2')

        # Plotting BOTTOM over polyline over records range(0, -1)
        vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                poly=[[0., 112.5], [550, 122.5]],
                record=[i for i  in range(0, res_vnv_1_t2dres.ntimestep, 4)],
                fig_size=(12, 5),
                fig_title='Free surface profil along (0.;112.5),(550.;112.5)',
                fig_name='img/free_surface')
        deltat = res_vnv_1_t2dres.times[1] - res_vnv_1_t2dres.times[0]
        for record in range(0, res_vnv_1_t2dres.ntimestep, 4):
            title = 'Tracer concentration at t = ' + str(int(record*deltat)) +' s'
            fig_file = 'img/tracer_' + str(int(record*deltat))
            vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=record,
                   filled_contours=True,
                   fig_size=(10, 4),
#                   vmin=30, vmax=105,
                   fig_name=fig_file,
#                       cbar = False,
                   cbar_label='Tracer concentration (g/L)',
                   fig_title=title)#,
#                   aspect_ratio='equal')
            title = 'Free Surface at t = ' + str(int(record*deltat)) +' s'
            fig_file = 'img/free_surf_' + str(int(record*deltat))
            vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=record,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name=fig_file,
                   cbar_label='Free surface (m)',
                   fig_title=title,
                   aspect_ratio='equal')
            title = 'Tracer concentration at t = ' + str(int(record*deltat)) +' s'
            fig_file = 'img/tracer2_' + str(int(record*deltat))
            vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=record,
                   filled_contours=True,
                   vmin=0, vmax=205,
                   fig_size=(10, 8),
                   fig_name=fig_file,
                   cbar_label='Tracer concentration (g/L)',
                   fig_title=title,
                   aspect_ratio='equal')

        for record in [2, 10, 20 ]:
                #Plot vectors
            title = 'Velocity at t = ' + str(int(record*deltat)) +' s'
            fig_file = 'img/velocity_' + str(int(record*deltat))
            
            vnv_plot2d(\
                'VELOCITY',
                 res_vnv_1_t2dres,
                 record=record,
                 fig_size=(10, 4),
                 fig_name=fig_file,
                 filled_contours=True,
                 fig_title=title,
                 vect_name='VELOCITY',
                 cbar_label='Velocity (m/s)',
                 vectors=True,
                 vectors_scale=5,
                 grid_resolution=[20, 20])#,vectors_normalize=True)
#                 grid_resolution=[10, 10])

        
        # Plotting TRACER 1 at -1
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 8),
                   fig_name='img/tracor1_tf')


        # Plotting TRACER 1 at 1
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=1,
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 8),
                   fig_name='img/tracor1_t1')


        # Plotting TRACER 1 at 10
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=10,
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 8),
                   fig_name='img/tracor1_t10')


        # Plotting TRACER 1 at 20
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=20,
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 8),
                   fig_name='img/tracor1_t20')


        # Plotting TRACER 2 at -1
        vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 8),
                   fig_name='img/tracor2_tf')


        # Plotting TRACER 2 at 1
        vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=1,
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 8),
                   fig_name='img/tracor2_t1')


        # Plotting TRACER 2 at 10
        vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=10,
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 8),
                   fig_name='img/tracor2_t10')


        # Plotting TRACER 2 at 20
        vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=20,
                   filled_contours=True,
                   cbar_label='Tracer concentration (g/L)',
                   fig_size=(10, 8),
                   fig_name='img/tracor2_t20')

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
