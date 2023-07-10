
"""
Validation script for cavity
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

        # cavity scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_cavity.cas')

        # cavity parallel mode
        cas = TelemacCas('t2d_cavity.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_cavity_par.cas',
                       cas=cas)
        del cas

        # cavity scalar mode CONSTANT VISCOSITY
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_cavity_cte.cas')

        # cavity parallel mode
        cas = TelemacCas('t2d_cavity_cte.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_cavity_cte_par.cas',
                       cas=cas)
        del cas

        # cavity scalar mode K-EPSILON MODEL
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_cavity_keps.cas')

        # cavity parallel mode
        cas = TelemacCas('t2d_cavity_keps.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_cavity_keps_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_cavity.slf',
                            eps=[8e-3, 6e-3, 6e-4, 6e-4, 1e-8, 1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_cavity.slf',
                            eps=[0.01, 6e-3, 6e-4, 6e-4, 1e-8, 1e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[0.01, 4e-3, 5e-4, 5e-4, 1e-8, 1e-5])


        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_cavity_cte.slf',
                            eps=[8e-3, 6e-3, 6e-4, 6e-4, 1e-8, 1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_cavity_cte.slf',
                            eps=[0.01, 6e-3, 6e-4, 6e-4, 1e-8, 1e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[0.01, 4e-3, 5e-4, 5e-4, 1e-8, 1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T2DRES',
                            'f2d_cavity_keps.slf',
                            eps=[8e-3, 6e-3, 6e-4, 6e-4, 1e-8, 2e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T2DRES',
                            'f2d_cavity_keps.slf',
                            eps=[0.01, 6e-3, 6e-4, 6e-4, 1e-8, 2e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[0.01, 4e-3, 5e-4, 5e-4, 1e-8, 3e-5])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        bnd_file = self.get_study_file('vnv_1:T2DCLI')

        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo, bnd_file)

        vnv_3_t2dres = self.get_study_file('vnv_3:T2DRES')
        res_vnv_3_t2dres = TelemacFile(vnv_3_t2dres)

        vnv_5_t2dres = self.get_study_file('vnv_5:T2DRES')
        res_vnv_5_t2dres = TelemacFile(vnv_5_t2dres)

        #TODO: Redo figure from documentation
        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   aspect_ratio='equal',
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_title='mesh of the domain',
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        #Plotting bottom
        vnv_plot2d('FOND',
                   res_vnv_1_t2dgeo,
                   aspect_ratio='equal',
                   fig_size=(12, 7),
                   fig_title='Bottom level',
                   cbar_label='Bottom elevation (m)',
                   record=0,
                   filled_contours=True,
                   fig_name='img/bottom')

        
        # Plotting BOTTOM over polyline over records range(0, -1)
        vnv_plot1d_polylines(\
                'FOND',
                res_vnv_1_t2dgeo,
                poly=[[14, 0], [14, 6]],
                fig_title='Bottom profile along (14.,0.),(14.,6.)',
                record=0,
                fig_size=(12, 5),
                fig_name='img/FondProfile')
        
        # Plotting BOTTOM over polyline over records range(0, -1)
        vnv_plot1d_polylines(\
                'BOTTOM',
                res_vnv_1_t2dres,
                poly=[[15, 0], [15, 6]],
                record=[i for i  in range(0, res_vnv_1_t2dres.ntimestep)],
                fig_size=(12, 5),
                fig_name='img/BottomProfile')

        # Plot velocity streamlines:

        for record in range(res_vnv_1_t2dres.ntimestep):
            title = 'Streamlines at t=' + str(15425+25*record) + '.0s'
            fig_file = 'img/velocity_streamlines_' + str(15425+25*record)
            vnv_plot2d('VELOCITY', res_vnv_1_t2dres,
                record=record,
                fig_size=(7.5, 7),
                xlim=[10, 18],
                ylim=[2, 6],
                fig_name=fig_file,
                fig_title=title,
                cbar_label='Velocity (m/s)',
                x_label='x (m)', y_label='y (m)',
                filled_contours=True,
                mask_tidal_flats=True,
                streamlines=True, streamlines_density=3,
                grid_resolution=[50, 50])

        # Plot velocity streamlines: viscosity constante

        for record in range(res_vnv_3_t2dres.ntimestep):
            title = 'Streamlines Cte at t=' + str(15425+25*record) + '.0s'
            fig_file = 'img/velocity_streamlines_cte_' + str(15425+25*record)
            vnv_plot2d('VELOCITY', res_vnv_3_t2dres,
                record=record,
                fig_size=(7.5, 7),
                xlim=[10, 18],
                ylim=[2, 6],
                fig_name=fig_file,
                fig_title=title,
                cbar_label='Velocity (m/s)',
                x_label='x (m)', y_label='y (m)',
                filled_contours=True,
                mask_tidal_flats=True,
                streamlines=True, streamlines_density=3,
                grid_resolution=[50, 50])

        # Plot velocity streamlines: viscosity constante

        for record in range(res_vnv_5_t2dres.ntimestep):
            title = 'Streamlines Keps at t=' + str(15425+25*record) + '.0s'
            fig_file = 'img/velocity_streamlines_keps_' + str(15425+25*record)
            vnv_plot2d('VELOCITY', res_vnv_5_t2dres,
                record=record,
                fig_size=(7.5, 7),
                xlim=[10, 18],
                ylim=[2, 6],
                fig_name=fig_file,
                fig_title=title,
                cbar_label='Velocity (m/s)',
                x_label='x (m)', y_label='y (m)',
                filled_contours=True,
                mask_tidal_flats=True,
                streamlines=True, streamlines_density=3,
                grid_resolution=[50, 50])

            # Plotting WATER DEPTH at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   fig_size=(6, 5),
                   xlim=[10, 18],
                   ylim=[2, 6],
                   cbar_label='Free Surface (m)',
                   filled_contours=True,
                   fig_name='img/freesurface_15625')

        # Plotting WATER DEPTH at -1
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Water depth (m)',
                   filled_contours=True,
                   fig_size=(20, 7),
                   fig_name='img/Water_depth')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_size=(20, 7),
                   fig_name='img/Velocity')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   fig_size=(6, 5),
                   xlim=[11, 17],
                   ylim=[3, 6],
                   record=0,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_name='img/VelocityROIt0',
                   streamlines=True)

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   fig_size=(6, 5),
                   xlim=[11, 17],
                   ylim=[3, 6],
                   record=-1,
                   filled_contours=True,
                   fig_name='img/VelocityROItf',
                   streamlines=True)

        # Closing files
        res_vnv_1_t2dres.close()
