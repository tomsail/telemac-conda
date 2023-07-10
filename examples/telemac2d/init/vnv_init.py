
"""
Validation script for init
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
        self.tags = ['telemac2d', 'fv']

    def _pre(self):
        """
        Defining the studies
        """

        # init_1 scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_init.cas')


        # init_1 parallel mode
        cas = TelemacCas('t2d_init.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_init_par.cas',
                       cas=cas)

        del cas


        # init no water scalar mode
        self.add_study('vnv_no_water_seq',
                       'telemac2d',
                       't2d_init-no-water.cas')


        # init no water parallel mode
        cas = TelemacCas('t2d_init-no-water.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_no_water_par',
                       'telemac2d',
                       't2d_init-no-water_par.cas',
                       cas=cas)

        del cas


        # init_cin scalar mode
        self.add_study('vnv_cin-seq',
                       'telemac2d',
                       't2d_init_cin.cas')


        # init_cin parallel mode
        cas = TelemacCas('t2d_init_cin.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 8)

        self.add_study('vnv_cin-par',
                       'telemac2d',
                       't2d_init_cin_par.cas',
                       cas=cas)

        del cas


        # init_cin scalar mode
        self.add_study('vnv_cin-src-seq',
                       'telemac2d',
                       't2d_init_cin_source_by_reg.cas')


        # init_cin parallel mode
        cas = TelemacCas('t2d_init_cin_source_by_reg.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 8)

        self.add_study('vnv_cin-src-par',
                       'telemac2d',
                       't2d_init_cin_source_by_reg_par.cas',
                       cas=cas)

        del cas


        # init restart scalar mode
        self.add_study('vnv_restart-seq',
                       'telemac2d',
                       't2d_init-restart.cas')


        # init restart parallel mode
        cas = TelemacCas('t2d_init-restart.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_restart-par',
                       'telemac2d',
                       't2d_init-restart_par.cas',
                       cas=cas)

        del cas


        # init restart scalar mode with source file
        self.add_study('vnv_restart-reg-seq',
                       'telemac2d',
                       't2d_init-restart_source_by_reg.cas')


        # init restart parallel mode with source file
        cas = TelemacCas('t2d_init-restart_source_by_reg.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_restart-reg-par',
                       'telemac2d',
                       't2d_init-restart_source_by_reg_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_init.slf',
                            eps=[4.E-4, 2.E-4, 2.E-5, 4.E-5, 1.E-15, 1.E-6, 1.E-9, 1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_init.slf',
                            eps=[4.E-4, 3.E-4, 2.E-5, 4.E-5, 1.E-15, 1.E-6, 1.E-9, 2.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[4.E-4, 2.E-4, 2.E-5, 4.E-5, 1.E-15, 1.E-6, 1.E-9, 2.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_no_water_seq:T2DRES',
                            'f2d_init-no-water.slf',
                            eps=[4.E-4, 6.E-4, 2.E-4, 2.E-4, 1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_no_water_par:T2DRES',
                            'f2d_init-no-water.slf',
                            eps=[1.2, 1.5, 0.92, 0.92, 1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_no_water_seq:T2DRES',
                            'vnv_no_water_par:T2DRES',
                            eps=[1.2, 1.5, 0.92, 0.92, 1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_cin-seq:T2DRES',
                            'f2d_init_cin.slf',
                            eps=[0.062, 0.022, 4.E-3, 4.E-3, 1.E-15, 2.E-3, 0.059])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_cin-par:T2DRES',
                            'f2d_init_cin.slf',
                            eps=[0.12, 0.047, 0.014, 0.014, 1.E-15, 3.E-3, 0.087])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_cin-seq:T2DRES',
                            'vnv_cin-par:T2DRES',
                            eps=[0.094, 0.046, 0.014, 0.014, 1.E-15, 2.E-3, 0.068])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_cin-src-seq:T2DRES',
                            'vnv_cin-seq:T2DRES',
                            eps=[0.021, 0.016, 0.039, 0.039, 1.E-15, 2.E-4, 90.])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_cin-src-par:T2DRES',
                            'vnv_cin-par:T2DRES',
                            eps=[0.022, 0.016, 0.040, 0.040, 1.E-15, 2.E-4, 91.])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_cin-src-seq:T2DRES',
                            'vnv_cin-src-par:T2DRES',
                            eps=[0.094, 0.046, 0.014, 0.014, 1.E-15, 2.E-3, 4.1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_restart-seq:T2DRES',
                            'f2d_init-restart.slf',
                            eps=[8.E-3, 4.E-3, 1.E-3, 2.E-3, 1.E-15, 0.10, 4.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_restart-par:T2DRES',
                            'f2d_init-restart.slf',
                            eps=[5.E-3, 3.E-3, 1.E-3, 2.E-3, 1.E-15, 0.056, 4.E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_restart-seq:T2DRES',
                            'vnv_restart-par:T2DRES',
                            eps=[0.012, 5.E-3, 5.E-4, 5.E-4, 1.E-15, 0.15, 6.E-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_restart-reg-seq:T2DRES',
                            'vnv_restart-seq:T2DRES',
                            eps=[9.E-3, 4.E-3, 2.E-3, 2.E-3, 1.E-15, 0.11, 4.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_restart-reg-par:T2DRES',
                            'vnv_restart-par:T2DRES',
                            eps=[6.E-3, 3.E-3, 2.E-3, 2.E-3, 1.E-15, 0.078, 4.E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_restart-reg-par:T2DRES',
                            'vnv_restart-reg-seq:T2DRES',
                            eps=[7.E-3, 4.E-3, 4.E-4, 4.E-4, 1.E-15, 0.091, 4.E-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        res_vnv_seq_t2dgeo, _ = self.get_study_res('vnv_seq:T2DGEO', load_bnd=True)
        res_vnv_seq_t2dres, _ = self.get_study_res('vnv_seq:T2DRES')
        res_vnv_no_water_t2dres, _ = self.get_study_res('vnv_no_water_seq:T2DRES')
        res_vnv_cin_t2dres, _ = self.get_study_res('vnv_cin-seq:T2DRES')
        res_vnv_cin_reg_t2dres, _ = self.get_study_res('vnv_cin-src-seq:T2DRES')
        res_vnv_restart_t2dres, _ = self.get_study_res('vnv_restart-seq:T2DRES')
        res_vnv_restart_reg_t2dres, _ = self.get_study_res('vnv_restart-reg-seq:T2DRES')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_seq_t2dgeo,
                   plot_mesh=True,
#                   annotate_bnd=True,
                   fig_size=(15, 3),
                   fig_name='img/Mesh')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_seq_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/Bottom')

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/FreeSurfaceSeq')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_seq_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/VelocitySeq',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=2)

        # Plotting VISCOSITY at final time step
        vnv_plot2d('VISCOSITY',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Viscosity (m$^2$/s)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/ViscositySeq')

        # Plotting TKE at final time step
        vnv_plot2d('TURBULENT ENERG.',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='TKE (m$^2$/s$^2$)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/TKESeq')

        # Plotting DISSIPATION at final time step
        vnv_plot2d('DISSIPATION',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Dissipation (m$^3$/s$^2$)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/DissipationSeq')

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_no_water_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/FreeSurfaceNoWater')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_no_water_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/VelocityNoWater',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=20)

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_cin_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/FreeSurfaceKin')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_cin_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/VelocityKin',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=10)

        # Plotting TRACER at final time step
        vnv_plot2d('TRACER 1',
                   res_vnv_cin_t2dres,
                   record=-1,
                   cbar_label='Tracer ()',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/TracerKin')

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_cin_reg_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/FreeSurfaceKinReg')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_cin_reg_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/VelocityKinReg',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=10)

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_restart_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/FreeSurfaceRestart')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_restart_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/VelocityRestart',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=15)

        # Plotting TRACER at final time step
        vnv_plot2d('TRACER 1',
                   res_vnv_restart_t2dres,
                   record=-1,
                   cbar_label='Tracer ()',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/TracerRestart')

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_restart_reg_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/FreeSurfaceRestartReg')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_restart_reg_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/VelocityRestartReg',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=15)

        # Plotting FREE SURFACE at 1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_cin_t2dres,
                   record=1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface_t1')

        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_cin_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface_tf')

        # Plotting VISCOSITY at -1
        vnv_plot2d('VISCOSITY',
                   res_vnv_cin_t2dres,
                   record=-1,
                   cbar_label='Viscosity (m$^2$/s)',
                   filled_contours=True,
                   fig_size=(10, 5),
                   fig_name='img/Turb_Visco_tf')

        # Closing files
        res_vnv_seq_t2dgeo.close()
        res_vnv_seq_t2dres.close()
        res_vnv_no_water_t2dres.close()
        res_vnv_cin_t2dres.close()
        res_vnv_cin_reg_t2dres.close()
        res_vnv_restart_t2dres.close()
        res_vnv_restart_reg_t2dres.close()
