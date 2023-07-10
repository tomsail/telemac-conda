
"""
Validation script for flotteurs
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

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

        # flotteurs scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_flotteurs_v1p0.cas')


        # flotteurs parallel mode
        cas = TelemacCas('t2d_flotteurs_v1p0.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_flotteurs_v1p0_par.cas',
                       cas=cas)

        del cas


        # flotteurs scalar mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_flotteurs_v2p0.cas')


        # flotteurs parallel mode
        cas = TelemacCas('t2d_flotteurs_v2p0.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_flotteurs_v2p0_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame for the v1p0 run
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_flotteurs_v1p0.slf',
                            eps=[5.E-3, 2.E-3, 1.E-3, 1.E-3, 1.E-8, 1.E-3, 5.E-2, 4.E-3])
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_flotteurs_v1p0.slf',
                            eps=[7.E-3, 3.E-3, 1.E-3, 1.E-3, 1.E-8, 1.E-3, 9.E-2, 7.E-3])
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[7.E-3, 2.E-3, 1.E-3, 1.E-3, 1.E-8, 1.E-3, 8.E-2, 7.E-3])

        # Comparison with the last time frame for the v2p0 run
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_flotteurs_v2p0.slf',
                            eps=[2.E-2, 7.E-3, 3.E-3, 3.E-3, 1.E-8, 2.E-3, 2.E-1, 2.E-2, 2.E-2])
        self.check_epsilons('vnv_4:T2DRES',
                            'f2d_flotteurs_v2p0.slf',
                            eps=[7.E-2, 1.E-2, 5.E-3, 5.E-3, 1.E-8, 6.E-3, 2.E-1, 4.E-2, 9.E-3])
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[6.E-2, 7.E-3, 5.E-3, 5.E-3, 1.E-8, 5.E-3, 3.E-1, 4.E-2, 2.E-2])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history, vnv_plot1d_polylines
        # Getting files
        res_vnv_1_t2dres, _ = self.get_study_res('vnv_1:T2DRES')
        res_vnv_3_t2dres, _ = self.get_study_res('vnv_3:T2DRES')
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)

        #Plotting FREE SURFACE on [270, 500], [1030, 460] over records range(0, res_vnv_1_t2dres.ntimestep)
        vnv_plot1d_history(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                'FREE SURFACE',
                points=[[270, 500], [1030, 460]],
                fig_size=(12, 5),
                fig_name='img/FreeSurfaceTimeSeries')


        # Plotting FREE SURFACE over polyline over records range(0, res_vnv_1_t2dres.ntimestep, 10)
        vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                poly=[[0, 500], [1000, 500]],
                record=[i for i in range(0, res_vnv_1_t2dres.ntimestep, 10)],
                fig_size=(12, 5),
                fig_name='img/FreeSurfaceProfiles')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(15, 3),
                   fig_name='img/Mesh',
                   annotate_bnd=True)


        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/FreeSurface1')


        # Plotting VELOCITY at final time step
        vnv_plot2d('SCALAR VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 2.5),
                   fig_name='img/Velocity1',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=10)

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_3_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 3),
                   fig_name='img/FreeSurface2')


        # Plotting VELOCITY at final time step
        vnv_plot2d('SCALAR VELOCITY',
                   res_vnv_3_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 2.5),
                   fig_name='img/Velocity2',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=10)

        # Closing files
        res_vnv_1_t2dres.close()
        res_vnv_3_t2dres.close()
        res_vnv_1_t2dgeo.close()
