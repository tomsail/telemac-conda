
"""
Validation script for riogrande
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
        self.rank = 4
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # riogrande scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_riogrande.cas')


        # riogrande parallel mode
        cas = TelemacCas('t2d_riogrande.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_riogrande_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_riogrande.slf',
                            eps=[0.003,0.004,0.0004,0.0004,1e-6,0.003,0.002,0.0003])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_riogrande.slf',
                            eps=[0.009,0.005,0.0003,0.0003,1E-6,0.003,0.003,0.0003])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[0.007,0.005,0.0005,0.0005,1e-6,0.002,0.003,0.0003])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        # Getting files
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1_t2dres, _ = self.get_study_res('vnv_1:T2DRES')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(4, 9),
                   fig_name='img/Mesh')

        #Plotting boundary conditions type without mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
#                   plot_mesh=True,
                   fig_size=(4, 9),
                   fig_name='img/BC',
                   annotate_bnd=True)

        #Plotting mesh (zoom inlet)
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   xlim=[1400, 1550],
                   ylim=[8800, 9000],
                   plot_mesh=True,
                   fig_size=(5, 5),
                   fig_name='img/Meshin')

        #Plotting mesh (zoom outlet)
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   xlim=[1550, 1680],
                   ylim=[3040, 3200],
                   plot_mesh=True,
                   fig_size=(5, 5),
                   fig_name='img/Meshout')

        # Plotting BOTTOM ELEVATION
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(4, 9),
                   fig_name='img/Bottom')

        # Plotting FREE SURFACE
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(3, 9),
                   fig_name='img/FreeSurface')

        # Plotting VELOCITY
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(3, 9),
                   fig_name='img/Velocity',
                   cbar_label='Velocity (m/s)')

        # Closing files
        res_vnv_1_t2dres.close()
        res_vnv_1_t2dgeo.close()
