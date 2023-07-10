
"""
Validation script for confluence
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

        # confluence scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_confluence.cas')


        # confluence parallel mode
        cas = TelemacCas('t2d_confluence.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_confluence_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_confluence.slf',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_confluence.slf',
                            eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[1.e-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d

        # Getting files
        res_vnv_seq_t2dgeo, _ = self.get_study_res('vnv_seq:T2DGEO', load_bnd=True)
        res_vnv_seq_t2dres, _ = self.get_study_res('vnv_seq:T2DRES')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_seq_t2dgeo,
                   plot_mesh=True,
                   fig_size=(9, 3),
                   fig_name='img/mesh',
                   annotate_bnd=True)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_seq_t2dgeo,
                   xlim=[-6.5, -4.0],
                   ylim=[0., 1.1],
                   plot_mesh=True,
                   fig_size=(9, 3),
                   fig_name='img/mesh2')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_seq_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(9, 3),
                   fig_name='img/bathy')

        # Plotting WATER DEPTH at -1
        vnv_plot2d('WATER DEPTH',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(9, 3),
                   fig_name='img/waterDepth')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_size=(9, 3),
                   fig_name='img/velocity')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_seq_t2dres,
                   xlim=[-6.5, -4.0],
                   ylim=[0., 1.1],
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   grid_resolution=[50, 50],
                   streamlines=True,
                   streamlines_density=1.2,
                   fig_size=(9, 3),
                   fig_name='img/velocity_strm')

        # Closing files
        res_vnv_seq_t2dgeo.close()
        res_vnv_seq_t2dres.close()
