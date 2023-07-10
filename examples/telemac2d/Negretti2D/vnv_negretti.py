
"""
Validation script for negretti
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
        self.rank = 2
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # negretti scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_negretti.cas')


        # negretti parallel mode
        cas = TelemacCas('t2d_negretti.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_negretti_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_negretti.slf',
                            eps=[1e-6,1e-7,1e-6,1e-5,1e-9,1e-8,1e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_negretti.slf',
                            eps=[6e-3,9e-3,9e-4,9e-4,1e-9,2e-3,3e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[6e-3,9e-3,9e-4,9e-4,1e-9,2e-3,3e-3])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_history, vnv_plot1d_polylines, \
                vnv_plot2d
        # Getting files
        res_vnv_seq_t2dgeo, _ = self.get_study_res('vnv_seq:T2DGEO', load_bnd=True)
        res_vnv_seq_t2dres, _ = self.get_study_res('vnv_seq:T2DRES')

        #Plotting FREE SURFACE on 703, [0, 1] over records range(0, ntimestep)
        vnv_plot1d_history(\
            'FREE SURFACE',
            res_vnv_seq_t2dres,
            'FREE SURFACE',
            points=[[0, 1]],
            nodes=[703],
            fig_size=(12, 3),
            fig_name='img/figure1')

        #Plotting FREE SURFACE in slice plane
        vnv_plot1d_polylines(\
            'FREE SURFACE', res_vnv_seq_t2dres, 'elevation',
            fig_size=(12, 3),
            record=-1,
            poly=[[0, 0.93], [21, 1.07]],
            fig_name='img/figure2',
            plot_bottom=True)

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True, vectors_scale=50,
                   fig_size=(15, 6),
                   fig_name='img/VelocityVect')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_seq_t2dgeo,
                   plot_mesh=True,
                   fig_size=(15, 6),
                   fig_name='img/Mesh',
                   annotate_bnd=True)

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 6),
                   fig_name='img/FreeSurface')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   streamlines=True, streamlines_density=1,
                   grid_resolution=[20, 50],
                   fig_size=(15, 6),
                   fig_name='img/VelocityStream')

        # Closing files
        res_vnv_seq_t2dres.close()
        res_vnv_seq_t2dgeo.close()
