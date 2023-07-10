
"""
Validation script for seccurrents
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
        # rank is moved to 5 (not tested) until
        # seccurent functionality has been improved
        self.rank = 5
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # seccurrents scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_seccurrents.cas')


        # seccurrents scalar mode
        cas = TelemacCas('t2d_seccurrents.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_seccurrents_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_seccurrents.slf',
                            eps=[1., 1., 1.e-2, 1.e-2,
                                 1.e-6, 10., 1.e-1, 1.,
                                 1., 1.e-1, 1.e-1, 100.,
                                 100., 10.])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_seccurrents.slf',
                            eps=[1., 1., 1.e-2, 1.e-2,
                                 1.e-6, 10., 1.e-1, 1.,
                                 1., 1.e-1, 1.e-1, 100.,
                                 100., 10.])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[1., 1., 1.e-2, 1.e-2,
                                 1.e-6, 10., 1.e-1, 1.,
                                 1., 1.e-1, 1.e-1, 100.,
                                 100., 10.])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        # Getting files
        res_vnv_seq_t2dres, _ = self.get_study_res('vnv_seq:T2DRES')
        res_vnv_seq_t2dgeo, _ = self.get_study_res('vnv_seq:T2DGEO', load_bnd=True)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_seq_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(15, 8),
                   fig_name='img/Mesh')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_seq_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(15, 7),
                   fig_name='img/Bottom')

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(15, 7),
                   fig_name='img/FreeSurface')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_seq_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(15, 7),
                   fig_name='img/Velocity',
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=25)

        # Closing files
        res_vnv_seq_t2dres.close()
        res_vnv_seq_t2dgeo.close()
        
