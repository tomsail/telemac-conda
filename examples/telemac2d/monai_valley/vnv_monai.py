
"""
Validation script for monai
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
        self.rank = 3
        self.tags = ['telemac2d', 'fv']

    def _pre(self):
        """
        Defining the studies
        """

        # monai scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_monai.cas')


        # monai scalar mode
        cas = TelemacCas('t2d_monai.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_monai_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_monai.slf',
                            eps=[8.E-5, 2.E-4, 1.E-6, 1.E-6, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_monai.slf',
                            eps=[1.e-3, 1e-3, 1e-6, 1e-6, 1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[1.e-3, 1e-3, 1e-6, 1e-6, 1e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        from data_manip.extraction.telemac_file import TelemacFile
                # Getting files
        vnv_seq_t2dres = self.get_study_file('vnv_seq:T2DRES')
        res_vnv_seq_t2dres = TelemacFile(vnv_seq_t2dres)
        res_vnv_seq_t2dgeo, _ = self.get_study_res('vnv_seq:T2DGEO', load_bnd=True)

        # TODO: Plot figures from documentation
        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_seq_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(12, 8),
                   fig_name='img/Mesh')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_seq_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(12, 8),
                   fig_name='img/Bathy')

        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(12, 6),
                   fig_name='img/FreeSurface')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_seq_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=10,
                   filled_contours=True,
                   fig_size=(12, 6),
                   fig_name='img/Velocity')

        # Closing files
        res_vnv_seq_t2dres.close()
        res_vnv_seq_t2dgeo.close()
