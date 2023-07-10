
"""
Validation script for swash
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
        self.tags = ['telemac2d', 'fv']

    def _pre(self):
        """
        Defining the studies
        """

        # swash scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_swash.cas')


        # swash scalar mode
        cas = TelemacCas('t2d_swash.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_swash_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_swash.slf',
                            eps=[1.e-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_swash.slf',
                            eps=[1.e-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-1])


    def _post(self):
        """
        Post-treatment processes
        """

        #TODO: Redo picture for documentation
        from postel.plot_vnv import vnv_plot1d_history, vnv_plot2d
        from data_manip.extraction.telemac_file import TelemacFile
                # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(10, 2),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   fig_size=(10, 3),
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_name='img/Bathy')

        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   vect_name='VELOCITY',
                   vectors=True,
                   vectors_scale=25,
                   fig_size=(10, 3),
                   fig_name='img/FreeSurface')

        # Closing files
        res_vnv_1_t2dres.close()
        res_vnv_1_t2dgeo.close()
