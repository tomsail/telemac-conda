
"""
Validation script for triangular_shelf
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

        # triangular_shelf scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_triangular_shelf.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_triangular_shelf.slf',
                            eps=[0.03, 0.03, 4.E-4, 4.E-4, 1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
                # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)

        # TODO: Plot figures from documentation
        #Plotting FREE SURFACE on [7.5, 0] over records range(0, res_vnv_1_t2dres.ntimestep)
        vnv_plot1d_history(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                'FREE SURFACE',
                points=[[7.5, 0]],
                fig_size=(12, 5),
                fig_name='img/Gages1')


        #Plotting FREE SURFACE on [17, -5] over records range(0, res_vnv_1_t2dres.ntimestep)
        vnv_plot1d_history(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                'FREE SURFACE',
                points=[[17, -5]],
                fig_size=(12, 5),
                fig_name='img/Gages2')


        #Plotting FREE SURFACE on [25, 0] over records range(0, res_vnv_1_t2dres.ntimestep)
        vnv_plot1d_history(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                'FREE SURFACE',
                points=[[25, 0]],
                fig_size=(12, 5),
                fig_name='img/Gages3')


        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(12, 8),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(12, 8),
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
                   fig_size=(12, 6),
                   fig_name='img/FreeSurface')

        # Closing files
        res_vnv_1_t2dres.close()
        res_vnv_1_t2dgeo.close()
