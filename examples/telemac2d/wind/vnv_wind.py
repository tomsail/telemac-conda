
"""
Validation script for wind
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

        # wind scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_wind.cas')


        # wind parallel mode
        cas = TelemacCas('t2d_wind.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_wind_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_wind.slf',
                            eps=[1e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_wind.slf',
                            eps=[1e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines

        # Getting files
        geom, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1_t2dres, _ = self.get_study_res('vnv_1:T2DRES')

        # Plotting FREE SURFACE at -1
        vnv_plot2d(\
            'FREE SURFACE',
            res_vnv_1_t2dres,
            record=-1,
            cbar_label='Free surface (m)',
            filled_contours=True,
            fig_size=(10, 3),
            fig_name='img/free_surface2D')

        # Plot free surface
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_vnv_1_t2dres,
            'free surface',
            poly=[[0, 50], [500, 50]],
            fig_size=(10, 3),
            record=-1,
            fig_name='img/free_surface1D')

        #Plotting mesh
        vnv_plot2d(\
            '',
            geom,
            plot_mesh=True,
            fig_size=(8, 2.5),
            fig_name='img/Mesh',
            annotate_bnd=True)

        #Plot vectors
        vnv_plot2d(\
            'WIND ALONG X',
            res_vnv_1_t2dres,
            record=-1,
            cbar_label='Wind velocity (m/s)',
            fig_size=(8, 2.5),
            fig_name='img/vector',
            filled_contours=True,
            vect_name='WIND ALONG',
            vectors=True,
            vectors_normalize=True,
            vectors_scale=10,
            grid_resolution=[10, 10])

        # Closing files
        geom.close
        res_vnv_1_t2dres.close()
