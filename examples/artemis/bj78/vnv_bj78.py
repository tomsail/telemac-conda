
"""
Validation script for bj78
"""
from data_manip.extraction.telemac_file import TelemacFile
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
        self.rank = 0
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # bj78 scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_bj78.cas')


        # bj78 parallel mode
        cas = TelemacCas('art_bj78.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_bj78_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_bj78.slf',
                            eps=[1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_bj78.slf',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-2, 1.e-2, 1., 1.e-8, 1.e-5, 1.e-5, 1.e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-2, 1.e-2, 1., 1.e-8, 1.e-5, 1.e-5, 1.e-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines

        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        bnd_artgeo = self.get_study_file('vnv_1:ARTCLI')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo, bnd_file=bnd_artgeo)


        # Plotting WAVE HEIGHT over polyline over records 0
        vnv_plot1d_polylines(\
                'BOTTOM',
                res_vnv_1_artres,
                legend_labels="bottom",
                poly=[[-6, 0.4], [14, 0.4]],
                fig_size=(10, 7),
                fig_name='img/Section_bottom')

        # Plotting WAVE HEIGHT over polyline over records 0
        vnv_plot1d_polylines(\
                'WAVE HEIGHT',
                res_vnv_1_artres,
                legend_labels="wave height at t=0.0s",
                poly=[[-6, 0.4], [14, 0.4]],
                fig_size=(10, 7),
                fig_name='img/Section')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   aspect_ratio='equal',
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 3),
                   fig_name='img/Bathy')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 3),
                   fig_name='img/WaveHeight')

        # Closing files
        res_vnv_1_artres.close()
        res_vnv_1_artgeo.close()
