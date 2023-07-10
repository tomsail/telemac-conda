
"""
Validation script for port_auto
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
        self.rank = 0
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # AUTOMATIC TETAP FOR SCHEMATIC DELFT HARBOUR
        self.add_study('vnv_1',
                       'artemis',
                       'art_port_auto.cas')


        # AUTOMATIC TETAP FOR SCHEMATIC DELFT HARBOUR
        cas = TelemacCas('art_port_auto.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'artemis',
                       'art_port_auto_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_port_auto.slf',
                            eps=[1.e-5, 4.e-4, 1.e-5, 1.e-5, 1.e-5, 1.e-8, 1.e-5, 1.e-5, 0.03])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_port_auto.slf',
                            eps=[1.e-6, 4e-4, 0.03, 0.04, 1.e-5, 1.e-8, 1.e-5, 1.e-5, 360.0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-5, 4.e-4, 0.03, 0.04, 1.e-5, 1.e-8, 1.e-5, 1.e-5, 360.0])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)

        polys = [[[1, 3], [9, 3]],
                 [[1, 4], [9, 4]],
                 [[1, 5], [8, 5]],
                 [[1, 6], [8, 6]]]

        for i, poly in enumerate(polys):

            # Plotting WAVE HEIGHT over polyline over records 0
            vnv_plot1d_polylines(\
                    'WAVE HEIGHT',
                    res_vnv_1_artres,
                    legend_labels="wave height at t=0.0s",
                    poly=poly,
                    fig_size=(10, 7),
                    fig_name='img/AutoSection{}'.format(i+1))

        #Plotting mesh
        # TODO: add polylines to plot
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   fig_size=(12, 8),
                   fig_name='img/AutoMesh')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 8),
                   fig_name='img/AutoWaveHeight')


        # Plotting FREE SURFACE at 0
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 8),
                   fig_name='img/AutoFreeSurface')

        # Closing files
        res_vnv_1_artgeo.close()
        res_vnv_1_artres.close()
