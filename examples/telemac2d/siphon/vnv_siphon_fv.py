
"""
Validation script for siphon
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
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """

        # siphon scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_siphon_fv.cas')


        # siphon parallel mode
        cas = TelemacCas('t2d_siphon_fv.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_siphon_fv_par.cas',
                       cas=cas)
        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_siphon_fv.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_siphon_fv.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        #TODO: Redo figures from documentation
        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(10, 8),
                   fig_name='img/Mesh')


        # Plotting TRACER 1 at -1
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/tracor1_tf')


        # Plotting TRACER 1 at 1
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/tracor1_t1')


        # Plotting TRACER 1 at 10
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=10,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/tracor1_t10')


        # Plotting TRACER 1 at 20
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t2dres,
                   record=20,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/tracor1_t20')


        # Plotting TRACER 2 at -1
        vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/tracor2_tf')


        # Plotting TRACER 2 at 1
        vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/tracor2_t1')


        # Plotting TRACER 2 at 10
        vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=10,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/tracor2_t10')


        # Plotting TRACER 2 at 20
        vnv_plot2d('TRACER 2',
                   res_vnv_1_t2dres,
                   record=20,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/tracor2_t20')

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
