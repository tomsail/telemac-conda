
"""
Validation script for tracer_decay
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
        self.tags = ['telemac2d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # tracer decay scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_tracer_decay.cas')


        # tracer decay parallel mode
        cas = TelemacCas('t2d_tracer_decay.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_tracer_decay_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_tracer_decay.slf',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_tracer_decay.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[1.E-15])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        geom_res, _ = self.get_study_res('vnv_seq:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('vnv_seq:T2DRES')

        # Only using annotate_lid_bnd if api are avaialable:
        try:
            import _api
            has_api = True
        except ImportError:
            has_api = False

        # TODO: Redo figures from documentation
        vnv_plot2d(\
            '',
            geom_res,
            record=0,
            fig_size=(7, 3),
            fig_name="img/mesh",
            annotate_liq_bnd=has_api,
            plot_mesh=True)

        vnv_plot2d(\
            '',
            geom_res,
            record=0,
            xlim=[0, 145],
            fig_size=(7, 3),
            fig_name="img/mesh_zoomed",
            annotate_bnd=True,
            plot_mesh=True)

        vnv_plot2d(\
            'TRAC',
            res,
            record=0,
            cbar_label='Tracer concentration',
            fig_size=(7, 3),
            fig_name="img/tracer_t0",
            filled_contours=True,
            plot_mesh=True)

        vnv_plot2d(\
            'TRAC',
            res,
            record=0,
            xlim=[0, 145],
            cbar_label='Tracer concentration',
            fig_size=(7, 3),
            fig_name="img/tracer_t0_zoomed",
            filled_contours=True,
            plot_mesh=True)
