
"""
Validation script for bumpsub
"""
import numpy as np
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
        self.rank = 1
        self.tags = ['telemac2d', 'fv']
        self.final_time = 100.
        self.nu = 1.
        self.mesh_type = 1

        if self.mesh_type==1:
            self.geo_file = "geo_flume_sym.slf"
            self.cli_file = "geo_flume_sym_neu.cli"
        elif self.mesh_type==2:
            self.geo_file = "geo_flume.slf"
            self.cli_file = "geo_flume_neu.cli"
        elif self.mesh_type==3:
            self.geo_file = "geo_flume_unstruct.slf"
            self.cli_file = "geo_flume_unstruct_neu.cli"

    def _pre(self):
        """
        Defining the studies
        """
        casname='t2d_diffusion_FV_bottom.cas'

        #======================================================================
        # TWO POINT FLUX SCHEME
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', [self.nu, self.nu])
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', [2,2])
        self.add_study('tpf_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('tpf_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # RECONSTRUCTED TWO POINT FLUX SCHEME
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', [self.nu, self.nu])
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', [3,3])
        self.add_study('rtpf_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('rtpf_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # HYBRID EXPLICIT FINITE ELEMENT
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', [self.nu, self.nu])
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', [1,1])
        self.add_study('hefe_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hefe_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # EF SCHEME
        cas = TelemacCas('t2d_diffusion_FE_bottom.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', [self.nu, self.nu])
        cas.set('OPTION FOR THE DIFFUSION OF TRACERS', 2)
        self.add_study('fe_seq', 'telemac2d', 't2d_diffusion_FE.cas', cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('fe_par', 'telemac2d', 't2d_diffusion_FE.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        if self.mesh_type==1:
            self.check_epsilons('tpf_seq:T2DRES', 'f2d_diffusion_bot_tpf.slf', eps=[1e-6])
            self.check_epsilons('tpf_par:T2DRES', 'f2d_diffusion_bot_tpf.slf', eps=[1e-6])
            self.check_epsilons('tpf_seq:T2DRES', 'tpf_par:T2DRES', eps=[1e-6])

            self.check_epsilons('hefe_seq:T2DRES', 'f2d_diffusion_bot_hefe.slf', eps=[1e-6])
            self.check_epsilons('hefe_par:T2DRES', 'f2d_diffusion_bot_hefe.slf', eps=[1e-6])
            self.check_epsilons('hefe_seq:T2DRES', 'hefe_par:T2DRES', eps=[1e-6])

            self.check_epsilons('rtpf_seq:T2DRES', 'f2d_diffusion_bot_rtpf.slf', eps=[1e-6])
            self.check_epsilons('rtpf_par:T2DRES', 'f2d_diffusion_bot_rtpf.slf', eps=[1e-6])
            self.check_epsilons('rtpf_seq:T2DRES', 'rtpf_par:T2DRES', eps=[1e-6])

            self.check_epsilons('fe_seq:T2DRES', 'f2d_diffusion_bot_fe.slf', eps=[1e-6])
            self.check_epsilons('fe_par:T2DRES', 'f2d_diffusion_bot_fe.slf', eps=[1e-6])
            self.check_epsilons('fe_seq:T2DRES', 'fe_par:T2DRES', eps=[1e-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot3d, vnv_plot2d, vnv_plot1d, vnv_plotbar, \
                vnv_plot1d_polylines, vnv_plot1d_history, vnv_plotbar_cpu_times
        from os import path
        import numpy as np
        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])
        geom, _ = self.get_study_res('tpf_seq:T2DGEO', load_bnd=True)
        res,  _ = self.get_study_res('tpf_seq:T2DRES')

        #======================================================================
        # PLOT 1D
        records = [0,-1]

        for idx, record in enumerate(records):
            vnv_plot1d_polylines(\
                'TRACER 1',
                res_list,
                res_labels,
                record=record,
                fig_size=(8, 5),
                y_label='tracer',
                x_label='x (m)',
                ylim=[0,1],
                fig_name='img/t2d_diffusiontracer_vf_T1_{}'.format(record),
                plot_bottom=False,
                markers=True,
                markevery=15)

            vnv_plot1d_polylines(\
                'TRACER 2',
                res_list,
                res_labels,
                record=record,
                fig_size=(8, 5),
                y_label='tracer',
                x_label='x (m)',
                ylim=[0,1],
                fig_name='img/t2d_diffusiontracer_vf_T2_{}'.format(record),
                plot_bottom=False,
                markers=True,
                markevery=15)

