
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
        self.final_time = 10.
        self.nu = 0.1
        self.mesh_type = 1 #default = 2

        if self.mesh_type==1:
            self.geo_file = "geo_cone_unstruct.slf"
            self.cli_file = "geo_cone_unstruct.cli"
        elif self.mesh_type==2:
            self.geo_file = "geo_cone.slf"
            self.cli_file = "geo_cone.cli"
        elif self.mesh_type==3:
            self.geo_file = "geo_cone_def.slf"
            self.cli_file = "geo_cone_def.cli"

    def _pre(self):
        """
        Defining the studies
        """
        casname='t2d_cone_diffusion_FV.cas'

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
        # RECONSTRUCTED TWO POINT FLUX SCHEME (OPTION 1)
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', [self.nu, self.nu])
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', [3,3])
        cas.set('OPTION FOR THE RTPF SCHEME RECONSTRUCTIONS', 1)
        self.add_study('rtpf1_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('rtpf1_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # RECONSTRUCTED TWO POINT FLUX SCHEME (OPTION 2)
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', [self.nu, self.nu])
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', [3,3])
        cas.set('OPTION FOR THE RTPF SCHEME RECONSTRUCTIONS', 2)
        self.add_study('rtpf2_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('rtpf2_par', 'telemac2d', casname, cas=cas)
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
        # DIFFUSION EF
        cas = TelemacCas('t2d_cone_diffusion_FE.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', [self.nu, self.nu])
        self.add_study('fe_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('fe_par', 'telemac2d', casname, cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        if self.mesh_type==2:
            self.check_epsilons('tpf_seq:T2DRES', 'f2d_diffusion_tpf.slf', eps=[1e-6])
            self.check_epsilons('tpf_par:T2DRES', 'f2d_diffusion_tpf.slf', eps=[1e-6])
            self.check_epsilons('tpf_seq:T2DRES', 'tpf_par:T2DRES', eps=[1e-6])

            self.check_epsilons('hefe_seq:T2DRES', 'f2d_diffusion_hefe.slf', eps=[1e-6])
            self.check_epsilons('hefe_par:T2DRES', 'f2d_diffusion_hefe.slf', eps=[1e-6])
            self.check_epsilons('hefe_seq:T2DRES', 'hefe_par:T2DRES', eps=[1e-6])

            self.check_epsilons('rtpf1_seq:T2DRES', 'f2d_diffusion_rtpf.slf', eps=[1e-6])
            self.check_epsilons('rtpf1_par:T2DRES', 'f2d_diffusion_rtpf.slf', eps=[1e-6])
            self.check_epsilons('rtpf1_seq:T2DRES', 'rtpf1_par:T2DRES', eps=[1e-6])

            self.check_epsilons('fe_seq:T2DRES', 'f2d_diffusion_fe.slf', eps=[1e-6])
            self.check_epsilons('fe_par:T2DRES', 'f2d_diffusion_fe.slf', eps=[1e-6])
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
        # DESCRIPTION PLOTS:
        #
        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 10),
            fig_name='img/diffusiontracer_mesh0_type{}'.format(self.mesh_type),
            annotate_bnd=True,
            plot_mesh=True)

        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 10),
            xlim=[9.,11.],
            ylim=[9.,11.],
            fig_name='img/diffusiontracer_mesh0_zoom_type{}'.format(self.mesh_type),
            plot_mesh=True)


        #======================================================================
        # PLOT 2D
        records = [0, -1]

        for j, res in  enumerate(res_list):
            for idx, record in enumerate(records):
                if record==-1:
                    vmax1 = 0.6
                    vmax2 = 0.9
                else:
                    vmax1 = 1.
                    vmax2 = 1.

                vnv_plot2d(\
                    'TRACER 1',
                    res,
                    record=record,
                    vmin=0., vmax=vmax1, nv=11,
                    cbar_extend='both',
                    fig_size=(10, 8),
                    fig_name='img/diffusiontracer_T1_type{}_{}_{}'
                              .format(self.mesh_type, res_labels[j], record),
                    filled_contours=True,
                    contours=True,
                    plot_mesh=False)

                vnv_plot2d(\
                    'TRACER 2',
                    res,
                    record=record,
                    vmin=0., vmax=vmax2, nv=11,
                    cbar_extend='both',
                    fig_size=(10, 8),
                    fig_name='img/diffusiontracer_T2_type{}_{}_{}'
                             .format(self.mesh_type, res_labels[j], record),
                    filled_contours=True,
                    contours=True,
                    plot_mesh=False)

        #======================================================================
        # PLOT 1D
        vnv_plotbar_cpu_times(\
             self.action_time,
             fig_size=(7, 3),
             fig_name='img/t2d_diffusiontracer_cpu_times_type{}'
                      .format(self.mesh_type))

        records = [0, -1]

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
                fig_name='img/t2d_diffusiontracer_vf_T1_type{}_{}'
                         .format(self.mesh_type, record),
                plot_bottom=True,
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
                fig_name='img/t2d_diffusiontracer_vf_T2_type{}_{}'
                         .format(self.mesh_type, record),
                plot_bottom=True,
                markers=True,
                markevery=15)

