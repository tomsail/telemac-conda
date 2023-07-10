
"""
Validation script for bumpsub
"""
import numpy as np
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
import scipy.special as sc

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
        self.nu = 1.
        self.mesh_type = 1

        if self.mesh_type==1:
            self.geo_file = "geo_flume_sym.slf"
            self.cli_file = "geo_flume_sym.cli"
        elif self.mesh_type==2:
            self.geo_file = "geo_flume.slf"
            self.cli_file = "geo_flume.cli"
        elif self.mesh_type==3:
            self.geo_file = "geo_flume_unstruct.slf"
            self.cli_file = "geo_flume_unstruct.cli"

    def _pre(self):
        """
        Defining the studies
        """
        casname='t2d_diffusion_FV.cas'

        #======================================================================
        # TWO POINT FLUX SCHEME
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', self.nu)
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', 2)
        cas.set('OPTION FOR DIRICHLET CONDITION IN FV DIFFUSION', 2)
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
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', self.nu)
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', 3)
        cas.set('OPTION FOR THE RTPF SCHEME RECONSTRUCTIONS', 1)
        cas.set('OPTION FOR DIRICHLET CONDITION IN FV DIFFUSION', 2)
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
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', self.nu)
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', 3)
        cas.set('OPTION FOR THE RTPF SCHEME RECONSTRUCTIONS', 2)
        cas.set('OPTION FOR DIRICHLET CONDITION IN FV DIFFUSION', 2)
        self.add_study('rtpf2_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('rtpf2_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # HIBRID EXPLICIT FINITE ELEMENT
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', self.geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', self.cli_file)
        cas.set('DURATION', self.final_time)
        cas.set('VELOCITY DIFFUSIVITY', self.nu)
        cas.set('COEFFICIENT FOR DIFFUSION OF TRACERS', self.nu)
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME FOR TRACER DIFFUSION', 1)
        cas.set('OPTION FOR DIRICHLET CONDITION IN FV DIFFUSION', 2)
        self.add_study('hefe_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hefe_par', 'telemac2d', casname, cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        if self.mesh_type==1:
            self.check_epsilons('tpf_seq:T2DRES', 'f2d_diffusion_tpf.slf', eps=[1e-6])
            self.check_epsilons('tpf_par:T2DRES', 'f2d_diffusion_tpf.slf', eps=[1e-6])
            self.check_epsilons('tpf_seq:T2DRES', 'tpf_par:T2DRES', eps=[1e-6])

            self.check_epsilons('hefe_seq:T2DRES', 'f2d_diffusion_hefe.slf', eps=[1e-6])
            self.check_epsilons('hefe_par:T2DRES', 'f2d_diffusion_hefe.slf', eps=[1e-6])
            self.check_epsilons('hefe_seq:T2DRES', 'hefe_par:T2DRES', eps=[1e-6])

            self.check_epsilons('rtpf1_seq:T2DRES', 'f2d_diffusion_rtpf.slf', eps=[1e-6])
            self.check_epsilons('rtpf1_par:T2DRES', 'f2d_diffusion_rtpf.slf', eps=[1e-6])
            self.check_epsilons('rtpf1_seq:T2DRES', 'rtpf1_par:T2DRES', eps=[1e-6])

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
            fig_size=(10, 2),
            fig_name='img/diffusion_mesh0',
            annotate_bnd=True,
            plot_mesh=True)


        #======================================================================
        # PLOT 2D
        records = [0, 10, -1]

        for j, res in  enumerate(res_list):
            for idx, record in enumerate(records):
                vnv_plot2d(\
                    'TRACER 1',
                    res,
                    record=record,
                    fig_size=(10, 2),
                    fig_name='img/diffusion_T1_{}_{}'.format(res_labels[j], record),
                    filled_contours=True,
                    plot_mesh=True)

        #======================================================================
        # PLOT 1D
        vnv_plotbar_cpu_times(\
             self.action_time,
             fig_size=(7, 3),
             fig_name='img/t2d_diffusion_cpu_times')

        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list,
            res_labels,
            record=-1,
            fig_size=(8, 5),
            y_label='z (m)',
            x_label='x (m)',
            fig_name='img/t2d_diffusion_h_{}'.format(record),
            plot_bottom=True)

        records = range(10)

        for idx, record in enumerate(records):

            ref_T1 = analytical_solution(t=res_list[0].times[record], nu=self.nu)

            vnv_plot1d_polylines(\
                'TRACER 1',
                res_list,
                res_labels,
                record=record,
                ref_data=ref_T1,
                ref_label="Analytical solution",
                fig_size=(8, 5),
                y_label='tracer',
                x_label='x (m)',
                ylim=[0,1],
                fig_name='img/t2d_diffusion_T1_{}'.format(record),
                plot_bottom=True,
                markers=True,
                markevery=15)


def analytical_solution(L=20.,t=10.,nu=1.):
    """
    ANALYTICAL SOLUTION
    """
    nx = 1000
    x = np.linspace(0, L, nx)
    x_adim = np.linspace(0, L, nx)
    u = np.zeros(nx)
    t_adim = nu*t/(L**2)

    for i in range(nx):
        x_adim[i] = x[i]/L
        if t > 0.:
            u[i] = sc.erf( x_adim[i]/(2.*np.sqrt(t_adim)) )
        else:
            u[i] = 1.

    return np.transpose(np.array([x, u]))
