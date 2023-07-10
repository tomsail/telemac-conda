
"""
Validation script for pildepon
"""
from os import path
from shutil import copyfile
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
        self.rank = 4
        self.tags = ['telemac2d', 'fv']
        self.walltime = '3:00:00'

    def _pre(self):
        """
        Defining the studies
        """

        # pildepon scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_pildepon.cas')

        # pildepon scalar mode
        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_pildepon_qua.cas')

        # pildepon scalar mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_pildepon_cin.cas')

        # pildepon parallel mode
        cas = TelemacCas('t2d_pildepon.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_pildepon_par.cas',
                       cas=cas)
        del cas

        # pildepon parallel mode
        cas = TelemacCas('t2d_pildepon_qua.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_pildepon_qua_par.cas',
                       cas=cas)
        del cas

        # pildepon parallel mode
        cas = TelemacCas('t2d_pildepon_cin.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_pildepon_cin_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_pildepon.slf',
                            eps=[1.E-6, 1.E-7, 1.E-6, 1.E-7, 1.E-8, 4.E-5, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T2DRES',
                            'f2d_pildepon.slf',
                            eps=[1.E-6, 1.E-7, 1.E-6, 1.E-7, 1.E-8, 3.E-5, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1.E-6, 1.E-7, 1.E-6, 1.E-7, 1.E-8, 4.E-5, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_pildepon_qua.slf',
                            eps=[5.E-4, 9.E-4, 2.E-4, 2.E-4, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T2DRES',
                            'f2d_pildepon_qua.slf',
                            eps=[1.E-3, 2.E-3, 2.E-4, 2.E-4, 1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_2:T2DRES',
                            'vnv_5:T2DRES',
                            eps=[9.E-4, 2.E-3, 3.E-4, 3.E-4, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_pildepon_cin.slf',
                            eps=[2.6, 2.6, 1.2, 1.2, 1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T2DRES',
                            'f2d_pildepon_cin.slf',
                            eps=[2.5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[2.5])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d

        # Getting files
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1_t2dres, _ = self.get_study_res('vnv_1:T2DRES')

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D')

        # Bathy 1d
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_vnv_1_t2dgeo,
            'Initial elevation',
            fig_size=(7, 3),
            poly=[[0., -10.], [0., 10.]],
            poly_number=[30],
            record=0,
            x_label='y (m)',
            plot_bottom=True,
            fig_name='img/bathy')

        #Plotting mesh
        vnv_plot2d(\
            '',
            res_vnv_1_t2dgeo,
            plot_mesh=True,
            annotate_bnd=True,
            fig_size=(6, 4.5),
            fig_name='img/Mesh')

        # Plotting BOTTOM at 0
        vnv_plot2d(\
            'BOTTOM',
            res_vnv_1_t2dres,
            record=0,
            filled_contours=True,
            cbar_label='Bottom (m)',
            fig_size=(10, 6),
            fig_name='img/Bathy')

        # Plotting VELOCITY at 1
        vnv_plot2d(\
            'VELOCITY',
            res_vnv_1_t2dres,
            record=1,
            filled_contours=True,
            annotate_time=True,
            cbar_label='Velocity (m/s)',
            fig_size=(6, 4),
            fig_name='img/Velocity_t1')

        # Plotting VELOCITY at tf
        vnv_plot2d(\
            'VELOCITY',
            res_vnv_1_t2dres,
            record=-1,
            filled_contours=True,
            annotate_time=True,
            cbar_label='Velocity (m/s)',
            fig_size=(6, 4),
            fig_name='img/Velocity_tf')

        # Plotting VECTORS at tf
        vnv_plot2d(\
            'VELOCITY',
            res_vnv_1_t2dres,
            record=-1,
            plot_mesh=True,
            cbar_priority='vector',
            colored_vectors=True,
            cbar_label='Velocity (m/s)',
            vectors_scale=30,
            grid_resolution=[25, 25],
            fig_size=(8, 6),
            fig_name='img/Velocity_arrows')

        # Comparison
        labels = ['CASE A', 'CASE B', 'CASE C', '', '', '']
        for idx, res in enumerate(res_list):
            time = 1000
            # Plotting WATER DEPTH
            vnv_plot2d(\
                'FREE SURFACE',
                res,
                time=time,
                contours=True,
                filled_contours=True,
                vmin=-0.45,
                vmax=0.35,
                nv=9,
                cbar_extend='both',
                cbar_label='Free surface (m)',
                fig_size=(6, 4),
                fig_title=labels[idx],
                fig_name='img/FS_'+res_labels[idx])

            # Plotting VELOCITY
            vnv_plot2d(\
                'VELOCITY',
                res,
                time=time,
                vmin=0.,
                vmax=3.2,
                nv=9,
                cbar_extend='both',
                contours=True,
                filled_contours=True,
                vectors=True,
                cbar_label='Velocity (m/s)',
                vectors_scale=30,
                grid_resolution=[15, 15],
                fig_size=(6, 4),
                fig_title=labels[idx],
                fig_name='img/Velocity_'+res_labels[idx])

            # Plotting VECTORS
            vnv_plot2d(\
                'VELOCITY',
                res,
                time=time,
                plot_mesh=True,
                cbar_priority='vector',
                colored_vectors=True,
                vectors_scale=30,
                cbar_label='Velocity (m/s)',
                grid_resolution=[20, 20],
                fig_size=(6, 4),
                fig_title=labels[idx],
                fig_name='img/Velocity_arrows_'+res_labels[idx])

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()

        # Plot forces:
        file_path = path.join(self.get_vnv_working_dir('vnv_1'), 'massb_A.txt')
        if path.exists(file_path):
            copyfile(file_path, 'img/massb_A.txt')
        file_path = path.join(self.get_vnv_working_dir('vnv_1'), 'table.txt')
        if path.exists(file_path):
            copyfile(file_path, 'img/table.txt')

        force1 = self.get_study_file('vnv_1:T2DRFO')
        force2 = self.get_study_file('vnv_2:T2DRFO')
        force3 = self.get_study_file('vnv_3:T2DRFO')

        plot_force(force1, dt=0.1, fig_name='img/force_P1')
        plot_force(force2, dt=0.1, fig_name='img/force_P2')
        plot_force(force3, dt=0.1, fig_name='img/force_vf')


# Custom plot function:
def plot_force(fforce, t0=600, t1=1200, dt=0.8, fig_name=''):
    """
    plot force and save plot
    """
    import matplotlib.pyplot as plt
    import numpy as np
    from postel.deco_vnv import decoVNV, decoVNV_1d
    file_force = open(fforce, "r")
    f_1 = []
    f_2 = []
    for line in file_force:
        data = line.split()
        f_1.append(float(data[0]))
        f_2.append(float(data[1]))
    time = np.linspace(t0, t1, int((t1-t0)/dt+1))

    # Plot force
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    fig = plt.figure(figsize=(9, 4))
    sub1 = fig.add_subplot(121)
    sub1.set_title('Force')
    sub1.plot(time, f_1, time, f_2)

    # Compute FFT
    fourier1 = np.fft.fft(f_1)/len(f_1)
    fourier2 = np.fft.fft(f_2)/len(f_2)
    n_l = len(f_1)
    freq1 = np.fft.fftfreq(n_l, d=dt)
    freq2 = np.fft.fftfreq(n_l, d=dt)
    positive_frequencies1 = freq1[np.where(freq1 > 0)]
    magnitudes1 = abs(fourier1[np.where(freq1 > 0)])  # magnitude spectrum
    positive_frequencies2 = freq2[np.where(freq2 > 0)]
    magnitudes2 = abs(fourier2[np.where(freq2 > 0)])  # magnitude spectrum

    # plot FFT
    sub2 = fig.add_subplot(122)
    sub2.set_title('FFT')
    sub2.plot(positive_frequencies1, magnitudes1)
    sub2.plot(positive_frequencies2, magnitudes2)

    # Write peaks
    peak1 = np.argmax(magnitudes1)
    str1 = positive_frequencies1[peak1]*4*0.947
    peak2 = np.argmax(magnitudes2)
    str2 = positive_frequencies2[peak2]*4*0.947
    sub2.set_xlim(0., 0.7)
    file_path = path.join('img/table.txt')
    with open(file_path, 'a') as outfile:
        outfile.write('%5.3f' % str1)
        outfile.write('& %5.3f \\\\ \n' % str2)

    # Save figure
    file_force.close()
    fig.savefig(fig_name)
    plt.close('all')
