
"""
Validation script for bump2d
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from math import atan, degrees

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # Evolution of a bump in 2D T2D+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_bump2d-t2d.cas')


        # Evolution of a bump in 2D T2D+SIS parallel mode
        cas = TelemacCas('t2d_bump2d-t2d.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_bump2d-t2d_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_bump2d-t2d.slf',
                            eps=[4.e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_bump2d-t2d.slf',
                            eps=[1.e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[1.e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_bump2d-t2d.slf',
                            eps=[1.e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_bump2d-t2d.slf',
                            eps=[1.e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-2])


    def _post(self):
        """
        Post-treatment processes
        """
        import numpy as np
        import matplotlib.pyplot as plt
        from postel.plot_vnv import vnv_plot2d
        vnv1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv1_t2dres)

        vnv1_sisres = self.get_study_file('vnv_1:SISRES')
        res_vnv_1_sisres = TelemacFile(vnv1_sisres)

        # Plotting bottom at record 0 with mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   plot_mesh=True,
                   fig_size=(9, 7),
                   fig_name='img/bottom')

        #TODO: Redo using postel
        #########################################################################
        #
        # factor is used for scaling (evolution/h0_factor = evolutionCut)
        # See the paper of Yen et al. 1995
        #########################################################################

        slf = res_vnv_1_sisres
        mesh = np.array(slf.ikle3)
        meshx = slf.meshx
        meshy = slf.meshy

        #last time step
        timePos = -1
        bottomEnd = slf.get_data_value('BOTTOM', timePos)
        t_end = slf.times[timePos]
        t_end = "t = %1.2f h" %(t_end/3600)

        #near the end
        near_last_index = 3
        timePos = slf.ntimestep - near_last_index
        bottom = slf.get_data_value('BOTTOM', timePos)
        t_near_end = slf.times[timePos]
        t_near_end = "t = %1.2f h" %(t_near_end/3600)


        fig = plt.figure()
        ax=fig.add_subplot(111)
        #ax.axis('off')
        levels = np.asarray([-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9])


        #first we extract contour at the end
        triplotcontour = plt.tricontour(meshx,meshy,mesh,bottomEnd,levels,extend='both',alpha=0.0)
        triplotcontourf = plt.tricontourf(meshx,meshy,mesh,bottomEnd,levels,extend='both')
        plt.triplot(meshx,meshy,mesh, 'k-',alpha= 0.1)

        cbar = plt.colorbar()
        cbar.set_label('BOTTOM')

        level = 4

        p = triplotcontour.collections[4].get_paths()[0]
        v = p.vertices
        plt.plot(v[:,0],v[:,1],"k-.",label=t_end)

        minXYEnd = [10.0E+20,10.0E+20]
        for xy in v:
          if (xy[1]<minXYEnd[1]):
             minXYEnd[0] = xy[0]
             minXYEnd[1] = xy[1]




        #second we extract contour near the end
        triplotcontour = plt.tricontour(meshx,meshy,mesh,bottom,levels,extend='both',alpha=0.0)
        level = 4

        p = triplotcontour.collections[4].get_paths()[0]
        v = p.vertices
        plt.plot(v[:,0],v[:,1],"k--",label=t_near_end)

        minXY = [10.0E+20,10.0E+20]
        for xy in v:
          if minXY[1] > xy[1]:
             minXY[0] = xy[0]
             minXY[1] = xy[1]

        plt.plot(minXY[0],minXY[1],"ko")
        plt.plot(minXYEnd[0],minXYEnd[1],"ko")
        plt.plot([0.0,1000.0],[0.0,0.0],"k-")
        plt.plot([minXY[0],minXYEnd[0]],[minXY[1],minXYEnd[1]],"g-")


        #line through bottom (a) and bottom end (b)
        a0 = minXY[0]
        a1 = minXY[1]
        b0 = minXYEnd[0]
        b1 = minXYEnd[1]

        m = (a1-b1)/(a0 - b0)
        b = a1-m*a0

        #spreading angle
        alpha = degrees(atan(abs(a1-b1)/abs(a0-b0)))



        xLine = [400,500,900]
        yLine = [value *m + b for value in xLine]
        plt.plot(xLine,yLine,"k-")
        plt.legend(loc='upper right')
        plt.axes().set_aspect('equal')
        fig_name = 'img/sis_bump2d-t2d'
        #plt.tight_layout()
        plt.xlabel("x (m)")
        plt.ylabel("y (m)")
        plt.xlim([0,1000])
        plt.ylim([-500,500])
        plt.title(r"spread angle $\alpha$ = %1.2f"%(alpha))
        print(" "*8+"~> Plotting img/sis_bump2d-t2d")
        plt.savefig(fig_name + ".pdf",dpi=300)
        plt.savefig(fig_name + ".png",dpi=300)
        plt.close('all')

        slf.close()
