      subroutine n2qn1 (simul, n, x, f, g, dxmin, df1, epsabs, imp, io,
     &                  mode, iter, nsim, binf, bsup, iz, rz, izs, rzs,
     &                  dzs)
c
c-----------------------------------------------------------------------
c
c     N2QN1, Version 3.0, June 2013
c     Claude Lemarechal, Eliane Panier.
c
c     Modifications made by J. Charles Gilbert, INRIA.
c
c     Double precision version.
c
c----
c
c     En entree
c
c     iz(1) contient le nombre de bornes inactives lors d'un
c         redemmarage a chaud
c
c-----------------------------------------------------------------------
c
c         arguments
c
      implicit double precision (a-h,o-z)
      dimension x(n),g(n),dxmin(n),bsup(n),binf(n)
      dimension iz(*),rz(*),izs(*),dzs(*)
      logical plantage, modifx
      real rzs(*)
      external simul
c
c --- variables locales
c
      double precision gpmopt
c
c --- verification des arguments
c
      if (imp.gt.0) then
          nw=n*(9+n)/2
          ni=2*n+1
          write (io,1000) n,mode,iter,nsim,imp,df1,epsabs
      endif
 1000 format (/" N2QN1 (Version 3.0, June, 2013): point d'entree"/
     &    5x,"dimension du probleme (n):",i9/
     &    5x,"mode d'entree (mode):     ",i9/
     &    5x,"max iterations (iter):    ",i9/
     &    5x,"max simulation (nsim):    ",i9/
     &    5x,"niveau d'impression (imp):",i9/
     &    5x,"decroissance attendue de f (df1):",1pd9.2/
     &    5x,"precision absolue (epsabs):      ",1pd9.2)
c

      if (imp.ge.4) then
          write (io,1001) ni,nw
      endif
 1001 format (/" Working zone:"/
     &    5x,"integer:          ", i6/
     &    5x,"double precision: ", i6)
c
      if (n.le.0
     &  .or. (mode.eq.1 .and. df1.le.0.d0)
     &  .or. epsabs.lt.0.d0
     &  .or. imp.lt.0 .or. imp.gt.5
     &  .or. mode.lt.1 .or. mode.gt.4
     &  .or. iter.le.0
     &  .or. nsim.le.0) then
          write (io,1002)
          mode=2
          return
      endif
 1002 format (/" >>> m2qn1: appel incoherent (n, df1, epsabs, imp,",
     &         " mode, iter ou nsim)"/)
c
c     - coherence entre x(i), dxmin(i), binf(i) et bsup(i)
c
      modifx   = .false.
      plantage = .false.
      do i=1,n
          if (dxmin(i).le.0.d0) then
              if (.not.(modifx.or.plantage)) write (io,'()')
              write (io,'(/a,1pd12.5,a,i4/)')
     &            " >>> n2qn1: dxmin(i) = ",dxmin(i)," est negatif, ",
     &            "i = ", i
              plantage = .true.
              mode=2
          elseif (bsup(i)-binf(i).lt.-2.d0*dxmin(i)) then
              if (.not.(modifx.or.plantage)) write (io,'()')
              write (io,'(/a,1pd12.5,a,d12.5,a,i4/)')
     &            " >>> n2qn1: binf(i) = ",binf(i),"  > bsup(i) = ",
     &            bsup(i),", i = ", i
              plantage = .true.
              mode=2
          elseif (x(i).lt.binf(i)-dxmin(i)) then
c<
              plantage = .true.
              mode=2
              if (.not.(modifx.or.plantage)) write (io,'()')
              write (io,'(/a,1pd12.5,a,d12.5,a,i4/)')
     &            " >>> n2qn1: x(i) = ",x(i),"  < binf(i) = ",
     &            binf(i),", i = ", i
c>
c             if (.not.(modifx.or.plantage)) write (io,'()')
c             write (io,'(a,1pd12.5,a,d12.5,a,i4,a)')
c    &            " >>> n2qn1: x(i) = ",x(i),"  < binf(i) = ",
c    &            binf(i),", i = ", i,"  ==>  x(i) = binf(i)"
c             x(i) = binf(i)
c             modifx = .true.
c     in addition, recompute f and g at the new point !!!
c=
          elseif (x(i).gt.bsup(i)+dxmin(i)) then
c<
              plantage = .true.
              mode=2
              if (.not.(modifx.or.plantage)) write (io,'()')
              write (io,'(/a,1pd12.5,a,d12.5,a,i4/)')
     &            " >>> n2qn1: x(i) = ",x(i),"  > bsup(i) = ",
     &            bsup(i),", i = ", i
c>
c             if (.not.(modifx.or.plantage)) write (io,'()')
c             write (io,'(a,1pd12.5,a,d12.5,a,i4,a)')
c    &            " >>> n2qn1: x(i) = ",x(i),"  > bsup(i) = ",
c    &            bsup(i),", i = ", i,"  ==>  x(i) = bsup(i)"
c             x(i) = bsup(i)
c             modifx = .true.
c     in addition, recompute f and g at the new point !!!
c=
          endif
      enddo
      if (plantage) return
c
c --- partage de la memoire
c
      nd=1+(n*(n+1))/2
      nww=nd+n
      nww1=nww+n
      nga=nww1+n
      nindi=1
      nibloc=nindi+n
      ni=nibloc+n
c
c --- calcul du test d arret
c
      s=0.d0
      do i=1,n
          s=s+dxmin(i)*dxmin(i)
      enddo
      epsabs=epsabs*dsqrt(s/dble(float(n)))
      call n2qn1a (simul,n,x,f,g,dxmin,epsabs,gpmopt,df1,mode,
     &             iter,nsim,imp,io,rz,rz(nd),rz(nww),rz(nww1),
     &             rz(nga),binf,bsup,iz(nindi),iz(nibloc),iz(ni),
     &             izs,rzs,dzs)

c
c --- sorties finales
c
cfz      call nqhess(n,imp,io,iz,rz)
      if (imp.ge.2) write(io,1003)
 1003 format (/1x,79("-"))
      if (imp.ge.1) write(io,1004) mode,iter,nsim,epsabs,gpmopt
 1004 format(/" N2QN1: sortie en mode ",i2/
     &     5x,"nombre d'iterations   = ",i4/
     &     5x,"nombre de simulations = ",i4/
     &     5x,"|gradient projete| moyen       = ",1pd10.3/
     &     5x,"|gradient_dxmin projete| moyen = ",1pd10.3)
      if (imp.ge.4) write (io,1005) (i,iz(nibloc+i-1),i=1,n)
 1005 format (5x,"bornes",
     &    " (0: inactive, -1: binf active, +1: bsup active)",/
     &    (9x,"| ",5(i5,": ",i2," |")))
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine n2qn1a (simul,n,x,f,ga,dxmin,acc,gpmopt,
     &                   df1,mode,niter,nsim,iprint,lp,h,d,w,w1,g,
     &                   binf,bsup,indi,ibloc,iz,izs,rzs,dzs)
c
      implicit double precision (a-h,o-z)
      dimension x(n),g(n),dxmin(n),h(*),d(n),w(n),w1(n)
      dimension binf(n),bsup(n),ga(n),izs(*),iz(*)
      dimension dzs(*),ibloc(n),indi(n)
      double precision gdxmin,gpmopt,r
      real rzs(*)
      external simul,fuclid
c
c-----------------------------------------------------------------------
c
c     The actual optimization code
c
c     Input
c
c       acc (double precision) = required average accuracy in scaled
c         space
c
c-----------------------------------------------------------------------
c
c --- local variables
c
      logical oltodo
      integer np
      double precision gpm1, gpm, opt, ys, shs, ol
c
c --- parameters
c
      double precision zero, pi
      parameter (zero = 0.d0, one = 1.d0, pi = 3.1415927d+0)
c
! 1024 format (1x)
c
c --- initialisation
c
      alfa=0.99d0
      beta=1.d-4
      prop=one
      nfun=1
c     iecri=0
      itr=0
      np=n+1
      indic2=1
      logic=0
      df=df1
      oltodo=.false.
      if (mode.eq.1) oltodo=.true.
      if (mode.eq.4) then
          nr=iz(1)
          go to 400
      endif
c
c --- calcul des bornes actives
c     nr = nbr of free variables
c     ibloc(i) = -1 borne inf active
c              =  0 bornes inactives
c              =  1 borne sup active
c     ga = gradient at the current point
c
      nr=0
      if (mode.eq.1) then
          do i=1,n
              if (x(i).ge.bsup(i)-dxmin(i) .and. ga(i).lt.0.) then
c
c                 - borne sup active
c
                  ibloc(i)=1
              elseif (x(i).le.binf(i)+dxmin(i) .and. ga(i).gt.0.) then
c
c                 - borne inf active
c
                  ibloc(i)=-1
              else
c
c                 - bornes inf et sup inactives
c
                  nr=nr+1
                  ibloc(i)=0
              endif
          enddo
c<
c J.Ch.G. ne comprend pas la logique de ce qui suit.
c Pourquoi ne pas faire jouer le gradient si mode = 2 ou 3 ?
c>
      else
          do i=1,n
              if (x(i).ge.bsup(i)-dxmin(i)) then
                  ibloc(i)=1
              elseif (x(i).le.binf(i)+dxmin(i)) then
                  ibloc(i)=-1
              else
                  nr=nr+1
                  ibloc(i)=0
              endif
          enddo
      endif
      nr1=nr+1
c
c --- le point de depart est-il optimal?
c
c     gpm = norm(g.dxmin)     if nr = 0   (it is used for printings)
c         = rel_norm(g.dxmin) otherwise
c
      r=zero
      c=zero
      dnr=dsqrt(dble(float(nr)))
      acc1=acc*dnr
      do 100 i=1,n
          if (ibloc(i).ne.0)  go to 100
          gi=ga(i)
          gdxmin=gi*dxmin(i)
          r=r+gi*gi
          c=c+gdxmin*gdxmin
  100 continue
      r=dsqrt(r)
      c=dsqrt(c)
      if (iprint.ge.1) then
          gpm1=c
          gpm=c
          if (nr.ne.0) then
              r=r/dnr
              gpm1=c/dnr
              gpm=one
          endif
          if (gpm1.gt.zero) opt=acc/gpm1
          write (lp,1004) r,gpm1,opt
      endif
 1004 format (/" Conditions initiales:"/
     &    5x,"|gradient projete| moyen:       ",1pd11.4/
     &    5x,"|gradient_dxmin projete| moyen: ",1pd11.4/
     &    5x,"optimalite relative demandee:   ",d11.4/)
c
c --- check optimality
c
      if (c.le.acc1) then
c
c         - is there a constraint to desactivate ?
c
          call fcomp1 (indic2,ibloc,indi,h,ga,d,w,w1,n,nr,ncs,dga,delta,
     &                 prop,acc,dxmin)
          if (ncs.eq.0) then
c
c             - ... no there isn't, then stop here
c
              itr=1
              mode=1
              go to 900
          endif
c
c             - ... yes there is
c
          ibloc(ncs)=0
          nr=nr+1
      endif
c
c-----------------------------------------------------------------------
c     initialisation du hessien, selon la valeur de "mode"
c-----------------------------------------------------------------------
c
c     (avant de transformer ce "computed goto" en if-then-else, mettre
c     ce qui est fait lorsque mode=1 en sous-routine, car il y a un
c     goto 300 !! Il n'y a pas de goto 310, ni de goto 320)
c
      go to (300,310,320),mode
c>    if (mode.eq.1) then
c
c         --- (mode = 1)
c             calcul du hessien initial (en fonction de dxmin et de df1)
c             et de indi (ici, nr = nb de variables inactives)
c
  300     continue
c
c         . c = inverse du pas de Fletcher = |g_dxmin|^2/(2*df1)
c
          c=zero
          do i=1,n
              if (ibloc(i).eq.0) then
                  gdxmin=ga(i)*dxmin(i)
                  c=c+gdxmin*gdxmin
              endif
          enddo
          c=0.5d0*c/df1
c
c         . la diagonale du hessien initial sera c/dxmin^2
c
          do i=1,n
              sc=dxmin(i)
              w(i)=c/(sc*sc)
          enddo
c
c         . h = 0
c
          nh=n*(n+1)/2
          do i=1,nh
              h(i)=zero
          enddo
c
c         . indi etablit une permutation de I = {1, ..., n}, c'est donc une
c           bijection qui a i dans I fait correspondre indi(i) dans I.
c           Les nr premiers elements de indi(I) correspondent aux
c           variables actives, les suivants aux variables inactives. Donc
c           indi^(-1){1, ..., nr} donne les indices des variables actives et
c           indi^(-1){nr, ..., n} donne les indices des variables inactives.
c
          k1=1
          k2=nr+1
          do i=1,n
              if (ibloc(i).eq.0) then
                  indi(i)=k1
                  k1=k1+1
              else
                  indi(i)=k2
                  k2=k2+1
              endif
          enddo
c
c         . initialize the Hessian matrix approximation H to a diagonal
c         matrix w
c
          mode=1
          call fmani1 (mode,n,w,d,indi)
          call n2qn1_init_diag (n, nr, h, d)
          go to 400
c
c     --- (mode = 2)
c         verification de la definie positivite de h
c         permutation et factorisation
c
c>    elseif (mode.eq.2) then
  310     call fmc11b (h,n,k)
          if (k.lt.n) then
              if (iprint.ne.0) write(lp,1010)
              goto 300
          endif
 1010     format (" n2qn1: remplace le hessien initial (qui n'est",
     &     " pas defini positif)"/" par une diagonale positive")
  312     nr=n
          do 313 i=1,n
  313     indi(i)=i
          do 314 i=1,n
          if (ibloc(i).eq.0) go to 314
          nc=i
          call fajc1 (n,nc,nr,h,w,indi)
  314     continue
          go to 400
c
c     --- (mode = 3)
c         verification que la diagonale est positive
c
c>    elseif (mode.eq.3) then
  320     k=1
          do i=1,n
              if (h(k).le.zero) then
                  if (iprint.ne.0) write(lp,1010)
                  goto 300
              endif
              k=k+np-i
          enddo
          go to 312
c>    else
c>    endif
c
c --- on est pret a y aller
c
  400 indic2=0
      if (iprint.lt.3.and.iprint.gt.0) then
          write(lp,'()')
      elseif (iprint.eq.4) then
          write (lp,1003) (i,ibloc(i),i=1,n)
 1003     format (5x,"bornes",
     &        " (0: inactive, -1: binf active, +1: bsup active)",/
     &        (9x,"| ",5(i5,": ",i2," |")))
          write(lp,'()')
      endif
      dnr=dsqrt(dble(float(nr)))
      acc1=acc*dnr
      if (iprint.eq.3) write(lp,1111,advance="no")
 1111 format (1x,79("-"))
c
c-----------------------------------------------------------------------
c     iteration loop
c-----------------------------------------------------------------------
c
c     start at 500, end at 900
c
  500 itr=itr+1
c     write (6,'(a,1pd12.5,2x,d12.5)')  "x    = ", (x(i),i=1,2)
      if (itr.ne.1)df=fa-f
      fa=f
      indic1=0
      if (itr.le.niter) go to 502
      mode=4
      go to 900
  502 continue
      flush (lp)
      if (iprint.eq.3) then
          if (mod(itr-1,40).eq.0) write (lp,'(//a/a)',advance="no")
     &       "  iters  simuls  nactiv       f         |gp|/|gp0|",
     &       "  ^^^^^  ^^^^^^  ^^^^^^  ^^^^^^^^^^^^  ^^^^^^^^^^^^"
          write (lp,1019,advance="no") itr, nfun, n-nr, f, gpm
 1019     format (/1x,i6,2x,i6,2x,i6,2x,1pe12.5,2x,es12.6)
      elseif (iprint.ge.4) then
          write(lp,1020) itr,nfun,f
 1020     format (1x,79("-")/" n2qn1:",i4," iters",i6," simuls","   f=",
     &        1pd15.7/)
      endif
c
c --- call simulator with indic = 1 at every iteration
c
c     iecri=iecri+1
c     if (iecri.eq.-iprint) then
c         iecri=0
          indic=1
          call simul(indic,n,x,f,g,izs,rzs,dzs)

c     endif
c
c --- calcul de la direction de recherche et du test d'arret
c     activation ou desactivation des bornes
c     retour ici avant RL si une contrainte s'active ou se deactive
c
  510 continue
c     write (6,'(a,8(i3))')  "indi = ", (indi(i),i=1,n)
c     write (6,'(a,i12)')  "nr   = ", nr
c     write (6,'(a,1pd12.5,2x,d12.5)')  "g    = ", (g(i),i=1,2)
      if (nr.ne.0) go to 511
      indic2=1
      go to 540
  511 mode=1
c     . w = - ga(indi^(-1))
      call fmani1 (mode,n,ga,w,indi)
      wii=zero
      do i=1,nr
          wi=w(i)
          wiii=wi*dxmin(i)
          wii=wii+wiii*wiii
          w(i)=-wi
      enddo
      wii=dsqrt(wii)
      gpm=wii
      if (wii.gt.acc1) go to 513
      indic2=1
c<
c  jchg: it is here that optimality is detected
c>
      go to 540
c
c     - calcul de w := H^(-1) w
c           et de d, la direction de recherche
c
  513 call fmc11e (h,nr,w,w1,nr)
c     write (6,'(a,1pd12.5,2x,d12.5)')  "Hr   = ", (w(i),i=1,2)
c     write (6,'(a,1pd12.5,2(2x,d12.5))')  "h    = ", (h(i),i=1,3)
      if (nr.lt.n) then
          nrp1=nr+1
          do i=nrp1,n
              w(i)=zero
          enddo
      endif
      mode=-1
      call fmani1 (mode,n,w,d,indi)
c     write (6,'(a,1pd12.5,2x,d12.5)')  "d    = ", (d(i),i=1,2)
c
c     - calcul de la derivee directionnelle
c           et de l'angle avec le gradient
c       JCHG: pourquoi dga peut-il etre >= 0 ?
c
      dga=zero
      do i=1,n
          dga=dga+ga(i)*d(i)
      enddo
      if (dga.lt.zero) go to 522
      indic2=1
      go to 540
  522 if (indic1.eq.1) go to 550
c
c     - contrainte sortante
c
  540 call fcomp1 (indic2,ibloc,indi,h,ga,w,d,g,n,nr,ncs,
     &             dga,delta,prop,acc,dxmin)
      if (ncs.ne.0) go to 543
      if (indic2.ne.1) go to 541
c<
c  jchg: it is here that optimality is detected
c>
      mode=1
      go to 900
  541 mode=-1
      call fmani1 (mode,n,w,d,indi)
      go to 550
  543 continue
      if (iprint.eq.3) then
          write(lp,'(a,i0)',advance="no") " -", ncs
      elseif (iprint.ge.2) then
          write(lp,1022) itr,nfun,f,ncs
      endif
 1022 format (" n2qn1:",i4," iters",i6," simuls","   f=",d15.7,
     & "   borne",i4,"  desactivee")
      indic1=1
      logic=6
c                mise a jour de ibloc et de h
      ibloc(ncs)=0
      call fretc1 (mode,n,ncs,nr,h,w,indi,indic2)
      indic2=0
      dnr=dsqrt(dble(float(nr)))
      acc1=acc*dnr
      if (mode.eq.0) go to 511
      mode=7
      if (iprint.ne.0) write(lp,'(/a/)')
     &    " >>> n2qn1: error in the update of L"
      go to 900
c
c     - calcul de romax (pas maximal)
c           et de nca (indice de la contrainte activee, si romax est
c                      nul a dxmin pres)
c
c       (here one should look whether binf = -infty or bsup = +infty, to
c       detect the presence of bounds)
c
  550 romax=1.d50
      nca=0
      do 555 i=1,n
          di=d(i)
          if (di.eq.zero) go to 555
          if (di.lt.zero) then
              bi=binf(i)
              xi=bi-x(i)
              if (-one.ge.di)go to 551
              if (xi.le.(di*1.d20)) go to 555
  551         rocand=xi/di
              i1=-1
          else
              bi=bsup(i)
              xi=bi-x(i)
              if (di.ge.one) go to 553
              if (xi.gt.(di*1.d20)) go to 555
  553         rocand=xi/di
              i1=1
          endif
          if (rocand.lt.romax) then
              nca=i
              romax=rocand
              isign=i1
          endif
  555 continue
c
      if ((nca.gt.0) .and. (dabs(romax*d(nca)).le.dxmin(nca))) then
c
c         . activatonstraint "nca" since romax is zero (or almost zero)
c
          ibloc(nca) = isign
          indic1 = 1
c
c         . update indi, h, and acc1
c
          call fajc1 (n,nca,nr,h,w,indi)
          dnr = dsqrt(dble(float(nr)))
          acc1 = acc*dnr
c
c         . printings
c
          if (iprint.eq.3) then
              if (isign.lt.0) then
                  write(lp,'(a,i0,a)',advance="no") " +",nca,"inf"
              else
                  write(lp,'(a,i0,a)',advance="no") " +",nca,"sup"
              endif
          elseif (iprint.ge.2) then
              if (isign.lt.0) then
                  write(lp,'(a,i4,a/)') " n2qn1: binf",nca," activated"
              else
                  write(lp,'(a,i4,a/)') " n2qn1: bsup",nca," activated"
              endif
          endif
c
c         . cycle
c
          go to 510
      endif
c
c --- recherche lineaire
c
c     write (6,'(a,1pd12.5,2x,d12.5)')  "d    = ", (d(i),i=1,2)
c
c     - impression de l'optimalite relative
c
      if (iprint.ge.3) then
          if (nr.gt.0) gpm=wii/dsqrt(dble(float(nr)))
          if (gpm1.gt.zero) gpm=gpm/gpm1
          if (iprint.ge.4) then
              write (lp,1005) gpm
c
c         - angle(-gp,d) (gp est le gradient projete)
c
              gg=zero
              dd=zero
              do i=1,n
                  if (ibloc(i).eq.0) then
                      gg=gg+ga(i)*ga(i)
                      dd=dd+d(i)*d(i)
                  endif
              enddo
              dd=dga/dsqrt(gg)/dsqrt(dd)
              dd=dmin1(-dd,1.d+0)
              dd=dacos(dd)*180.d0/pi
              write (lp,1021) sngl(dd)
          endif
      endif
 1005 format (1x,"n2qn1: optimalite relative: ",1pd11.4)
 1021 format (/1x,"n2qn1: angle(-gp,d) = ",f5.1," degrees")
c
c     - appel de nlis0
c
      if ((itr.le.n.and.itr.ne.1).and.mode.eq.1) go to 571
      ro=one
      go to 573
  571 if (logic.eq.1) go to 573
      if (logic.ne.6) go to 572
      ro=one
      go to 573
  572 ro=-2.d0*df/dga
  573 roa=ro
      ro=dmin1(ro,romax)
      romin=zero
      do i=1,n
          z=d(i)
          romin = dmax1(romin,dabs(z/dxmin(i)))
      enddo
      romin=one/romin
c
      if (iprint.ge.4) write (lp,'(/a)') " n2qn1: linesearch"
      call nlis0 (n,simul,fuclid,x,f,dga,ro,romin,romax,d,g,
     &            alfa,beta,iprint,lp,logic,nfun,nsim,
     &            w,izs,rzs,dzs)
c
c     if (iprint.gt.3) write(lp,1024)
      if (logic.le.1) go to 575
      if (logic.eq.4)mode=5
      if (logic.eq.5)mode=0
      if (logic.eq.6)mode=6
      if (logic.eq.7)mode=indic
      go to 900
c
c --- update of H by the BFGS formula ----------------------------------
c
c     - en cas de descente bloquee, on multipliera le nouveau gradient
c       gg par theta de maniere a avoir theta * gg'*d = alfa * g'*d, ou
c       alfa = 0.99 est le coeff d'Armijo
c
c       (premiere fois que J.Ch.G. voit cela ..., mieux vaut peut-etre
c       faire une correction de Powell ou sauter la maj, car on change
c       de toutes facons d'ensemble actif, ce que l'on ne fera pas
c       indefiniment)
c
  575 theta=one
c
c     if (logic.ne.0) then
c         dgaa=zero
c         do i=1,n
c             dgaa=dgaa+g(i)*d(i)
c         enddo
c         if (dgaa.lt.alfa*dga) theta=alfa*dga/dgaa
c     endif
c
c     - Modification by J.Ch.G. into: if the descent is blocked, then
c       skip the update (without forgetting to put the new gradient in
c       ga)
c
      if (logic.ne.0) then
c>        if (oltodo) then
c
c             . the matrix H has not been initialized; use the stepsize
c               to to scale it
c
c>            ro = one/ro
c>            call n2qn1_mult_diagh (n, nr, h, ro)
c>            if (iprint.ge.4) write (lp,'(/1x,a,1pd12.5)')
c>   &            "n2qn1: Hessian approximation multiplied by ",ro
c>        elseif (iprint.ge.4) then
              write (lp,'(1x,a)',advance='no') "BFGS update skipped"
c>        endif
          do i = 1,n
              ga(i) = g(i)
          enddo
          goto 500
      endif
c
c     - faire la mise a l'echelle d'Oren-Luenberger la premiere fois
c       que l'on passe ici si le mode d'entree est 1
c       (alors la RL realise Wolfe)
c
      if (oltodo .and. nr.gt.0) then
          ys=zero
          ii=1
          do i=1,n
              ys=ys+(g(i)-ga(i))*d(i)
              ii=ii+np-i
          enddo
          call fmani1 (1,n,d,w,indi)
          shs=zero
          k=1
          do i=1,nr
              shs=shs+h(k)*w(i)*w(i)
              k=k+nr1-i
          enddo
          if (ys.le.zero .or. shs.le.zero) then
              mode=3
              if (iprint.gt.0)
     &            write (lp,'(/a,a/(12x,a,1pd12.5))')
     &            " >>> n2qn1: unsafe vectors y and s for BFGS ",
     &            "update",
     &            "y'*s = ", ys*ro,
     &            "s'*H*s = ", shs*ro*ro
              goto 900
          endif
c
          ol=ys/shs/ro
          if (iprint.ge.4) write (lp,'(/1x,a,1pd8.2)')
     &        "n2qn1: OL factor for matrix initialization ", ol
          if (oltodo) call n2qn1_mult_diagh (n, nr, h, ol)
          oltodo=.false.
      endif
c
c     - Compute . (H*d)_a   put it in the last n-nr position of d
c               . d'*ga     put it in dga
c
c       Let us denote by v_i (resp. v_a) the components of a vector v
c       corresponding to the inactive (resp. active) bounds.
c
c       Knowing that d_a = 0, there hold
c
c          (H*d)_a = H_ai * d_i
c          d'*ga = d_i' * ga_i
c
      mode = 1
      call fmani1 (mode,n,d,w,indi)
      ir = -nr
      call fmani1 (mode,n,ga,d,indi)
      do i = 1,nr
          d(i) = -d(i)
      enddo
c
      call fmlag1 (n,nr,h,w,d)
c
      dga = zero
      do i = 1,nr
          dga = dga-w(i)*d(i)
      enddo
c
c     - update H := H + (1/dga) * d * d'
c       with d = (-ga_i, (H*d)_a)
c            dga = d'*ga
c
      call fmc11z (h,n,nr,d,one/dga,w1,ir,1,zero)
c
c     - Compute . y = g-ga  put it in d
c               . d'*y      put it in dga
c
c       update ga := g
c
      ir = -ir
      do i = 1,n
          gi = g(i)
          g(i) = theta*gi-ga(i)
          ga(i) = gi
      enddo
      call fmani1 (mode,n,g,d,indi)
c
      dga = zero
      do i = 1,nr
          dga = dga+w(i)*d(i)
      enddo
      dga = dga*ro
      ro = roa
c
c     - update H := H + (1/dga) * d * d'
c       with d = y
c            dga = d'*y
c
      call fmc11z (h,n,nr,d,one/dga,w1,ir,0,zero)
c
c     - test du rang de la nouvelle sous-matrice active
c
      if (ir.lt.nr) then
          mode = 3
          if (iprint.gt.0) write (lp,'(/a/)')
     &        " >>> n2qn1: the updated BFGS matrix is rank deficient"
          goto 900
      endif
c
c --- loop -------------------------------------------------------------
c
      goto 500
c
c-----------------------------------------------------------------------
c     end of the iterations
c-----------------------------------------------------------------------
c
  900 if (mode.ne.5.and.mode.ne.3.and.mode.ge.0) go to 910
          indic=4
          call simul (indic,n,x,f,ga,izs,rzs,dzs)
  910 iz(1)=nr
c           calcul de la precision obtenue
      acc=zero
      gpmopt=zero
      do 920 i=1,n
          if (ibloc(i).ne.0) go to 920
          gi=ga(i)
          gdxmin=ga(i)*dxmin(i)
          acc=acc+gi*gi
          gpmopt=gpmopt+gdxmin*gdxmin
  920 continue
      if (dnr.gt.zero) then
          acc=dsqrt(acc)/dnr
          gpmopt=dsqrt(gpmopt)/dnr
      endif
      niter=itr
      nsim=nfun
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fcomp1 (indic2,ibloc,indi,h,g,d,w,w1,n,nr,ncs,
     &                   dga,delta,prop,acc,dxmin)
c
      implicit double precision (a-h,o-z)
      dimension ibloc(n),indi(n),h(*),g(n),d(n),
     &w(n),w1(n),dxmin(n)
c
c-----------------------------------------------------------------------
c
c     This routine indicates which bound is interesting to desactivate
c     by looking at the expected decrease on the quadratic model.
c
c     indic2 = 0 normal study
c            = 1 the user has decided to desactivate, hence the sign of
c                the multiplier must be analysed.
c     g: gradient
c     d: direction
c     w, w1: working zones
c     ncs: index of a constraint to desactivate
c     dga: directional derivative
c     prop; about 0.5 ?
c
c-----------------------------------------------------------------------
c
      ncs=0
      if (nr.eq.n) return
c
      zm=0.d0
      if (indic2.eq.1) go to 900
      delta=0.d0
      nh=nr*(nr+1)/2
      nrr=n-nr
      call fmlag1 (n,nr,h,d,w)
      do 500 i=1,n
          ibi=ibloc(i)
          if (ibi.eq.0) go to 500
          gi=g(i)
          inc=indi(i)
          inc1=inc-1
          inr=inc-nr
          winc=w(inc)
          dmu=winc+gi
          am=dmin1(dabs(gi),dabs(dmu))
          if (2.d0*dabs(winc).ge.am) go to 500
          if (ibi.eq.-1.and.dmu.ge.0.d0) go to 500
          if (ibi.eq.1.and.dmu.le.0.d0) go to 500
          dmu=dabs(dmu)
          if (dmu*dxmin(i).le.acc) go to 500
          dmu1=dmu*dmu
          k=inr
          nh1=(inc1)*(n+1)-(inc1)*inc/2+1
          z=h(nh1)
          if (nr.eq.0) go to 350
          do j=1,nr
              w1(j)=h(nh+k)
              k=k+nrr
          enddo
          call fmc11e (h,nr,w1,w1,nr)
          k=inr
          do j=1,nr
              z=z-w1(j)*h(nh+k)
              k=k+nrr
          enddo
  350     dmu1=dmu1/z
          if (dmu1.le.delta) go to 500
          delta=dmu1
          ncs=i
          zm=dmu
  500 continue
      if (ncs.eq.0) return
      if (delta.le.-prop*dga)ncs=0
      return
  900 do 910 i=1,n
          ibi=ibloc(i)
          if (ibi.eq.0) go to 910
          dmu=g(i)
          if (ibi.eq.-1.and.dmu.ge.0.d0) go to 910
          if (ibi.eq.1.and.dmu.le.0.d0) go to 910
          dmu=dabs(dmu)*dxmin(i)
          if (dmu.le.zm) go to 910
          zm=dmu
          ncs=i
  910 continue
      if (zm.le.acc) ncs=0
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine n2qn1_init_diag (n, nr, h, diag)
c
      implicit none
      integer n, nr
      double precision h(*), diag(n)
c
c-----------------------------------------------------------------------
c
c     Initialize the Hessian matrix H to a diagional matrix, whose
c     diagonal elements are those of the vector "diag".
c
c     The matrix H is stored in the vector "h" in a particaular form.
c     The first part of "h" contains H_ii, the second H_ai, and the
c     third H_aa (here, an index "i" corresponds to the inactive or
c     free variables, an index "a" corresponds to the active or fixed
c     variables). H_ii is stored in factored form with the diagonal
c     factor on the diagonal of H_ii. The matrices are stored column by
c     column.
c
c     On entry
c     ^^^^^^^^
c     n, integer: order of the Hessian matrix H;
c     nr, integer: order of H_ii, or number of inactive variables;
c     diag(n), double precision: the elements to put on the diagonal of
c         H; the elements are ranged such that the indices 1, ..., nr
c         correspond to the inactive variables.
c
c     On return
c     ^^^^^^^^^
c     h(*), double precision: matrix H initialized to a diagonal
c         matrix.
c
c-----------------------------------------------------------------------
c
c --- local variables
c
      integer i, k, n1, nr1
c
c --- initialization
c
      n1 = n+1
      nr1 = nr+1
c
c --- initialize the diagonal of H_ii
c
      if (nr.gt.0) then
          k = 1
          do i = 1,nr
              h(k) = diag(i)
              k = k+nr1-i
          enddo
      endif
c
c --- initialize the diagonal of H_aa
c
      if (nr.lt.n) then
          k = nr*nr1/2 + nr*(n-nr) + 1
          do i = nr1,n
              h(k) = diag(i)
              k = k+n1-i
          enddo
      endif
c
c --- there is no diagonal elements in the rectangular part H_ai
c
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine n2qn1_mult_diagh (n, nr, h, r)
c
      implicit none
      integer n, nr
      double precision h(*), r
c
c-----------------------------------------------------------------------
c
c     This subroutine is very similar to n2qn1_init_diag, but instead
c     of initializing the diagonal of the Hessian approximation, it
c     multiplies it by the scalar r.
c
c     It is not checked that r is positive, but it should be.
c
c     On entry
c     ^^^^^^^^
c     n, integer: order of the Hessian matrix H;
c     nr, integer: order of H_ii, or number of inactive variables;
c     r, double precision: the scalar by which the diagonal entries of
c         H must be multiply.
c
c     On return
c     ^^^^^^^^^
c     h(*), double precision: updated matrix H.
c
c-----------------------------------------------------------------------
c
c --- local variables
c
      integer i, k, n1, nr1
c
c --- initialization
c
      n1 = n+1
      nr1 = nr+1
c
c --- multiply the diagonal of H_ii
c
      if (nr.gt.0) then
          k = 1
          do i = 1,nr
              h(k) = h(k)*r
              k = k+nr1-i
          enddo
      endif
c
c --- multiply the diagonal of H_aa
c
      if (nr.lt.n) then
          k = nr*nr1/2 + nr*(n-nr) + 1
          do i = nr1,n
              h(k) = h(k)*r
              k = k+n1-i
          enddo
      endif
c
c --- there is no diagonal elements in the rectangular part H_ai
c
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fmc11z (a,n,nr,z,sig,w,ir,mk,eps)
      implicit double precision (a-h,o-z)
      dimension a(*),z(n),w(n)
c
c-----------------------------------------------------------------------
c
c     Update A := A + sigma * z * z',
c
c     where A is stored as triangular NW part (nr x nr)
c              followed by rectangular SW part ((n-nr) x nr)
c              followed by triangular SE part ((n-nr) x (n-nr))
c
c-----------------------------------------------------------------------
c
      if (nr.eq.n) go to 45
      nr1=nr+1
      nh=nr*(nr1)/2+1
c
c --- update the rectangular SW part ((n-nr) x nr)
c
      if (nr.ne.0) then
          do i=1,nr
              do j=nr1,n
                  a(nh)=a(nh)+sig*z(i)*z(j)
                  nh=nh+1
              enddo
          enddo
      endif
c
c --- update the triangular SE part ((n-nr) x (n-nr))
c
      do j=nr1,n
          do i=j,n
              a(nh)=a(nh)+sig*z(i)*z(j)
              nh=nh+1
          enddo
      enddo
c
c --- update the triangular NW part (nr x nr), in factor form
c
      if (nr.eq.0) return
   45 call fmc11a (a,nr,z,sig,w,ir,mk,eps)
c
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fajc1(n,nc,nr,h,w,indi)
c
c-----------------------------------------------------------------------
c
c     Makes the appropriate modifications to h, indi, ..., when
c     constraint nc is activated.
c
c-----------------------------------------------------------------------
c
      implicit double precision (a-h,o-z)
      dimension h(*),w(n),indi(n)
c
      inc=indi(nc)
      nr1=nr+1
      nr2=nr-1
      nrr=n-nr
      nkk=nr-inc
c
c          recherche des composantes de H
c
      do 260 i=1,nr
          ik=i
          ij=inc
          ii=1
          ko=min0(ik,inc)
          v=0.d0
          if (ko.eq.1) go to 252
          kom1=ko-1
          do 250 k=1,kom1
              nk=nr1-k
              v=v+h(ij)*h(ik)*h(ii)
              ij=ij+nk-1
              ii=ii+nk
              ik=ik+nk-1
  250     continue
  252     a=1
          b=1
          if (ko.eq.i) go to 253
          a=h(ik)
  253     if (ko.eq.inc) go to 259
          b=h(ij)
  259     w(i)=v+a*b*h(ii)
  260 continue
c
c          mise a jour de L
c
      if (inc.eq.nr) go to 315
      inc1=inc-1
      nh=inc1*nr1-inc1*inc/2+2
      nh1=nh+nkk
      di=h(nh-1)
      do 310 j=1,nkk
          di1=h(nh1)
          nh1=nh1+1
          a=h(nh)
          ai=a*di
          c=(a**2)*di+di1
          h(nh)=c
          nh=nh+1
          if (j.eq.nkk) go to 315
          nkkmj=nkk-j
          do 300 i=1,nkkmj
              h1=h(nh)
              h2=h(nh1)
              u=ai*h1+h2*di1
              h(nh)=u/c
              h(nh1)=-h1+a*h2
              nh=nh+1
              nh1=nh1+1
  300     continue
          nh=nh+1
          di=di*di1/c
  310 continue
c
c          condensation de la matrice L
c
  315 nh=inc+1
      nsaut=1
      nj=nr-2
      if (inc.eq.1) nj=nj+1
      if (nr.eq.1) go to 440
      do 430 i=1,nr2
          do 425 j=1,nj
              h(nh-nsaut)=h(nh)
              nh=nh+1
  425     continue
          nsaut=nsaut+1
          nh=nh+1
          if (i.eq.inc-1) go to 430
          nj=nj-1
          if (nj.eq.0) go to 440
  430 continue
c
c          mise a jour de la matrice H
c
  440 nh=((nr*nr2)/2)+1
      nw=1
      nsaut=nr
      if (inc.eq.1) go to 470
      incm1=inc-1
      do 460 i=1,incm1
          h(nh)=w(nw)
          nw=nw+1
          nsaut=nsaut-1
          if (n.eq.nr) go to 455
          do 450 j=1,nrr
              h(nh+j)=h(nh+nsaut+j)
  450     continue
  455     nh=nh+nrr+1
  460 continue
  470 nw=nw+1
      if (nr.eq.n) go to 485
      do 480 i=1,nrr
          w(nr+i)=h(nh+nsaut+i-1)
  480 continue
      nsaut=nsaut+nrr
  485 if (inc.eq.nr) go to 510
      do 500 i=1,nkk
          nsaut=nsaut-1
          h(nh)=w(nw)
          nw=nw+1
          if (nr.eq.n) go to 495
          do 490 j=1,nrr
              h(nh+j)=h(nh+nsaut+j)
  490     continue
  495     nh=nh+nrr+1
  500 continue
  510 h(nh)=w(inc)
      if (nr.eq.n) go to 540
      do 520 i=1,nrr
  520 h(nh+i)=w(nr+i)
c
c          mise a jour de indi
c
  540 do 550 i=1,n
          ii=indi(i)
          if (ii.le.inc.or.ii.gt.nr) go to 550
          indi(i)=ii-1
  550 continue
      indi(nc)=nr
      nr=nr-1
c
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fretc1(mode,n,nc,nr,h,w,indi,indic2)
      implicit double precision (a-h,o-z)
      dimension h(*),w(n),indi(n)
c
      inc=indi(nc)
      nr1=nr+1
      nr2=nr-1
      nrr=n-nr
      nii=n-inc
      incmr=inc-nr1
c
c      addition d'une ligne a l
c
c          stockage des elements de la colonne inc dans w
      nsaut=nii+1
      nh=inc*(n+1)-inc*(inc+1)/2
      nw=n
      if (inc.eq.n) go to 20
      do 10 i=1,nii
      w(nw)=h(nh)
      nw=nw-1
   10 nh=nh-1
   20 w(nr1)=h(nh)
      nh=nh-1
      if (inc.eq.nr1) go to 60
      do 40 i=1,incmr
      nl=nii+i-1
      if (nl.eq.0) go to 35
      do 30 j=1,nl
      h(nh+nsaut)=h(nh)
   30 nh=nh-1
   35 w(nw)=h(nh)
      nw=nw-1
      nh=nh-1
   40 nsaut=nsaut+1
      do 50 j=1,incmr
      h(nh+nsaut)=h(nh)
   50 nh=nh-1
c
   60 nw=nw-1
      nsaut=1
      if (nr.eq.0) go to 125
      if (inc.eq.n) go to 80
      do 70 i=1,nii
      h(nh+nsaut)=h(nh)
   70 nh=nh-1
   80 if (nr.eq.1) go to 110
      do 100 i=1,nr2
      w(nw)=h(nh)
      nw=nw-1
      nh=nh-1
      nsaut=nsaut+1
      if (n.eq.nr1) go to 100
      nrm1=n-nr1
      do 90 j=1,nrm1
      h(nh+nsaut)=h(nh)
   90 nh=nh-1
  100 continue
  110 w(nw)=h(nh)
      nh=nh-1
      nsaut=nsaut+1
      if (inc.eq.nr1) go to 125
      incmr=inc-nr1
      do 120 i=1,incmr
      h(nh+nsaut)=h(nh)
  120 nh=nh-1
c         mise a jour de l
  125 if (nr.ne.0) go to 130
      if (w(1).gt.0.d0) go to 220
      mode=-1
      return
  130 if (nr.eq.1) go to 160
      do 150 i=2,nr
      ij=i
      i1=i-1
      v=w(i)
      do 140 j=1,i1
      v=v-h(ij)*w(j)
  140 ij=ij+nr-j
  150 w(i)=v
  160 ij=1
      v=w(nr1)
      do 170 i=1,nr
      wi=w(i)
      hij=h(ij)
      v=v-(wi**2)/hij
      w(i)=wi/hij
  170 ij=ij+nr1-i
      if (v.gt.0.d0) go to 180
      mode=-1
      return
  180 w(nr1)=v
      if (indic2.ne.1) go to 190
      do 185 i=1,nr
  185 w(i)=0.d0
      if (n.eq.nr1) go to 190
      nr1p1=nr1+1
      do 187 i=nr1p1,n
  187 w(i)=0.d0
c          stockage de w dans h
  190 nh=nr*(nr+1)/2
      nw=nr1
      nsaut=nw
      h(nh+nsaut)=w(nw)
      nw=nw-1
      nsaut=nsaut-1
      if (nr.eq.1) go to 220
      do 210 i=1,nr2
      h(nh+nsaut)=w(nw)
      nw=nw-1
      nsaut=nsaut-1
      do 200 j=1,i
      h(nh+nsaut)=h(nh)
  200 nh=nh-1
  210 continue
  220 h(nr1)=w(1)
      if (n.eq.nr1) go to 233
      nh1=nr*(n+1)-nr*(nr+1)/2+1
      nw=nr1
      nmr1=n-nr1
      do 230 i=1,nmr1
  230 h(nh1+i)=w(nw+i)
c          mise a jour de indi
  233 do 235 i=1,n
      ii=indi(i)
      if (ii.le.nr.or.ii.ge.inc) go to 235
      indi(i)=ii+1
  235 continue
      nr=nr+1
      indi(nc)=nr
      mode=0
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fmani1 (mode, n, d, w, indi)
c
      implicit none
      integer mode, n, indi(n)
      double precision d(n), w(n)
c
c-----------------------------------------------------------------------
c
c     Makes the following copy
c
c         w = d o indi          if mode = -1
c         w = d o indi^(-1)     otherwise
c
c     where "o" denotes composition and "indi" is the usual permutation.
c
c-----------------------------------------------------------------------
c
c --- local variables
c
      integer i
c
      if (mode.eq.-1) then
          do i = 1,n
              w(i) = d(indi(i))
          enddo
      else
          do i = 1,n
              w(indi(i)) = d(i)
          enddo
      endif
c
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fmlag1 (n,nr,a,z,w)
c
      implicit double precision (a-h,o-z)
      dimension a(*),z(n),w(n)
c
c-----------------------------------------------------------------------
c
c     Computes the the product w := B z, where B is the SW part of the
c     triangular matrix A. The triangular matrix A is supposed to be
c     stored column by column, ignoring its upper side.
c
c     Vectors z and w are supposed to have been ordered, with the
c     components of the inactive variables first.
c
c     - If B has no row, w is not modified.
c     - If B has no column, w is set to 0.
c
c-----------------------------------------------------------------------
c
c --- local variables
c
      integer i, j, nr1, nrr, nh1, nh, nj
      double precision u
c
c --- the SW part has no row
c
      if (nr.eq.n) return
c
c --- the SW part has no column
c
      if (nr.eq.0) then
          do i = 1,n
              w(i) = 0.d0
          enddo
          return
      endif
c
c --- the SW part is (n-nr) x nr
c     (JCHG: it would be more efficient to run along the columns of A,
c     instead of running along its rows)
c
      nr1 = nr+1
      nrr = n-nr
      nh1 = nr*nr1/2
      nh = nh1+1
      do j = nr1,n
          u = 0.d0
          nj = nh
          do i = 1,nr
              u = u+a(nj)*z(i)
              nj = nj+nrr
          enddo
          nh = nh+1
          w(j) = u
      enddo
c
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine nlis0 (n,simul,prosca,xn,fn,fpn,t,tmin,tmax,d,g,
     &                  amd,amf,imp,io,logic,nap,napmax,x,izs,rzs,dzs)
c
c-----------------------------------------------------------------------
c
c     nlis0 + minuscules + commentaires
c     ---------------------------------
c
c        en sortie logic =
c
c        0          descente serieuse
c        1          descente bloquee
c        2          arguments a revoir
c        4          nap > napmax
c        5          arret demande par l'utilisateur
c        6          fonction et gradient pas d'accord
c        < 0        contrainte implicite active
c
c-----------------------------------------------------------------------
c
c --- arguments
c
      external simul,prosca
      integer n,imp,io,logic,nap,napmax,izs(*)
      real rzs(*)
      double precision xn(n),fn,fpn,t,tmin,tmax,d(n),g(n),amd,amf,x(n)
      double precision dzs(*)
c
c --- variables locales
c
      integer i,indic,indica,indicd
      double precision tesf,tesd,tg,fg,fpg,td,ta,fa,fpa,d2,f,fp,ffn,fd,
     &    fpd,z,z1,test
c
 1000 format (5x,"nlis0   ",4x,"fpn=",1pd10.3," d2=",d9.2,
     & "  tmin=",d9.2," tmax=",d9.2)
 1001 format (/5x,"nlis0",3x,"stop on tmin",8x,
     & "pas",12x,"fonctions",5x,"derivees")
 1002 format (5x,"nlis0",37x,1pd10.3,2d11.3)
 1003 format (5x,"nlis0",1pd14.3,2d11.3)
 1004 format (5x,"nlis0",37x,1pd10.3," indic=",i3)
 1005 format (5x,"nlis0",14x,1pd18.8,d18.8,d11.3)
 1006 format (5x,"nlis0",14x,1pd18.8,"      indic=",i3)
c
c --- check the arguments
c
      if (n.gt.0 .and. fpn.lt.0.d+0 .and. t.gt.0.d+0
     & .and. tmax.gt.0.d+0 .and. amf.gt.0.d+0
     & .and. amd.gt.amf .and. amd.lt.1.d+0) go to 5
          logic=2
          go to 999
    5 tesf=amf*fpn
      tesd=amd*fpn
      td=0.d+0
      tg=0.d+0
      fg=fn
      fpg=fpn
      ta=0.d+0
      fa=fn
      fpa=fpn
      call prosca (n,d,d,d2,izs,rzs,dzs)
c
c               elimination d'un t initial ridiculement petit
c
c     if (t.gt.tmin) go to 20
c     t=tmin
c     if (t.le.tmax) go to 20
c     if (imp.gt.0) write (io,1007)
c1007 format (/4x," nlis0",10x,"tmin force a tmax")
c     tmin=tmax
      if (t.lt.tmin) then
          t=tmin
          if (t.gt.tmax) then
              if (imp.gt.0) write (io,'(/5x,a,10x,a,1pd12.5,a,d12.5)')
     &            "nlis0","tmin = ",tmin," force a tmax = ",tmax
              tmin=tmax
          endif
      endif
   20 if (fn+t*fpn.lt.fn+0.9d+0*t*fpn) go to 30
      t=2.d+0*t
      go to 20
   30 indica=1
      logic=0
      if (t.gt.tmax) then
          t=tmax
          logic=1
      endif
      if (imp.ge.4) write (io,1000) fpn,d2,tmin,tmax
c
c     --- nouveau x
c
      do 50 i=1,n
          x(i)=xn(i)+t*d(i)
   50 continue
c
c --- boucle
c
  100 nap=nap+1
      if (nap.gt.napmax) then
          logic=4
          fn=fg
          do 120 i=1,n
              xn(i)=xn(i)+tg*d(i)
  120     continue
          go to 999
      endif
      indic=4
c
c     --- appel simulateur
c
      call simul (indic,n,x,f,g,izs,rzs,dzs)
      if (indic.eq.0) then
c
c         --- arret demande par l'utilisateur
c
          logic=5
          if (imp.ge.3) write (io,'(/a)')
     &        " >>> n2qn1: stop required by the simulator"
          fn=f
          do 170 i=1,n
              xn(i)=x(i)
  170     continue
          go to 999
      endif
      if (indic.lt.0) then
c
c         --- les calculs n'ont pas pu etre effectues par le simulateur
c
          td=t
          indicd=indic
          logic=0
          if (imp.ge.4) write (io,1004) t,indic
          t=tg+0.1d+0*(td-tg)
          go to 905
      endif
c
c     --- les tests elementaires sont faits, on y va
c
      call prosca (n,d,g,fp,izs,rzs,dzs)
c
c     --- premier test de Wolfe
c
      ffn=f-fn
      if (ffn.gt.t*tesf) then
          td=t
          fd=f
          fpd=fp
          indicd=indic
          logic=0
          if (imp.ge.4) write (io,1002) t,ffn,fp
          go to 500
      endif
c
c     --- test 1 ok, donc deuxieme test de Wolfe
c
      if (imp.ge.4) write (io,1003) t,ffn,fp
      if (fp.gt.tesd) then
          logic=0
          go to 320
      endif
      if (logic.eq.0) go to 350
c
c     --- test 2 ok, donc pas serieux, on sort
c
  320 fn=f
      do 330 i=1,n
          xn(i)=x(i)
  330 continue
      go to 999
c
c
c
  350 tg=t
      fg=f
      fpg=fp
      if (td.ne.0.d+0) go to 500
c
c              extrapolation
c
      ta=t
      t=9.d+0*tg
      z=fpn+3.d+0*fp-4.d+0*ffn/tg
      if (z.gt.0.d+0) t=dmin1(t,tg*dmax1(1.d+0,-fp/z))
      t=tg+t
      if (t.lt.tmax) go to 900
      logic=1
      t=tmax
      go to 900
c
c              interpolation
c
  500 if (indica.le.0) then
          ta=t
          t=0.9d+0*tg+0.1d+0*td
          go to 900
      endif
      z=fp+fpa-3.d+0*(fa-f)/(ta-t)
      z1=z*z-fp*fpa
      if (z1.lt.0.d+0) then
          ta=t
          t=0.5d+0*(td+tg)
          go to 900
      endif
      if (t.lt.ta) z1=z-dsqrt(z1)
      if (t.gt.ta) z1=z+dsqrt(z1)
      z=fp/(fp+z1)
      z=t+z*(ta-t)
      ta=t
      test=0.1d+0*(td-tg)
      t=dmax1(z,tg+test)
      t=dmin1(t,td-test)
c
c --- fin de boucle
c     - t peut etre bloque sur tmax
c       (venant de l'extrapolation avec logic=1)
c
  900 fa=f
      fpa=fp
  905 indica=indic
c
c --- faut-il continuer ?
c
      if (td.eq.0.d+0) go to 950
      if (td-tg.lt.tmin) go to 920
c
c     --- limite de precision machine (arret de secours) ?
c
      do 910 i=1,n
          z=xn(i)+t*d(i)
          if (z.ne.xn(i).and.z.ne.x(i)) go to 950
  910 continue
c
c --- arret sur dxmin ou de secours
c
  920 logic=6
c
c     si indicd<0, les derniers calculs n'ont pas pu etre fait par simul
c
      if (indicd.lt.0) logic=indicd
c
c     si tg=0, xn = xn_depart,
c     sinon on prend xn=x_gauche qui fait decroitre f
c
      if (tg.eq.0.d+0) go to 940
      fn=fg
      do 930 i=1,n
  930 xn(i)=xn(i)+tg*d(i)
  940 if (imp.le.0) go to 999
      write (io,1001)
      write (io,1005) tg,fg,fpg
      if (logic.eq.6) write (io,1005) td,fd,fpd
      if (logic.eq.7) write (io,1006) td,indicd
      go to 999
c
c               recopiage de x et boucle
c
  950 do 960 i=1,n
  960 x(i)=xn(i)+t*d(i)
      go to 100
  999 return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fcube(t,f,fp,ta,fa,fpa,tlower,tupper)
      implicit double precision (a-h,o-z)
c
c           Using f and fp at t and ta, computes new t by cubic formula
c           safeguarded inside [tlower,tupper].
c
      z1=fp+fpa-3.d0*(fa-f)/(ta-t)
      b=z1+fp
c
c              first compute the discriminant (without overflow)
c
      if (dabs(z1).le.1.d0) then
          discri=z1*z1-fp*fpa
        else
          discri=fp/z1
          discri=discri*fpa
          discri=z1-discri
          if (z1.ge.0.d0 .and. discri.ge.0.d0) then
              discri=dsqrt(z1)*dsqrt(discri)
              go to 200
          endif
          if (z1.le.0.d0 .and. discri.le.0.d0) then
              discri=dsqrt(-z1)*dsqrt(-discri)
              go to 200
          endif
          discri=-1.d0
      endif
      if (discri.lt.0.d0) then
          if (fp.lt.0.d0) t=tupper
          if (fp.ge.0.d0) t=tlower
          go to 990
      endif
c
c       discriminant nonnegative, stable solution formula
c
      discri=dsqrt(discri)
  200 if (t-ta.lt.0.d0) discri=-discri
      sign=(t-ta)/dabs(t-ta)
      if (b*sign.gt.0.) then
          anum=(ta-t)*fp
          den=b+discri
        else
          den=z1+b+fpa
          anum=(ta-t)*(b-discri)
      endif
c
c               now compute the ratio (without overflow)
c
      if (dabs(den).ge.1.d0) then
          t=t+anum/den
        else
          if (dabs(anum).lt.(tupper-tlower)*dabs(den)) then
              t=t+anum/den
            else
              if (fp.lt.0.d0) t=tupper
              if (fp.ge.0.d0) t=tlower
          endif
      endif
c
c                       finally, safeguard
c
      t=dmax1(t,tlower)
      t=dmin1(t,tupper)
  990 return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fuclid (n,x,y,ps,izs,rzs,dzs)
      implicit real(selected_real_kind(10,60)) (a-h,o-z)
      dimension x(n),y(n),izs(*),dzs(*)
      real rzs(*)
      ps=0.d0
      do 10 i=1,n
   10 ps=ps+x(i)*y(i)
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fmc11a(a,n,z,sig,w,ir,mk,eps)
      implicit double precision (a-h,o-z)
      dimension a(*),z(n),w(n)
c   update factors given in a by   sig*z*ztranspose
      if (n.le.1) then
          a(1)=a(1)+sig *z(1)**2
          ir=1
          if (a(1).gt.0.d0)return
          a(1)=0.d0
          ir=0
          return
      endif
c
      np=n+1
      if (sig.gt.0.d0)goto40
      if (sig.eq.0.d0.or.ir.eq.0)return
      ti=1.d0/sig
      ij=1
      if (mk.eq.0)goto10
      do 7 i=1,n
      if (a(ij).ne.0.d0)ti=ti+w(i)**2/a(ij)
    7 ij=ij+np-i
      goto20
   10 continue
      do 11 i=1,n
   11 w(i)=z(i)
      do 15 i=1,n
      ip=i+1
      v=w(i)
      if (a(ij).gt.0.d0)goto12
      w(i)=0.d0
      ij=ij+np-i
      goto15
   12 continue
      ti=ti+v**2/a(ij)
      if (i.eq.n)goto14
      do 13 j=ip,n
      ij=ij+1
   13 w(j)=w(j)-v*a(ij)
   14 ij=ij+1
   15 continue
   20 continue
      if (ir.le.0 )goto21
      if (ti.gt.0.d0)goto22
      if (mk-1)40,40,23
   21 ti=0.d0
      ir=-ir-1
      goto23
   22 ti=eps/sig
      if (eps.eq.0.d0)ir=ir-1
   23 continue
      mm=1
      tim=ti
      do 30 i=1,n
      j=np-i
      ij=ij-i
      if (a(ij).ne.0.d0)tim=ti-w(j)**2/a(ij)
      w(j)=ti
   30 ti=tim
      goto41
   40 continue
      mm=0
      tim=1.d0/sig
   41 continue
      ij=1
      do 66 i=1,n
      ip=i+1
      v=z(i)
      if (a(ij).gt.0.d0)goto53
      if (ir.gt.0 .or.sig.lt.0.d0.or.v.eq.0.d0)goto52
      ir=1-ir
      a(ij)=v**2/tim
      if (i.eq.n)return
      do 51 j=ip,n
      ij=ij+1
   51 a(ij)=z(j)/v
      return
   52 continue
      ti=tim
      ij=ij+np-i
      goto66
   53 continue
      al=v/a(ij)
      if (mm)54,54,55
   54 ti=tim+v*al
      goto56
   55 ti=w(i)
   56 continue
      r=ti/tim
      a(ij)=a(ij)*r
      if (r.eq.0.d0)goto70
      if (i.eq.n)goto70
      b=al/ti
      if (r.gt.4.d0)goto62
      do 61 j=ip,n
      ij=ij+1
      z(j)=z(j)-v*a(ij)
   61 a(ij)=a(ij)+b*z(j)
      goto64
   62 gm=tim/ti
      do 63 j=ip,n
      ij=ij+1
      y=a(ij)
      a(ij)=b*z(j)+y*gm
   63 z(j)=z(j)-v*y
   64 continue
      tim=ti
      ij=ij+1
   66 continue
   70 continue
      if (ir.lt.0)ir=-ir
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fmc11b(a,n,ir)
c   factorize a matrix given in a
      implicit double precision (a-h,o-z)
      dimension a(*)
      ir=n
      if (n.gt.1)goto 100
      if (a(1).gt.0.d0)return
      a(1)=0.d0
      ir=0
      return
  100 continue
      np=n+1
      ii=1
      do 104 i=2,n
      aa=a(ii)
      ni=ii+np-i
      if (aa.gt.0.d0)goto101
      a(ii)=0.d0
      ir=ir-1
      ii=ni+1
      goto104
  101 continue
      ip=ii+1
      ii=ni+1
      jk=ii
      do 103 ij=ip,ni
      v=a(ij)/aa
      do 102 ik=ij,ni
      a(jk)=a(jk)-a(ik)*v
  102 jk=jk+1
  103 a(ij)=v
  104 continue
      if (a(ii).gt.0.d0)return
      a(ii)=0.d0
      ir=ir-1
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine fmc11e(a,n,z,w,ir)
c
      implicit double precision (a-h,o-z)
      dimension a(*),z(n),w(n)
c
c-----------------------------------------------------------------------
c
c     It is assumed that the matrix M = L*D*L', where L is lower
c     triangular with 1 on its diagonal and D is a nonsingular diagonal
c     matrix. Then, FMC11E computes
c
c         w := L^(-1) z     and     z := M^(-1) z
c
c     The matrix L and D are stored in the triangular matrix T, with
c     T(i,j) := L(i,j) for i different from j and T(i,i) = D(i,i). The
c     matrix T is stored in the vector A, as follows T(1,1), ...,
c     T(n,1), T(2,1), ..., T(n,1), ..., T(n-1,n-1), T(n,n-1), T(n,n).
c
c     On entry
c
c       ir must be equal to n, otherwise nothing is done
c
c-----------------------------------------------------------------------
c
      if (ir.lt.n) return
c
      w(1)=z(1)
      if (n.le.1) then
          z(1)=z(1)/a(1)
          return
      endif
c
c --- Compute w := L^(-1) z
c         and z := w
c
      do i=2,n
          ij=i
          i1=i-1
          v=z(i)
          do j=1,i1
              v=v-a(ij)*z(j)
              ij=ij+n-j
          enddo
          w(i)=v
          z(i)=v
      enddo
c
c --- Compute z := D^(-1) z and next z := L^(-T) z
c     at the begining, ij = n*(n+1)/2
c
      z(n)=z(n)/a(ij)
      np=n+1
      do nip=2,n
          i=np-nip
          ii=ij-nip
          v=z(i)/a(ii)
          ip=i+1
          ij=ii
          do j=ip,n
              ii=ii+1
              v=v-a(ii)*z(j)
          enddo
          z(i)=v
      enddo
c
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine nqhess(n,imp,lp,iz,rz)
      implicit double precision(a-h,o-z)
      dimension iz(*),rz(*)
c
 1000 format(//)
 1001 format(34h nqhess   hessienne au point final)
 1002 format(9h   nqhess,i4,5d12.4,/,(9h   nqhess,4x,5d12.4))
c
      ni=2*n
      nw=n*(n+1)/2
      nw1=nw+n
      if (n.eq.1) go to 50
      nr=iz(ni+1)
      if (nr.eq.0) go to 20
c
c         defactorisation de la hessienne
c
      do 10 i=1,n
      if (iz(n+i).ne.0) go to 10
      nc=i
      call fajc1 (n,nc,nr,rz(1),rz(nw+1),iz(1))
      if (nr.eq.0) go to 20
   10 continue
c
c         permutation de la hessienne
c
   20 n1=n-1
      do 40 i=1,n1
      j1=iz(i)
      if (j1.eq.i) go to 40
      ni=i
      nj=j1
      call f1qhes (n,ni,nj,nw,rz)
      call f1qhes (n,nj,ni,nw1,rz)
      call f2qhes (n,nj,nw,rz)
      call f2qhes (n,ni,nw1,rz)
      if (i.eq.n1) go to 50
      i1=i+1
      do 30 k=i1,n
      if (iz(k).ne.i) go to 30
      iz(k)=j1
      go to 40
   30 continue
   40 continue
   50 if (imp.le.0) return
      write(lp,1000)
      write(lp,1001)
      do 60 i=1,n
      write(lp,1002) i,(rz(i+(j-1)*(2*n-j)/2),j=1,i)
   60 continue
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine f1qhes(n,ni,nj,nw,rz)
      implicit double precision(a-h,o-z)
      dimension rz(*)
c
      nii=ni
      nwi=nw+ni
      nwj=nw+nj
      do 20 k=1,n
      rz(nw+k)=rz(nii)
      if (k.ge.ni) go to 10
      nii=nii+(n-k)
      go to 20
   10 nii=nii+1
   20 continue
      rznw=rz(nwi)
      rz(nwi)=rz(nwj)
      rz(nwj)=rznw
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
      subroutine f2qhes(n,ni,nw,rz)
      implicit double precision(a-h,o-z)
      dimension rz(*)
c
      nii=ni
      do 20 k=1,n
      rz(nii)=rz(nw+k)
      if (k.ge.ni) go to 10
      nii=nii+(n-k)
      go to 20
   10 nii=nii+1
   20 continue
      return
      end
c
c========0=========0=========0=========0=========0=========0=========0==
c
c>      subroutine printv (string,nc,v,n)
c>c
c>      implicit none
c>      integer n, nc
c>      character (len=nc) :: string
c>      double precision v(n)
c>c
c>c --- local variables
c>c
c>      integer n5, i
c>c
c>c --- print the comment
c>c
c>      write (UNIT=6,'(a)') string
c>c
c>c --- print v
c>c
c>      n5 = n/5
c>      do i=1,n5
c>          write (6,'((i4":",5(2x,1pd12.5)))') (i-1)*5+1, v((i-1)*5+1),
c>     &        v((i-1)*5+2),v((i-1)*5+3), v((i-1)*5+4),v((i-1)*5+5)
c>      enddo
c>      if (n.gt.n5*5) then
c>          write (6,'((i4":",5(2x,1pd12.5)))') n5*5+1,
c>     &        (v(n5*5+1), i=1,mod(n,5))
c>      endif
c>c
c>      return
c>      end
