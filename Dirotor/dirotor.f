      program dirotor

      implicit none


      integer  j1,j2,jboth,l,j,j1_max,j2_max,l_max,l_lim,point,l_min
      real*8   l_upper,l_lower,b1,b2,bl,elimit,e1,e2
      logical  levels,trig_l,trig_u

      dimension levels(0:1000)
      dimension l_upper(0:1000)
      dimension l_lower(0:1000)


      parameter(b1=10.0)
      parameter(b2=10.0)
      parameter(bl=1.0)
      parameter(elimit=100.0)

      do point=0,1000
      l_lower(point)=-999.9
      l_upper(point)=-999.9
      end do

      do j=0,1000

      do point=0,1000
      levels(point)=.false.
      end do

      j1_max=0.5*(-b1+dsqrt((b1*b1)+4.0*b1*elimit))/b1

      do j1=0,j1_max

      e1=b1*dfloat(j1*(j1+1))

      j2_max=0.5*(-b2+dsqrt((b2*b2)+4.0*b2*(elimit-e1)))/b2

      do j2=0,j2_max

      e2=b2*dfloat(j2*(j2+1))

      l_max=0.5*(-bl+dsqrt((bl*bl)+4.0*bl*(elimit-e1-e2)))/bl

      do jboth=abs(j1-j2),j1+j2

      l_min=abs(j-jboth)
      l_lim=j+jboth

      if (l_min.le.l_max) then

      do l=l_min,min(l_max,l_lim)

      levels(l)=.true.

      end do !l

      end if

      end do !jboth

      end do !j2

      end do !j1

      trig_l=.true.
      trig_u=.true.
      do point=0,1000
      if (levels(point)) then
      if (trig_l) then
      l_lower(j)=point
      trig_l=.false.
      end if
      end if
      if (levels(1000-point)) then
      if (trig_u) then
      l_upper(j)=1000-point
      trig_u=.false.
      end if
      end if
      end do

      write(10,*) j,',',l_lower(j),',',l_upper(j)
      call flush(10)

      end do !j








      end