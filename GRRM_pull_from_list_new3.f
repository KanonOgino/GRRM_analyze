c**********************************************************************
c*  Replace the result of calculation obutained by GRRM Geometry to the
c*  Gaussian input file
c*  Edited by K. Ogino
c**********************************************************************
!   How to use
c**********************************************************************
!  Caution
c*  Compiling after properly filling the inputfile.
!  inputfile (GRRM_pull_from_list.inp)
c*  filename1 = ~~_DC_list.log
c*  filename2 = ~~_EQ_list.log
c*  filename3 = ~~_TS_list.log
c*  natom = number of atom
c*  nos = name of system
c:  charge = electronic charge
c*  spin =  spin multiplicity
!  compile and run
c*  1 : ifort GRRM_pull_from_list.f -o Gpull
c*  2 : Gpull < GRRM_pull_from_list.inp &
c**********************************************************************
      program Gabriel
c**********************************************************************

      implicit none
      integer :: i,j,charge,spin
      integer :: mem,nproc
      character(len=2),allocatable :: atom(:)
      character(len=80) oupfile,DFT,basis,cor
      double precision,allocatable :: geom(:,:)

      integer :: stn !structure number
      character(len=80) :: sea !search

      character(len=80) :: nos !name of system

      character(len=48) :: filename1
      character(len=48) :: filename2
      character(len=48) :: filename3 !name of file

      integer :: natom !number of atom

      character(len=48) :: outf !outputfile

      integer :: flnb = 10 !file number
      integer :: output = 13 !output file number

      logical :: gauswitch = .false.
      logical :: calcswitch = .false.

c***********************************************************************
c     take from .inp
c***********************************************************************
      namelist /input/ filename1,filename2,filename3,natom,nos,
     &charge,spin,gauswitch,calcswitch,mem,nproc,oupfile,DFT,basis,cor
      read(5,input)

5000  if (gauswitch .eqv. .false.)then
!remove and make directry
        call system ('rm -r *zyx_*')
        call system ('mkdir zyx_DC_list')
        call system ('mkdir zyx_EQ_list')
        call system ('mkdir zyx_TS_list')
      else
        call system ('rm -r *zz_*')
        call system ('mkdir zz_DC_list')
        call system ('mkdir zz_TS_list')
        call system ('mkdir zz_EQ_list')
      endif

      allocate (geom(natom,3),atom(natom))

      stn=0
!DC
!open log file of DC(10)
      open(flnb,file=filename1,status='old')

      do
      !read ~_DC_list.log
        read(flnb,'(a)',end=100) sea

!if '# Geometry' is in box of search
        if (sea(1:10)=='# Geometry')then
!named and open
          if (gauswitch .eqv. .false.)then
            write(outf,'("Fnd_",a4,"_DC_",i2.2".gjf")') nos,stn
            open(output,file=outf,status='replace')
          else
            write(outf,'("Cal_",a4,"_DC_",i2.2".gjf")') nos,stn
            open(output,file=outf,status='replace')
          endif
!make gaussian input file
          if (gauswitch .eqv. .true.)then
            call make_gau_inp(output,charge,spin)
          else
            write(output,'(a)') sea
            write(output,*) ''
            write(output,'(a5)') 'title'
            write(output,*) ''
            write(output,9005) charge, spin
          endif

          do i=1,natom
            read(flnb,*) atom(i),(geom(i,j),j=1,3)
            write(output,'(a,3f18.12)') atom(i),(geom(i,j),j=1,3)
          enddo

          write(output,*) ''

          if(gauswitch .eqv. .false.)then
            read(flnb,'(a)') sea
            write(output,'(a)') sea
            do
              read(flnb,'(a)') sea
              if (sea(1:12)=='Dissociation')then
                read(flnb,'(a)') sea
                write(output,'(a)') sea
                goto 110
              endif
            enddo

110         do
              read(flnb,'(a)') sea
              if (sea(1:10)=='CONNECTION')then
                write(output,'(a)') sea
                goto 120
              endif
            enddo

120         write(output,*) ''
          endif
!add 1 in structure number and unit number
              stn=stn+1
              output=output+1
        endif
      enddo
!close file of input '.log'
100   close(flnb)

      if (gauswitch .eqv. .false.) then
        call system('mv *Fnd_* ./zyx_DC_list')
      else
        call system('mv *Cal_* ./zz_DC_list/')
      endif

!add 1 in flnb(unit number)
      flnb=flnb+1

!EQ

      stn=0

!open log file of EQ(11)
      open(flnb,file=filename2,status='old')

      do

!read ~_EQ_list.log
        read(flnb,'(a)',end=200) sea

!if '# Geometry' is in box of search
        if (sea(1:10)=='# Geometry') then
!named and open
          if (gauswitch .eqv. .false.)then
            write(outf,'("Fnd_",a4,"_EQ_",i2.2".gjf")')nos,stn
            open(output,file=outf,status='replace')
          else
            write(outf,'("Cal_",a4,"_EQ_",i2.2".gjf")')nos,stn
            open(output,file=outf,status='replace')
          endif

!make gaussian input file
          if (gauswitch .eqv. .true.) then
            call make_gau_inp(output,charge,spin)
          else
            write(output,'(a)') sea
            write(output,*) ''
            write(output,'(a5)') 'title'
            write(output,*) ''
            write(output,9005) charge, spin
          endif

          do i=1,natom
            read(flnb,*) atom(i),(geom(i,j),j=1,3)
            write(output,'(a,3f18.12)') atom(i),(geom(i,j),j=1,3)
          enddo

          write(output,*) ''

          if(gauswitch .eqv. .false.) then
            read(flnb,'(a)') sea
            write(output,'(a)') sea
            write(output,*) ''
          endif

!add 1 in structure number and unit number
          stn=stn+1
          output=output+1

        endif
      enddo

!close file of input '.log'
200   close(flnb)
      if (gauswitch .eqv. .false.) then
        call system('mv *Fnd_* ./zyx_EQ_list')
      else
        call system('mv *Cal_* ./zz_EQ_list/')
      endif

!add 1 in flnb(unit number)
      flnb=flnb+1
!TS

      stn=0

!open log file of TS(12)
      open(flnb,file=filename3,status='old')
      do

!read ~_TS_list.log
        read(flnb,'(a)',end=300) sea

!if '# Geometry' is in box of search
        if (sea(1:10)=='# Geometry') then
!named and open
          if (gauswitch .eqv. .false.)then
            write(outf,'("Fnd_",a4,"_TS_",i2.2".gjf")') nos,stn
            open(output,file=outf,status='replace')
          else
            write(outf,'("Cal_",a4,"_TS_",i2.2".gjf")') nos,stn
            open(output,file=outf,status='replace')
          endif
!make gaussian input file
          if (gauswitch .eqv. .true.)then
            call make_gau_inp(output,charge,spin)
          else
            write(output,'(a)') sea
            write(output,*) ''
            write(output,'(a5)') 'title'
            write(output,*) ''
            write(output,9005) charge, spin
          endif
          do i=1,natom
            read(flnb,*) atom(i),(geom(i,j),j=1,3)
            write(output,'(a,3f18.12)') atom(i),(geom(i,j),j=1,3)
          enddo
          write(output,*) ''
          if(gauswitch .eqv. .false.)then
            read(flnb,'(a)') sea
            write(output,'(a)') sea
            do
              read(flnb,'(a)') sea
              if (sea(1:10)=='CONNECTION') then
                write(output,'(a)') sea
                goto 160
              endif
            enddo

160         write(output,*) ''
          endif
!add 1 in structure number and unit number
          stn=stn+1
          output=output+1
        endif
      enddo
!close file of input '.log'
300   close(flnb)

      if (gauswitch .eqv. .false.) then
        call system('mv *Fnd_* ./zyx_TS_list')
      else
        call system('mv *Cal_* ./zz_TS_list/')
      endif

      print *,'Done'
      if(calcswitch .eqv. .true.)then
        print *,'calculation in tako52'
        call system('scp -r zz_* tako52:~/stable_ssh')
        call system('ssh XXX ./calc.sh & exit')!XXX = servername
        print *,'normal termination'
      endif

9000  format('%mem=',i2,'GB')
9001  format('%nproc=',i2)
9002  format('%chk=',a20)
9003  format('#'a5'/'a10'' 'EmpiricalDispersion='a3' '
     &  'int=ultrafine'' ''guess=mix'' ''stable=opt'' '
     &  'scf=(xqc,max=128,maxcyc=512,conver=8) nosymm')
9004  format('title')
9005  format(i2,i2)
      end program

c*******************************************************************
      subroutine make_gau_inp(output,charge,spin)
c*******************************************************************
      implicit none
      integer :: output,charge,spin,mem,nproc
      character(len=80) :: oupfile,DFT,basis,cor
      character :: natom,nos
      character(len=48) :: filename1
      character(len=48) :: filename2
      character(len=48) :: filename3
      logical :: gauswitch = .false.
      logical :: calcswitch = .false.
      
      namelist /input/ filename1,filename2,filename3,natom,nos,
     &charge,spin,gauswitch,calcswitch,mem,nproc,oupfile,DFT,basis,cor

      rewind(5)
      read(5,input)

5001  write(output,9000) mem
      write(output,9001) nproc
      write(output,9002) oupfile
      write(output,9003) DFT,basis,cor
      write(output,*)''
      write(output,9004)
      write(output,*)''
      write(output,9005) charge,spin


9000  format('%mem=',i2,'GB')
9001  format('%nproc=',i2)
9002  format('%chk=',a20)
9003  format('#'a6'/'a10'' 'EmpiricalDispersion='a3' '
     &  'int=ultrafine' ' ' 'guess=mix' ' ' 'stable=opt' ' '
     &  'scf=(xqc,maxcon=128,maxcyc=512,conver=8) nosymm')
9004  format('title')
9005  format(i2,i2)

      return
      end subroutine
