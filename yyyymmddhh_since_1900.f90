!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      This file is a component of the volcanic ash transport and dispersion model Ash3d,
!      written at the U.S. Geological Survey by Hans F. Schwaiger (hschwaiger@usgs.gov),
!      Larry G. Mastin (lgmastin@usgs.gov), and Roger P. Denlinger (roger@usgs.gov).
!
!      The model and its source code are products of the U.S. Federal Government and therefore
!      bear no copyright.  They may be copied, redistributed and freely incorporated
!      into derivative products.  However as a matter of scientific courtesy we ask that
!      you credit the authors and cite published documentation of this model (below) when
!      publishing or distributing derivative products.
!
!      Schwaiger, H.F., Denlinger, R.P., and Mastin, L.G., 2012, Ash3d, a finite-
!         volume, conservative numerical model for ash transport and tephra deposition,
!         Journal of Geophysical Research, 117, B04204, doi:10.1029/2011JB008968.
!
!      Although this program has been used by the USGS, no warranty, expressed or
!      implied, is made by the USGS or the United States Government as to the accuracy
!      and functioning  of the program and related program material nor shall the fact of
!      distribution constitute  any such warranty, and no responsibility is assumed by
!      the USGS in connection therewith.
!
!      We make no guarantees, expressed or implied, as to the usefulness of the software
!      and its documentation for any purpose.  We assume no responsibility to provide
!      technical support to users of this software.
!
!      This program is just a wrapper for the function call to
!      HS_hours_since_baseyear with base_year set to 1900
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      program yyyymmddhh_since_1900

!     Stand-alone version of the function HS_yyyymmddhh_since with baseyear
!     set to 1900.
!     Returns a character string yyyymmddhh.hh giving the year, month, day, and hour, given
!     the number of hours since January 1, 1900.

      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,input_unit,output_unit,error_unit

      implicit none
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      !integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      character(len=18)    :: string1
      character(len=80)    :: linebuffer
      real(kind=dp)        :: HoursSince1900
      integer              :: iyear  = 0
      integer              :: imonth = 0
      integer              :: iday   = 0
      integer              :: ihour  = 0
      integer              :: ifrac
      integer              :: idoy   = 0
      real(kind=dp)        :: frac
      real(kind=dp)        :: hour   = 0.0_dp
      integer              :: nargs

      integer              :: iostatus
      character(len=120)   :: iomessage = ""

      integer              :: byear    = 1900
      logical              :: useLeaps = .true.

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none  (type, external)
          integer            ,parameter   :: dp        = 8 ! double precision
          real(kind=dp) ,intent(in)       :: HoursSince
          integer       ,intent(in)       :: byear
          logical       ,intent(in)       :: useLeaps
          integer       ,intent(out)      :: iyear
          integer       ,intent(out)      :: imonth
          integer       ,intent(out)      :: iday
          real(kind=dp) ,intent(out)      :: hours
          integer       ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Test read command line arguments
      nargs = command_argument_count()
      if(nargs /= 1) then
        write(output_unit,*) 'error in input to yyyymmddhh_since_1900'
        write(output_unit,*) 'input should be a single real number.'
        write(output_unit,*) 'program stopped'
        stop
      else
        call get_command_argument(number=1, value=linebuffer, status=iostatus)
        read(linebuffer,*,iostat=iostatus,iomsg=iomessage) HoursSince1900
        if(iostatus /= 0)then
          write(output_unit,*)'HS ERROR:  Error reading value from command-line argument'
          write(output_unit,*)'           Expecting to read: HoursSince1900 (real*8)'
          write(output_unit,*)'           From the following input line : '
          write(output_unit,*)linebuffer
          write(output_unit,*)'HS System Message: '
          write(output_unit,*)iomessage
          stop 1
        endif
      endif

      call HS_Get_YMDH(HoursSince1900,byear,useLeaps,iyear,imonth,iday,hour,idoy)

      ihour = int(hour)
      frac = hour-real(ihour,kind=dp)
      if(frac > 1.0_dp)then
        ! if the nearest integer of ifrac is actually the next
        ! hour, adjust ifrac and ihour accordingly
        ihour = ihour + int(frac)
        frac = frac-int(frac)
      endif
      ifrac = nint(frac*60.0_dp)            ! turn hour frac into minutes

      write(string1,2) iyear, imonth, iday, ihour, ifrac
2     format(i4,'.',i2.2,'.',i2.2,'.',2i2.2,'UTC')

      write(output_unit,*) string1

      end program yyyymmddhh_since_1900
