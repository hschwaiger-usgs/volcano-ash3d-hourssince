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
!      This file contains several functions and a subroutine that calculate the
!      difference in hours of calendar dates compared to a reference (or base)
!      year.  The consideration of leap years can be set using the logical parameter
!      useLeaps.  If leap years are used, the proleptic Gregorian calendar is used.
!      Given an 'HoursSince' value, several functions are given to calculate the
!      corresponding calendar information as well as a few functions to format text
!      strings.
!
!      contains:
!        function HS_IsLeapYear(iyear)
!        function HS_hours_since_baseyear(iyear,imonth,iday,hours,byear,useLeaps)
!        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
!        function HS_xmltime(HoursSince,byear,useLeaps)
!        function HS_yyyymmddhhmm_since(HoursSince,byear,useLeaps)
!        function HS_yyyymmddhh_since(HoursSince,byear,useLeaps)
!        function HS_DayOfYear(HoursSince,byear,useLeaps)
!        function HS_HourOfDay(HoursSince,byear,useLeaps)
!        function HS_YearOfEvent(HoursSince,byear,useLeaps)
!        function HS_MonthOfEvent(HoursSince,byear,useLeaps)
!        function HS_DayOfEvent(HoursSince,byear,useLeaps)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!##############################################################################
!
!     HS_IsLeapYear
!
!     This function takes and integer year as input and returns a logical
!      value (.true. or .false.) if that year is a leap year
!
!##############################################################################

      logical function HS_IsLeapYear(iyear)

      implicit none
      !implicit none (type, external)

      integer,intent(in)  :: iyear

      ! Note, this uses the proleptic Gregorian calendar which includes year 0
      ! and considers y=0 to be a leap year.

      if ((mod(iyear,  4) == 0) .and. &
          (mod(iyear,100) /= 0) .or.  &
          (mod(iyear,400) == 0)) then
        HS_IsLeapYear = .true.
      else
        HS_IsLeapYear = .false.
      endif

      return

      end function HS_IsLeapYear

!##############################################################################
!
!     HS_hours_since_baseyear
!
!     function that calculates the number of hours since Jan 1, of a base year,
!     given the input (year, month, day, and hour (UT))
!
!##############################################################################

      real(kind=8) function HS_hours_since_baseyear(iyear,imonth,iday,hours,byear,useLeaps)

      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      integer      ,intent(in) :: iyear
      integer      ,intent(in) :: imonth
      integer      ,intent(in) :: iday
      real(kind=dp),intent(in) :: hours
      integer      ,intent(in) :: byear
      logical      ,intent(in) :: useLeaps

                                  ! cumulative hours in each month
      integer, dimension(0:12), parameter :: monthhours = [0,744,1416,2160,2880,3624,4344,5088,5832,6552,7296,8016,8760]
      integer                  :: i
      integer                  :: ileaphours
      logical                  :: IsLeap

      INTERFACE
        logical function HS_IsLeapYear(iyear)
          implicit none
          !implicit none (type, external)
          integer,intent(in)  :: iyear
        end function HS_IsLeapYear
      END INTERFACE

      ! First check input values
      if (iyear < byear) then
        write(error_unit,*)"HS ERROR: HS_hours_since_baseyear"
        write(error_unit,*)"HS ERROR:  year must be greater or equal to base year."
        write(error_unit,*)"      Base Year = ",byear
        write(error_unit,*)"     Input Year = ",iyear
        stop 1
      endif
      if (imonth < 1 .or. &
          imonth > 12) then
        write(error_unit,*)"HS ERROR: HS_hours_since_baseyear"
        write(error_unit,*)"HS ERROR:  month must be between 1 and 12."
        write(error_unit,*)"     Input Month = ",imonth
        stop 1
      endif
      if (iday < 1) then
        write(error_unit,*)"HS ERROR: HS_hours_since_baseyear"
        write(error_unit,*)"HS ERROR:  day must be greater than 0."
        write(error_unit,*)"     Input Day = ",iday
        stop 1
      endif
      if ((imonth == 1.or.&
           imonth == 3.or.&
           imonth == 5.or.&
           imonth == 7.or.&
           imonth == 8.or.&
           imonth == 10.or.&
           imonth == 12).and.iday > 31)then
        write(error_unit,*)"HS ERROR: HS_hours_since_baseyear"
        write(error_unit,*)"HS ERROR:  day must be <= 31 for this month."
        write(error_unit,*)"     Input Month = ",imonth
        write(error_unit,*)"     Input Day = ",iday
        stop 1
      endif
      if ((imonth == 4.or.&
           imonth == 6.or.&
           imonth == 9.or.&
           imonth == 11).and.iday > 30)then
        write(error_unit,*)"HS ERROR: HS_hours_since_baseyear"
        write(error_unit,*)"HS ERROR:  day must be <= 30 for this month."
        write(error_unit,*)"     Input Month = ",imonth
        write(error_unit,*)"     Input Day = ",iday
        stop 1
      endif
      if ((imonth == 2).and.iday > 29)then
        write(error_unit,*)"HS ERROR: HS_hours_since_baseyear"
        write(error_unit,*)"HS ERROR:  day must be <= 29 for this month."
        write(error_unit,*)"     Input Month = ",imonth
        write(error_unit,*)"     Input Day = ",iday
        stop 1
      endif

      if (useLeaps) then
        ! First find out if given year is a leap year
        IsLeap = HS_IsLeapYear(iyear)

        ! Now find out how many leap days (actually hours) are between the given
        ! year and the base year
        ileaphours = 0
        do i = byear,iyear
          if (HS_IsLeapYear(i)) ileaphours = ileaphours + 24
        enddo

        ! If this is a leap year, but still in Jan or Feb, remove the
        ! extra 24 hours credited above
        if (IsLeap.and.imonth < 3) ileaphours = ileaphours - 24
      else
        ileaphours = 0
      endif

      HS_hours_since_baseyear = real((iyear-byear)*monthhours(12) + & ! number of hours per normal year
                                      monthhours(imonth-1)        + & ! hours in year at beginning of month
                                      ileaphours                 + & ! total leap hours since base year
                                      24*(iday-1),kind=dp)        + & ! hours in day
                                          hours                      ! hour of the day

      return

      end function HS_hours_since_baseyear

!##############################################################################
!
!     HS_Get_YMDH
!
!     subroutine that calculates the year, month, day and hour, given the
!     number of hours since Jan 1, of a base year.
!     This is essentially the inverse of HS_hours_since_baseyear
!
!##############################################################################

      subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      ! This module requires Fortran 2003 or later
      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp),intent(in)       :: HoursSince
      integer      ,intent(in)       :: byear
      logical      ,intent(in)       :: useLeaps
      integer      ,intent(out)      :: iyear
      integer      ,intent(out)      :: imonth
      integer      ,intent(out)      :: iday
      real(kind=dp),intent(out)      :: hours
      integer      ,intent(out)      :: idoy

      integer, dimension(0:12), parameter :: monthhours     = [0,744,1416,2160,2880,3624,4344,5088,5832,6552,7296,8016,8760]
      integer, dimension(0:12), parameter :: leapmonthhours = [0,744,1440,2184,2904,3648,4368,5112,5856,6576,7320,8040,8784]

      integer :: HoursIn_Century
      integer :: HoursIn_This_Century
      integer :: HoursIn_Year
      integer :: HoursIn_This_Year
      integer :: HoursIn_Leap
      integer :: ileaphours
      integer :: byear_correction
      integer :: i
      integer :: icent
      integer :: BaseYear_Y0_OffsetHours_int
      real(kind=dp) :: rem_hours
      real(kind=dp) :: InYear_Y0_OffsettHours
      logical :: IsLeap
      real(kind=dp) :: month_start_hours,month_end_hours

      INTERFACE
        logical function HS_IsLeapYear(iyear)
          implicit none
          !implicit none (type, external)
          integer,intent(in)  :: iyear
        end function HS_IsLeapYear
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if (HoursSince < 0.0_dp .or. &
          HoursSince > 1.0e9_dp) then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      ! div-by-four ARE leapyears -> +1
      ! div-by-100  NOT leapyears -> -1
      ! div-by-400  ARE leapyears -> +1
      !  So, every 400 years, the cycle repeats with 97 extra leap days
      !  Otherwise, normal centuries have 24 leap days
      !  And four-year packages have 1 leap day
      if(useLeaps)then
        HoursIn_Century  = 24*(365 * 100 + 24)
        HoursIn_Year     = 24*(365)
        HoursIn_Leap     = 24
      else
        HoursIn_Century  = 24*(365 * 100)
        HoursIn_Year     = 24*(365)
        HoursIn_Leap     = 0
      endif

      ! Get the number of hours between base year and year 0
      !  First leap hours
      ileaphours = 0
      byear_correction = 0
      if(useLeaps)then
        if(byear >= 0)then
          ! clock starts at 0 so include 0 in the positive accounting
          do i = 0,byear
            if (HS_IsLeapYear(i)) ileaphours = ileaphours + 24
          enddo
          if (HS_IsLeapYear(byear))then
            ! If the base year is itself a leapyear, remove the extra 24 hours
            ! since we will always be using Jan 1 of the base year
            byear_correction = -24
          endif
        else
          ! For negative years, count from year -1 to byear
          do i = byear,-1
            if (HS_IsLeapYear(i)) ileaphours = ileaphours + 24
          enddo
          if (HS_IsLeapYear(byear))then
            ! If the base year is itself a leapyear, remove the extra 24 hours
            ! since we will always be using Jan 1 of the base year
            byear_correction = -24
          endif
        endif
      else
        byear_correction = 0
      endif
      ! Now total hours
      BaseYear_Y0_OffsetHours_int = abs(byear)*HoursIn_Year  + &
                                     ileaphours               + &
                                     byear_correction
      BaseYear_Y0_OffsetHours_int = sign(BaseYear_Y0_OffsetHours_int,byear)

      InYear_Y0_OffsettHours = real(BaseYear_Y0_OffsetHours_int,kind=dp) + &
                                     HoursSince
      rem_hours = InYear_Y0_OffsettHours
      if(InYear_Y0_OffsettHours >= 0.0)then
        ! byear and HoursSince result in an iyear  >=  0
          icent = 0
          HoursIn_This_Century = HoursIn_Century + HoursIn_Leap
          HoursIn_This_Year = HoursIn_Year + HoursIn_Leap

          ! Find which century we are in
          do while (rem_hours >= HoursIn_This_Century)
              ! Account for this century
            icent = icent + 1
            rem_hours = rem_hours - HoursIn_This_Century
              ! Figure out the number of hours in the next century to check
            if (mod(icent,4) == 0) then
              HoursIn_This_Century = HoursIn_Century + HoursIn_Leap
              HoursIn_This_Year = HoursIn_Year + HoursIn_Leap
            else
              HoursIn_This_Century = HoursIn_Century
              HoursIn_This_Year = HoursIn_Year
            endif
          enddo

          ! Find which year we are in
          iyear = 0
          do while (rem_hours >= HoursIn_This_Year)
              ! Account for this year
            iyear = iyear + 1
            rem_hours = rem_hours - HoursIn_This_Year
              ! Figure out the number of hours in the next year to check
            if (mod(iyear,4) == 0) then
              HoursIn_This_Year = HoursIn_Year + HoursIn_Leap
            else
              HoursIn_This_Year = HoursIn_Year
            endif
          enddo

          iyear = iyear + 100*icent
      else
        ! iyear will be negative
        stop 1
      endif
        ! Check if iyear is a leap year

      if(useLeaps)then
        if (HS_IsLeapYear(iyear))then
           IsLeap = .true.
        else
          IsLeap = .false.
        endif
      else
        IsLeap = .false.
      endif
        ! Calculate the day-of-year
      idoy = int(rem_hours/24.0_dp)+1

        ! Get the month we are in
      do imonth=1,12
        if(IsLeap)then
          month_start_hours = real(leapmonthhours(imonth-1),kind=dp)
          month_end_hours   = real(leapmonthhours(imonth),kind=dp)
        else
          month_start_hours = real(monthhours(imonth-1),kind=dp)
          month_end_hours   = real(monthhours(imonth),kind=dp)
        endif
        if(rem_hours >= month_start_hours.and.rem_hours < month_end_hours)then
          rem_hours = rem_hours - month_start_hours
          exit
        endif
      enddo
        ! And the day-of month
      iday = int(rem_hours/24.0_dp)+1
        ! Hours of day
      hours = rem_hours - real((iday-1)*24,kind=dp)

      return

      end subroutine HS_Get_YMDH

!##############################################################################
!
!     HS_xmltime
!
!     Returns the xml time stamp 'yyyy-mm-ddThh:mm:ssZ',
!     giving the year, month, day, hour, minutes, and seconds in
!     Universal Time, given the number of hours since January 1, baseyear.
!
!##############################################################################

      character (len=20) function HS_xmltime(HoursSince,byear,useLeaps)

      ! This module requires Fortran 2003 or later
      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp)     ,intent(in) :: HoursSince
      integer           ,intent(in) :: byear
      logical           ,intent(in) :: useLeaps

      character (len=20) :: string1
      integer            :: iyear, imonth, iday, idoy
      real(kind=dp)      :: hours
      integer            :: ihours, iminutes, iseconds

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          real(kind=8),intent(in)       :: HoursSince
          integer     ,intent(in)       :: byear
          logical     ,intent(in)       :: useLeaps
          integer     ,intent(out)      :: iyear
          integer     ,intent(out)      :: imonth
          integer     ,intent(out)      :: iday
          real(kind=8),intent(out)      :: hours
          integer     ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if(HoursSince < 0.0_dp .or. &
         HoursSince > 1.0e9_dp)then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      ihours = int(hours)
      iminutes = int(60.0_dp*(hours-real(ihours,kind=dp)))
      iseconds = int((60.0_dp*(hours-real(ihours,kind=dp)))-iminutes)*60

        ! build the string
      write(string1,1) iyear, imonth, iday, ihours, iminutes, iseconds
1     format(i4,'-',i2.2,'-',i2.2,'T',i2.2,':',i2.2,':',i2.2,'Z')

      HS_xmltime = string1

      return

      end function HS_xmltime

!##############################################################################
!
!     HS_yyyymmddhhmm_since
!
!     Returns a character string yyyymmddhh.hh giving the year, month, day, and
!     hour, given the number of hours since January 1, of baseyear.
!
!##############################################################################

      character (len=13) function HS_yyyymmddhhmm_since(HoursSince,byear,useLeaps)

      ! This module requires Fortran 2003 or later
      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp)  ,intent(in) ::  HoursSince
      integer        ,intent(in) ::  byear
      logical        ,intent(in) ::  useLeaps

      character (len=1)          ::  string0                        ! a filler character
      character (len=13)         ::  string1
      integer                    ::  iyear, imonth, iday, idoy
      real(kind=dp)              ::  hours

      integer                    ::  ihours, iminutes

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          real(kind=8),intent(in)       :: HoursSince
          integer     ,intent(in)       :: byear
          logical     ,intent(in)       :: useLeaps
          integer     ,intent(out)      :: iyear
          integer     ,intent(out)      :: imonth
          integer     ,intent(out)      :: iday
          real(kind=8),intent(out)      :: hours
          integer     ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if (HoursSince < 0.0_dp .or. &
          HoursSince > 1.0e9_dp) then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      string0 = ':'

      call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      ihours = int(hours)
      iminutes = int(60.0_dp*(hours-real(ihours,kind=dp)))
        ! build the string
      write(string1,'(i4,3i2.2,a,i2.2)') iyear, imonth, iday, ihours, string0, iminutes

      HS_yyyymmddhhmm_since = string1

      return

      end function HS_yyyymmddhhmm_since

!##############################################################################
!
!     HS_yyyymmddhh_since
!
!     Returns a character string yyyymmddhh.hh giving the year, month, day, and
!     hour, given the number of hours since January 1, of baseyear.
!
!##############################################################################

      character (len=13) function HS_yyyymmddhh_since(HoursSince,byear,useLeaps)

      ! This module requires Fortran 2003 or later
      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp),intent(in)    ::  HoursSince
      integer      ,intent(in)    ::  byear
      logical      ,intent(in)    ::  useLeaps

      integer                     ::  iyear, imonth, iday, idoy
      real(kind=dp)               ::  hours

      character (len=13)         ::  string1
      character (len=1)          ::  string0              ! a filler character
      integer                    ::  ihours, ifraction

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          real(kind=8),intent(in)       :: HoursSince
          integer     ,intent(in)       :: byear
          logical     ,intent(in)       :: useLeaps
          integer     ,intent(out)      :: iyear
          integer     ,intent(out)      :: imonth
          integer     ,intent(out)      :: iday
          real(kind=8),intent(out)      :: hours
          integer     ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if (HoursSince < 0.0_dp .or. &
         HoursSince > 1.0e9_dp) then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      string0 = '.'

      call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      ihours = int(hours)
      ifraction = nint(100.0_dp*(hours-real(ihours,kind=dp)))
      if (ifraction == 100) then
        ! if the nearest integer of ifraction is actually the next
        ! hour, adjust ifraction and ihour accordingly
        ifraction = 0
        ihours = ihours + 1
      endif

        ! build the string
      write(string1,'(i4,3i2.2,a,i2.2)') iyear, imonth, iday, ihours, string0, ifraction

      HS_yyyymmddhh_since = string1

      return

      end function HS_yyyymmddhh_since

!##############################################################################
!
!     HS_DayOfYear
!
!     function that calculates the integer day of year given the
!     HoursSince, base year and useLeaps
!       Check against calculator on
!       http://www.7is7.com/otto/datediff.html
!
!##############################################################################

      integer function HS_DayOfYear(HoursSince,byear,useLeaps)

      ! This module requires Fortran 2003 or later
      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp),intent(in) :: HoursSince
      integer      ,intent(in) :: byear
      logical      ,intent(in) :: useLeaps

      integer                ::  iyear, imonth, iday, idoy
      real(kind=dp)          ::  hours

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          real(kind=8),intent(in)       :: HoursSince
          integer     ,intent(in)       :: byear
          logical     ,intent(in)       :: useLeaps
          integer     ,intent(out)      :: iyear
          integer     ,intent(out)      :: imonth
          integer     ,intent(out)      :: iday
          real(kind=8),intent(out)      :: hours
          integer     ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if (HoursSince < 0.0_dp .or. &
         HoursSince > 1.0e9_dp) then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      HS_DayOfYear = idoy

      return

      end function HS_DayOfYear

!##############################################################################
!
!     HS_HourOfDay
!
!     function that calculates the real hour of day given the
!     HoursSince
!
!##############################################################################

      real(kind=8) function HS_HourOfDay(HoursSince,byear,useLeaps)

      ! This module requires Fortran 2003 or later
      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp),intent(in) :: HoursSince
      integer      ,intent(in) :: byear
      logical      ,intent(in) :: useLeaps

      integer              ::  iyear, imonth, iday, idoy
      real(kind=dp)        ::  hours

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          real(kind=8),intent(in)       :: HoursSince
          integer     ,intent(in)       :: byear
          logical     ,intent(in)       :: useLeaps
          integer     ,intent(out)      :: iyear
          integer     ,intent(out)      :: imonth
          integer     ,intent(out)      :: iday
          real(kind=8),intent(out)      :: hours
          integer     ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if (HoursSince < 0.0_dp .or. &
          HoursSince > 1.0e9_dp) then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      HS_HourOfDay = hours

      return

      end function HS_HourOfDay

!##############################################################################
!
!     HS_YearOfEvent
!
!     function that calculates the integer year given the
!     HoursSince
!
!##############################################################################

      integer function HS_YearOfEvent(HoursSince,byear,useLeaps)

      ! This module requires Fortran 2003 or later
      use , intrinsic ::iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp),intent(in) :: HoursSince
      integer      ,intent(in) :: byear
      logical      ,intent(in) :: useLeaps

      integer                ::  iyear, imonth, iday, idoy
      real(kind=dp)          ::  hours

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          real(kind=8),intent(in)       :: HoursSince
          integer     ,intent(in)       :: byear
          logical     ,intent(in)       :: useLeaps
          integer     ,intent(out)      :: iyear
          integer     ,intent(out)      :: imonth
          integer     ,intent(out)      :: iday
          real(kind=8),intent(out)      :: hours
          integer     ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if (HoursSince < 0.0_dp .or. &
          HoursSince > 1.0e9_dp) then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      HS_YearOfEvent = iyear

      return

      end function HS_YearOfEvent

!##############################################################################
!
!     HS_MonthOfEvent
!
!     function that calculates the integer month of year given the
!     HoursSince
!
!##############################################################################

      integer function HS_MonthOfEvent(HoursSince,byear,useLeaps)

      ! This module requires Fortran 2003 or later
      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp),intent(in) :: HoursSince
      integer      ,intent(in) :: byear
      logical      ,intent(in) :: useLeaps

      integer                ::  iyear, imonth, iday, idoy
      real(kind=dp)          ::  hours

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          real(kind=8),intent(in)       :: HoursSince
          integer     ,intent(in)       :: byear
          logical     ,intent(in)       :: useLeaps
          integer     ,intent(out)      :: iyear
          integer     ,intent(out)      :: imonth
          integer     ,intent(out)      :: iday
          real(kind=8),intent(out)      :: hours
          integer     ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if (HoursSince < 0.0_dp .or. &
          HoursSince > 1.0e9_dp) then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      HS_MonthOfEvent = imonth

      return

      end function HS_MonthOfEvent

!##############################################################################
!
!     HS_DayOfEvent
!
!     function that calculates the integer day of month given the
!     HoursSince
!
!##############################################################################

      integer function HS_DayOfEvent(HoursSince,byear,useLeaps)

      ! This module requires Fortran 2003 or later
      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,error_unit

      implicit none 
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision

      real(kind=dp),intent(in) :: HoursSince
      integer      ,intent(in) :: byear
      logical      ,intent(in) :: useLeaps

      integer                ::  iyear, imonth, iday, idoy
      real(kind=dp)          ::  hours

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          real(kind=8),intent(in)       :: HoursSince
          integer     ,intent(in)       :: byear
          logical     ,intent(in)       :: useLeaps
          integer     ,intent(out)      :: iyear
          integer     ,intent(out)      :: imonth
          integer     ,intent(out)      :: iday
          real(kind=8),intent(out)      :: hours
          integer     ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
      END INTERFACE

      ! Error checking the first argument
      ! Note: this must be real*8
      if (HoursSince < 0.0_dp .or. &
          HoursSince > 1.0e9_dp) then
        write(error_unit,*)"HS ERROR: HoursSince variable is either negative or larger"
        write(error_unit,*)"          than ~100,000 years."
        write(error_unit,*)"          Double-check that it was passed as real*8"
        stop 1
      endif

      call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)

      HS_DayOfEvent = iday

      return

      end function HS_DayOfEvent

!##############################################################################
