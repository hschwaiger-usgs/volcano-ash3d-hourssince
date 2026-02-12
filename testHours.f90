      program testHours

      use, intrinsic :: iso_fortran_env, only : &
         real32,real64,input_unit,output_unit,error_unit

      implicit none
      !implicit none (type, external)

        ! These single and double precision parameters should be 4 and 8
      !integer, parameter :: sp = real32  ! selected_real_kind( 6,   37) ! single precision
      integer, parameter :: dp = real64  ! selected_real_kind(15,  307) ! double precision


      integer,parameter       :: NRAND      = 10000    ! Number of pseudo-random tests
      real(kind=dp),parameter :: HOURTHRESH = 0.01_dp   ! Tolerance in hours for inverse functions

      integer            :: i

      integer            :: iyear    = 0
      integer            :: imonth   = 0
      integer            :: iday     = 0
      integer            :: idoy     = 0
      real(kind=dp)      :: hours    = 0.0_dp
      integer            :: byear    = 1000
      logical            :: useLeaps = .true.

      integer                          :: n
      integer,allocatable,dimension(:) :: seed
      real(kind=dp)                    :: s_rand
      real(kind=dp)      :: HoursSince
      real(kind=dp)      :: hours2
      logical            :: Check_failed

      INTERFACE
        subroutine HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
          implicit none
          !implicit none (type, external)
          integer       ,parameter        :: dp        = 8 ! double precision
          real(kind=dp) ,intent(in)       :: HoursSince
          integer       ,intent(in)       :: byear
          logical       ,intent(in)       :: useLeaps
          integer       ,intent(out)      :: iyear
          integer       ,intent(out)      :: imonth
          integer       ,intent(out)      :: iday
          real(kind=dp) ,intent(out)      :: hours
          integer       ,intent(out)      :: idoy
        end subroutine HS_Get_YMDH
        real(kind=8) function HS_hours_since_baseyear(iyear,imonth,iday,hours,byear,useLeaps)
          implicit none
          !implicit none (type, external)
          integer       ,parameter        :: dp        = 8 ! double precision
          integer       ,intent(in)       :: iyear
          integer       ,intent(in)       :: imonth
          integer       ,intent(in)       :: iday
          real(kind=dp) ,intent(in)       :: hours
          integer       ,intent(in)       :: byear
          logical       ,intent(in)       :: useLeaps
        end function HS_hours_since_baseyear
      END INTERFACE

      ! You can always check against calculator on:
      !   http://www.7is7.com/otto/datediff.html
      ! Use the Proleptic Gregorian calendar
      ! https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar

      write(output_unit,*)"Verifying that HS_Get_YMDH and HS_hours_since_baseyear are inverses"
      write(output_unit,*)"for 10000 random times between 1000 and 2015"

      Check_failed = .false.
      call random_seed(size=n)
      allocate(seed(n))
      seed(:) = 123456789
      call random_seed(put=seed)
      deallocate(seed)

      do i = 1,NRAND
        call random_number(s_rand)
        ! scale s_rand to the range byear-> 2015
        HoursSince = s_rand*8760.0_dp*real((2015-byear),kind=dp)
        call HS_Get_YMDH(HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy)
        hours2 = HS_hours_since_baseyear(iyear,imonth,iday,hours,byear,useLeaps)
        if(abs(HoursSince-hours2) > HOURTHRESH)then
          Check_failed = .true.
          write(output_unit,*)"ERROR",HoursSince,byear,useLeaps,iyear,imonth,iday,hours,idoy,hours2
        endif
      enddo

      if(Check_failed)then
        write(output_unit,*)"volcano-ash3d-hourssince internal check: FAIL"
        stop 1
      else
        write(output_unit,*)"volcano-ash3d-hourssince internal check: PASS"
        stop 0
      endif

      end program testHours
