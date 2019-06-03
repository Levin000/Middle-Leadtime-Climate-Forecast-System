!****************************************************************************
!
!  SUBROUTINE: Display_Time_Current
!
!  PURPOSE:  display current time.
!
!****************************************************************************
  SUBROUTINE Display_Time_Current(date,now)
    IMPLICIT NONE
    ! Variables
    INTEGER date_time(8)
    CHARACTER*10 date,clock,zone
    CHARACTER*8 :: now
    ! Body of Display_Time_Current
    CALL TIME (now)     ! 读系统时间
    CALL DATE_AND_TIME(date,clock,zone,date_time)
    RETURN
  END SUBROUTINE Display_Time_Current