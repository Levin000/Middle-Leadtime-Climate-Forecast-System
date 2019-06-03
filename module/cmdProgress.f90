!****************************************************************************
!
!  PROGRAM: MODULE CMD_Progress
!
!  PURPOSE:  进度条
!
!****************************************************************************    
MODULE cmdProgress
    IMPLICIT NONE
    PRIVATE
    LOGICAL , PARAMETER , PUBLIC :: CMD_PROGRESS_ABSOLUTE = .true.
    TYPE , PUBLIC :: CLS_CMD_Progress
        INTEGER , PRIVATE :: N , lens , i
        CHARACTER :: M = "*" , O = "."
        CHARACTER(LEN=64) :: Prefix
    CONTAINS
    PROCEDURE :: Set
    PROCEDURE :: Put
    END TYPE CLS_CMD_Progress
CONTAINS
SUBROUTINE Set( this , N , L )
    CLASS( CLS_CMD_Progress ) :: this
    INTEGER , INTENT( IN ) :: N , L
    this % N    = N
    this % lens = L
    this % i = 0
    this % Prefix = " Progress: " !//
END SUBROUTINE Set
SUBROUTINE Put( this , K , bAbsol )
    CLASS( CLS_CMD_Progress ) :: this
    INTEGER , INTENT( IN ) :: K
    LOGICAL , optional :: bAbsol
    CHARACTER(LEN=1) :: br
    INTEGER :: jm
    this % i = this % i + K
    IF ( PRESENT( bAbsol ) ) THEN
        IF ( bAbsol ) this % i = K
    END IF
    IF ( this % i > this % n ) this % i = this % n
    jm = NINT( REAL( this%i * this%lens ) / REAL( this%N ) )
    IF ( this%i < this%n ) THEN
        br = CHAR(13)
    ELSE
        br = CHAR(10)
    END IF
    !write( * , '(5a,f6.2,2a)',advance="no") trim(this%Prefix) , '[' , &
    WRITE( * , '(5a,f6.2,2a\)') TRIM(this%Prefix) , ' |' , & !// please use above line if you compiler can't pass this line 如您的编译器不支持，请用上方语句代替
    REPEAT(this%M , jm ) , REPEAT( this%O , this%lens-jm ) , '| ' , this%i*100.0/this%N , "%" , br
END SUBROUTINE Put

END MODULE cmdProgress