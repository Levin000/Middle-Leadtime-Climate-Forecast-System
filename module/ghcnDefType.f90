
!****************************************************************************
!
!  PROGRAM: Module Typedef
!
!  PURPOSE:  Structure for GhcnPrcpData
!
!****************************************************************************    
MODULE ghcnDefType
        IMPLICIT NONE
        TYPE GhcnPrcpDataFormat
            INTEGER(KIND = 8) :: Code
            REAL :: Flag
            REAL :: Year
            REAL :: PrcpData(12)
        END TYPE
        TYPE GhcnStaInfo
            INTEGER(KIND = 8) :: Code !վ�����
            CHARACTER(LEN = 20) :: City !����
            CHARACTER(LEN = 20) :: Country !����
            REAL :: Lat  !γ��
            REAL :: Lon  !����
            INTEGER :: Ele  !����
        END TYPE
END MODULE