
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
            INTEGER(KIND = 8) :: Code !站点编码
            CHARACTER(LEN = 20) :: City !城市
            CHARACTER(LEN = 20) :: Country !国家
            REAL :: Lat  !纬度
            REAL :: Lon  !经度
            INTEGER :: Ele  !海拔
        END TYPE
END MODULE