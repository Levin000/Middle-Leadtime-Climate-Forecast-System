!****************************************************************************
!
!  PROGRAM: Module global
!
!  PURPOSE:  global variables
!****************************************************************************
MODULE global   
        USE GhcnDefType
        IMPLICIT NONE
        REAL(KIND = 8), ALLOCATABLE :: GhcnPrcp(:,:)   !GHCN Prcp数据库
        REAL(KIND = 8), ALLOCATABLE :: GhcnTavg(:,:)  !GHCN Tavg数据库
        TYPE(GhcnStaInfo), ALLOCATABLE :: GhcnTavgStationInfo(:)  !GHCN Station 信息数据库
        TYPE(GhcnStaInfo), ALLOCATABLE :: StationInfo(:)  !GHCN Station 信息数据库
        REAL(KIND = 8), ALLOCATABLE :: tempFactorPrcpMonth(:),tempStudyPrcpMonth(:),tempFactorTavgMonth(:)
END MODULE