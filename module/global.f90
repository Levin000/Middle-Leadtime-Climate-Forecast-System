!****************************************************************************
!
!  PROGRAM: Module global
!
!  PURPOSE:  global variables
!****************************************************************************
MODULE global   
        USE GhcnDefType
        IMPLICIT NONE
        REAL(KIND = 8), ALLOCATABLE :: GhcnPrcp(:,:)   !GHCN Prcp���ݿ�
        REAL(KIND = 8), ALLOCATABLE :: GhcnTavg(:,:)  !GHCN Tavg���ݿ�
        TYPE(GhcnStaInfo), ALLOCATABLE :: GhcnTavgStationInfo(:)  !GHCN Station ��Ϣ���ݿ�
        TYPE(GhcnStaInfo), ALLOCATABLE :: StationInfo(:)  !GHCN Station ��Ϣ���ݿ�
        REAL(KIND = 8), ALLOCATABLE :: tempFactorPrcpMonth(:),tempStudyPrcpMonth(:),tempFactorTavgMonth(:)
END MODULE