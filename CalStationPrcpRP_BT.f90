!  CalStationPrcpRP.f90 
!
!  FUNCTIONS:
!  CalStationPrcpRP - Entry point of console application.
!
!  This function can project stations precipitation using other stations Precipitation
    

!****************************************************************************
!
!  PROGRAM: CalStationPrcpRP
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

SUBROUTINE CalStationPrcpRP_BT(StartRate,EndRate)
      !!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"CalStationPrcpRP_BT" :: CalStationPrcpRP_BT
      USE IFPORT
      USE DFPORT
      USE global
      USE ghcnDefType
      USE cmdProgress
      USE ghcnPort
      USE arrayPort
      USE statisticPort
      IMPLICIT NONE
      !****************************************************************************
      ! !                          Variables
      !****************************************************************************
      CHARACTER(LEN = 10) :: DateT                         !��¼����
      CHARACTER(LEN = 8) :: NowTime                        !��¼ʱ��
      CHARACTER(LEN = 500) :: Path                         !�����ļ�exe·��, Name
      CHARACTER(LEN = 500) :: GhcnTavgFile                 !Temperature average�ļ����Ե�ַ
      CHARACTER(LEN = 500) :: StationInfoFile              !station��Ϣ�ļ�
      CHARACTER(LEN = 11) :: StationDirName                !ÿһ��Station���ļ�����ʱ��
      CHARACTER(LEN = 5)  :: StationNumStr                 !����������ʾ��վ����
      CHARACTER(LEN = 500) :: WorkSpace
      CHARACTER(LEN = 500) :: PrcpWorkSpace
      CHARACTER(LEN = 500) :: GhcnPrcpStandardDBFile
      CHARACTER(LEN = 500) :: dirname
      CHARACTER(LEN = 5) :: PressKey
      CHARACTER(LEN = 5) :: StartStationStr,EndStationStr
      INTEGER :: times
      INTEGER :: GhcnTavgColNum = 14                       !GHCNTavg��������
      INTEGER :: MissVal                                   !ȱʡֵ����1Ϊ����0Ϊ������
      INTEGER :: TraceVal                                  !�ۼ���ˮ�Ĵ���1Ϊ����0Ϊ������
      INTEGER :: AheadMonthNum                             !�����ǰԤ�����·���
      INTEGER :: StartMonth                                !�������ݼ�¼��Ghcn Temperature average����ʼ���·�
      INTEGER :: MonthNum                                  !һ����·���
      INTEGER :: StartYear,EndYear,YearLen                 !Ԥ���о��Ŀ�ʼ��ݡ�������ݡ�ʱ�䳤��
      INTEGER :: RankNum                                   !վ����Ч����
      INTEGER :: II,JJ,KK                                  !���ʹ�õı���
      INTEGER :: N                                         !���ʱ�Ա�������ߴη���
      INTEGER :: CoverYears                                !���ñ���վ���Ԥ������վ�㽵ˮ��ֵ�����ܳ�Couple����������
      INTEGER :: StationInfoFileLen                        !GHCN Station��Ϣ������
      INTEGER :: StationNum                                !Ghcn Temperature average������վ������
      INTEGER :: GhcnTavgRowNum                            ! GhcnTemperature average���ݵ�����
      INTEGER :: ValidTavgStationNum                       !GhcnTavg���ݿ�����ʼ��ֹ����StartYear-EndYear֮���վ����
      INTEGER :: ValidPrcpStationNum                       !GhcnPrcp���ݿ�����ʼ��ֹ����StartYear-EndYear֮���վ����
      INTEGER :: fileID = 10, iosval                       !�ļ�ID���ļ�����״̬
      INTEGER :: istatus,istatus_dir_ch                    ! �ı䵱ǰ����Ŀ¼�ɹ�����״̬���½��ļ��гɹ�����״̬
      INTEGER :: tempNum ,tempCount,ColStart                 !��ʱ����
      INTEGER :: FactorTavgLen                             !Ԥ������StartYear->EndYear�ڼ����ݳ���
      INTEGER :: i,j,k,ic,im,StartStationNum,EndStationNum,analysisStationNum
      INTEGER :: ClimateStatus
      INTEGER(KIND = 8)  StudyCode
      INTEGER(KIND = 8), ALLOCATABLE :: ValidTavgStationCodesIndex(:)   !��ȡ��Factor��վ����
      INTEGER(KIND = 8), ALLOCATABLE :: ValidPrcpStationCodesIndex(:)   !��ȡ���о���վ����
      INTEGER(KIND = 8), ALLOCATABLE :: CodesIndexLocation(:)           !�洢ĳһ��վ�������ݿ��е�����ֵ(��λ��)
      INTEGER(KIND = 8), ALLOCATABLE :: GhcnTavgYear(:,:)               !GhcnTavgվ�����ݵĿ�ʼ��ݺͽ��������Ϣ
      INTEGER(KIND = 8), ALLOCATABLE :: IntegerArrayTemp2D(:,:)         ! 2D���ͱ�����ʱ�洢
      INTEGER(KIND = 8), ALLOCATABLE :: ValidStationCoupled(:,:)      !ͨ������������վ�㣬��ͨ�������1ͨ����0��ͨ��
      REAL(KIND = 8) :: DataNumNotEnough = -9.0    !,R_Inf = -5.0, P_Inf = -6.0 ,tg
      REAL(KIND = 8) :: tavgLeftLimit,tavgRightLimit,prcp_anomaly_value,tavg_anomaly_value
      REAL(KIND = 8), ALLOCATABLE :: StudyPrcp(:),TempStudyPrcp(:),TempMonthStudyPrcp(:)     !�о���վ��Ľ�ˮ����
      REAL(KIND = 8), ALLOCATABLE :: FactorTavg(:),TempFactorTavg(:),TempMonthFactorTavg(:)  !Ԥ������վ��Ľ�ˮ����
      REAL(KIND = 8), ALLOCATABLE :: StationCodesUnique(:)                                   !GhcnTavg��Codes���
      REAL(KIND = 8), ALLOCATABLE :: RealArrayTemp2D(:,:)                                    !��ȡGhcnTavgվ�����ݵ���ʱ���ݿ�
      REAL(KIND = 8), ALLOCATABLE :: GhcnTavgStandardDB(:,:)                                 !GhcnTavg��Ÿ�ʽ��׼�����ݿ�
      REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:)                                 !GhcnPrcp��Ÿ�ʽ��׼�����ݿ�
      REAL, ALLOCATABLE :: P(:,:),R(:,:),TempR(:,:),TempP(:,:)                               ! Ԥ��ÿһ��վ��ʱ��P\R��Ϣ
      REAL :: StartRate,EndRate
      REAL :: PPvalue                                       !������ˮƽ
      REAL :: TrainingRate                                  !ѵ������ռ�����ݵı���
      REAL :: maxR2
      TYPE( CLS_CMD_Progress ) :: Progress                  !Commands line process bar
      LOGICAL(4) :: istatus_dir_mk,alive                                !�ļ�����״̬
      NAMELIST /CSPRPBT/ prcp_anomaly_value,tavg_anomaly_value,times,GhcnTavgColNum,MissVal,TraceVal,AheadMonthNum,StartMonth,StartYear,EndYear,MonthNum,ClimateStatus,RankNum,PPvalue,TrainingRate,CoverYears
      !****************************************************************************
      ! !                        Formatting
      !****************************************************************************

100   FORMAT(1X,A10,A4,A1,A2,A1,A2,A3,A8/)
200   FORMAT(I11,1X,A20,A10,F7.2,1X,F7.2,I5/)
300   FORMAT(I20,I20,I20)
400   FORMAT(I20,1320f10.1)
500   FORMAT(f20.0,1320f10.1)
700   FORMAT(f20.0,1320f10.1)
600   FORMAT(I15,24F8.4)

      !****************************************************************************
      ! !                        Initialization
      !****************************************************************************
      ! get and print current time
      PRINT "(1X,A17/)", '>>����ʼ����...'
      CALL Display_Time_Current(DateT,NowTime)
      WRITE(*,100)'��ǰʱ�䣺',DateT(1:4),'/',DateT(5:6),'/',DateT(7:8),'   ',NowTime      ! д��ʱ���ַ���
      ! get current direction and set work direction
      PRINT "(1X,A19)", '>>���������ʼ��...'
      CALL SLEEP(1)
      istatus = GETCWD(Path)
      GhcnTavgFile = TRIM(Path)//'\GhcnData\ghcnm.tavg.v3.3.0.20170515.qcu.dat'
      StationInfoFile = TRIM(Path)//'\GhcnData\ghcnm.tavg.v3.3.0.20161009.qcu.inv'
      WorkSpace = TRIM(Path)//'\BaseOnTavg\CalculateStationPrcpRP\'
      PrcpWorkSpace = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\'
      GhcnPrcpStandardDBFile = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\GhcnPrcpStandardDB.dat'
      istatus_dir_ch = CHDIR(TRIM(WorkSpace)) !����Ĭ�ϵĹ���Ŀ¼
      ! read default namelist file which define default parameters
      OPEN(UNIT = FileID,FILE = './CalStationPrcpRP_BT.namelist')
      READ (fileID,NML = CSPRPBT,ERR = 8089)
      CLOSE(fileID)
      PRINT "(1X,A18)", ">>������ʼ����ɣ�"
      PRINT *,'Main parameters as below:'
      WRITE (*,NML = CSPRPBT)
      CALL SLEEP(1)
      !********************************************************************************************************************************************************
      !********************************************************************************************************************************************************
      !
      !                                                Body of CalStationPrcpRP
      !
      !********************************************************************************************************************************************************
      !********************************************************************************************************************************************************
      
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !                             Read GhcnTavg Data
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PRINT *,'>>��ȡGhcnTavg����...'
      CALL SLEEP(1)
      CALL Inqire_Text_Row(TRIM(GhcnTavgFile),LEN(TRIM(GhcnTavgFile)), GhcnTavgRowNum)
      ALLOCATE(GhcnTavg(GhcnTavgRowNum,GhcnTavgColNum)) !��̬����GHCNԭʼ��ƽ���¶����ݿ��С
      CALL Read_GHCN_Tavg(TRIM(GhcnTavgFile), LEN(TRIM(GhcnTavgFile)), GhcnTavgRowNum, GhcnTavgColNum, MissVal)!��ȡGHCN��ˮ����
      PRINT *,'>>��ȡGhcnTavg������ɣ�'
      CALL SLEEP(1)
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !           get and save ghcnTavg original station codes information 
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PRINT *,">>��ѯGhcnTavgվ������Ϣ�ļ��Ƿ����..."
      CALL SLEEP(1)
      INQUIRE(DIRECTORY = TRIM(WorkSpace)//'DataBase', EXIST = alive)
      IF (alive == .false.) THEN
        istatus_dir_mk = makedirqq(TRIM('DataBase'))
      END IF
      INQUIRE(FILE = './DataBase/StationCodes.dat', EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>GhcnTavgվ������Ϣ�ļ����ڣ�"
        CALL SLEEP(1)
        PRINT *, ">>��ȡGhcnTavgվ������Ϣ..."
        CALL SLEEP(1)
        CALL Inqire_Text_Row('./DataBase/StationCodes.dat',LEN('./DataBase/StationCodes.dat'),StationNum)
        ALLOCATE(StationCodesUnique(StationNum))
        OPEN(UNIT = fileID,FILE = './DataBase/StationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          &POSITION = 'REWIND') ! RECL = 20,
        READ(fileID,'(F20.0)') StationCodesUnique
        CLOSE(fileID)
        PRINT *, ">>��ȡGhcnTavgվ������Ϣ�ɹ���"
        CALL SLEEP(1)
      ELSE
        PRINT *, ">>GhcnTavgվ������Ϣ�ļ�������"
        CALL SLEEP(1)
        PRINT *, ">>��ȡGhcnTavg��վ������Ϣ..."
        CALL SLEEP(1)
        CALL Unique(GhcnTavg(1:GhcnTavgRowNum,1),StationCodesUnique)
        StationNum = SIZE(StationCodesUnique)
        PRINT *, ">>��ȡGhcnTavg��վ������Ϣ��ɣ�"
        CALL SLEEP(1)
        PRINT *,">>����GhcnTavg��վ������Ϣ..."
        CALL SLEEP(1)
        OPEN(UNIT = fileID,FILE = './DataBase/StationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') ! RECL = 20,
        WRITE(fileID,'(I20)') INT(StationCodesUnique,8)
        CLOSE(fileID)
        PRINT *,">>����GhcnTavg��վ������Ϣ���(��ǰexeĿ¼��\StationPrediction\databaseStationCodes.dat�ļ�)��"
        CALL SLEEP(1)
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !        get and save ghcnTavg station start-end year information
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PRINT *,">>��ѯGhcnTavg Station Start-End Year�ļ��Ƿ����..."
      CALL SLEEP(1)
      inquire(FILE = './DataBase/StationStartEndYear.dat', EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>GhcnTavg Station Start-End Year�ļ����ڣ�"
        CALL SLEEP(1)
        PRINT *, ">>��ȡGhcnTavg Station Start-End Year�ļ���Ϣ..."
        CALL SLEEP(1)
        ALLOCATE(GhcnTavgYear(StationNum,3))
        ALLOCATE(IntegerArrayTemp2D(3,StationNum))
        OPEN(UNIT = 50,FILE = './DataBase/StationStartEndYear.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          &  POSITION = 'REWIND') !RECL = 60,
        READ(50,300) IntegerArrayTemp2D
        CLOSE(fileID)
        GhcnTavgYear = TRANSPOSE(IntegerArrayTemp2D)
        PRINT *, ">>��ȡGhcnTavg Station Start-End Year�ļ��ɹ���"
        CALL SLEEP(1)
      ELSE
        PRINT *, ">>GhcnTavg Station Start-End Year�ļ�������..."
        CALL SLEEP(1)
        PRINT *, ">>��ȡGhcnTavg Station Start-End Year��Ϣ..."
        CALL SLEEP(1)
        ALLOCATE(GhcnTavgYear(StationNum,3))
        GhcnTavgYear(1:StationNum,1) = INT(StationCodesUnique,8)
        GhcnTavgYear(1:StationNum,2:3) = 0
        CALL Progress % Set( N = StationNum , L = 30 )!// StationNum�Σ���ʾ����30
        Progress % Prefix = "Station Start-End Year:  "  !// ǰ����ʾ���֣����Ǳ���
        Progress % M = "|" !// ����ɲ��ֵ��ַ������Ǳ���
        Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
        DO II = 1,StationNum
          tempNum = COUNT(INT(GhcnTavg(:,1),8) == GhcnTavgYear(II,1))
          tempCount = 0
          DO JJ = 1,GhcnTavgRowNum
            IF(GhcnTavgYear(II,1) == INT(GhcnTavg(JJ,1),8).AND.tempCount<tempNum) THEN
              tempCount = tempCount + 1
              IF(tempCount == 1) THEN
                GhcnTavgYear(II,2) = INT(GhcnTavg(JJ,2),8)
                GhcnTavgYear(II,3) = INT(GhcnTavg(JJ,2),8)
              ELSE IF(GhcnTavgYear(II,1)==INT(GhcnTavg(JJ,1),8).AND.INT(GhcnTavg(JJ,2),8)<GhcnTavgYear(II,2)) THEN
                GhcnTavgYear(II,2) = INT(GhcnTavg(JJ,2),8)
              ELSE IF(GhcnTavgYear(II,1)==INT(GhcnTavg(JJ,1),8).AND.INT(GhcnTavg(JJ,2),8)>GhcnTavgYear(II,3)) THEN
                GhcnTavgYear(II,3) = INT(GhcnTavg(JJ,2),8)
              END IF
            END IF
          END DO
          CALL Progress % Put( II , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
        END DO
        PRINT *, ">>��ȡGhcnTavg Station Start-End Year��Ϣ��ɣ�"
        CALL SLEEP(1)
        PRINT *,">>����GhcnTavg Station Start-End Year�ļ�..."
        CALL SLEEP(1)
        OPEN(UNIT = fileID,FILE = './DataBase/StationStartEndYear.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') ! RECL = 60,
        WRITE(fileID,300) TRANSPOSE(GhcnTavgYear)
        CLOSE(fileID)
        PRINT *,">>����GhcnTavg Station Start-End Year�ļ����(��ǰexeĿ¼��StationStartEnd.dat�ļ�)��"
        CALL SLEEP(1)
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !         ѡȡվ������ʱ������1901-2010���վ��,������׼���洢���ݿ�
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      YearLen = EndYear - StartYear + 1
      WRITE(*,'(1x,a,i4,a,i4,a)')">>��ȡ",StartYear,"-",EndYear,"������Чվ�㣬��������׼���洢���ݿ�..."
      CALL SLEEP(1)
      ! get and save valid station codes to file
      INQUIRE(FILE = './DataBase/ValidStationCodes.dat', EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>ValidStationCodes.dat�ļ����ڣ�"
        CALL SLEEP(1)
        PRINT *, ">>��ȡValidStationCodes.dat�ļ���..."
        CALL SLEEP(1)
        CALL Inqire_Text_Row('./DataBase/ValidStationCodes.dat',LEN('./DataBase/ValidStationCodes.dat'),ValidTavgStationNum)
        ALLOCATE(ValidTavgStationCodesIndex(ValidTavgStationNum))
        OPEN(UNIT = fileID,FILE = './DataBase/ValidStationCodes.dat')
        READ(fileID,'(I20)') ValidTavgStationCodesIndex
        CLOSE(fileID)
        PRINT *, ">>��ȡValidStationCodes.dat�ļ��ɹ���"
        CALL SLEEP(1)
      ELSE
        tempNum = COUNT(GhcnTavgYear(:,2)<=EndYear.AND.GhcnTavgYear(:,3)>=StartYear)
        tempCount = 0
        ALLOCATE(ValidTavgStationCodesIndex(tempNum))
        PRINT *,">>��ȡվ����..."
        CALL SLEEP(1)
        DO II = 1,StationNum
          IF(GhcnTavgYear(II,2)<=EndYear.AND.GhcnTavgYear(II,3)>=StartYear) THEN
            tempCount = tempCount + 1
            ValidTavgStationCodesIndex(tempCount) = GhcnTavgYear(II,1)
          END IF
        END DO
        ! ������Чվ��������
        PRINT *,">>������Чվ��������..."
        OPEN(UNIT = fileID,FILE = './DataBase/ValidStationCodes.dat') ! RECL = 20,
        WRITE(fileID,'(I20)') INT(ValidTavgStationCodesIndex,8)
        CLOSE(fileID)
        ValidTavgStationNum = SIZE(ValidTavgStationCodesIndex)
        PRINT *,">>������Чվ�������ݳɹ���"
        PRINT *,">>��ȡվ���ųɹ���"
        CALL SLEEP(1)
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !          Build standard Precipitation DataBase
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PRINT *,">>������׼���洢���ݿ�..."
      CALL SLEEP(1)

      ALLOCATE(GhcnTavgStandardDB(ValidTavgStationNum,1+MonthNum*YearLen))
      GhcnTavgStandardDB(:,2:) = -9998  !��׼���ݿ��У�ԭ���ݿ�û�е����ݣ�ȫ������Ϊ-9998��ȱ��Ϊ-9999
      PRINT *,">>������׼���洢���ݿ�ɹ���"
      CALL SLEEP(1)
      PRINT *,">>��ѯ��׼���洢���ݿ������ļ��Ƿ����..."
      CALL SLEEP(1)
      INQUIRE(FILE = './DataBase/GhcnTavgStandardDB.dat', EXIST = alive)
      IF(alive) THEN
        PRINT *, ">>��׼���洢���ݿ������ļ����ڣ�"
        CALL SLEEP(1)
        PRINT *, ">>��ȡ��׼���洢���ݿ������ļ���Ϣ..."
        CALL SLEEP(1)
        ALLOCATE(RealArrayTemp2D(1+YearLen*MonthNum,ValidTavgStationNum))
        OPEN(UNIT = fileID,FILE = './DataBase/GhcnTavgStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') !RECL = 300,
        READ(fileID,500) RealArrayTemp2D
        GhcnTavgStandardDB = TRANSPOSE(RealArrayTemp2D)
        PRINT *, ">>��ȡ��׼���洢���ݿ������ļ���Ϣ��ɣ�"
        CALL SLEEP(1)
        CLOSE(fileID)
        DEALLOCATE(RealArrayTemp2D)
      ELSE
        PRINT *,">>��׼���洢���ݿ������ļ�������..."
        CALL SLEEP(1)
        CALL Progress % Set( N = ValidTavgStationNum , L = 30 )!// size(ValidTavgStationCodesIndex)�Σ���ʾ����30
        Progress % Prefix = "��ȡ��׼���洢���ݿ�����:  "  !// ǰ����ʾ���֣����Ǳ���
        Progress % M = "#" !// ����ɲ��ֵ��ַ������Ǳ���
        Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
        DO II = 1,ValidTavgStationNum
          GhcnTavgStandardDB(II,1) = ValidTavgStationCodesIndex(II)
          tempNum = COUNT(GhcnTavg(:,1) == ValidTavgStationCodesIndex(II))
          ALLOCATE(CodesIndexLocation(tempNum))
          CodesIndexLocation = ArrayFind(GhcnTavg(:,1),'=',REAL(ValidTavgStationCodesIndex(II),8))
          DO KK = 1,tempNum
            IF(GhcnTavg(CodesIndexLocation(KK),2)>=StartYear.AND.GhcnTavg(CodesIndexLocation(KK),2)<=EndYear) THEN
              ColStart =(GhcnTavg(CodesIndexLocation(KK),2) - StartYear)*MonthNum +1  !GhcnTavgStandardDB�е��к�
              GhcnTavgStandardDB(II,1+ColStart:1+ColStart+MonthNum) = GhcnTavg(CodesIndexLocation(KK),3:14)
            END IF
          END DO
          DEALLOCATE(CodesIndexLocation)
          CALL Progress % Put( II , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
        END DO
        PRINT *,">>����GhcnTavgStandardDB�ļ�..."
        CALL SLEEP(1)
        OPEN(UNIT = fileID,FILE = './DataBase/GhcnTavgStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') !RECL = 300,
        DO i = 1,ValidTavgStationNum!*(EndYear - StartYear + 1)
          WRITE(fileID,400) INT(GhcnTavgStandardDB(i,1),8),GhcnTavgStandardDB(i,2:)
        END DO
        CLOSE(fileID)
        PRINT *,">>����GhcnTavgStandardDB�ļ����(��ǰexeĿ¼��GhcnTavgStandardDB.dat�ļ�)!"
        CALL SLEEP(1)
        !!-------------------------------------------------------------------------
        !!     ɸѡ�¶�ÿ��С��   ��+times�� (���ұ߽�)����
        !!-------------------------------------------------------------------------
        !print *,'>> ���Ц�+/-3�Ҵ���'
        !do ic = 1,ValidTavgStationNum
        !  do im = 1,MonthNum
        !    tempCount = 0
        !    do i = 1,YearLen
        !      if(GhcnTavgStandardDB(ic,(i-1)*MonthNum+im+1) > -9998) then
        !        tempCount = tempCount + 1
        !      end if
        !    end do
        !    ALLOCATE(CodesIndexLocation(tempCount))
        !    tempCount = 0
        !    do i = 1,YearLen
        !      if(GhcnTavgStandardDB(ic,(i-1)*MonthNum+im+1) > -9998) then
        !        tempCount = tempCount + 1
        !        CodesIndexLocation(tempCount) = (i-1)*MonthNum+im+1
        !      end if
        !    end do
        !    CALL getTavgValidLimit(GhcnTavgStandardDB(ic,CodesIndexLocation),tempCount,tavgLeftLimit,tavgRightLimit,times)
        !    do i = 1,tempCount
        !      if((GhcnTavgStandardDB(ic,CodesIndexLocation(i)) > tavgRightLimit) .or. (GhcnTavgStandardDB(ic,CodesIndexLocation(i)) < tavgLeftLimit)) then
        !        GhcnTavgStandardDB(ic,CodesIndexLocation(i)) = -9997
        !      end if
        !    end do
        !    DEALLOCATE(CodesIndexLocation)
        !  end do
        !end do
        !PRINT *,">>����GhcnTavgStandardDB�ļ�..."
        !CALL SLEEP(1)
        !OPEN(UNIT = fileID,FILE = './DataBase/GhcnTavgStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
        !  & POSITION = 'REWIND') !RECL = 300,
        !DO i = 1,ValidTavgStationNum!*(EndYear - StartYear + 1)
        !  WRITE(fileID,400) INT(GhcnTavgStandardDB(i,1),8),GhcnTavgStandardDB(i,2:)
        !END DO
        !CLOSE(fileID)
        !PRINT *,">>����GhcnTavgStandardDB�ļ����(��ǰexeĿ¼��GhcnTavgStandardDB.dat�ļ�)!"
        !CALL SLEEP(1)
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !         ��ȡ��ˮ������Чվ����  ValidPrcpStationCodesIndex
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INQUIRE(FILE = TRIM(PrcpWorkSpace)//'\ValidStationCodes.dat', EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>GhcnPrcp��ValidStationCodes.dat�ļ����ڣ�"
        CALL SLEEP(1)
        PRINT *, ">>��ȡGhcnPrcp��ValidStationCodes.dat��..."
        CALL SLEEP(1)
        CALL Inqire_Text_Row(TRIM(PrcpWorkSpace)//'\ValidStationCodes.dat',LEN(TRIM(PrcpWorkSpace)//'\ValidStationCodes.dat'),ValidPrcpStationNum)
        ALLOCATE(ValidPrcpStationCodesIndex(ValidPrcpStationNum))
        OPEN(UNIT = fileID,FILE = TRIM(PrcpWorkSpace)//'\ValidStationCodes.dat')
        READ(fileID,'(I20)') ValidPrcpStationCodesIndex
        CLOSE(fileID)
        PRINT *, ">>��ȡGhcnPrcpStandardDBFile��Ϣ�ɹ���"
        CALL SLEEP(1)
      ELSE
        PRINT *, ">>GhcnPrcpStandardDBFile�ļ������ڣ�"
        PAUSE
      END IF
      !PRINT *,'length of array:',size(ValidPrcpStationCodesIndex)
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !         ��ȡ��ˮ����1901-2010��׼���洢���ݿ�
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INQUIRE(FILE = TRIM(GhcnPrcpStandardDBFile), EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>GhcnPrcpStandardDBFile�ļ����ڣ�"
        CALL SLEEP(1)
        PRINT *, ">>��ȡGhcnPrcpStandardDBFile��..."
        CALL SLEEP(1)
        ALLOCATE(RealArrayTemp2D(1+YearLen*MonthNum,ValidPrcpStationNum))
        ALLOCATE(GhcnPrcpStandardDB(ValidPrcpStationNum,1+YearLen*MonthNum))
        OPEN(UNIT = fileID,FILE = TRIM(GhcnPrcpStandardDBFile))
        READ(fileID,700) RealArrayTemp2D
        CLOSE(fileID)
        GhcnPrcpStandardDB = TRANSPOSE(RealArrayTemp2D)
        PRINT *, ">>��ȡGhcnPrcpStandardDBFile��Ϣ�ɹ���"
        CALL SLEEP(1)
        DEALLOCATE(RealArrayTemp2D)
      ELSE
        PRINT *, ">>GhcnPrcpStandardDBFile�ļ������ڣ�"
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !         ��վ�����Ԥ������
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ALLOCATE(StudyPrcp(YearLen*MonthNum))
      ALLOCATE(FactorTavg(YearLen*MonthNum))
      INQUIRE(DIRECTORY = TRIM(WorkSpace)//'StationList', EXIST = alive)
      IF (alive == .false.) THEN
        istatus_dir_mk = makedirqq(TRIM('StationList'))
      END IF
      istatus_dir_ch = CHDIR(TRIM(WorkSpace)//TRIM('StationList\'))
      ALLOCATE(R(ValidTavgStationNum*MonthNum,AheadMonthNum))      !��ValidTavgStationNum ���¶�վ����Ԥ��
      ALLOCATE(TempR(ValidTavgStationNum*MonthNum,AheadMonthNum))
      ALLOCATE(P(ValidTavgStationNum*MonthNum,AheadMonthNum))
      ALLOCATE(TempP(ValidTavgStationNum*MonthNum,AheadMonthNum))
      CALL Progress % Set( N = ValidPrcpStationNum , L = 30 )!// size(ValidTavgStationCodesIndex)�Σ���ʾ����30
      Progress % M = "-" !// ����ɲ��ֵ��ַ������Ǳ���
      Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
      
      PRINT *,'�Ƿ���Ҫ����ĳһ������ĳһ��վ�㣿 y/n'
      READ (*,*),PressKey
      IF(TRIM(PressKey) == 'n') THEN
          StartStationNum = FLOOR(ValidPrcpStationNum*StartRate)+1
          EndStationNum = FLOOR(ValidPrcpStationNum*EndRate)
      ELSE
          PRINT *,' '
96        PRINT *,'Please input StartNum(as integer format ,StartNum should >= 1 and <= 20547):'
          PRINT *,' '
          READ (*,*) StartStationNum
          PRINT *,' '
          PRINT *,'Please input EndNum(as integer format ,EndNum should >= 1 and <= 20547):'
          PRINT *,' '
          READ (*,*) EndStationNum
          PRINT *,' '
      END IF
      
      analysisStationNum = EndStationNum - StartStationNum + 1
      ALLOCATE(ValidStationCoupled(analysisStationNum,13))
      ValidStationCoupled = 0
      
      PRINT *,analysisStationNum

      ! Ϊ�˽��쳣ֵ�ų����⣬�����Ԥ����ʱ���ǽ����еĽ�ˮ�쳣ֵ��Ϊ-9996���¶ȵ��쳣ֵ��Ϊ-9996
      WHERE (GhcnPrcpStandardDB < 0)
        GhcnPrcpStandardDB = -9996
      END WHERE
      WHERE (GhcnTavgStandardDB == -9999 .OR. GhcnTavgStandardDB == -9998)
        GhcnTavgStandardDB = -9996
      END WHERE
      
      DO i = StartStationNum,EndStationNum
        ValidStationCoupled(i - StartStationNum + 1,1) = ValidPrcpStationCodesIndex(i)
        R = DataNumNotEnough
        P = DataNumNotEnough
        WRITE(StationDirName,'(I11)') ValidPrcpStationCodesIndex(i)!StudyCode
        istatus_dir_mk = MAKEDIRQQ(TRIM(StationDirName))
        istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'StationList\'//TRIM(StationDirName))
        StudyPrcp = GhcnPrcpStandardDB(i,2:)
        WRITE(StationNumStr,'(I5)') i
        Progress % Prefix = "StationNum:"//TRIM(StationNumStr)//'/20547   '  !// ǰ����ʾ���֣����Ǳ���
        DO j = 1,ValidTavgStationNum
          FactorTavg = GhcnTavgStandardDB(j,2:)
          FactorTavgLen = MonthNum*YearLen
          DO k = 1,AheadMonthNum
            ALLOCATE(TempFactorTavg(FactorTavgLen - k))
            ALLOCATE(TempStudyPrcp(FactorTavgLen - k))
            TempFactorTavg = FactorTavg(1:FactorTavgLen - k)
            TempStudyPrcp = StudyPrcp(k+1:)
            DO ii = 1,MonthNum            
              ALLOCATE(TempMonthFactorTavg(SIZE(TempFactorTavg(ii:(FactorTavgLen - k):MonthNum))))
              ALLOCATE(TempMonthStudyPrcp(SIZE(TempStudyPrcp(ii:(FactorTavgLen - k):MonthNum))))
              TempMonthFactorTavg = TempFactorTavg(ii:(FactorTavgLen - k):MonthNum)
              TempMonthStudyPrcp = TempStudyPrcp(ii:(FactorTavgLen - k):MonthNum)
              
              if((isContinuityGT_M(TempMonthStudyPrcp,ClimateStatus,prcp_anomaly_value) .EQ. .false.) .and. (isContinuityGT_M(TempMonthFactorTavg,ClimateStatus,tavg_anomaly_value) .EQ. .false.)) then
                tempCount = COUNT(TempMonthFactorTavg>-9998.AND.TempMonthStudyPrcp>=0)
                ALLOCATE(CodesIndexLocation(tempCount))
                tempNum = 0
                DO jj = 1,SIZE(TempMonthFactorTavg)
                  IF(TempMonthFactorTavg(jj)>-9998.AND.TempMonthStudyPrcp(jj)>=0) THEN
                    tempNum = tempNum + 1
                    CodesIndexLocation(tempNum) = jj
                  END IF
                END DO
                !print *,'tempCount',tempCount
                IF(tempCount >= CoverYears) THEN
                  IF(MOD(k + ii + StartMonth - 1,MonthNum) == 0) THEN
                    ValidStationCoupled(i - StartStationNum + 1 ,MonthNum+1) = 1  !ͳ��վ���Ƿ������Լ�¼�����ڴ˴�ֻ��Ϊ��ͳ���Ƿ���ڼ�¼
                    CALL Correlation(FLOOR(tempCount*TrainingRate),TempMonthFactorTavg(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),&
                      TempMonthStudyPrcp(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),R(j+(MonthNum-1)*ValidTavgStationNum,k))
                    !IF(R(j+(MonthNum-1)*ValidTavgStationNum,k) == R_Inf) THEN
                    !  P(j+(MonthNum-1)*ValidTavgStationNum,k) = R_Inf
                    !ELSE
                    CALL Pvalue(FLOOR(tempCount*TrainingRate),R(j+(MonthNum-1)*ValidTavgStationNum,k),P(j+(MonthNum-1)*ValidTavgStationNum,k))
                    !END IF
                  ELSE
                    ValidStationCoupled(i - StartStationNum + 1 ,MOD(k + ii + StartMonth - 1,MonthNum)+1) = 1  !ͳ��վ���Ƿ������Լ�¼�����ڴ˴�ֻ��Ϊ��ͳ���Ƿ���ڼ�¼
                    CALL Correlation(FLOOR(tempCount*TrainingRate),TempMonthFactorTavg(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),&
                      TempMonthStudyPrcp(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),&
                      R(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidTavgStationNum,k))
                    !IF(R(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidTavgStationNum,k) == R_Inf) THEN
                    !  P(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidTavgStationNum,k) = R_Inf
                    !ELSE
                    CALL Pvalue(FLOOR(tempCount*TrainingRate),R(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidTavgStationNum,k),&
                      P(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidTavgStationNum,k))
                    !END IF
                  END IF
                END IF
                DEALLOCATE(CodesIndexLocation)
              endif
              DEALLOCATE(TempMonthFactorTavg)
              DEALLOCATE(TempMonthStudyPrcp)
              
            END DO
            DEALLOCATE(TempFactorTavg)
            DEALLOCATE(TempStudyPrcp)
          END DO
        END DO
        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
        
        ALLOCATE(CodesIndexLocation(2))
        
        IF(TRIM(PressKey) == 'y') THEN
            open(fileID,FILE = 'R.txt',IOSTAT = iosval)
            do ii = 1,MonthNum * ValidTavgStationNum
              if(mod(ii,ValidTavgStationNum) /= 0) then
                write(fileID, '(I15,24F6.2)' ) ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)), R(ii,:)  !CodesIndex(jj),
              else
                write(fileID, '(I15,24F6.2)' ) ValidTavgStationCodesIndex(ValidTavgStationNum), R(ii,:)  !CodesIndex(jj),
              endif
            end do
            close(fileID)
            open(fileID,FILE = 'P.txt',IOSTAT = iosval)
            do ii = 1,MonthNum*ValidTavgStationNum
              if(mod(ii,ValidTavgStationNum) /= 0) then
                write(fileID, '(I15,24F6.2)') ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)),P(ii, :)  !CodesIndex(jj),
              else
                write(fileID, '(I15,24F6.2)') ValidTavgStationCodesIndex(ValidTavgStationNum),P(ii, :)  !CodesIndex(jj),
              endif
            end do
            close(fileID)
        END IF
        !OPEN(fileID,FILE = 'R.bin',IOSTAT = iosval, FORM = 'UNFORMATTED', ACCESS = 'DIRECT',RECL = 26, STATUS = 'REPLACE')
        !DO ii = 1,MonthNum * ValidTavgStationNum
        !    IF(mod(ii,ValidTavgStationNum) ==0 ) then
        !        WRITE(fileID, rec = ii ) ValidTavgStationCodesIndex(ValidTavgStationNum),(R(ii,k), k = 1, AheadMonthNum)  !ValidTavgStationCodesIndex(jj),
        !    ELSE
        !        WRITE(fileID, rec = ii ) ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)),(R(ii,k), k = 1, AheadMonthNum)  !ValidTavgStationCodesIndex(jj),
        !    ENDIF
        !END DO
        !CLOSE(fileID)
        !OPEN(fileID,FILE = 'P.bin',IOSTAT = iosval, FORM = 'UNFORMATTED', ACCESS = 'DIRECT',RECL = 26, STATUS = 'REPLACE')
        !DO ii = 1,MonthNum*ValidTavgStationNum
        !    IF(mod(ii,ValidTavgStationNum)) then
        !        WRITE(fileID, rec = ii) ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)),(P(ii, k),k = 1, AheadMonthNum)  !ValidTavgStationCodesIndex(jj),
        !    ELSE
        !        WRITE(fileID, rec = ii) ValidTavgStationCodesIndex(ValidTavgStationNum),(P(ii, k), k = 1, AheadMonthNum)  !ValidTavgStationCodesIndex(jj),
        !    ENDIF
        !END DO
        !CLOSE(fileID)


        ! ����ǰ15��,P<0.01
        PPvalue = 0.01
        TempR = R**2
        WHERE(P >= PPvalue)   !ɾ������ˮƽ����0.01��ֵ
          TempR = 0
        END WHERE
        WHERE(P == DataNumNotEnough)   !the number of coupled records is less than 50, R .EQ. -9.0
          TempR = DataNumNotEnough
        END WHERE
        !WHERE(TempR == R_Inf)   !R .EQ. NaN -5.0
        !  TempR = 0
        !END WHERE
        !WHERE(TempP == P_Inf)   !P .EQ. Infinity -6.0
        !  TempR = 0
        !END WHERE
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.01.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          DO jj = 1,RankNum
            maxR2 = MAXVAL(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
            CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
            WRITE(fileID,'(I15,I10,3F6.2)') ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),maxR2,&
              &R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
              &P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
            IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
              TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
            END IF
          END DO
        END DO
        CLOSE(fileID)
        ! ����ǰ15��,P<0.05
        PPvalue = 0.05
        TempR = R**2
        WHERE(P >= PPvalue)   !ɾ������ˮƽ����0.05��ֵ
          TempR = 0
        END WHERE
        WHERE(P == DataNumNotEnough)   !R .EQ. NaN
          TempR = DataNumNotEnough
        END WHERE
        !WHERE(TempR == R_Inf)   !R .EQ. NaN
        !  TempR = 0
        !END WHERE
        !WHERE(TempP == P_Inf)   !P .EQ. Infinity
        !  TempR = 0
        !END WHERE
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.05.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          DO jj = 1,RankNum
            maxR2 = MAXVAL(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
            CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
            WRITE(fileID,'(I15,I10,3F6.2)') ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),maxR2,&
              &R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
              &P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
            IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
              TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
            END IF
          END DO
        END DO
        CLOSE(fileID)

        ! ����ǰ15��,P<0.1
        PPvalue = 0.1
        TempR = R**2
        WHERE(P >= PPvalue)   !ɾ������ˮƽ����0.05��ֵ
          TempR = 0
        END WHERE
        WHERE(P == DataNumNotEnough)   !R .EQ. NaN
          TempR = DataNumNotEnough
        END WHERE
        !WHERE(TempR == R_Inf)   !R .EQ. NaN
        !  TempR = 0
        !END WHERE
        !WHERE(TempP == P_Inf)   !P .EQ. Infinity
        !  TempR = 0
        !END WHERE
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.1.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          DO jj = 1,RankNum
            maxR2 = MAXVAL(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
            CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
            WRITE(fileID,'(I15,I10,3F6.2)') ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),maxR2,&
              &R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
              &P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
            IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
              TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
            END IF
          END DO
        END DO
        CLOSE(fileID)
        DEALLOCATE(CodesIndexLocation)
        istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'StationList\')
      END DO
      !================================================================================================================
      istatus_dir_ch = CHDIR(TRIM(WorkSpace)) !����Ĭ�ϵĹ���Ŀ¼
      WRITE(StartStationStr,'(I5)') StartStationNum
      WRITE(EndStationStr,'(I5)') EndStationNum!StudyCode
      OPEN(fileID,FILE = TRIM(ADJUSTL(StartStationStr))//'-'//TRIM(ADJUSTL(EndStationStr))//'.dat',IOSTAT = iosval)
      DO i = 1, analysisStationNum
        WRITE(fileID,'(I15,12I3)') ValidStationCoupled(i,:)
      END DO
      CLOSE(fileID)
      !================================================================================================================
      PRINT "(1X,A17/)", '>>�������н�����'
      CALL Display_Time_Current(DateT,NowTime)
      WRITE(*,100)'��ǰʱ�䣺',DateT(1:4),'/',DateT(5:6),'/',DateT(7:8),'   ',NowTime      ! д��ʱ���ַ���
      PRINT "(/1X,A19)",'���������������...'
      PAUSE
      STOP
8089  PRINT *,'�ļ���ȡ����'

END SUBROUTINE CalStationPrcpRP_BT


    
