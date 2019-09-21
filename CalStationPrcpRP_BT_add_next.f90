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

  SUBROUTINE CalStationPrcpRP_BT_add_next(StartRate,EndRate)
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
  CHARACTER(LEN = 5)  :: StationNumStr ,TotalAnalysisStationNumStr                !����������ʾ��վ����
  CHARACTER(LEN = 500) :: WorkSpace
  CHARACTER(LEN = 500) :: PrcpWorkSpace
  CHARACTER(LEN = 500) :: GhcnPrcpStandardDBFile
  CHARACTER(LEN = 500) :: dirname
  CHARACTER(LEN = 5) :: PressKey
  CHARACTER(LEN = 5) :: StartStationStr,EndStationStr
  CHARACTER(LEN = 5) :: monthStr,RankNumStr

  CHARACTER(LEN = 500) :: PredicteAndCheckDataPath  !�������ݿ�

  INTEGER :: times
  INTEGER :: GhcnTavgColNum = 14                       !GHCNTavg��������
  INTEGER :: MissVal                                   !ȱʡֵ����1Ϊ����0Ϊ������
  INTEGER :: TraceVal                                  !�ۼ���ˮ�Ĵ���1Ϊ����0Ϊ������
  INTEGER :: AheadMonthNum                             !�����ǰԤ�����·���
  INTEGER :: StartMonth                                !�������ݼ�¼��Ghcn Temperature average����ʼ���·�
  INTEGER :: MonthNum                                  !һ����·���
  INTEGER :: StartYear,EndYear,YearLen                 !Ԥ���о��Ŀ�ʼ��ݡ�������ݡ�ʱ�䳤��
  INTEGER :: RankNum                                   !վ����Ч����
  INTEGER :: saveRankNum                                          !��������ʱ��վ����Ч��������ֻ����ǰsaveRankNum��վ�����Ϣ
  INTEGER :: II,JJ,KK                                  !���ʹ�õı���
  INTEGER :: N                                         !���ʱ�Ա�������ߴη���
  INTEGER :: CoverYears                                !���ñ���վ���Ԥ������վ�㽵ˮ��ֵ�����ܳ�Couple����������
  INTEGER :: StationInfoFileLen                        !GHCN Station��Ϣ������
  INTEGER :: StationNum                                !Ghcn Temperature average������վ������
  INTEGER :: GhcnTavgRowNum                            ! GhcnTemperature average���ݵ�����
  INTEGER :: ValidTavgStationNum                       !GhcnTavg���ݿ�����ʼ��ֹ����StartYear-EndYear֮���վ����
  INTEGER :: ValidPrcpStationNum                       !GhcnPrcp���ݿ�����ʼ��ֹ����StartYear-EndYear֮���վ����
  INTEGER :: fileID = 10, iosval                       !�ļ�ID���ļ�����״̬
  INTEGER :: savePrcpFileID = 200
  INTEGER :: istatus,istatus_dir_ch                    ! �ı䵱ǰ����Ŀ¼�ɹ�����״̬���½��ļ��гɹ�����״̬
  INTEGER :: tempNum ,tempCount,ColStart                 !��ʱ����
  INTEGER :: FactorTavgLen                             !Ԥ������StartYear->EndYear�ڼ����ݳ���
  INTEGER :: i,j,k,ic,im,StartStationNum,EndStationNum,analysisStationNum
  INTEGER :: ClimateStatus
  INTEGER :: R2CountTotal

  INTEGER :: StudyStationNum001
  INTEGER :: pTandMonth                                           !Ԥ����վ��ĵ�ǰ�·ݣ���AheadMonth��MonthRoll�����£���
  INTEGER :: trainLen, saveTrainLen, leaveOut

  INTEGER(KIND = 8)  StudyCode
  INTEGER(KIND = 8), ALLOCATABLE :: ValidTavgStationCodesIndex(:)   !��ȡ��Factor��վ����
  INTEGER(KIND = 8), ALLOCATABLE :: ValidPrcpStationCodesIndex(:)   !��ȡ���о���վ����
  INTEGER(KIND = 8), ALLOCATABLE :: CodesIndexLocation(:)           !�洢ĳһ��վ�������ݿ��е�����ֵ(��λ��)
  INTEGER(KIND = 8), ALLOCATABLE :: CIL(:)                           !����ͬCodesIndexLocation
  INTEGER(KIND = 8), ALLOCATABLE :: GhcnTavgYear(:,:)               !GhcnTavgվ�����ݵĿ�ʼ��ݺͽ��������Ϣ
  INTEGER(KIND = 8), ALLOCATABLE :: IntegerArrayTemp2D(:,:)         ! 2D���ͱ�����ʱ�洢
  INTEGER(KIND = 8), ALLOCATABLE :: ValidStationCoupled(:,:)      !ͨ������������վ�㣬��ͨ�������1ͨ����0��ͨ��
  REAL(KIND = 8) :: DataNumNotEnough = -9.0    !,R_Inf = -5.0, P_Inf = -6.0 ,tg
  REAL(KIND = 8) :: tavgLeftLimit,tavgRightLimit,prcp_anomaly_missing,prcp_anomaly_trace,tavg_anomaly_missing
  REAL(KIND = 8), ALLOCATABLE :: StudyPrcp(:),TempStudyPrcp(:),TempMonthStudyPrcp(:)     !�о���վ��Ľ�ˮ����
  REAL(KIND = 8), ALLOCATABLE :: FactorTavg(:),TempFactorTavg(:),TempMonthFactorTavg(:)  !Ԥ������վ��Ľ�ˮ����
  REAL(KIND = 8), ALLOCATABLE :: StationCodesUnique(:)                                   !GhcnTavg��Codes���
  REAL(KIND = 8), ALLOCATABLE :: RealArrayTemp2D(:,:)                                    !��ȡGhcnTavgվ�����ݵ���ʱ���ݿ�
  REAL(KIND = 8), ALLOCATABLE :: GhcnTavgStandardDB(:,:)                                 !GhcnTavg��Ÿ�ʽ��׼�����ݿ�
  REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:)                                 !GhcnPrcp��Ÿ�ʽ��׼�����ݿ�

  REAL(KIND = 8),ALLOCATABLE :: StudyStationCodes001(:)
  REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData_Predictable(:,:)
  REAL(KIND = 8), ALLOCATABLE :: ptor2k(:,:), ptor2b(:,:)
  REAL(KIND = 8) :: ptor1k, ptor1b, tempPtor2k, tempPtor2b
  REAL(KIND = 8) :: pstandID, pstor1ID, pstor1LM, pstor2ID, pstor2LM

  REAL(KIND = 8), ALLOCATABLE :: TempMonthFactorTavgModify(:),TempMonthStudyPrcpModify(:) !�޶�����pstor1ID������ͬ���ȵ�pstor2ID��pstandIDվ��Ľ�ˮ����
  REAL(KIND = 8), ALLOCATABLE :: tempFactorTavgMonthModify(:)
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcp(:), ptor1Tavg(:), ptor2Tavg(:)   !����Ԥ����վ�㡢��һԤ�����ӡ��ڶ�Ԥ�����ӽ���������,
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY1(:), ptandPrcpY1Residual(:)     !����������Իع�Ԥ����Ԥ����վ�㽵�������ݣ�ֻ������һԤ�����ӡ��ٴ�����µĲв�
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY2(:)                             !����������Իع�Ԥ����Ԥ�����в�ڶ�Ԥ���������һԤ������Ԥ���в�֮������Թ�ϵ
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY(:)                              !������һԤ�����Ӻ͵ڶ�Ԥ�����ӵ���Ԥ��������

  REAL(KIND = 8), ALLOCATABLE :: savePtror1Tavg(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror2Tavg(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand1Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand2Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror1TavgModify(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror2TavgModify(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand1PrcpModify(:)

  REAL :: RptandY, PptandY, Rtandtor1, R2tandtor1

  REAL, ALLOCATABLE :: P(:,:),R(:,:),TempR(:,:),TempP(:,:),R2Count(:,:)                               ! Ԥ��ÿһ��վ��ʱ��P\R��Ϣ
  REAL :: StartRate,EndRate
  REAL :: PPvalue                                       !������ˮƽ
  REAL :: TrainingRate                                  !ѵ������ռ�����ݵı���
  REAL :: maxR2, minR2, avgR2
  REAL :: R2Total
  TYPE( CLS_CMD_Progress ) :: Progress                  !Commands line process bar
  LOGICAL(4) :: istatus_dir_mk,alive                                !�ļ�����״̬
  NAMELIST /CSPRPBT/ prcp_anomaly_missing,prcp_anomaly_trace,tavg_anomaly_missing,&
    times,GhcnTavgColNum,MissVal,TraceVal,AheadMonthNum,StartMonth,&
    StartYear,EndYear,MonthNum,ClimateStatus,RankNum,PPvalue,TrainingRate,CoverYears,saveRankNum,leaveOut
  !****************************************************************************
  ! !                        Formatting
  !****************************************************************************

100 FORMAT(1X,A10,A4,A1,A2,A1,A2,A3,A8/)
200 FORMAT(I11,1X,A20,A10,F7.2,1X,F7.2,I5/)
300 FORMAT(I20,I20,I20)
400 FORMAT(I20,1320f10.1)
500 FORMAT(f20.0,1320f10.1)
700 FORMAT(f20.0,1320f10.1)
600 FORMAT(I15,24F8.4)
3000 FORMAT(f13.0,f3.0,f13.0,f3.0,3f6.2)

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
  PredicteAndCheckDataPath = TRIM(Path)//'\BaseOnTavg\PredicteAndCheck\'
  GhcnTavgFile = TRIM(Path)//'\GhcnData\ghcnm.tavg.v3.3.0.20170515.qcu.dat'
  StationInfoFile = TRIM(Path)//'\GhcnData\ghcnm.tavg.v3.3.0.20161009.qcu.inv'
  WorkSpace = TRIM(Path)//'\BaseOnTavg\CalculateStationPrcpRP\'
  PrcpWorkSpace = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\'
  GhcnPrcpStandardDBFile = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\GhcnPrcpStandardDB.dat'
  istatus_dir_ch = CHDIR(TRIM(WorkSpace)) !����Ĭ�ϵĹ���Ŀ¼
  ! read default namelist file which define default parameters
  OPEN(UNIT = FileID,FILE = './CalStationPrcpRP_BT_add_next.namelist')
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

  !***********************************************************************************************************
  ! ��ѯ��Ԥ��վ���б��ļ��Ƿ���ڣ��������ȡ
  !*****************************************************************************************
  ! �л�·�����������·��
  istatus = CHDIR(TRIM(PredicteAndCheckDataPath))

  INQUIRE(FILE = 'StudyCode_RankNum15_P.LT.0.01.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *,'>>StudyCode_RankNum15_P.LT.0.01�ļ����ڣ���ȡ�ļ�...'
    CALL Inqire_Text_Row('StudyCode_RankNum15_P.LT.0.01.dat',LEN('StudyCode_RankNum15_P.LT.0.01.dat'),tempNum)
    ALLOCATE(StudyStationCodes001(tempNum))
    OPEN(fileID,FILE = 'StudyCode_RankNum15_P.LT.0.01.dat')
    READ(fileID,'(f15.0)')StudyStationCodes001
    CLOSE(fileID)
    StudyStationNum001 = SIZE(StudyStationCodes001)
    PRINT *,'StudyStationNum: ',StudyStationNum001
    PRINT *,'>>��ȡStudyCode_RankNum15_P.LT.0.01�ļ����!'
  END IF
  !*****************************************************************************************
  ! get and save the information of station which can be predicted
  !*****************************************************************************************
  ALLOCATE(P_R_001RankData_Predictable(StudyStationNum001*MonthNum*RankNum,7))
  INQUIRE(FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *,'>>R_RankNum15_P.LT.0.01_Predicted�ļ����ڣ���ȡ�ļ�...'
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat')
    DO i = 1,StudyStationNum001*MonthNum*RankNum
      READ(fileID,3000)P_R_001RankData_Predictable(i,:)
    END DO
    CLOSE(fileID)
    PRINT *,'>>��ȡR_RankNum15_P.LT.0.01_Predicted�ļ���ɣ�'
  END IF

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         ��վ�����Ԥ������
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !�л���Ĭ�Ϲ���·��
  istatus_dir_ch = CHDIR(TRIM(WorkSpace))
  !��̬�������
  ALLOCATE(StudyPrcp(YearLen*MonthNum))
  ALLOCATE(FactorTavg(YearLen*MonthNum))
  ALLOCATE(R(ValidTavgStationNum*MonthNum,AheadMonthNum))      !��ValidTavgStationNum ���¶�վ����Ԥ��
  ALLOCATE(TempR(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(P(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(TempP(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(R2Count(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(ptor2k(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(ptor2b(ValidTavgStationNum*MonthNum,AheadMonthNum))
  !��ѯ�������Ŀ¼�Ƿ����
  INQUIRE(DIRECTORY = TRIM(WorkSpace)//'add_next_StationList', EXIST = alive)
  IF (alive == .false.) THEN
    istatus_dir_mk = makedirqq(TRIM('add_next_StationList'))
  END IF
  istatus_dir_ch = CHDIR(TRIM(WorkSpace)//TRIM('add_next_StationList\'))
  !����ǰ�Ƿ���Ҫ����ĳһ��վ�㣬�����ô˲��ԣ���ᱣ�����е�Ԥ��վ��R\R2\P��Ϣ��ռ�ô洢�����ռ�
  PRINT *,'�Ƿ���Ҫ����ĳһ������ĳһ��վ�㣿 y/n'
  READ (*,*),PressKey
  IF(TRIM(PressKey) == 'n') THEN
    StartStationNum = FLOOR(StudyStationNum001*StartRate)+1
    EndStationNum = FLOOR(StudyStationNum001*EndRate)
  ELSE
    PRINT *,' '
96  PRINT *,'Please input StartNum(as integer format ,StartNum should >= 1 and <= 20547):'
    PRINT *,' '
    READ (*,*) StartStationNum
    PRINT *,' '
    PRINT *,'Please input EndNum(as integer format ,EndNum should >= 1 and <= 20547):'
    PRINT *,' '
    READ (*,*) EndStationNum
    PRINT *,' '
  END IF
  !�������յ�Ԥ��վ����Ϣ��ȷ��Ԥ��վ����������Ϣ
  analysisStationNum = EndStationNum - StartStationNum + 1
  ALLOCATE(ValidStationCoupled(analysisStationNum,13))
  ValidStationCoupled = 0

  PRINT *,StudyStationNum001
  !����������վ����������
  WRITE(TotalAnalysisStationNumStr,'(I5)') StudyStationNum001
  PRINT *,analysisStationNum

  ! Ϊ�˽��쳣ֵ�ų����⣬�����Ԥ����ʱ���ǽ����еĽ�ˮ�쳣ֵ��Ϊ-9996���¶ȵ��쳣ֵ��Ϊ-9996
  WHERE (GhcnPrcpStandardDB == -9998)
    GhcnPrcpStandardDB = -9999
  END WHERE
  WHERE (GhcnTavgStandardDB == -9998)
    GhcnTavgStandardDB = -9999
  END WHERE

  !��ʼ�������н�����
  CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// size(ValidTavgStationCodesIndex)�Σ���ʾ����30
  Progress % M = "-" !// ����ɲ��ֵ��ַ������Ǳ���
  Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���

  DO i = StartStationNum,EndStationNum
    !��ȡ��Ԥ��վ�㼰����ǿ��ص�Ԥ������վ����Ϣ
    pstandID = P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+1,1)
    ValidStationCoupled(i - StartStationNum + 1,1) = pstandID
    R = DataNumNotEnough
    P = DataNumNotEnough
    ptor2k = DataNumNotEnough
    ptor2B = DataNumNotEnough
    WRITE(StationDirName,'(I11)') INT(pstandID,KIND=8)!StudyCode
    !��ѯվ��Ŀ¼�Ƿ����
    INQUIRE(DIRECTORY = TRIM(WorkSpace)//'add_next_StationList\'//TRIM(StationDirName), EXIST = alive)
    IF (alive == .false.) THEN
      istatus_dir_mk = MAKEDIRQQ(TRIM(StationDirName))
    END IF
    istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'add_next_StationList\'//TRIM(StationDirName))

    WRITE(StationNumStr,'(I5)') i
    !���������н�������// ǰ����ʾ���֣����Ǳ���
    Progress % Prefix = "StationNum:"//TRIM(StationNumStr)//TotalAnalysisStationNumStr
    !Ԥ����վ�㽵������
    !��Ҫ����ȷ��Ԥ����վ��������ʲôλ��
    DO tempNum = 1,ValidPrcpStationNum
      IF (GhcnPrcpStandardDB(tempNum,1) == pstandID) EXIT
    END DO
    StudyPrcp = GhcnPrcpStandardDB(tempNum,2:)

    !print *,pstandID,GhcnPrcpStandardDB(tempNum,1)
    !pause

    DO j = 1,ValidTavgStationNum
      pstor2ID = GhcnTavgStandardDB(j,1)
      FactorTavg = GhcnTavgStandardDB(j,2:)
      FactorTavgLen = MonthNum*YearLen

      !print *,"j",j

      DO k = 1,AheadMonthNum
        !print *,"k",k
        pstor2LM = REAL(k,KIND=8)
        ALLOCATE(TempFactorTavg(FactorTavgLen - k))
        ALLOCATE(TempStudyPrcp(FactorTavgLen - k))
        TempFactorTavg = FactorTavg(1:FactorTavgLen - k)
        TempStudyPrcp = StudyPrcp(k+1:)
        DO ii = 1,MonthNum
          !print *,"ii",ii

          CALL pTandMonthAtAheadMonthAndMonthRoll(k, ii, StartMonth, MonthNum, pTandMonth)
          !��ii�·ݶ�Ӧ����ǿԤ������վ����Ϣ
          pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,3)
          pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,4)
          R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,5)
          Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,6)
          IF (((pstor1ID == pstor2ID) .AND. ((pstor1LM == pstor2LM))) .OR. (Rtandtor1 < -1)) THEN
            !�հ��У�����һ������վ�����lead time��ڶ���������ͬʱִ�У����ߵ�һ�����ӵ����ϵ��Ϊ-9ʱִ��
          ELSE
            !print *,"enter"
            !��ȡ��pstandID��month��pstor1ID��pstor1LM��Ӧ��Ԥ������Ԥ������վ������
            !����Ϊȫ�ֱ���tempFactorTavgMonth��tempStudyPrcpMonth
            CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(pTandMonth,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,RankNum,&
              ValidPrcpStationNum,ValidTavgStationNum,StartMonth,&
              GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth
            !��ȡ��pstor2ID��Ӧ��Ԥ����վ���Ԥ������վ������

            ALLOCATE(TempMonthFactorTavg(SIZE(TempFactorTavg(ii:(FactorTavgLen - k):MonthNum))))
            ALLOCATE(TempMonthStudyPrcp(SIZE(TempStudyPrcp(ii:(FactorTavgLen - k):MonthNum))))
            TempMonthFactorTavg = TempFactorTavg(ii:(FactorTavgLen - k):MonthNum)
            TempMonthStudyPrcp = TempStudyPrcp(ii:(FactorTavgLen - k):MonthNum)

            !print *,"enter1"
            !����pstor1ID��pstor2ID��������޶�Ԥ������Ԥ������1��Ԥ������2�Ľ���������
            IF (SIZE(tempFactorTavgMonth)>SIZE(TempMonthFactorTavg)) THEN
              ALLOCATE(tempFactorTavgMonthModify(SIZE(TempMonthFactorTavg)))
              tempFactorTavgMonthModify = tempFactorTavgMonth(SIZE(tempFactorTavgMonth)-SIZE(TempMonthFactorTavg)+1:)
              !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
              ALLOCATE(TempMonthFactorTavgModify(SIZE(TempMonthFactorTavg)))
              TempMonthFactorTavgModify = TempMonthFactorTavg
              !��ʱԤ����վ��ʹ����pstor2ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(TempMonthStudyPrcp)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp
            ELSE IF(SIZE(tempFactorTavgMonth)<SIZE(TempMonthFactorTavg)) THEN
              ALLOCATE(TempMonthFactorTavgModify(SIZE(tempFactorTavgMonth)))
              TempMonthFactorTavgModify = TempMonthFactorTavg(SIZE(TempMonthFactorTavg)-SIZE(tempFactorTavgMonth)+1:)
              !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
              ALLOCATE(tempFactorTavgMonthModify(SIZE(tempFactorTavgMonth)))
              tempFactorTavgMonthModify = tempFactorTavgMonth
              !��ʱԤ����վ��ʹ����pstor2ID��Ӧ��Ԥ����������޶�ֵ����ʱ��Ҫ�޵�
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(tempFactorTavgMonth)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp(SIZE(TempMonthFactorTavg)-SIZE(tempFactorTavgMonth)+1:)
            ELSE
              !��ʱ��������Ҫ�޶�
              ALLOCATE(TempMonthFactorTavgModify(SIZE(TempMonthFactorTavg)))
              TempMonthFactorTavgModify = TempMonthFactorTavg
              !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
              ALLOCATE(tempFactorTavgMonthModify(SIZE(tempFactorTavgMonth)))
              tempFactorTavgMonthModify = tempFactorTavgMonth
              !��ʱԤ����վ��ʹ����pstor2ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(TempMonthFactorTavg)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp
            END IF

            !print *,"enter2"

            IF((isContinuityGT_M(TempMonthStudyPrcpModify,ClimateStatus,prcp_anomaly_missing) .EQ. .false.) .and.&
              (isContinuityGT_M(TempMonthStudyPrcpModify,ClimateStatus,prcp_anomaly_trace) .EQ. .false.) .and.&
              (isContinuityGT_M(TempMonthFactorTavgModify,ClimateStatus,tavg_anomaly_missing) .EQ. .false.) .and. &
              (isContinuityGT_M(tempFactorTavgMonthModify,ClimateStatus,tavg_anomaly_missing) .EQ. .false.)) THEN

              !print *,"enter-enter"

              tempCount = COUNT((TempMonthFactorTavgModify > -9998) .AND. &
                (tempFactorTavgMonthModify > -9998) .AND. &
                (TempMonthStudyPrcpModify >= 0) )

              ALLOCATE(CodesIndexLocation(tempCount))
              tempNum = 0
              DO jj = 1,SIZE(TempMonthFactorTavgModify)
                IF( (TempMonthFactorTavgModify(jj) > -9998) .AND. &
                  (tempFactorTavgMonthModify(jj) > -9998) .AND. &
                  (TempMonthStudyPrcpModify(jj) >= 0)  ) THEN
                  tempNum = tempNum + 1
                  CodesIndexLocation(tempNum) = jj
                END IF
              END DO
              !print *,"enter-enter1"
              !print *,'tempCount',tempCount
              IF(tempCount >= CoverYears) THEN
                !print *,"enter-enter-enter"
                !ͳ��վ���Ƿ������Լ�¼�����ڴ˴�ֻ��Ϊ��ͳ���Ƿ���ڼ�¼
                ValidStationCoupled(i - StartStationNum + 1 ,pTandMonth+1) = 1

                IF (leaveOut < 0) THEN
                  trainLen = FLOOR(tempCount*TrainingRate)
                ELSE
                  trainLen = tempCount - leaveOut
                END IF
                ALLOCATE(ptandPrcp(trainLen))
                ALLOCATE(ptor1Tavg(trainLen))
                ALLOCATE(ptor2Tavg(trainLen))
                !print *,"enter-enter-enter-allocate"
                ptandPrcp = TempMonthStudyPrcpModify(CodesIndexLocation(1:trainLen))
                !print *,"enter-enter-enter-allocate1"
                ptor1Tavg = tempFactorTavgMonthModify(CodesIndexLocation(1:trainLen))
                !print *,"enter-enter-enter-allocate2"
                ptor2Tavg = TempMonthFactorTavgModify(CodesIndexLocation(1:trainLen))
                !print *,"enter-enter-enter-allocate3"
                !print *,"enter-enter-enter1"
                !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
                CALL LinearRegression(ptor1Tavg,ptandPrcp,trainLen,ptor1k,ptor1b)
                ALLOCATE(ptandPrcpY1(trainLen))
                ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
                !����Ԥ�������һԤ�����ӵġ��в
                ALLOCATE(ptandPrcpY1Residual(trainLen))
                ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
                !���Իع����Ԥ�������һԤ�����ӵġ��в�͵ڶ�Ԥ������֮������Թ�ϵ
                CALL LinearRegression(ptor2Tavg,ptandPrcpY1Residual,trainLen,tempPtor2k, tempPtor2b)
                ptor2k(j+(pTandMonth-1)*ValidTavgStationNum,k) = tempPtor2k
                ptor2b(j+(pTandMonth-1)*ValidTavgStationNum,k) = tempPtor2b

                !print *,"enter-enter-enter2"

                ALLOCATE(ptandPrcpY2(trainLen))
                ptandPrcpY2 = ptor2Tavg*tempPtor2k + tempPtor2b
                !������Ԥ��������
                ALLOCATE(ptandPrcpY(trainLen))
                ptandPrcpY = ptandPrcpY1+ptandPrcpY2
                !����Ԥ��������Y < 0ʱ��ֱ����Ϊ0����
                WHERE (ptandPrcpY < 0)
                  ptandPrcpY = 0
                END WHERE
                !����Ԥ����վ����ڶ������ӵ����ϵ��
                CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)
                R(j+(pTandMonth-1)*ValidTavgStationNum,k) = RptandY
                !����������ˮƽ
                CALL Pvalue(trainLen, RptandY, PptandY)
                P(j+(pTandMonth-1)*ValidTavgStationNum,k) = PptandY
                !pause
                !print *,"enter-enter-enter3"

                DEALLOCATE(ptandPrcp)
                DEALLOCATE(ptor1Tavg)
                DEALLOCATE(ptor2Tavg)
                DEALLOCATE(ptandPrcpY)
                DEALLOCATE(ptandPrcpY1)
                DEALLOCATE(ptandPrcpY2)
                DEALLOCATE(ptandPrcpY1Residual)

              END IF


              DEALLOCATE(CodesIndexLocation)
            END IF

            DEALLOCATE(TempMonthFactorTavg)
            DEALLOCATE(TempMonthStudyPrcp)
            DEALLOCATE(tempFactorTavgMonth)
            DEALLOCATE(tempStudyPrcpMonth)
            DEALLOCATE(tempFactorTavgMonthModify)
            DEALLOCATE(TempMonthFactorTavgModify)
            DEALLOCATE(TempMonthStudyPrcpModify)
            !print *,"del vars finished"
            !pause
          END IF
        END DO
        DEALLOCATE(TempFactorTavg)
        DEALLOCATE(TempStudyPrcp)
      END DO

      !print *,j

    END DO

    ALLOCATE(CodesIndexLocation(2))

    IF(TRIM(PressKey) == 'y') THEN
      open(fileID,FILE = 'R.txt',IOSTAT = iosval)
      do ii = 1,MonthNum * ValidTavgStationNum
        if(mod(ii,ValidTavgStationNum) /= 0) then
          write(fileID, '(I15,24F6.2)' ) ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)), R(ii,:)  !ValidTavgStationCodesIndex(jj),
        else
          write(fileID, '(I15,24F6.2)' ) ValidTavgStationCodesIndex(ValidTavgStationNum), R(ii,:)  !ValidTavgStationCodesIndex(jj),
        endif
      end do
      close(fileID)
      open(fileID,FILE = 'P.txt',IOSTAT = iosval)
      do ii = 1,MonthNum*ValidTavgStationNum
        if(mod(ii,ValidTavgStationNum) /= 0) then
          write(fileID, '(I15,24F6.2)') ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)),P(ii, :)  !ValidTavgStationCodesIndex(jj),
        else
          write(fileID, '(I15,24F6.2)') ValidTavgStationCodesIndex(ValidTavgStationNum),P(ii, :)  !ValidTavgStationCodesIndex(jj),
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


    ! ����ǰ15��,P<0.1

    !print *,"����P<0.1���"

    PPvalue = 0.1
    TempR = R**2
    !TempP = P
    WHERE(P >= PPvalue)   !ɾ������ˮƽ����0.1��ֵ
      TempR = 0
    END WHERE

    WHERE(P == DataNumNotEnough)   !R .EQ. NaN
      TempR = DataNumNotEnough
    END WHERE
    !WHERE(TempR == R_Inf)   !R .EQ. Inf
    !  TempR = 0
    !END WHERE
    !WHERE(TempP == P_Inf)   !R .EQ. Infinity
    !  TempR = 0
    !END WHERE
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.1.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum
      !print *,"��ȡ��һ���ӵ������Ϣ"
      !��ii�·ݶ�Ӧ����ǿԤ������վ����Ϣ
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"��ȡ���һ�����Ӷ�Ӧ��Prcp����"

      CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,RankNum,&
        ValidPrcpStationNum,ValidTavgStationNum,StartMonth,&
        GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth

      ALLOCATE(savePtror1Tavg(size(tempFactorTavgMonth)))
      savePtror1Tavg = tempFactorTavgMonth
      ALLOCATE(savePtand1Prcp(size(tempStudyPrcpMonth)))
      savePtand1Prcp = tempStudyPrcpMonth
      DEALLOCATE(tempFactorTavgMonth)
      DEALLOCATE(tempStudyPrcpMonth)

      !print *,savePtand1Prcp
      !pause
      !print *,savePtror1Tavg
      !pause

      DO jj = 1,saveRankNum
        maxR2 = MAXVAL(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
        CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
        !print *,Rtandtor1,maxR2
        !pause
        IF (Rtandtor1 < -1) THEN
          !print *,"1"
          !pause
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,DataNumNotEnough,DataNumNotEnough, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
        ELSE IF (maxR2 <= 0) THEN
          !print *,"2"
          !pause
          !print *,"����Prcp����"
          !����pstor1ID��pstor2ID��������޶�Ԥ������Ԥ������1��Ԥ������2�Ľ���������
          !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
          ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
          savePtror1TavgModify = savePtror1Tavg
          !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Tavg)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1TavgModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtand1PrcpModify>=0 .AND. savePtror1TavgModify>= -9998)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror1TavgModify)
            IF(savePtand1PrcpModify(kk)>=0 .AND. savePtror1TavgModify(kk)>= -9998) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"
          saveTrainLen = FLOOR(tempCount*1.0)
          !����trainLen�ĳ���
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          ! ����ptor1Tavg
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Tavg(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Tavg = savePtror1TavgModify(CIL(1:saveTrainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Tavg(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Tavg,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"

          !������Ԥ��������
          ALLOCATE(ptandPrcpY(saveTrainLen))
           ptandPrcpY1Residual = -9999
          ptandPrcpY = ptandPrcpY1
          !����Ԥ��������Y < 0ʱ��ֱ����Ϊ0����
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)

          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "�����ļ�"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))

          IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"�������"

          !����Ԥ�����
          !ptandPrcp
          !ptor1Tavg
          !ptor2Tavg
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          !print *,"ת���ļ�����"
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          !print *,"ת���ļ��������"

          !print *,'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat'
          !pause

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.1.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Tavg(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY1(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror1TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        ELSE
          !print *,"3"
          !pause
          !print *,"��ȡ��ڶ���������ص����ݣ���",jj,"��"
          CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),REAL(ValidTavgStationCodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8), YearLen,MonthNum,RankNum,&
            ValidPrcpStationNum,ValidTavgStationNum,StartMonth,&
            GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth

          ALLOCATE(savePtror2Tavg(size(tempFactorTavgMonth)))
          savePtror2Tavg = tempFactorTavgMonth
          ALLOCATE(savePtand2Prcp(size(tempStudyPrcpMonth)))
          savePtand2Prcp = tempStudyPrcpMonth

          DEALLOCATE(tempFactorTavgMonth)
          DEALLOCATE(tempStudyPrcpMonth)

          !print *,savePtand2Prcp
          !pause
          !print *,savePtror2Tavg
          !pause

          !print *,"����Prcp����"
          !����pstor1ID��pstor2ID��������޶�Ԥ������Ԥ������1��Ԥ������2�Ľ���������
          IF (SIZE(savePtror1Tavg)>SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror2Tavg)))
            savePtror1TavgModify = savePtror1Tavg(SIZE(savePtror1Tavg)-SIZE(savePtror2Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ��Ҫ�޵�
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Tavg)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Tavg)-SIZE(savePtror2Tavg)+1:)
          ELSE IF(SIZE(savePtror1Tavg)<SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror1Tavg)))
            savePtror2TavgModify = savePtror2Tavg(SIZE(savePtror2Tavg)-SIZE(savePtror1Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
            savePtror1TavgModify = savePtror1Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Tavg)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !��ʱ��������Ҫ�޶�
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
            savePtror1TavgModify = savePtror1Tavg
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1TavgModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtror2TavgModify>=-9998 .AND. savePtand1PrcpModify>=0 .AND. savePtror1TavgModify>= -9998)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2TavgModify)
            IF(savePtror2TavgModify(kk)>=-9998 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1TavgModify(kk)>=-9998) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"
          saveTrainLen = FLOOR(tempCount*1.0)
          !����trainLen�ĳ���
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Tavg(saveTrainLen))
          ALLOCATE(ptor2Tavg(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Tavg = savePtror1TavgModify(CIL(1:saveTrainLen))
          ptor2Tavg = savePtror2TavgModify(CIL(1:saveTrainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Tavg(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Tavg,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"
          !���Իع����Ԥ�������һԤ�����ӵġ��в�͵ڶ�Ԥ������֮������Թ�ϵ
          CALL LinearRegression(ptor2Tavg(1:trainLen),ptandPrcpY1Residual(1:trainLen),trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(saveTrainLen))
          ptandPrcpY2 = -9999
          ptandPrcpY2 = ptor2Tavg*tempPtor2k + tempPtor2b
          !������Ԥ��������
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !����Ԥ��������Y < 0ʱ��ֱ����Ϊ0����
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "�����ļ�"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2), tempPtor2k, tempPtor2b, maxR2,& !RptandY*RptandY, RptandY,PptandY,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))

          IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"�������"

          !����Ԥ�����
          !ptandPrcp
          !ptor1Tavg
          !ptor2Tavg
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          !print *,"ת���ļ�����"
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          !print *,"ת���ļ��������"

          !print *,'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat'
          !pause

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.1.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Tavg(kk), &
              ptor2Tavg(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror2Tavg)

          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1TavgModify)
          DEALLOCATE(savePtror2TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Tavg)
          DEALLOCATE(ptor2Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY2)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        END IF

      END DO
      DEALLOCATE(savePtror1Tavg)
      DEALLOCATE(savePtand1Prcp)
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
    !WHERE(TempR == R_Inf)   !
    !  TempR = 0
    !END WHERE
    !WHERE(TempP == P_Inf)   !
    !  TempR = 0
    !END WHERE
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.05.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum

      !print *,"��ȡ��һ���ӵ������Ϣ"
      !��ii�·ݶ�Ӧ����ǿԤ������վ����Ϣ
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"��ȡ���һ�����Ӷ�Ӧ��Prcp����"
      CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,RankNum,&
        ValidPrcpStationNum,ValidTavgStationNum,StartMonth,&
        GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth
      ALLOCATE(savePtror1Tavg(size(tempFactorTavgMonth)))
      savePtror1Tavg = tempFactorTavgMonth
      ALLOCATE(savePtand1Prcp(size(tempStudyPrcpMonth)))
      savePtand1Prcp = tempStudyPrcpMonth
      DEALLOCATE(tempFactorTavgMonth)
      DEALLOCATE(tempStudyPrcpMonth)

      !print *,savePtand1Prcp
      !pause
      !print *,savePtror1Tavg
      !pause

      DO jj = 1,saveRankNum
        maxR2 = MAXVAL(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
        CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
        IF (Rtandtor1 < -1) THEN
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,DataNumNotEnough,DataNumNotEnough, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
        ELSE IF (maxR2 <= 0) THEN

          !print *,"����Prcp����"
          !����pstor1ID��pstor2ID��������޶�Ԥ������Ԥ������1��Ԥ������2�Ľ���������
          !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
          ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
          savePtror1TavgModify = savePtror1Tavg
          !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Tavg)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1TavgModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtand1PrcpModify>=0 .AND. savePtror1TavgModify>= -9998)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror1TavgModify)
            IF(savePtand1PrcpModify(kk)>=0 .AND. savePtror1TavgModify(kk)>= -9998) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

           !print *,"���µ���ptand��ptor1��ptor2����"
          saveTrainLen = FLOOR(tempCount*1.0)
          !����trainLen�ĳ���
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          ! ����ptor1Tavg
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Tavg(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Tavg = savePtror1TavgModify(CIL(1:saveTrainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Tavg(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Tavg,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"

          !������Ԥ��������
          ALLOCATE(ptandPrcpY(saveTrainLen))
           ptandPrcpY1Residual = -9999
          ptandPrcpY = ptandPrcpY1
          !����Ԥ��������Y < 0ʱ��ֱ����Ϊ0����
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)

          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "�����ļ�"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))

          IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"�������"

          !����Ԥ�����
          !ptandPrcp
          !ptor1Tavg
          !ptor2Tavg
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          !print *,"ת���ļ�����"
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          !print *,"ת���ļ��������"

          !print *,'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat'
          !pause

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.05.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Tavg(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY1(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)


          DEALLOCATE(savePtror1TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        ELSE
          !print *,"��ȡ��ڶ���������ص����ݣ���",jj,"��"
          CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),REAL(ValidTavgStationCodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8), YearLen,MonthNum,RankNum,&
            ValidPrcpStationNum,ValidTavgStationNum,StartMonth,&
            GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth
          ALLOCATE(savePtror2Tavg(size(tempFactorTavgMonth)))
          savePtror2Tavg = tempFactorTavgMonth
          ALLOCATE(savePtand2Prcp(size(tempStudyPrcpMonth)))
          savePtand2Prcp = tempStudyPrcpMonth

          DEALLOCATE(tempFactorTavgMonth)
          DEALLOCATE(tempStudyPrcpMonth)

          !print *,savePtand2Prcp
          !pause
          !print *,savePtror2Tavg
          !pause

          !print *,"����Prcp����"
          !����pstor1ID��pstor2ID��������޶�Ԥ������Ԥ������1��Ԥ������2�Ľ���������
          IF (SIZE(savePtror1Tavg)>SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror2Tavg)))
            savePtror1TavgModify = savePtror1Tavg(SIZE(savePtror1Tavg)-SIZE(savePtror2Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ��Ҫ�޵�
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Tavg)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Tavg)-SIZE(savePtror2Tavg)+1:)
          ELSE IF(SIZE(savePtror1Tavg)<SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror1Tavg)))
            savePtror2TavgModify = savePtror2Tavg(SIZE(savePtror2Tavg)-SIZE(savePtror1Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
            savePtror1TavgModify = savePtror1Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Tavg)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !��ʱ��������Ҫ�޶�
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
            savePtror1TavgModify = savePtror1Tavg
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1TavgModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtror2TavgModify>=-9998 .AND. savePtand1PrcpModify>=0 .AND. savePtror1TavgModify>= -9998)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2TavgModify)
            IF(savePtror2TavgModify(kk)>=-9998 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1TavgModify(kk)>=-9998) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"
          saveTrainLen = FLOOR(tempCount*1.0)
          !����trainLen�ĳ���
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Tavg(saveTrainLen))
          ALLOCATE(ptor2Tavg(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Tavg = savePtror1TavgModify(CIL(1:saveTrainLen))
          ptor2Tavg = savePtror2TavgModify(CIL(1:saveTrainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Tavg(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Tavg,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"
          !���Իع����Ԥ�������һԤ�����ӵġ��в�͵ڶ�Ԥ������֮������Թ�ϵ
          CALL LinearRegression(ptor2Tavg(1:trainLen),ptandPrcpY1Residual(1:trainLen),trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(saveTrainLen))
          ptandPrcpY2 = -9999
          ptandPrcpY2 = ptor2Tavg*tempPtor2k + tempPtor2b
          !������Ԥ��������
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !����Ԥ��������Y < 0ʱ��ֱ����Ϊ0����
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "�����ļ�"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2), tempPtor2k, tempPtor2b, maxR2,& !RptandY*RptandY, RptandY,PptandY,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))

          IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
          END IF

          !����Ԥ�����
          !ptandPrcp
          !ptor1Tavg
          !ptor2Tavg
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.05.dat')
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Tavg(kk), &
              ptor2Tavg(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)



          DEALLOCATE(savePtror2Tavg)

          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1TavgModify)
          DEALLOCATE(savePtror2TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Tavg)
          DEALLOCATE(ptor2Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY2)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        END IF
      END DO
      DEALLOCATE(savePtror1Tavg)
      DEALLOCATE(savePtand1Prcp)
    END DO

    CLOSE(fileID)


    ! ����ǰ15��,P<0.01
    PPvalue = 0.01
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
    !WHERE(TempP == P_Inf)   !R .EQ. Infinity
    !  TempR = 0
    !END WHERE
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.01.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum

      !print *,"��ȡ��һ���ӵ������Ϣ"
      !��ii�·ݶ�Ӧ����ǿԤ������վ����Ϣ
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"��ȡ���һ�����Ӷ�Ӧ��Prcp����"
      CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,RankNum,&
        ValidPrcpStationNum,ValidTavgStationNum,StartMonth,&
        GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth
      ALLOCATE(savePtror1Tavg(size(tempFactorTavgMonth)))
      savePtror1Tavg = tempFactorTavgMonth
      ALLOCATE(savePtand1Prcp(size(tempStudyPrcpMonth)))
      savePtand1Prcp = tempStudyPrcpMonth
      DEALLOCATE(tempFactorTavgMonth)
      DEALLOCATE(tempStudyPrcpMonth)

      !print *,savePtand1Prcp
      !pause
      !print *,savePtror1Tavg
      !pause

      DO jj = 1,saveRankNum
        maxR2 = MAXVAL(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
        CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidTavgStationNum:ii*ValidTavgStationNum,:))
        !print *,CodesIndexLocation
        !pause

        IF (Rtandtor1 < -1) THEN
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,DataNumNotEnough,DataNumNotEnough, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            maxR2,& !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
        ELSE IF (maxR2 <= 0) THEN

          !print *,"����Prcp����"
          !����pstor1ID��pstor2ID��������޶�Ԥ������Ԥ������1��Ԥ������2�Ľ���������
          !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
          ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
          savePtror1TavgModify = savePtror1Tavg
          !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Tavg)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1TavgModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtand1PrcpModify>=0 .AND. savePtror1TavgModify>= -9998)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror1TavgModify)
            IF(savePtand1PrcpModify(kk)>=0 .AND. savePtror1TavgModify(kk)>= -9998) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

           !print *,"���µ���ptand��ptor1��ptor2����"
          saveTrainLen = FLOOR(tempCount*1.0)
          !����trainLen�ĳ���
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          ! ����ptor1Tavg
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Tavg(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Tavg = savePtror1TavgModify(CIL(1:saveTrainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Tavg(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Tavg,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"

          !������Ԥ��������
          ALLOCATE(ptandPrcpY(saveTrainLen))
           ptandPrcpY1Residual = -9999
          ptandPrcpY = ptandPrcpY1
          !����Ԥ��������Y < 0ʱ��ֱ����Ϊ0����
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)

          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "�����ļ�"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))

          IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"�������"

          !����Ԥ�����
          !ptandPrcp
          !ptor1Tavg
          !ptor2Tavg
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          !print *,"ת���ļ�����"
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          !print *,"ת���ļ��������"

          !print *,'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat'
          !pause

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Tavg(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY1(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror1TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        ELSE

          !print *,REAL(ValidTavgStationCodesIndex(CodesIndexLocation(1)),KIND=8),REAL(CodesIndexLocation(2),KIND=8)
          !pause

          !print *,"��ȡ��ڶ���������ص����ݣ���",jj,"��"
          CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),REAL(ValidTavgStationCodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8), YearLen,MonthNum,RankNum,&
            ValidPrcpStationNum,ValidTavgStationNum,StartMonth,&
            GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth

          ALLOCATE(savePtror2Tavg(size(tempFactorTavgMonth)))
          savePtror2Tavg = tempFactorTavgMonth
          ALLOCATE(savePtand2Prcp(size(tempStudyPrcpMonth)))
          savePtand2Prcp = tempStudyPrcpMonth

          DEALLOCATE(tempFactorTavgMonth)
          DEALLOCATE(tempStudyPrcpMonth)

          !print *,savePtror1Tavg, savePtand1Prcp, savePtror2Tavg, savePtand2Prcp
          !pause
          !print *,savePtand2Prcp
          !pause
          !print *,savePtror2Tavg
          !pause

          !print *,"����Prcp����"
          !����pstor1ID��pstor2ID��������޶�Ԥ������Ԥ������1��Ԥ������2�Ľ���������
          IF (SIZE(savePtror1Tavg)>SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror2Tavg)))
            savePtror1TavgModify = savePtror1Tavg(SIZE(savePtror1Tavg)-SIZE(savePtror2Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ��Ҫ�޵�
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Tavg)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Tavg)-SIZE(savePtror2Tavg)+1:)
          ELSE IF(SIZE(savePtror1Tavg)<SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror1Tavg)))
            savePtror2TavgModify = savePtror2Tavg(SIZE(savePtror2Tavg)-SIZE(savePtror1Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
            savePtror1TavgModify = savePtror1Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Tavg)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !��ʱ��������Ҫ�޶�
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
            savePtror1TavgModify = savePtror1Tavg
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1TavgModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtror2TavgModify>=-9998 .AND. savePtand1PrcpModify>=0 .AND. savePtror1TavgModify>= -9998)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2TavgModify)
            IF(savePtror2TavgModify(kk)>=-9998 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1TavgModify(kk)>=-9998) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"
          saveTrainLen = FLOOR(tempCount*1.0)
          !����trainLen�ĳ���
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Tavg(saveTrainLen))
          ALLOCATE(ptor2Tavg(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Tavg = savePtror1TavgModify(CIL(1:saveTrainLen))
          ptor2Tavg = savePtror2TavgModify(CIL(1:saveTrainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Tavg(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Tavg,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"
          !���Իع����Ԥ�������һԤ�����ӵġ��в�͵ڶ�Ԥ������֮������Թ�ϵ
          CALL LinearRegression(ptor2Tavg(1:trainLen),ptandPrcpY1Residual(1:trainLen),trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(saveTrainLen))
          ptandPrcpY2 = -9999
          ptandPrcpY2 = ptor2Tavg*tempPtor2k + tempPtor2b
          !������Ԥ��������
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !����Ԥ��������Y < 0ʱ��ֱ����Ϊ0����
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "�����ļ�"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2), tempPtor2k, tempPtor2b, maxR2,&  ! RptandY*RptandY, RptandY,PptandY,
            R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))

          !print *,"k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "r:",R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))

          !print *,tempPtor2k,tempPtor2b,RptandY

          !pause

          IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)) = 0
          END IF

          !����Ԥ�����
          !ptandPrcp
          !ptor1Tavg
          !ptor2Tavg
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat')
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Tavg(kk), &
              ptor2Tavg(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror2Tavg)
          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1TavgModify)
          DEALLOCATE(savePtror2TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Tavg)
          DEALLOCATE(ptor2Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY2)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        END IF
      END DO

      DEALLOCATE(savePtror1Tavg)
      DEALLOCATE(savePtand1Prcp)

    END DO

    CLOSE(fileID)



    DEALLOCATE(CodesIndexLocation)
    istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'add_next_StationList\')

    CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ

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
8089 PRINT *,'�ļ���ȡ����'

  END SUBROUTINE CalStationPrcpRP_BT_add_next



