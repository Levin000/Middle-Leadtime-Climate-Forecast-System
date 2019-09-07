  !****************************************************************************
  !
  !  PROGRAM: CalStationPrcpRP
  !
  !  PURPOSE:  Entry point for the console application.
  !  This subroutine coded for calculate partial correlation of each predicted
  !  station using all predictor stations, controlling by the strongest correlation predictor station.
  !
  !****************************************************************************

  SUBROUTINE CalStationPrcpRP_BP_add_next(StartRate,EndRate)
  !!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"CalStationPrcpRP_BP" :: CalStationPrcpRP_BP
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
  CHARACTER(LEN = 10) :: DateT                                    !��¼����
  CHARACTER(LEN = 8) :: NowTime                                   !��¼ʱ��
  CHARACTER(LEN = 500) :: Path                                    !�����ļ�exe·��       , Name
  CHARACTER(LEN = 500) :: GhcnPrcpFile                            !Prcp�ļ����Ե�ַ
  CHARACTER(LEN = 500) :: StationInfoFile                         !station��Ϣ�ļ�
  CHARACTER(LEN = 11) :: StationDirName                           !ÿһ��Station���ļ�����ʱ��
  CHARACTER(LEN = 5)  :: StationNumStr,TotalAnalysisStationNumStr ! ����������ʾ��վ����
  CHARACTER(LEN = 500) :: PrcpWorkSpace, TavgWorkSpace
  CHARACTER(LEN = 100) :: dirname
  CHARACTER(LEN = 5) :: PressKey
  CHARACTER(LEN = 5) :: StartStationStr,EndStationStr
  CHARACTER(LEN = 5) :: monthStr,RankNumStr

  CHARACTER(LEN = 500) :: PredicteAndCheckDataPath  !�������ݿ�

  INTEGER :: times                                                !
  INTEGER :: GhcnPrcpColNum                                       !GHCNPrcp��������
  INTEGER :: MissVal                                              !ȱʡֵ������1Ϊ������0Ϊ��������
  INTEGER :: TraceVal                                             !�ۼ���ˮ�Ĵ�����1Ϊ������0Ϊ��������
  INTEGER :: AheadMonthNum                                        !�����ǰԤ�����·���
  INTEGER :: StartMonth                                           !�������ݼ�¼��GhcnPrcp����ʼ���·�
  INTEGER :: MonthNum                                             !һ����·���
  INTEGER :: pTandMonth                                           !Ԥ����վ��ĵ�ǰ�·ݣ���AheadMonth��MonthRoll�����£���
  INTEGER :: StartYear,EndYear,YearLen                            !Ԥ���о��Ŀ�ʼ��ݡ�������ݡ�ʱ�䳤��
  INTEGER :: RankNum                                              !վ����Ч����
  INTEGER :: saveRankNum                                          !��������ʱ��վ����Ч��������ֻ����ǰsaveRankNum��վ�����Ϣ
  INTEGER :: II,JJ,KK                                             !���ʹ�õı���
  INTEGER :: N                                                   !���ʱ�Ա�������ߴη���
  INTEGER :: CoverYears                                           !���ñ���վ���Ԥ������վ�㽵ˮ��ֵ�����ܳ�Couple����������
  INTEGER :: StationInfoFileLen                                   ! GHCN Station��Ϣ������
  INTEGER :: StationNum                                           ! GhcnPrcp������վ������
  INTEGER :: GhcnPrcpRowNum                                       ! GHCNPrcp���ݵ�����
  INTEGER :: ValidStationNum                                      !GhcnPrcp���ݿ�����ʼ��ֹ����StartYear-EndYear֮���վ����
  INTEGER :: ValidTavgStationNum                                  !GhcnTavg���ݿ�����ʼ��ֹ����StartYear-EndYear֮���վ����
  INTEGER :: fileID = 10, iosval                                  !�ļ�ID���ļ�����״̬
  INTEGER :: savePrcpFileID = 200
  INTEGER :: istatus,istatus_dir_ch                               ! �ı䵱ǰ����Ŀ¼�ɹ�����״̬���½��ļ��гɹ�����״̬
  INTEGER :: tempNum ,tempCount,ColStart                            !��ʱ����
  INTEGER :: FactorTavgLen                                        !Ԥ������StartYear->EndYear�ڼ����ݳ���
  INTEGER :: i,j,k,ic,im,StartStationNum,EndStationNum,analysisStationNum
  INTEGER :: ClimateStatus
  INTEGER :: R2CountTotal                                          !��վ�㵥�·���ЧR2��������

  INTEGER :: trainLen

  INTEGER(KIND = 8)  StudyCode
  INTEGER(KIND = 8), ALLOCATABLE :: ValidPrcpStationCodesIndex(:)                 !��ȡ���о���վ����
  INTEGER(KIND = 8), ALLOCATABLE :: ValidTavgStationCodesIndex(:)
  INTEGER(KIND = 8), ALLOCATABLE :: CodesIndexLocation(:)         !�洢ĳһ��վ�������ݿ��е�����ֵ(��λ��)
  INTEGER(KIND = 8), ALLOCATABLE :: CIL(:)                           !����ͬCodesIndexLocation
  INTEGER(KIND = 8), ALLOCATABLE :: GhcnPrcpYear(:,:)             !GhcnPrcpվ�����ݵĿ�ʼ��ݺͽ��������Ϣ
  INTEGER(KIND = 8), ALLOCATABLE :: IntegerArrayTemp2D(:,:)       ! 2D���ͱ�����ʱ�洢
  INTEGER(KIND = 8), ALLOCATABLE :: ValidStationCoupled(:,:)      !ͨ������������վ�㣬��ͨ�������1ͨ����0��ͨ��

  INTEGER :: StudyStationNum001

  REAL(KIND = 8) :: DataNumNotEnough = -9.0    !, R_Inf = -5.0, P_Inf = -6.0   ���ڴ��ڸ����
  REAL(KIND = 8) :: prcpRightLimit,prcp_anomaly_missing,prcp_anomaly_trace, tavg_anomaly_missing
  REAL(KIND = 8), ALLOCATABLE :: StudyPrcp(:),TempStudyPrcp(:),TempMonthStudyPrcp(:)    !�о���վ��Ľ�ˮ����
  REAL(KIND = 8), ALLOCATABLE :: FactorTavg(:),TempFactorTavg(:),TempMonthFactorTavg(:) !Ԥ������վ��Ľ�ˮ����

  REAL(KIND = 8), ALLOCATABLE :: TempMonthFactorTavgModify(:),TempMonthStudyPrcpModify(:) !�޶�����pstor1ID������ͬ���ȵ�pstor2ID��pstandIDվ��Ľ�ˮ����
  REAL(KIND = 8), ALLOCATABLE :: tempFactorPrcpMonthModify(:)
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcp(:), ptor1Prcp(:), ptor2Tavg(:)   !����Ԥ����վ�㡢��һԤ�����ӡ��ڶ�Ԥ�����ӽ���������,
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY1(:), ptandPrcpY1Residual(:)     !����������Իع�Ԥ����Ԥ����վ�㽵�������ݣ�ֻ������һԤ�����ӡ��ٴ�����µĲв�
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY2(:)                             !����������Իع�Ԥ����Ԥ�����в�ڶ�Ԥ���������һԤ������Ԥ���в�֮������Թ�ϵ
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY(:)                              !������һԤ�����Ӻ͵ڶ�Ԥ�����ӵ���Ԥ��������
  REAL(KIND = 8), ALLOCATABLE :: savePtror1Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror2Tavg(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand1Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand2Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror1PrcpModify(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror2TavgModify(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand1PrcpModify(:)

  REAL(KIND = 8), ALLOCATABLE :: StationCodesUnique(:)            !GhcnPrcp��Codes���
  REAL(KIND = 8), ALLOCATABLE :: RealArrayTemp2D(:,:)             !��ȡGhcnPrcpվ�����ݵ���ʱ���ݿ�
  REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:)          !GhcnPrcp��Ÿ�ʽ��׼�����ݿ�
  REAL(KIND = 8), ALLOCATABLE :: GhcnTavgStandardDB(:,:)
  REAL, ALLOCATABLE :: P(:,:),R(:,:),TempR(:,:),TempP(:,:),R2Count(:,:)        ! Ԥ��ÿһ��վ��ʱ��P\R��Ϣ

  REAL(KIND = 8), ALLOCATABLE :: ptor2k(:,:), ptor2b(:,:)
  REAL(KIND = 8) :: ptor1k, ptor1b, tempPtor2k, tempPtor2b

  REAL :: StartRate,EndRate
  REAL :: PPvalue                     !������ˮƽ
  REAL :: TrainingRate                !ѵ������ռ�����ݵı���
  REAL :: maxR2, minR2,avgR2
  REAL :: R2Total                                          !��վ�㵥�·���ЧR2���ܺ�

  REAL(KIND = 8),ALLOCATABLE :: StudyStationCodes001(:)
  REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData_Predictable(:,:)
  REAL(KIND = 8) :: pstandID, pstor1ID, pstor1LM, pstor2ID, pstor2LM
  REAL :: RptandY, PptandY, Rtandtor1, R2tandtor1

  TYPE( CLS_CMD_Progress ) ::Progress  !������
  LOGICAL(4) :: istatus_dir_mk,alive                              !�ļ�����״̬
  NAMELIST /CSPRPBP/ prcp_anomaly_missing,prcp_anomaly_trace,tavg_anomaly_missing,times,GhcnPrcpColNum,MissVal,&
    TraceVal,AheadMonthNum,StartMonth,StartYear,EndYear,MonthNum,ClimateStatus,&
    RankNum,PPvalue,TrainingRate,CoverYears,saveRankNum
  !****************************************************************************
  ! !                        Formatting
  !****************************************************************************

100 FORMAT(1X,A10,A4,A1,A2,A1,A2,A3,A8/)
200 FORMAT(I11,1X,A20,A10,F7.2,1X,F7.2,I5/)
300 FORMAT(I20,I20,I20)
400 FORMAT(I20,1320f10.1)
500 FORMAT(f20.0,1320f10.1)
600 FORMAT(I15,24F8.4)
3000 FORMAT(f13.0,f3.0,f13.0,f3.0,3f6.2)

  !****************************************************************************
  ! !                        Initialization
  !****************************************************************************
  ! get and print current time
  PRINT "(1X,A17/)", '>>����ʼ����...'
  CALL Display_Time_Current(DateT,NowTime)
  WRITE(*,100)'��ǰʱ�䣺',DateT(1:4),'/',DateT(5:6),'/',DateT(7:8),'   ',NowTime
  ! get current direction and set work direction
  PRINT "(1X,A19)", '>>���������ʼ��...'
  CALL SLEEP(1)
  istatus = GETCWD(Path)

  PredicteAndCheckDataPath = TRIM(Path)//'\BaseOnPrcp\PredicteAndCheck\'

  GhcnPrcpFile = TRIM(Path)//'\GhcnData\v2.prcp.dat'
  StationInfoFile = TRIM(Path)//'\GhcnData\v2.prcp.inv'
  PrcpWorkSpace = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\'
  TavgWorkSpace = TRIM(Path)//'\BaseOnTavg\CalculateStationPrcpRP\'
  istatus_dir_ch = CHDIR(TRIM(PrcpWorkSpace)) !����Ĭ�ϵĹ���Ŀ¼
  ! read default namelist file which define default parameters
  OPEN(UNIT = fileID,FILE = './CalStationPrcpRP_BP_add_next.namelist')
  READ (fileID,NML = CSPRPBP,ERR = 8089)
  CLOSE(fileID)
  PRINT "(1X,A18)", ">>������ʼ����ɣ�"
  PRINT *,'Main parameters as below:'
  WRITE (*,NML = CSPRPBP)
  CALL SLEEP(1)
  !****************************************************************************
  ! !                        Body of CalStationPrcpRP
  !****************************************************************************

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !                             Read GhcnPrcp Data
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  PRINT *,'>>��ȡGHCN����...'
  CALL SLEEP(1)
  CALL Inqire_Text_Row(TRIM(GhcnPrcpFile),LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum)
  ALLOCATE(GhcnPrcp(GhcnPrcpRowNum, GhcnPrcpColNum)) !��̬����GHCNԭʼ�������ݿ��С
  CALL Read_GHCN_Prcp(TRIM(GhcnPrcpFile), LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum, GhcnPrcpColNum, MissVal, TraceVal)!��ȡGHCN��ˮ����
  PRINT *,'>>��ȡGHCN������ɣ�'
  CALL SLEEP(1)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         get and save ghcnPrcp original station codes information
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  PRINT *,">>��ѯGhcnPrcpվ������Ϣ�ļ��Ƿ����..."
  CALL SLEEP(1)
  INQUIRE(DIRECTORY = TRIM(PrcpWorkSpace)//'DataBase', EXIST = alive)
  IF (alive == .false.) THEN
    istatus_dir_mk = MAKEDIRQQ(TRIM('DataBase'))
  END IF
  INQUIRE(FILE = './DataBase/StationCodes.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *, ">>GhcnPrcpվ������Ϣ�ļ����ڣ�"
    CALL SLEEP(1)
    PRINT *, ">>��ȡGhcnPrcpվ������Ϣ..."
    CALL SLEEP(1)
    CALL Inqire_Text_Row('./DataBase/StationCodes.dat',LEN('./DataBase/StationCodes.dat'),StationNum)
    ALLOCATE(StationCodesUnique(StationNum))
    OPEN(UNIT = fileID,FILE = './DataBase/StationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') ! RECL = 20,
    READ(fileID,'(F20.0)') StationCodesUnique
    CLOSE(fileID)
    PRINT *, ">>��ȡGhcnPrcpվ������Ϣ�ɹ���"
    CALL SLEEP(1)
  ELSE
    PRINT *, ">>GhcnPrcpվ������Ϣ�ļ�������"
    CALL SLEEP(1)
    PRINT *, ">>��ȡGhcnPrcp��վ������Ϣ..."
    CALL SLEEP(1)
    CALL Unique(GhcnPrcp(1:GhcnPrcpRowNum,1),StationCodesUnique)
    PRINT *, ">>��ȡGhcnPrcp��վ������Ϣ��ɣ�"
    StationNum = SIZE(StationCodesUnique)
    PRINT *,'Station number of original ghcn Precipitation data is :',StationNum
    CALL SLEEP(1)
    PRINT *,">>����GhcnPrcp��վ������Ϣ..."
    CALL SLEEP(1)
    OPEN(UNIT = fileID,FILE = './DataBase/StationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') ! RECL = 20,
    WRITE(fileID,'(I20)') INT(StationCodesUnique,8)
    CLOSE(fileID)
    PRINT *,">>����GhcnPrcp��վ������Ϣ���(��ǰexeĿ¼��\database\StationCodes.dat�ļ�)��"
    CALL SLEEP(1)
  END IF
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !           get and save ghcnPrcp station start-end year information
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  PRINT *,">>��ѯGhcnPrcp Station Start-End Year�ļ��Ƿ����..."
  CALL SLEEP(1)
  INQUIRE(FILE = './DataBase/StationStartEndYear.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *, ">>GhcnPrcp Station Start-End Year�ļ����ڣ�"
    CALL SLEEP(1)
    PRINT *, ">>��ȡGhcnPrcp Station Start-End Year�ļ���Ϣ..."
    CALL SLEEP(1)
    ALLOCATE(GhcnPrcpYear(StationNum,3))
    ALLOCATE(IntegerArrayTemp2D(3,StationNum))
    OPEN(UNIT = 50,FILE = './DataBase/StationStartEndYear.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') !RECL = 60,
    READ(50,300) IntegerArrayTemp2D
    CLOSE(fileID)
    GhcnPrcpYear = TRANSPOSE(IntegerArrayTemp2D)
    PRINT *, ">>��ȡGhcnPrcp Station Start-End Year�ļ��ɹ���"
    CALL SLEEP(1)
  ELSE
    PRINT *, ">>GhcnPrcp Station Start-End Year�ļ�������..."
    CALL SLEEP(1)
    PRINT *, ">>��ȡGhcnPrcp Station Start-End Year��Ϣ..."
    CALL SLEEP(1)
    ALLOCATE(GhcnPrcpYear(StationNum,3))
    GhcnPrcpYear(1:StationNum,1) = INT(StationCodesUnique,8)
    !print *,GhcnPrcpYear(1:StationNum,1)
    GhcnPrcpYear(1:StationNum,2:3) = 0
    CALL Progress % Set( N = StationNum , L = 30 )!// StationNum�Σ���ʾ����30
    Progress % Prefix = "Station Start-End Year:  "  !// ǰ����ʾ���֣����Ǳ���
    Progress % M = "|" !// ����ɲ��ֵ��ַ������Ǳ���
    Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
    DO II = 1,StationNum
      tempNum = COUNT(INT(GhcnPrcp(:,1),8) == GhcnPrcpYear(II,1))
      tempCount = 0
      DO JJ = 1,GhcnPrcpRowNum
        IF(GhcnPrcpYear(II,1) == INT(GhcnPrcp(JJ,1),8).AND.tempCount<tempNum) THEN
          tempCount = tempCount + 1
          IF(tempCount == 1) THEN
            GhcnPrcpYear(II,2) = INT(GhcnPrcp(JJ,3),8)
            GhcnPrcpYear(II,3) = INT(GhcnPrcp(JJ,3),8)
          ELSE IF(GhcnPrcpYear(II,1)==INT(GhcnPrcp(JJ,1),8).AND.INT(GhcnPrcp(JJ,3),8)<GhcnPrcpYear(II,2)) THEN
            GhcnPrcpYear(II,2) = INT(GhcnPrcp(JJ,3),8)
          ELSE IF(GhcnPrcpYear(II,1)==INT(GhcnPrcp(JJ,1),8).AND.INT(GhcnPrcp(JJ,3),8)>GhcnPrcpYear(II,3)) THEN
            GhcnPrcpYear(II,3) = INT(GhcnPrcp(JJ,3),8)
          END IF
        END IF
      END DO
      CALL Progress % Put( II , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
    END DO
    PRINT *, ">>��ȡGhcnPrcp Station Start-End Year��Ϣ��ɣ�"
    CALL SLEEP(1)
    PRINT *,">>����GhcnPrcp Station Start-End Year�ļ�..."
    CALL SLEEP(1)
    OPEN(UNIT = fileID,FILE = './DataBase/StationStartEndYear.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') ! RECL = 60,
    WRITE(fileID,300) TRANSPOSE(GhcnPrcpYear)
    CLOSE(fileID)
    PRINT *,">>����GhcnPrcp Station Start-End Year�ļ����(��ǰexeĿ¼��StationStartEnd.dat�ļ�)��"
    CALL SLEEP(1)
  END IF
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         select station which in 1901-2010 to build standard prcp database(ValidStationNum,1+MonthNum*YearLen)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  YearLen = EndYear - StartYear + 1
  WRITE(*,'(1x,a,i4,a,i4,a)')">>��ѯ",StartYear,"-",EndYear,"����վ�����ļ��Ƿ����..."
  CALL SLEEP(1)
  INQUIRE(FILE = './DataBase/ValidStationCodes.dat', EXIST = alive)
  IF(alive) THEN
    WRITE(*,'(1x,a,i4,a,i4,a)')">>",StartYear,"-",EndYear,"����վ�����ļ�����..."
    CALL SLEEP(1)
    WRITE(*,'(1x,a,i4,a,i4,a)')">>��ȡ",StartYear,"-",EndYear,"����վ�����ļ���..."
    CALL SLEEP(1)
    CALL Inqire_Text_Row('./DataBase/ValidStationCodes.dat',LEN('./DataBase/ValidStationCodes.dat'),tempNum)
    ALLOCATE(ValidPrcpStationCodesIndex(tempNum))
    OPEN(UNIT = fileID,FILE = './DataBase/ValidStationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') !RECL = 300,
    READ(fileID,'(I20)') ValidPrcpStationCodesIndex
    WRITE(*,'(1x,a,i4,a,i4,a)')">>��ȡ",StartYear,"-",EndYear,"����վ�����ļ���ɣ�"
    CALL SLEEP(1)
    CLOSE(fileID)
  ELSE
    WRITE(*,'(1x,a,i4,a,i4,a)')">>��ȡ",StartYear,"-",EndYear,"����վ���ţ���������׼���洢���ݿ�..."
    CALL SLEEP(1)
    tempNum = COUNT(GhcnPrcpYear(:,2)<=EndYear.AND.GhcnPrcpYear(:,3)>=StartYear)
    tempCount = 0
    ALLOCATE(ValidPrcpStationCodesIndex(tempNum))
    PRINT *,">>��ȡվ����..."
    CALL SLEEP(1)
    DO II = 1,StationNum
      IF(GhcnPrcpYear(II,2)<=EndYear.AND.GhcnPrcpYear(II,3)>=StartYear) THEN
        tempCount = tempCount + 1
        ValidPrcpStationCodesIndex(tempCount) = GhcnPrcpYear(II,1)
      END IF
    END DO
    ! save valid station codes to file
    PRINT *,">>������Чվ��������..."
    OPEN(UNIT = fileID,FILE = './DataBase/ValidStationCodes.dat') ! RECL = 20,
    WRITE(fileID,'(I20)') INT(ValidPrcpStationCodesIndex,8)
    CLOSE(fileID)
    PRINT *,">>������Чվ�������ݳɹ���"
    PRINT *,">>��ȡվ���ųɹ���"
    CALL SLEEP(1)
  END IF
  ! Build standard Precipitation DataBase
  PRINT *,">>������׼���洢���ݿ�..."
  CALL SLEEP(1)
  ValidStationNum = SIZE(ValidPrcpStationCodesIndex)
  ALLOCATE(GhcnPrcpStandardDB(ValidStationNum,1+MonthNum*YearLen))
  GhcnPrcpStandardDB(:,2:) = -9998  !��׼���ݿ��У�ԭ���ݿ�û�е����ݣ�ȫ������Ϊ-9998��ԭʼȱ��Ϊ-9999��ԭʼ�ۼ�Ϊ-8888
  PRINT *,">>������׼���洢���ݿ�ɹ���"
  CALL SLEEP(1)
  PRINT *,">>��ѯ��׼���洢���ݿ������ļ��Ƿ����..."
  CALL SLEEP(1)
  INQUIRE(FILE = './DataBase/GhcnPrcpStandardDB.dat', EXIST = alive)
  IF(alive) THEN
    PRINT *, ">>��׼���洢���ݿ������ļ����ڣ�"
    CALL SLEEP(1)
    PRINT *, ">>��ȡ��׼���洢���ݿ������ļ���Ϣ..."
    CALL SLEEP(1)
    ALLOCATE(RealArrayTemp2D(1+YearLen*MonthNum,ValidStationNum))
    OPEN(UNIT = fileID,FILE = './DataBase/GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') !RECL = 300,
    READ(fileID,500) RealArrayTemp2D
    GhcnPrcpStandardDB = TRANSPOSE(RealArrayTemp2D)
    DEALLOCATE(RealArrayTemp2D)
    PRINT *, ">>��ȡ��׼���洢���ݿ������ļ���Ϣ��ɣ�"
    CALL SLEEP(1)
    CLOSE(fileID)
  ELSE
    PRINT *,">>��׼���洢���ݿ������ļ�������..."
    CALL SLEEP(1)
    CALL Progress % Set( N = ValidStationNum , L = 30 )!// size(ValidPrcpStationCodesIndex)�Σ���ʾ����30
    Progress % Prefix = "��ȡ��׼���洢���ݿ�����:  "  !// ǰ����ʾ���֣����Ǳ���
    Progress % M = "#" !// ����ɲ��ֵ��ַ������Ǳ���
    Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
    DO II = 1,ValidStationNum
      GhcnPrcpStandardDB(II,1) = ValidPrcpStationCodesIndex(II)
      tempNum = COUNT(GhcnPrcp(:,1) == ValidPrcpStationCodesIndex(II))
      ALLOCATE(CodesIndexLocation(tempNum))
      CodesIndexLocation = ArrayFind(GhcnPrcp(:,1),'=',REAL(ValidPrcpStationCodesIndex(II),8))
      DO KK = 1,tempNum
        IF(GhcnPrcp(CodesIndexLocation(KK),3)>=StartYear.and.GhcnPrcp(CodesIndexLocation(KK),3)<=EndYear) THEN
          ColStart = (GhcnPrcp(CodesIndexLocation(KK),3) - StartYear)*MonthNum +1  !GhcnPrcpStandardDB�е��к�
          GhcnPrcpStandardDB(II,1+ColStart:1+ColStart+MonthNum) = GhcnPrcp(CodesIndexLocation(KK),4:15)
        END IF
      END DO
      DEALLOCATE(CodesIndexLocation)
      CALL Progress % Put( II , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
    END DO
    PRINT *,">>����GhcnPrcpStandardDB�ļ�..."
    CALL SLEEP(1)
    OPEN(UNIT = fileID,FILE = './DataBase/GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') !RECL = 300,
    DO i = 1,ValidStationNum!*(EndYear - StartYear + 1)
      WRITE(fileID,400) INT(GhcnPrcpStandardDB(i,1),8),GhcnPrcpStandardDB(i,2:)
    END DO
    CLOSE(fileID)
    PRINT *,">>����GhcnPrcpStandardDB�ļ����(��ǰexeĿ¼��GhcnPrcpStandardDB.dat�ļ�)!"
    CALL SLEEP(1)
    !-------------------------------------------------------------------------
    !     ɸѡ��ˮ��ÿ��С��   ��+times�� (�ұ߽�)����
    !-------------------------------------------------------------------------
    !print *,'>> ���Ц�+3�Ҵ���'
    !do ic = 1,ValidStationNum
    !  do im = 1,MonthNum
    !    tempCount = 0
    !    do i = 1,YearLen
    !      if(GhcnPrcpStandardDB(ic,(i-1)*MonthNum+im+1) >= 0) then
    !        tempCount = tempCount + 1
    !      end if
    !    end do
    !    ALLOCATE(CodesIndexLocation(tempCount))
    !    tempCount = 0
    !    do i = 1,YearLen
    !      if(GhcnPrcpStandardDB(ic,(i-1)*MonthNum+im+1) >= 0) then
    !        tempCount = tempCount + 1
    !        CodesIndexLocation(tempCount) = (i-1)*MonthNum+im+1
    !      end if
    !    end do
    !    CALL getPrcpValidRight(GhcnPrcpStandardDB(ic,CodesIndexLocation),tempCount,prcpRightLimit,times)
    !    do i = 1,tempCount
    !      if(GhcnPrcpStandardDB(ic,CodesIndexLocation(i)) > prcpRightLimit) then
    !        GhcnPrcpStandardDB(ic,CodesIndexLocation(i)) = -9997
    !      end if
    !    end do
    !    DEALLOCATE(CodesIndexLocation)
    !  end do
    !end do
    !PRINT *,">>����GhcnPrcpStandardDB�ļ�..."
    !CALL SLEEP(1)
    !OPEN(UNIT = fileID,FILE = './DataBase/GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
    !     POSITION = 'REWIND') !RECL = 300,
    !DO i = 1,ValidStationNum!*(EndYear - StartYear + 1)
    !  WRITE(fileID,400) INT(GhcnPrcpStandardDB(i,1),8),GhcnPrcpStandardDB(i,2:)
    !END DO
    !CLOSE(fileID)
    !PRINT *,">>����GhcnPrcpStandardDB�ļ����(��ǰexeĿ¼��GhcnPrcpStandardDB.dat�ļ�)!"
    !CALL SLEEP(1)
  END IF

  !***********************************************************************************************************
  ! ��ѯ���ڽ�����վ��Ŀ�Ԥ��վ���б��ļ��Ƿ���ڣ��������ȡ
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

  !***********************************************************************************************************
  ! ��ѯTavg��׼���ݿ��ļ�
  !*****************************************************************************************
  ! ��ȡ�¶�վ������Ϣ
  PRINT *, ">>��ѯValidTavgStationCodes.dat�ļ��Ƿ����..."
  INQUIRE(FILE = TRIM(ADJUSTL(TavgWorkSpace))//'DataBase\ValidStationCodes.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *, ">>ValidTavgStationCodes.dat�ļ����ڣ�"
    CALL SLEEP(1)
    PRINT *, ">>��ȡ��,���Ժ�..."
    CALL SLEEP(1)
    CALL Inqire_Text_Row(TRIM(ADJUSTL(TavgWorkSpace))//'DataBase\ValidStationCodes.dat',LEN(TRIM(ADJUSTL(TavgWorkSpace))//'DataBase\ValidStationCodes.dat'),ValidTavgStationNum)
    ALLOCATE(ValidTavgStationCodesIndex(ValidTavgStationNum))
    OPEN(UNIT = fileID,FILE = TRIM(ADJUSTL(TavgWorkSpace))//'DataBase\ValidStationCodes.dat')
    READ(fileID,'(I20)') ValidTavgStationCodesIndex
    CLOSE(fileID)
    PRINT *, ">>��ȡValidTavgStationCodes.dat�ļ��ɹ���"
    CALL SLEEP(1)
  END IF
  ! ��ȡ�¶�վ���׼���ݿ��ļ�
  ALLOCATE(GhcnTavgStandardDB(ValidTavgStationNum,1+MonthNum*YearLen))
  GhcnTavgStandardDB(:,2:) = -9998  !��׼���ݿ��У�ԭ���ݿ�û�е����ݣ�ȫ������Ϊ-9998��ȱ��Ϊ-9999
  PRINT *,">>������׼���洢���ݿ�ɹ���"
  CALL SLEEP(1)
  PRINT *,">>��ѯ��׼���洢���ݿ������ļ��Ƿ����..."
  CALL SLEEP(1)
  INQUIRE(FILE = TRIM(ADJUSTL(TavgWorkSpace))//'DataBase\GhcnTavgStandardDB.dat', EXIST = alive)
  IF(alive) THEN
    PRINT *, ">>��׼���洢���ݿ������ļ����ڣ�"
    CALL SLEEP(1)
    PRINT *, ">>��ȡ��׼���洢���ݿ������ļ���Ϣ..."
    CALL SLEEP(1)
    ALLOCATE(RealArrayTemp2D(1+YearLen*MonthNum,ValidTavgStationNum))
    OPEN(UNIT = fileID,FILE = TRIM(ADJUSTL(TavgWorkSpace))//'DataBase\GhcnTavgStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      & POSITION = 'REWIND') !RECL = 300,
    READ(fileID,500) RealArrayTemp2D
    GhcnTavgStandardDB = TRANSPOSE(RealArrayTemp2D)
    PRINT *, ">>��ȡ��׼���洢���ݿ������ļ���Ϣ��ɣ�"
    CALL SLEEP(1)
    CLOSE(fileID)
    DEALLOCATE(RealArrayTemp2D)
  END IF


  !*****************************************************************************************
  ! calculate partial R R2 P of each predicted station in 12 month in max aheadmonth 24,
  ! controlling by the strogest correlation predictor station
  !*****************************************************************************************
  !�л���Ĭ�Ϲ���·��
  istatus_dir_ch = CHDIR(TRIM(PrcpWorkSpace))
  !��̬�������
  ALLOCATE(StudyPrcp(YearLen*MonthNum))
  ALLOCATE(FactorTavg(YearLen*MonthNum))
  ALLOCATE(R(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(TempR(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(P(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(TempP(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(R2Count(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(ptor2k(ValidTavgStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(ptor2b(ValidTavgStationNum*MonthNum,AheadMonthNum))
  !��ѯ�������Ŀ¼�Ƿ����
  INQUIRE(DIRECTORY = TRIM(PrcpWorkSpace)//'add_next_StationList_BT', EXIST = alive)
  IF (alive == .false.) THEN
    istatus_dir_mk = makedirqq(TRIM('add_next_StationList_BT'))
  END IF
  istatus_dir_ch = CHDIR(TRIM(PrcpWorkSpace)//TRIM('add_next_StationList_BT\'))
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
    PRINT *,'Please input StartNum(as integer format ,StartNum should >= 1 and <= 20547):'
    PRINT *,' '
    READ (*,*) EndStationNum
    PRINT *,' '
  END IF
  !�������յ�Ԥ��վ����Ϣ��ȷ��Ԥ��վ����������Ϣ
  analysisStationNum = EndStationNum - StartStationNum + 1
  ALLOCATE(ValidStationCoupled(analysisStationNum,13))
  ValidStationCoupled = 0
  !����������վ����������
  WRITE(TotalAnalysisStationNumStr,'(I5)') StudyStationNum001
  PRINT *,analysisStationNum
  ! Ϊ�˽��쳣ֵ�ų����⣬�����Ԥ����ʱ���ǽ�-9998ת��Ϊ-9999
  WHERE (GhcnPrcpStandardDB == -9998)
    GhcnPrcpStandardDB = -9999
  END WHERE
  WHERE (GhcnTavgStandardDB == -9998)
        GhcnTavgStandardDB = -9999
  END WHERE

  !��ʼ�������н�����
  CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// size(ValidPrcpStationCodesIndex)�Σ���ʾ����30
  Progress % M = "=" !// ����ɲ��ֵ��ַ������Ǳ���
  Progress % O = "." !// δ��ɲ��ֵ��ַ������Ǳ���

  DO i = StartStationNum,EndStationNum
    !��ȡ��Ԥ��վ�㼰����ǿ��ص�Ԥ������վ����Ϣ
    pstandID = P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+1,1)

    !print*,pstandID
    !pause

    ValidStationCoupled(i - StartStationNum + 1,1) = pstandID
    R = DataNumNotEnough
    P = DataNumNotEnough
    ptor2k = DataNumNotEnough
    ptor2B = DataNumNotEnough
    WRITE(StationDirName,'(I11)') INT(pstandID,KIND=8)!StudyCode

    !��ѯվ��Ŀ¼�Ƿ����
    INQUIRE(DIRECTORY = TRIM(PrcpWorkSpace)//'add_next_StationList_BT\'//TRIM(StationDirName), EXIST = alive)
    IF (alive == .false.) THEN
      istatus_dir_mk = MAKEDIRQQ(TRIM(StationDirName))
    END IF
    istatus_dir_ch = CHDIR(TRIM(PrcpWorkSpace)//'add_next_StationList_BT\'//TRIM(StationDirName))

    !istatus_dir_mk = MAKEDIRQQ(TRIM(StationDirName))
    !istatus_dir_ch = CHDIR(TRIM(PrcpWorkSpace)//'add_next_StationList_BT\'//TRIM(StationDirName))
    WRITE(StationNumStr,'(I5)') i
    !���������н�������// ǰ����ʾ���֣����Ǳ���
    Progress % Prefix = "StationNum:"//TRIM(StationNumStr)//" of "//TotalAnalysisStationNumStr
    !Ԥ����վ�㽵������
    !��Ҫ����ȷ��Ԥ����վ��������ʲôλ��
    DO tempNum = 1,ValidStationNum
      IF (GhcnPrcpStandardDB(tempNum,1) == pstandID) THEN
        EXIT
      END IF
    END DO
    StudyPrcp = GhcnPrcpStandardDB(tempNum,2:)
    !print *,tempNum,"=======",pstandID,GhcnPrcpStandardDB(tempNum,1)
    !pause

    DO j = 1,ValidTavgStationNum
      pstor2ID = GhcnTavgStandardDB(j,1)
      FactorTavg = GhcnTavgStandardDB(j,2:)
      FactorTavgLen = MonthNum*YearLen
      DO k = 1,AheadMonthNum
        pstor2LM = REAL(k,KIND=8)
        ALLOCATE(TempFactorTavg(FactorTavgLen - k))
        ALLOCATE(TempStudyPrcp(FactorTavgLen - k))
        TempFactorTavg = FactorTavg(1:FactorTavgLen - k)
        TempStudyPrcp = StudyPrcp(k+1:)
        DO ii = 1,MonthNum
          !print *,"enter"

          CALL pTandMonthAtAheadMonthAndMonthRoll(k, ii, StartMonth, MonthNum, pTandMonth)
          !��ii�·ݶ�Ӧ����ǿԤ������վ����Ϣ
          pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,3)
          pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,4)
          R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,5)
          Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,6)
          IF (Rtandtor1 < -1) THEN
            !�հ��У�����һ������վ�����lead time��ڶ���������ͬʱִ�У����ߵ�һ�����ӵ����ϵ��Ϊ-9ʱִ��
          ELSE

            !��ȡ��pstandID��month��pstor1ID��pstor1LM��Ӧ��Ԥ������Ԥ������վ������
            !����Ϊȫ�ֱ���tempFactorPrcpMonth��tempStudyPrcpMonth
            CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(pTandMonth,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,&
              &RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB) !,tempFactorPrcpMonth,tempStudyPrcpMonth

            !��ȡ��pstor2ID��Ӧ��Ԥ����վ���Ԥ������վ������
            ALLOCATE(TempMonthFactorTavg(SIZE(TempFactorTavg(ii:SIZE(TempFactorTavg):MonthNum))))
            ALLOCATE(TempMonthStudyPrcp(SIZE(TempStudyPrcp(ii:SIZE(TempStudyPrcp):MonthNum))))
            TempMonthFactorTavg = TempFactorTavg(ii:SIZE(TempFactorTavg):MonthNum)
            TempMonthStudyPrcp = TempStudyPrcp(ii:SIZE(TempStudyPrcp):MonthNum)

            !print *,"deal..."

            !����pstor1ID��pstor2ID��������޶�Ԥ������Ԥ������1��Ԥ������2�Ľ���������
            IF (SIZE(tempFactorPrcpMonth)>SIZE(TempMonthFactorTavg)) THEN
              ALLOCATE(tempFactorPrcpMonthModify(SIZE(TempMonthFactorTavg)))
              tempFactorPrcpMonthModify = tempFactorPrcpMonth(SIZE(tempFactorPrcpMonth)-SIZE(TempMonthFactorTavg)+1:)
              !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
              ALLOCATE(TempMonthFactorTavgModify(SIZE(TempMonthFactorTavg)))
              TempMonthFactorTavgModify = TempMonthFactorTavg
              !��ʱԤ����վ��ʹ����pstor2ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(TempMonthFactorTavg)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp
            ELSE IF(SIZE(tempFactorPrcpMonth)<SIZE(TempMonthFactorTavg)) THEN
              ALLOCATE(TempMonthFactorTavgModify(SIZE(tempFactorPrcpMonth)))
              TempMonthFactorTavgModify = TempMonthFactorTavg(SIZE(TempMonthFactorTavg)-SIZE(tempFactorPrcpMonth)+1:)
              !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
              ALLOCATE(tempFactorPrcpMonthModify(SIZE(tempFactorPrcpMonth)))
              tempFactorPrcpMonthModify = tempFactorPrcpMonth
              !��ʱԤ����վ��ʹ����pstor2ID��Ӧ��Ԥ����������޶�ֵ����ʱ��Ҫ�޵�
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(tempFactorPrcpMonth)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp(SIZE(TempMonthFactorTavg)-SIZE(tempFactorPrcpMonth)+1:)
            ELSE
              !��ʱ��������Ҫ�޶�
              ALLOCATE(TempMonthFactorTavgModify(SIZE(TempMonthFactorTavg)))
              TempMonthFactorTavgModify = TempMonthFactorTavg
              !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
              ALLOCATE(tempFactorPrcpMonthModify(SIZE(tempFactorPrcpMonth)))
              tempFactorPrcpMonthModify = tempFactorPrcpMonth
              !��ʱԤ����վ��ʹ����pstor2ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(TempMonthFactorTavg)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp
            END IF

            !print *,SIZE(TempMonthFactorTavgModify),SIZE(TempMonthStudyPrcpModify),SIZE(tempFactorPrcpMonthModify)
            !print *,"judage"

            IF((isContinuityGT_M(TempMonthStudyPrcpModify,ClimateStatus,prcp_anomaly_missing) == .false.) .AND.&
              (isContinuityGT_M(TempMonthStudyPrcpModify,ClimateStatus,prcp_anomaly_trace) == .false.) .AND.&
              (isContinuityGT_M(TempMonthFactorTavgModify,ClimateStatus,tavg_anomaly_missing) == .false.) .AND.&
              (isContinuityGT_M(tempFactorPrcpMonthModify,ClimateStatus,prcp_anomaly_missing) == .false.) .AND.&
              (isContinuityGT_M(tempFactorPrcpMonthModify,ClimateStatus,prcp_anomaly_trace) == .false.))then

              !print *,"enter IF"

              tempCount = COUNT(TempMonthFactorTavgModify>-9998 .AND. TempMonthStudyPrcpModify>=0 .AND. tempFactorPrcpMonthModify>=0)
              ALLOCATE(CodesIndexLocation(tempCount))
              tempNum = 0
              !print *,"deal do"
              DO jj = 1,SIZE(TempMonthFactorTavgModify)
                IF(TempMonthFactorTavgModify(jj)>-9998 .AND. TempMonthStudyPrcpModify(jj)>=0 .AND. tempFactorPrcpMonthModify(jj)>=0) THEN
                  tempNum = tempNum + 1
                  CodesIndexLocation(tempNum) = jj
                END IF
              END DO

              IF(tempCount >= CoverYears) THEN
                !ͳ��վ���Ƿ������Լ�¼�����ڴ˴�ֻ��Ϊ��ͳ���Ƿ���ڼ�¼
                ValidStationCoupled(i - StartStationNum + 1 ,pTandMonth+1) = 1

                trainLen = FLOOR(tempCount*TrainingRate)
                ALLOCATE(ptandPrcp(trainLen))
                ALLOCATE(ptor1Prcp(trainLen))
                ALLOCATE(ptor2Tavg(trainLen))
                ptandPrcp = TempMonthStudyPrcpModify(CodesIndexLocation(1:trainLen))
                ptor1Prcp = tempFactorPrcpMonthModify(CodesIndexLocation(1:trainLen))
                ptor2Tavg = TempMonthFactorTavgModify(CodesIndexLocation(1:trainLen))

                !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
                CALL LinearRegression(ptor1Prcp,ptandPrcp,trainLen,ptor1k,ptor1b)
                ALLOCATE(ptandPrcpY1(trainLen))
                ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
                !CALL Correlation(trainLen,ptandPrcpY1,ptandPrcp,RptandY)
                !print *,"k:",Rtandtor1,",  b:",RptandY
                !pause

                !����Ԥ�������һԤ�����ӵġ��в
                ALLOCATE(ptandPrcpY1Residual(trainLen))
                ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
                !���Իع����Ԥ�������һԤ�����ӵġ��в�͵ڶ�Ԥ������֮������Թ�ϵ
                CALL LinearRegression(ptor2Tavg,ptandPrcpY1Residual,trainLen,tempPtor2k, tempPtor2b)
                ptor2k(j+(pTandMonth-1)*ValidTavgStationNum,k) = tempPtor2k
                ptor2b(j+(pTandMonth-1)*ValidTavgStationNum,k) = tempPtor2b

                ALLOCATE(ptandPrcpY2(trainLen))
                ptandPrcpY2 = ptor2Tavg*tempPtor2k + tempPtor2b
                !������Ԥ��������
                ALLOCATE(ptandPrcpY(trainLen))
                ptandPrcpY = ptandPrcpY1+ptandPrcpY2
                !����Ԥ����վ����ڶ������ӵ����ϵ��
                CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)

                !������һ��δ��Ч���������һ��RptandY��Ϊ��ʱ����
                !CALL Correlation(trainLen,ptor2Tavg,ptandPrcpY1Residual,RptandY)
                R(j+(pTandMonth-1)*ValidTavgStationNum,k) = RptandY
                !����������ˮƽ
                !CALL Pvalue(trainLen, RptandY, PptandY)

                !������һ��δ��Ч���������һ��RptandY��Ϊ��ʱ����
                CALL Pvalue(trainLen, RptandY, PptandY)
                P(j+(pTandMonth-1)*ValidTavgStationNum,k) = PptandY
                !pause

                DEALLOCATE(ptandPrcp)
                DEALLOCATE(ptor1Prcp)
                DEALLOCATE(ptor2Tavg)
                DEALLOCATE(ptandPrcpY)
                DEALLOCATE(ptandPrcpY1)
                DEALLOCATE(ptandPrcpY2)
                DEALLOCATE(ptandPrcpY1Residual)

              END IF


              DEALLOCATE(CodesIndexLocation)
            END IF

            !print *,RptandY,PptandY,"del vars",Rtandtor1
            !pause

            DEALLOCATE(TempMonthFactorTavg)
            DEALLOCATE(TempMonthStudyPrcp)
            DEALLOCATE(tempFactorPrcpMonth)
            DEALLOCATE(tempStudyPrcpMonth)
            DEALLOCATE(tempFactorPrcpMonthModify)
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
          write(fileID, '(I15,24F6.2)' ) ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)), R(ii,:)  !ValidPrcpStationCodesIndex(jj),
        else
          write(fileID, '(I15,24F6.2)' ) ValidTavgStationCodesIndex(ValidTavgStationNum), R(ii,:)  !ValidPrcpStationCodesIndex(jj),
        endif
      end do
      close(fileID)
      open(fileID,FILE = 'P.txt',IOSTAT = iosval)
      do ii = 1,MonthNum*ValidTavgStationNum
        if(mod(ii,ValidTavgStationNum) /= 0) then
          write(fileID, '(I15,24F6.2)') ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)),P(ii, :)  !ValidPrcpStationCodesIndex(jj),
        else
          write(fileID, '(I15,24F6.2)') ValidTavgStationCodesIndex(ValidTavgStationNum),P(ii, :)  !ValidTavgStationCodesIndex(jj),
        endif
      end do
      close(fileID)
    END IF

    ! Save into binary file in order to save save hard disk space
    !open(fileID,FILE = 'R.bin',IOSTAT = iosval, FORM = 'UNFORMATTED', ACCESS = 'DIRECT',RECL = 26, STATUS = 'REPLACE')
    !do ii = 1,MonthNum * ValidTavgStationNum
    !    if(mod(ii,ValidTavgStationNum) ==0 ) then
    !        write(fileID, rec = ii ) ValidTavgStationCodesIndex(ValidTavgStationNum),(R(ii,k), k = 1, AheadMonthNum)  !ValidPrcpStationCodesIndex(jj),
    !    else
    !        write(fileID, rec = ii ) ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)),(R(ii,k), k = 1, AheadMonthNum)  !ValidPrcpStationCodesIndex(jj),
    !    endif
    !end do
    !close(fileID)
    !open(fileID,FILE = 'P.bin',IOSTAT = iosval, FORM = 'UNFORMATTED', ACCESS = 'DIRECT',RECL = 26, STATUS = 'REPLACE')
    !do ii = 1,MonthNum*ValidTavgStationNum
    !    if(mod(ii,ValidTavgStationNum)) then
    !        write(fileID, rec = ii) ValidTavgStationCodesIndex(mod(ii,ValidTavgStationNum)),(P(ii, k),k = 1, AheadMonthNum)  !ValidPrcpStationCodesIndex(jj),
    !    else
    !        write(fileID, rec = ii) ValidTavgStationCodesIndex(ValidTavgStationNum),(P(ii, k), k = 1, AheadMonthNum)  !ValidPrcpStationCodesIndex(jj),
    !    endif
    !end do
    !close(fileID)

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
      CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(ii,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,&
        &RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB)
      ALLOCATE(savePtror1Prcp(size(tempFactorPrcpMonth)))
      savePtror1Prcp = tempFactorPrcpMonth
      ALLOCATE(savePtand1Prcp(size(tempStudyPrcpMonth)))
      savePtand1Prcp = tempStudyPrcpMonth
      DEALLOCATE(tempFactorPrcpMonth)
      DEALLOCATE(tempStudyPrcpMonth)

      !print *,savePtand1Prcp
      !pause
      !print *,savePtror1Prcp
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
          ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
          savePtror1PrcpModify = savePtror1Prcp
          !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror1PrcpModify)
            IF(savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"

          trainLen = FLOOR(tempCount*TrainingRate)
          ALLOCATE(ptandPrcp(trainLen))
          ALLOCATE(ptor1Prcp(trainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:trainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:trainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Prcp,ptandPrcp,trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(trainLen))
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(trainLen))
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"

          !������Ԥ��������
          ALLOCATE(ptandPrcpY(trainLen))
          ptandPrcpY = ptandPrcpY1
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)

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
          !ptor1Prcp
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
              ptor1Prcp(kk), &
              tavg_anomaly_missing, &
              ptandPrcpY1(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        ELSE
          !print *,"��ȡ��ڶ���������ص����ݣ���",jj,"��"
          CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),REAL(ValidTavgStationCodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8),YearLen,MonthNum,RankNum,&
            ValidStationNum,ValidTavgStationNum,StartMonth,GhcnPrcpStandardDB,GhcnTavgStandardDB)

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
          IF (SIZE(savePtror1Prcp)>SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror2Tavg)))
            savePtror1PrcpModify = savePtror1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ��Ҫ�޵�
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Tavg)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Tavg)+1:)
          ELSE IF(SIZE(savePtror1Prcp)<SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror1Prcp)))
            savePtror2TavgModify = savePtror2Tavg(SIZE(savePtror2Tavg)-SIZE(savePtror1Prcp)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !��ʱ��������Ҫ�޶�
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtror2TavgModify>-9998 .AND. savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2TavgModify)
            IF(savePtror2TavgModify(kk)>-9998 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"

          trainLen = FLOOR(tempCount*TrainingRate)
          ALLOCATE(ptandPrcp(trainLen))
          ALLOCATE(ptor1Prcp(trainLen))
          ALLOCATE(ptor2Tavg(trainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:trainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:trainLen))
          ptor2Tavg = savePtror2TavgModify(CIL(1:trainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Prcp,ptandPrcp,trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(trainLen))
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(trainLen))
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"
          !���Իع����Ԥ�������һԤ�����ӵġ��в�͵ڶ�Ԥ������֮������Թ�ϵ
          CALL LinearRegression(ptor2Tavg,ptandPrcpY1Residual,trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(trainLen))
          ptandPrcpY2 = ptor2Tavg*tempPtor2k + tempPtor2b
          !������Ԥ��������
          ALLOCATE(ptandPrcpY(trainLen))
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
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
          !ptor1Prcp
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
              ptor1Prcp(kk), &
              ptor2Tavg(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror2Tavg)

          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtror2TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptor2Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY2)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        END IF

      END DO
      DEALLOCATE(savePtror1Prcp)
      DEALLOCATE(savePtand1Prcp)
    END DO

    CLOSE(fileID)




    ! ����ǰ15��,P<0.05
    PPvalue = 0.05
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
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.05.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum
      !print *,"��ȡ��һ���ӵ������Ϣ"
      !��ii�·ݶ�Ӧ����ǿԤ������վ����Ϣ
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"��ȡ���һ�����Ӷ�Ӧ��Prcp����"
      CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(ii,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,&
        &RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB)
      ALLOCATE(savePtror1Prcp(size(tempFactorPrcpMonth)))
      savePtror1Prcp = tempFactorPrcpMonth
      ALLOCATE(savePtand1Prcp(size(tempStudyPrcpMonth)))
      savePtand1Prcp = tempStudyPrcpMonth
      DEALLOCATE(tempFactorPrcpMonth)
      DEALLOCATE(tempStudyPrcpMonth)

      !print *,savePtand1Prcp
      !pause
      !print *,savePtror1Prcp
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
          ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
          savePtror1PrcpModify = savePtror1Prcp
          !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror1PrcpModify)
            IF(savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"

          trainLen = FLOOR(tempCount*TrainingRate)
          ALLOCATE(ptandPrcp(trainLen))
          ALLOCATE(ptor1Prcp(trainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:trainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:trainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Prcp,ptandPrcp,trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(trainLen))
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(trainLen))
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"

          !������Ԥ��������
          ALLOCATE(ptandPrcpY(trainLen))
          ptandPrcpY = ptandPrcpY1
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)

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
          !ptor1Prcp
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
              ptor1Prcp(kk), &
              tavg_anomaly_missing, &
              ptandPrcpY1(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        ELSE
          !print *,"��ȡ��ڶ���������ص����ݣ���",jj,"��"
          CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),REAL(ValidTavgStationCodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8),YearLen,MonthNum,RankNum,&
            ValidStationNum,ValidTavgStationNum,StartMonth,GhcnPrcpStandardDB,GhcnTavgStandardDB)

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
          IF (SIZE(savePtror1Prcp)>SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror2Tavg)))
            savePtror1PrcpModify = savePtror1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ��Ҫ�޵�
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Tavg)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Tavg)+1:)
          ELSE IF(SIZE(savePtror1Prcp)<SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror1Prcp)))
            savePtror2TavgModify = savePtror2Tavg(SIZE(savePtror2Tavg)-SIZE(savePtror1Prcp)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !��ʱ��������Ҫ�޶�
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtror2TavgModify>-9998 .AND. savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2TavgModify)
            IF(savePtror2TavgModify(kk)>-9998 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"

          trainLen = FLOOR(tempCount*TrainingRate)
          ALLOCATE(ptandPrcp(trainLen))
          ALLOCATE(ptor1Prcp(trainLen))
          ALLOCATE(ptor2Tavg(trainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:trainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:trainLen))
          ptor2Tavg = savePtror2TavgModify(CIL(1:trainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Prcp,ptandPrcp,trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(trainLen))
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(trainLen))
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"
          !���Իع����Ԥ�������һԤ�����ӵġ��в�͵ڶ�Ԥ������֮������Թ�ϵ
          CALL LinearRegression(ptor2Tavg,ptandPrcpY1Residual,trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(trainLen))
          ptandPrcpY2 = ptor2Tavg*tempPtor2k + tempPtor2b
          !������Ԥ��������
          ALLOCATE(ptandPrcpY(trainLen))
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
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
          !ptor1Prcp
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
              ptor1Prcp(kk), &
              ptor2Tavg(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror2Tavg)

          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtror2TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptor2Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY2)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        END IF

      END DO
      DEALLOCATE(savePtror1Prcp)
      DEALLOCATE(savePtand1Prcp)
    END DO

    CLOSE(fileID)


    ! ����ǰ15��,P<0.01
    PPvalue = 0.01
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
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.01.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum
      !print *,"��ȡ��һ���ӵ������Ϣ"
      !��ii�·ݶ�Ӧ����ǿԤ������վ����Ϣ
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"��ȡ���һ�����Ӷ�Ӧ��Prcp����"
      CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(ii,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,&
        &RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB)
      ALLOCATE(savePtror1Prcp(size(tempFactorPrcpMonth)))
      savePtror1Prcp = tempFactorPrcpMonth
      ALLOCATE(savePtand1Prcp(size(tempStudyPrcpMonth)))
      savePtand1Prcp = tempStudyPrcpMonth
      DEALLOCATE(tempFactorPrcpMonth)
      DEALLOCATE(tempStudyPrcpMonth)

      !print *,savePtand1Prcp
      !pause
      !print *,savePtror1Prcp
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
          ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
          savePtror1PrcpModify = savePtror1Prcp
          !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror1PrcpModify)
            IF(savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"

          trainLen = FLOOR(tempCount*TrainingRate)
          ALLOCATE(ptandPrcp(trainLen))
          ALLOCATE(ptor1Prcp(trainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:trainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:trainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Prcp,ptandPrcp,trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(trainLen))
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(trainLen))
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"

          !������Ԥ��������
          ALLOCATE(ptandPrcpY(trainLen))
          ptandPrcpY = ptandPrcpY1
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)

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
          !ptor1Prcp
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
              ptor1Prcp(kk), &
              tavg_anomaly_missing, &
              ptandPrcpY1(kk), &
              prcp_anomaly_missing, &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        ELSE
          !print *,"��ȡ��ڶ���������ص����ݣ���",jj,"��"
          CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),REAL(ValidTavgStationCodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8),YearLen,MonthNum,RankNum,&
            ValidStationNum,ValidTavgStationNum,StartMonth,GhcnPrcpStandardDB,GhcnTavgStandardDB)

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
          IF (SIZE(savePtror1Prcp)>SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror2Tavg)))
            savePtror1PrcpModify = savePtror1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Tavg)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ��Ҫ�޵�
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Tavg)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Tavg)+1:)
          ELSE IF(SIZE(savePtror1Prcp)<SIZE(savePtror2Tavg)) THEN
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror1Prcp)))
            savePtror2TavgModify = savePtror2Tavg(SIZE(savePtror2Tavg)-SIZE(savePtror1Prcp)+1:)
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !��ʱ��������Ҫ�޶�
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !��һ������Ҫ�޸ģ�ԭ�ⲻ����copy����
            ALLOCATE(savePtror2TavgModify(SIZE(savePtror2Tavg)))
            savePtror2TavgModify = savePtror2Tavg
            !��ʱԤ����վ��ʹ����pstor1ID��Ӧ��Ԥ����������޶�ֵ����ʱ����Ҫ�޵ƣ�ֱ��copy����
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2TavgModify
          !pause

          !print *,"ͳ�Ʋ�Ϊ0����������λ��"

          tempCount = COUNT(savePtror2TavgModify>-9998 .AND. savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2TavgModify)
            IF(savePtror2TavgModify(kk)>-9998 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"���µ���ptand��ptor1��ptor2����"

          trainLen = FLOOR(tempCount*TrainingRate)
          ALLOCATE(ptandPrcp(trainLen))
          ALLOCATE(ptor1Prcp(trainLen))
          ALLOCATE(ptor2Tavg(trainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:trainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:trainLen))
          ptor2Tavg = savePtror2TavgModify(CIL(1:trainLen))

          !print *,"��ϵ�һԤ��������Ԥ����"

          !���Իع���ϵ�һԤ������վ����Ԥ����վ��֮������Թ�ϵ
          CALL LinearRegression(ptor1Prcp,ptandPrcp,trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(trainLen))
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !����Ԥ�������һԤ�����ӵġ��в
          ALLOCATE(ptandPrcpY1Residual(trainLen))
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"��ϵڶ�Ԥ��������Ԥ����"
          !���Իع����Ԥ�������һԤ�����ӵġ��в�͵ڶ�Ԥ������֮������Թ�ϵ
          CALL LinearRegression(ptor2Tavg,ptandPrcpY1Residual,trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(trainLen))
          ptandPrcpY2 = ptor2Tavg*tempPtor2k + tempPtor2b
          !������Ԥ��������
          ALLOCATE(ptandPrcpY(trainLen))
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !����Ԥ����վ��۲��¼��Ԥ�������ϵ��
          CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"��һ��������Ϣ��"
          !print *,"ԭʼ��¼��",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"�ڶ���������Ϣ��"
          !print *,"��¼��",ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidTavgStationNum,CodesIndexLocation(2))
          !print *,"���μ�����Ϣ��"
          !print *,ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
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
          !ptor1Prcp
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
              ptor1Prcp(kk), &
              ptor2Tavg(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror2Tavg)

          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtror2TavgModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptor2Tavg)
          DEALLOCATE(ptandPrcpY1)
          DEALLOCATE(ptandPrcpY1Residual)
          DEALLOCATE(ptandPrcpY2)
          DEALLOCATE(ptandPrcpY)
          DEALLOCATE(CIL)

        END IF

      END DO
      DEALLOCATE(savePtror1Prcp)
      DEALLOCATE(savePtand1Prcp)
    END DO

    CLOSE(fileID)


    DEALLOCATE(CodesIndexLocation)
    istatus_dir_ch = CHDIR(TRIM(PrcpWorkSpace)//'add_next_StationList_BT\')

    CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ


  END DO
  !================================================================================================================
  istatus_dir_ch = CHDIR(TRIM(PrcpWorkSpace)//'add_next_StationList_BT') !����Ĭ�ϵĹ���Ŀ¼
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

  END SUBROUTINE CalStationPrcpRP_BP_add_next



1