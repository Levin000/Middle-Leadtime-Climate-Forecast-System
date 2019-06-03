!SUBROUTINE PredictLastOneData_BP
!      !Program PredictLastOneData_BP
!      !!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"PredictLastData_BP" :: PredictLastData_BP
!      USE, INTRINSIC :: IEEE_ARITHMETIC
!      USE IFPOSIX
!      USE IFPORT
!      USE cmdProgress
!      USE global
!      USE ghcnDefType
!      USE ghcnPort
!      USE arrayPort
!      USE statisticPort
!      IMPLICIT NONE
!
!      CHARACTER(LEN = 500) :: Path  !�����ļ�exe·��
!      CHARACTER(LEN = 500) :: GhcnPrcpFile
!      CHARACTER(LEN = 500) :: StationPRPath  !վ�����ݿ�Ŀ¼
!      CHARACTER(LEN = 500) :: PredicteAndCheckDataPath  !�������ݿ�
!      CHARACTER(LEN = 500) :: DataPath  !���ݿ�
!      CHARACTER(LEN = 500) :: PredictedPrcpPath !Ԥ��Ľ�ˮ�͹۲⽵ˮ
!      CHARACTER(LEN = 500) :: PredictedPrcpPathOne
!      CHARACTER(LEN = 500) :: KFPredictedPrcpPath  !K-Flod Ԥ��Ľ�ˮ�͹۲⽵ˮ
!      CHARACTER(LEN = 500) :: StationInfoFile  !station��Ϣ�ļ�
!      CHARACTER(LEN = 500) :: DirName,FileName
!      CHARACTER(LEN = 500) :: errmsg
!      CHARACTER(LEN = 11) :: Str
!      INTEGER :: GhcnPrcpColNum   !GHCNPrcp��������
!      INTEGER :: StationInfoFileLen  ! GHCN Station��Ϣ������
!      INTEGER :: ValidStationNum,StartMonth,MonthNum,RankNum, MonthIndex
!      INTEGER :: StartYear,EndYear,YearLen  !Ԥ���о��Ŀ�ʼ��ݡ�������ݡ�ʱ�䳤��
!      INTEGER :: FactorStationNum001,FactorStationNum005,FactorStationNum01,StudyStationNum001,PredictedStationNum005,PredictedStationNum01
!      INTEGER :: i,j,k,jj,ii,ik
!      INTEGER :: istatus = -9999,ilen = -9999,ierror = -9999,iosval
!      INTEGER :: tempNum,ClimateStatus
!      INTEGER :: KFNum    !Group Number of K-Flod Cross Check
!      INTEGER :: fid = 10,fidd = 55,fiddd = 555
!      INTEGER :: MissVal!ȱʡֵ����1Ϊ����0Ϊ������
!      INTEGER :: TraceVal !�ۼ���ˮ�Ĵ���1Ϊ����0Ϊ������
!      INTEGER :: GhcnPrcpRowNum  ! GHCNPrcp���ݵ�����
!      INTEGER(KIND = 8) :: tempLength,tempLen
!      INTEGER(KIND = 8),ALLOCATABLE :: ValidStationCode(:)  !վ������
!      INTEGER(KIND = 8),ALLOCATABLE :: ValidStationLocation(:)  !δ����
!      REAL(KIND = 8) :: DataNumNotEnough = -9.0,P_Inf = -6.0,R_Inf = -5.0,Trash = -7   !R_EQ_NAN = -8.0,
!      REAL(KIND = 8),ALLOCATABLE :: ValidStationList(:)
!      REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData(:,:)
!      REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData_Predictable(:,:)
!      REAL(KIND = 8),ALLOCATABLE :: FactorStationCodes001(:),StudyStationCodes001(:)
!      REAL(KIND = 8),ALLOCATABLE :: P_EQ_1_Codes(:)
!      REAL(KIND = 8),ALLOCATABLE :: StudyStations_Month_Predictable(:,:)
!      REAL(KIND = 8),ALLOCATABLE :: tempArray(:),tempPR001(:,:)
!      REAL(KIND = 8) :: keyValue ,TrainingRate
!      REAL(KIND = 8),ALLOCATABLE :: tempPR(:,:)
!      REAL(KIND = 8), ALLOCATABLE :: FactorPrcp(:),StudyPrcp(:)
!      REAL(KIND = 8), ALLOCATABLE :: tempFactorPrcp(:),tempStudyPrcp(:)
!      REAL(KIND = 8),ALLOCATABLE :: tempPredictedPrcp(:,:),tempKFPredictedPrcp(:),tempKFPredictedPrcpCp(:),X_Value !tempPredictedPrcpCP(:),
!      REAL(KIND = 8) Pc,Pm,Fc,Fm
!      REAL,ALLOCATABLE :: R(:),P(:),KFR(:),KFP(:)
!      REAL(KIND = 8), ALLOCATABLE :: Pk(:),Pb(:)
!      REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:),RealArrayTemp2D(:,:)  !GhcnPrcp��Ÿ�ʽ��׼�����ݿ�
!      LOGICAL :: lstatus,lrelase,llstatus
!      LOGICAL :: alive001,alive005,alive01,alive
!      TYPE( CLS_CMD_Progress ) ::Progress  !������
!
!      NAMELIST /PLODBP/ GhcnPrcpColNum,StartMonth,MonthNum,RankNum,StartYear,EndYear,ClimateStatus,MissVal,TraceVal,TrainingRate
!
!100   FORMAT(a60:,i)
!200   FORMAT(f15.0,f10.0,3f10.6)
!300   FORMAT(f13.0,f3.0,f13.0,f3.0,3f10.6)
!400   FORMAT(i13,i3,i13,i3,3f10.6)
!500   FORMAT(f20.0,1320f10.1)
!600   FORMAT(i13,i3,i13,i3,3f15.8,3f15.8,2f30.5)
!700   FORMAT(f10.3,f10.3,f15.3)
!800   FORMAT(i13,i3,i13,i3,3f15.8,3f15.8)
!900   FORMAT(i13,i3,i13,i3,3f10.2)
!    
!      istatus = GETCWD(Path)
!      GhcnPrcpFile = TRIM(Path)//'\GhcnData\v2.prcp.dat'
!      PredicteAndCheckDataPath = TRIM(Path)//'\BaseOnPrcp\PredicteAndCheck\'
!      DataPath = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\'  !���ݴ��·�����������ݣ�
!      StationPRPath = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\StationList\' !Station����P��R��R2�Ĵ��·��
!      PredictedPrcpPath = 'PredictedAndStudyPrcp'
!      PredictedPrcpPathOne = 'PredictedAndStudyPrcpOne'
!      KFPredictedPrcpPath = 'PredictedAndStudyPrcpKF'
!      StationInfoFile = TRIM(Path)//'\GhcnData\v2.prcp.inv'  !station��Ϣ�ļ�
!    
!      istatus = CHDIR(TRIM(PredicteAndCheckDataPath))
!      OPEN(UNIT = fid,FILE = './PredictLastOneData_BP.namelist')
!      READ (fid,NML = PLODBP,ERR = 8089)
!      CLOSE(fid)
!      PRINT *,'Main parameters as below:'
!      WRITE (*,NML = PLODBP)
!    
!      !****************************************************************************
!      ! !                 Read ValidStationCode
!      !****************************************************************************
!      istatus = CHDIR(TRIM(DataPath))  ! �л�·�������ݿ�·��
!      PRINT *,">>��ȡ�о�ʱ���վ����..."
!      CALL Inqire_Text_Row('ValidStationCodes.dat',LEN('ValidStationCodes.dat'),ValidStationNum)
!      ALLOCATE(ValidStationCode(ValidStationNum))
!      OPEN(fid,FILE = 'ValidStationCodes.dat')
!      READ(fid,'(i20)') ValidStationCode
!      CLOSE(fid)
!      PRINT *,">>��ȡ�о�ʱ���վ������ɣ�"
!      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!      !                             Read GhcnPrcp Data
!      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!      PRINT *,'>>��ȡGHCN����...'
!      CALL SLEEP(1)
!      CALL Inqire_Text_Row(TRIM(GhcnPrcpFile),LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum)
!      ALLOCATE(GhcnPrcp(GhcnPrcpRowNum, GhcnPrcpColNum)) !��̬����GHCNԭʼ�������ݿ��С
!      CALL Read_GHCN_Prcp(TRIM(GhcnPrcpFile),LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum, GhcnPrcpColNum, MissVal, TraceVal)!��ȡGHCN��ˮ����
!      PRINT *,'>>��ȡGHCN������ɣ�'
!      CALL SLEEP(1)
!      !****************************************************************************
!      ! !                Read P_GT_0.01 Station Data
!      !****************************************************************************
!      ilen = 47
!      ALLOCATE(P_R_001RankData(MonthNum*RankNum*ValidStationNum,7))
!      ALLOCATE(tempPR001(7,MonthNum*RankNum*ValidStationNum))
!      istatus = CHDIR(TRIM(PredicteAndCheckDataPath))  ! �л�·�������ݿ�·��
!      INQUIRE(FILE = 'R_RankNum15_P.LT.0.01.dat', EXIST = alive001)
!      IF (alive001) THEN
!        PRINT *,'>>��ȡPValue<0.01����...'
!        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01.dat')
!        READ(fid,300) tempPR001
!        CLOSE(fid)
!        P_R_001RankData = TRANSPOSE(tempPR001)
!        PRINT *,'>>��ȡPValue<0.01������ɣ�'
!      ELSE
!        PRINT *,TRIM(PredicteAndCheckDataPath)//'R_RankNum15_P.LT.0.01.dat���ݲ�����...'
!        ALLOCATE(tempPR(5,MonthNum*RankNum))
!        PRINT *,'>>��ȡ�о�ʱ���վ��P��R��Ϣ...'
!        istatus = CHDIR(TRIM(StationPRPath))  !�л����ݿ�·��
!        SELECT CASE (istatus)
!        CASE (2)  ! ENOENT
!          errmsg = 'The directory '//TRIM(StationPRPath)//' does not exist'
!          PAUSE
!        CASE (20) ! ENOTDIR
!          errmsg = TRIM(StationPRPath)//' is not a directory'
!          PAUSE
!        CASE (0) ! NO error
!          GOTO 40
!          CASE DEFAULT
!          WRITE (errmsg,*) 'Error with code ', istatus
!          PAUSE
!        END SELECT
!40      CALL Progress % Set( N = ValidStationNum , L = 30 )!// StationNum�Σ���ʾ����30
!        Progress % M = ">" !// ����ɲ��ֵ��ַ������Ǳ���
!        Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
!        Progress % Prefix = "Read P R Data(P.LT.0.01):  "  !// ǰ����ʾ���֣����Ǳ���
!        DO i = 1,ValidStationNum
!          WRITE(Str,'(i11)') ValidStationCode(i)
!          lstatus = CHANGEDIRQQ(Str)
!          IF (lstatus) THEN
!            P_R_001RankData((i-1)*MonthNum*RankNum+1:i*MonthNum*RankNum,1) = ValidStationCode(i)
!            DO j = 1,MonthNum
!              P_R_001RankData((i-1)*MonthNum*RankNum+(j-1)*RankNum+1:(i-1)*MonthNum*RankNum+j*RankNum,2) = j
!            END DO
!            OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01.dat')
!            READ(fid,200) tempPR
!            CLOSE(fid)
!            P_R_001RankData((i-1)*MonthNum*RankNum+1:i*MonthNum*RankNum,3:) = TRANSPOSE(tempPR)
!          ELSE
!            PRINT *,">>�л�վ�����ݿ�ʧ��,վ���ţ�"//Str
!            PAUSE
!          END IF
!          CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
!          istatus = CHDIR(TRIM(StationPRPath))
!          IF (istatus.NE.0) THEN
!            PRINT *,">>�л�վ�����ݿ��б��ļ���ʧ�ܣ�"
!            PAUSE
!          END IF
!        END DO
!        istatus = CHDIR(TRIM(PredicteAndCheckDataPath))  ! �л�·�������ݿ�·��
!        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01.dat')
!        DO i = 1,MonthNum*RankNum*ValidStationNum
!          WRITE(fid,400)INT(P_R_001RankData(i,1),8),INT(P_R_001RankData(i,2),8),INT(P_R_001RankData(i,3),8),INT(P_R_001RankData(i,4),8),&
!                            P_R_001RankData(i,5),P_R_001RankData(i,6),P_R_001RankData(i,7)
!        END DO
!        CLOSE(fid)
!        PRINT *,'>>��ȡ�о�ʱ���վ��P��R��Ϣ��ɣ�'
!      END IF
!      !*************************************************************************************************
!      ! !                ������Ԥ��վ����Ԥ��վ�����Ϣ--Factor\Predictedվ���ż�������ͨ�� P = 0.01��
!      !*************************************************************************************************
!      INQUIRE(FILE = 'StudyCode_RankNum15_P.LT.0.01.dat', EXIST = alive001)
!      IF (alive001) THEN
!        PRINT *,'>>StudyCode_RankNum15_P.LT.0.01�ļ����ڣ���ȡ�ļ�...'
!        CALL Inqire_Text_Row('StudyCode_RankNum15_P.LT.0.01.dat',LEN('StudyCode_RankNum15_P.LT.0.01.dat'),tempNum)
!        ALLOCATE(StudyStationCodes001(tempNum))
!        OPEN(fid,FILE = 'StudyCode_RankNum15_P.LT.0.01.dat')
!        READ(fid,'(f15.0)')StudyStationCodes001
!        CLOSE(fid)
!        StudyStationNum001 = SIZE(StudyStationCodes001)
!        PRINT *,'StudyStationNum: ',StudyStationNum001
!        PRINT *,'>>��ȡStudyCode_RankNum15_P.LT.0.01�ļ����!'
!      ELSE
!        PRINT *,'>>StudyCode_RankNum15_P.LT.0.01�ļ������ڣ���ȡStudyCodes...'
!        ALLOCATE(tempArray(MonthNum*RankNum*ValidStationNum))
!        tempArray = 0
!        WHERE((P_R_001RankData(:,7)/=DataNumNotEnough).and.(P_R_001RankData(:,7)/=P_Inf).and.(P_R_001RankData(:,7)/=R_Inf))  !.and.(P_R_001RankData(:,7)/=R_EQ_NAN).and.(P_R_001RankData(:,7)/=P_OV_PV)
!          tempArray = P_R_001RankData(:,1)
!        END WHERE
!        CALL Unique(tempArray,ValidStationList)
!        IF(ValidStationList(1) == 0) THEN
!          ALLOCATE(StudyStationCodes001(SIZE(ValidStationList)-1))
!          StudyStationCodes001 = ValidStationList(2:)
!          StudyStationNum001 = SIZE(StudyStationCodes001)
!        ELSE
!          ALLOCATE(StudyStationCodes001(SIZE(ValidStationList)))
!          StudyStationCodes001 = ValidStationList
!          StudyStationNum001 = SIZE(StudyStationCodes001)
!        END IF
!
!        PRINT *,'StudyStationNum: ',StudyStationNum001
!        OPEN(fid,FILE = 'StudyCode_RankNum15_P.LT.0.01.dat')
!        WRITE(fid,'(i15)')INT(StudyStationCodes001,8)
!        CLOSE(fid)
!        DEALLOCATE(tempArray)
!        DEALLOCATE(ValidStationList)
!        PRINT *,'>>��ȡStudyCodes�ɹ�������ΪStudyCode_RankNum15_P.LT.0.01.dat�ļ�!'
!      END IF
!      
!      INQUIRE(FILE = 'FactorCode_RankNum15_P.LT.0.01.dat', EXIST = alive001)
!      IF (alive001) THEN
!        PRINT *,'>>FactorCode_RankNum15_P.LT.0.01�ļ����ڣ���ȡ�ļ�...'
!        CALL Inqire_Text_Row('FactorCode_RankNum15_P.LT.0.01.dat',LEN('FactorCode_RankNum15_P.LT.0.01.dat'),tempNum)
!        ALLOCATE(FactorStationCodes001(tempNum))
!        OPEN(fid,FILE = 'FactorCode_RankNum15_P.LT.0.01.dat')
!        READ(fid,'(f15.0)')FactorStationCodes001
!        CLOSE(fid)
!        FactorStationNum001 = SIZE(FactorStationCodes001)
!        PRINT *,'FactorStationNum: ',FactorStationNum001
!        PRINT *,'>>��ȡFactorCode_RankNum15_P.LT.0.01�ļ����!'
!      ELSE
!        PRINT *,'>>FactorCode_RankNum15_P.LT.0.01�ļ������ڣ���ȡFactorCodes...'
!        ALLOCATE(tempArray(MonthNum*RankNum*ValidStationNum))
!        tempArray = 0
!        WHERE((P_R_001RankData(:,7)/=DataNumNotEnough).and.(P_R_001RankData(:,7)/=P_Inf).and.(P_R_001RankData(:,7)/=R_Inf)) !.and.(P_R_001RankData(:,7)/=R_EQ_NAN).and.(P_R_001RankData(:,7)/=P_OV_PV)
!          tempArray = P_R_001RankData(:,3)
!        END WHERE
!        CALL Unique(tempArray,ValidStationList)
!        IF(ValidStationList(1)==0)THEN
!          ALLOCATE(FactorStationCodes001(SIZE(ValidStationList)-1))
!          FactorStationCodes001 = ValidStationList(2:)
!          FactorStationNum001 = SIZE(FactorStationCodes001)
!        ELSE
!          ALLOCATE(FactorStationCodes001(SIZE(ValidStationList)))
!          FactorStationCodes001 = ValidStationList
!          FactorStationNum001 = SIZE(FactorStationCodes001)
!        END IF
!        PRINT *,'FactorStationNum: ',FactorStationNum001
!        OPEN(fid,FILE = 'FactorCode_RankNum15_P.LT.0.01.dat')
!        WRITE(fid,'(i15)')INT(FactorStationCodes001,8)
!        CLOSE(fid)
!        DEALLOCATE(tempArray)
!        DEALLOCATE(ValidStationList)
!        PRINT *,'>>��ȡFactorCodes�ɹ�������ΪFactorCode_RankNum15_P.LT.0.01.dat�ļ�!'
!      END IF
!      !***********************************************************************************************************
!      ! get and save the information of station which can be predicted 
!      ALLOCATE(P_R_001RankData_Predictable(StudyStationNum001*MonthNum*RankNum,7))
!      INQUIRE(FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat', EXIST = alive001)
!      IF (alive001) THEN
!        PRINT *,'>>R_RankNum15_P.LT.0.01_Predicted�ļ����ڣ���ȡ�ļ�...'
!        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat')
!        DO i = 1,StudyStationNum001*MonthNum*RankNum
!          READ(fid,300)P_R_001RankData_Predictable(i,:)
!        END DO
!        CLOSE(fid)
!        PRINT *,'>>��ȡR_RankNum15_P.LT.0.01_Predicted�ļ���ɣ�'
!      ELSE
!        PRINT *,'>>R_RankNum15_P.LT.0.01_Predicted�ļ�������,��ȡ�����Ϣ...'
!        ALLOCATE(ValidStationLocation(MonthNum*RankNum))
!        CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum�Σ���ʾ����30
!        Progress % M = ">" !// ����ɲ��ֵ��ַ������Ǳ���
!        Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
!        Progress % Prefix = "Build DataBase of P_R_001RankData_Predictable:  "  !// ǰ����ʾ���֣����Ǳ���
!        DO i = 1,StudyStationNum001
!          ValidStationLocation = ArrayFind(P_R_001RankData(:,1),'=',StudyStationCodes001(i))
!          P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+1:i*MonthNum*RankNum,:)=P_R_001RankData(ValidStationLocation,:)
!          CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
!        END DO
!        DEALLOCATE(ValidStationLocation)
!        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat')
!        DO i = 1,StudyStationNum001*MonthNUM*RankNum
!          WRITE(fid,400)INT(P_R_001RankData_Predictable(i,1),8),INT(P_R_001RankData_Predictable(i,2),8),&
!            INT(P_R_001RankData_Predictable(i,3),8),INT(P_R_001RankData_Predictable(i,4),8),P_R_001RankData_Predictable(i,5:7)
!        END DO
!        CLOSE(fid)
!        PRINT *,'>>��ȡR_RankNum15_P.LT.0.01_Predicted�ļ���ɣ������棡'
!      END IF
!      !*************************************************************************************************************************
!      !
!      istatus = CHDIR(TRIM(DataPath))!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      YearLen = EndYear - StartYear + 1
!      PRINT *,">>������׼���洢���ݿ�..."
!      CALL SLEEP(1)
!      ALLOCATE(GhcnPrcpStandardDB(ValidStationNum,1+MonthNum*YearLen))
!      GhcnPrcpStandardDB(:,2:) = -9998  !��׼���ݿ��У�ԭ���ݿ�û�е����ݣ�ȫ������Ϊ-9998��ȱ��Ϊ-9999���ۼ�Ϊ-8888
!      PRINT *,">>������׼���洢���ݿ�ɹ���"
!      CALL SLEEP(1)
!      PRINT *,">>��ѯ��׼���洢���ݿ������ļ��Ƿ����..."
!      CALL SLEEP(1)
!      INQUIRE(FILE = 'GhcnPrcpStandardDB.dat', EXIST = alive)
!      IF(alive) THEN
!        PRINT *, ">>��׼���洢���ݿ������ļ����ڣ�"
!        CALL SLEEP(1)
!        PRINT *, ">>��ȡ��׼���洢���ݿ������ļ���Ϣ..."
!        CALL SLEEP(1)
!        ALLOCATE(RealArrayTemp2D(1+MonthNum*YearLen,ValidStationNum))
!        OPEN(UNIT = fid,FILE = 'GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
!          & POSITION = 'REWIND') !RECL = 300,
!        READ(fid,500) RealArrayTemp2D
!        GhcnPrcpStandardDB = TRANSPOSE(RealArrayTemp2D)
!        PRINT *, ">>��ȡ��׼���洢���ݿ������ļ���Ϣ��ɣ�"
!        CALL SLEEP(1)
!        CLOSE(fid)
!      END IF
!      
!      ALLOCATE(FactorPrcp(YearLen*MonthNum))
!      ALLOCATE(StudyPrcp(YearLen*MonthNum))
!      ALLOCATE(Pk(MonthNum*RankNum*StudyStationNum001))
!      ALLOCATE(Pb(MonthNum*RankNum*StudyStationNum001))
!      ALLOCATE(tempPredictedPrcp(StudyStationNum001*MonthNum,7))
!      istatus = CHDIR(TRIM(PredicteAndCheckDataPath))!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum�Σ���ʾ����30
!      Progress % M = ">" !// ����ɲ��ֵ��ַ������Ǳ���
!      Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
!      Progress % Prefix = "Predicted and Statistic Each Month:  "  !// ǰ����ʾ���֣����Ǳ���
!      tempPredictedPrcp = -999
!      Pk = -999
!      Pb = -999
!      KFNum = 10
!      PRINT *,'������Ԥ��ʱ���õ�RankNum��'
!      READ(*,*) k
!      PRINT *,k
!      DO i = 1,StudyStationNum001
!        DO j = 1,MonthNum
!          !PAUSE
!          !DO k = 1,1  !RankNum
!          tempLength = (i-1)*MonthNum*RankNum + (j-1)*RankNum + k !Ԥ���ǲ��õĳ�ʼ��
!          tempLen = (i-1)*MonthNum + j
!          tempPredictedPrcp(tempLen,1) = P_R_001RankData_Predictable(tempLength,1)
!          tempPredictedPrcp(tempLen,2) = P_R_001RankData_Predictable(tempLength,2)
!          tempPredictedPrcp(tempLen,3) = P_R_001RankData_Predictable(tempLength,3)
!          tempPredictedPrcp(tempLen,4) = P_R_001RankData_Predictable(tempLength,4)
!          IF (P_R_001RankData_Predictable(tempLength,5) == 1) THEN   !R2������1
!            Pc = P_R_001RankData_Predictable(tempLength,1)
!            Pm = P_R_001RankData_Predictable(tempLength,2)
!            Fc = P_R_001RankData_Predictable(tempLength,3)
!            Fm = P_R_001RankData_Predictable(tempLength,4)
!          ELSE IF((P_R_001RankData_Predictable(tempLength,7) /= DataNumNotEnough).and.(P_R_001RankData_Predictable(tempLength,7) /= R_Inf).and.(P_R_001RankData_Predictable(tempLength,7) /= P_Inf) ) THEN
!            Pc = P_R_001RankData_Predictable(tempLength,1)
!            Pm = P_R_001RankData_Predictable(tempLength,2)
!            Fc = P_R_001RankData_Predictable(tempLength,3)
!            Fm = P_R_001RankData_Predictable(tempLength,4)
!            CALL StudyMonthAndFactorPreData_BP(Pc,Pm,Fc,Fm,YearLen,MonthNum,RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB) !,tempFactorPrcpMonth,tempStudyPrcpMonth
!            ALLOCATE(ValidStationLocation(COUNT(tempFactorPrcpMonth>=0.AND.tempStudyPrcpMonth>=0)))
!            tempNum = 0
!            DO jj = 1,SIZE(tempFactorPrcpMonth)
!              IF(tempFactorPrcpMonth(jj)>=0.and.tempStudyPrcpMonth(jj)>=0) THEN
!                tempNum = tempNum + 1
!                ValidStationLocation(tempNum) = jj
!              END IF
!            END DO
!            !==============================================================================================
!            !               �ж��Ƿ�������20��������ޱ仯����������20�������ޱ仯ʱ������
!            !==============================================================================================
!            !IF((isContinuityGT_M(tempFactorPrcpMonth(ValidStationLocation),ClimateStatus) .EQ. .false.).AND. &
!            !   (isContinuityGT_M(tempStudyPrcpMonth(ValidStationLocation),ClimateStatus) .EQ. .false.)) THEN    !ֻ���㵱20������̬��û���������ޱ仯��ֵʱ��ֵ
!              !===========================================================
!              !           Simple LinearRegression
!              !===========================================================
!              CALL LinearRegression(tempFactorPrcpMonth(ValidStationLocation(1:tempNum-1)),tempStudyPrcpMonth(ValidStationLocation(1:tempNum-1)),tempNum-1,Pk(tempLength),Pb(tempLength))
!              !tempPredictedPrcp(tempLen,1) = Pc
!              !tempPredictedPrcp(tempLen,2) = Pm
!              !tempPredictedPrcp(tempLen,3) = Fc
!              !tempPredictedPrcp(tempLen,4) = Fm
!              tempPredictedPrcp(tempLen,5) = Pk(tempLength)*tempFactorPrcpMonth(ValidStationLocation(tempNum))+Pb(tempLength)
!              IF(tempPredictedPrcp(tempLen,5)<0) THEN
!                tempPredictedPrcp(tempLen,6) = 0
!              ELSE
!                tempPredictedPrcp(tempLen,6) = tempPredictedPrcp(tempLen,5)
!              END IF
!              tempPredictedPrcp(tempLen,7) = tempStudyPrcpMonth(ValidStationLocation(tempNum))
!            !END IF
!            DEALLOCATE(ValidStationLocation)
!            DEALLOCATE(tempFactorPrcpMonth)
!            DEALLOCATE(tempStudyPrcpMonth)
!          END IF
!          !END DO
!        END DO
!        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
!        !!!!CALL Progress % Put( i-8000 , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
!      END DO
!      CLOSE(fiddd)
!      !=========================================================================================
!      PRINT *,'Writing Simple LinearRegression Results...'
!      OPEN(fid,FILE = 'SimpleLinear_LastOneData_P0.01_FirstRank_BP.dat')
!      DO ii = 1,StudyStationNum001*MonthNum
!        !IF(tempPredictedPrcp(ii,1)==-999.0) THEN
!        !  WRITE(fid,900),INT(tempPredictedPrcp(ii,1),8),-9,INT(tempPredictedPrcp(ii,3),8),-9,tempPredictedPrcp(ii,5:7)
!        !ELSE
!        WRITE(fid,900),INT(tempPredictedPrcp(ii,1),8),INT(tempPredictedPrcp(ii,2),8),INT(tempPredictedPrcp(ii,3),8),&
!                       INT(tempPredictedPrcp(ii,4),8),tempPredictedPrcp(ii,5:7)
!        !END IF
!      END DO
!      CLOSE(fid)
!      PRINT *,'Writing Simple LinearRegression Results Success!'
!      DEALLOCATE(tempPredictedPrcp)
!      !***************************************************************************************************************************************
!      istatus = CHDIR(TRIM(Path))  ! �л�·������Ŀ¼
!      !***************************************************************************************************************************************
!      
!      PRINT *,'�����������...'
!      PAUSE
!      STOP
!8089  PRINT *,'�ļ���ȡ����'
!      PAUSE
!      
!END SUBROUTINE PredictLastOneData_BP