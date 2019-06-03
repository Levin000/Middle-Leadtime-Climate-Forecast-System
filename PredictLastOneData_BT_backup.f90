!SUBROUTINE PredictLastOneData_BT
!      !!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"PredicteAndCheckSystem_BT" :: PredicteAndCheckSystem_BT
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
!      CHARACTER(LEN = 500) :: Path                                   !�����ļ�exe·��
!      CHARACTER(LEN = 500) :: GhcnPrcpFile
!      CHARACTER(LEN = 500) :: StationPRPath                          !վ�����ݿ�Ŀ¼
!      CHARACTER(LEN = 500) :: PredicteAndCheckDataPath               !�������ݿ�
!      CHARACTER(LEN = 500) :: DataPath                               !Tavg���ݿ�
!      CHARACTER(LEN = 500) :: PrcpDataPath                           !Prcp���ݿ�
!      CHARACTER(LEN = 500) :: PredictedPrcpPath                      !Ԥ��Ľ�ˮ�͹۲⽵ˮ
!      CHARACTER(LEN = 500) :: KFPredictedPrcpPath                    !K-Flod Ԥ��Ľ�ˮ�͹۲⽵ˮ
!      CHARACTER(LEN = 500) :: StationInfoFile                        !station��Ϣ�ļ�
!      CHARACTER(LEN = 500) :: DirName,FileName
!      CHARACTER(LEN = 500) :: errmsg
!      CHARACTER(LEN = 11) :: Str
!      INTEGER :: GhcnPrcpColNum                                     !GHCNPrcp��������
!      INTEGER :: StationInfoFileLen                                 ! GHCN Station��Ϣ������
!      INTEGER :: ValidPrcpStationNum,ValidTavgStationNum,StartMonth,MonthNum,RankNum, MonthIndex
!      INTEGER :: StartYear ,EndYear ,YearLen                        !Ԥ���о��Ŀ�ʼ��ݡ�������ݡ�ʱ�䳤��
!      INTEGER :: FactorStationNum001,FactorStationNum005,FactorStationNum01,StudyStationNum001,PredictedStationNum005,PredictedStationNum01
!      INTEGER :: i,j,k,jj,ii,ik
!      INTEGER :: istatus = -9999,ilen = -9999,ierror = -9999,iosval
!      INTEGER :: tempNum,ClimateStatus
!      INTEGER :: KFNum                                               !Group Number of K-Flod Cross Check
!      INTEGER :: fid = 10,fidd = 55,fiddd = 555
!      INTEGER :: MissVal                                             !ȱʡֵ����1Ϊ����0Ϊ������
!      INTEGER :: TraceVal                                            !�ۼ���ˮ�Ĵ���1Ϊ����0Ϊ������
!      INTEGER :: GhcnPrcpRowNum                                      ! GHCNPrcp���ݵ�����
!      INTEGER(KIND = 8) :: tempLength,tempLen
!      INTEGER(KIND = 8),ALLOCATABLE :: ValidTavgStationCodesIndex(:),ValidPrcpStationCodesIndex(:)  !վ������
!      INTEGER(KIND = 8),ALLOCATABLE :: ValidStationLocation(:)       !δ����
!      REAL,ALLOCATABLE :: R(:),P(:),KFR(:),KFP(:)
!      REAL(KIND = 8) :: DataNumNotEnough = -9.0,P_Inf = -6.0,R_Inf = -5.0,Trash = -7   !DataNumNotEnough = -9.0,R_EQ_NAN = -8.0,P_OV_PV = -7,P_EQ_INF = -6,R_EQ_INF = -5
!      REAL(KIND = 8),ALLOCATABLE :: ValidStationList(:)
!      REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData(:,:)
!      REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData_Predictable(:,:)
!      REAL(KIND = 8),ALLOCATABLE :: FactorStationCodes001(:),StudyStationCodes001(:)
!      REAL(KIND = 8),ALLOCATABLE :: P_EQ_1_Codes(:)
!      REAL(KIND = 8),ALLOCATABLE :: StudyStations_Month_Predictable(:,:)
!      REAL(KIND = 8),ALLOCATABLE :: tempArray(:),tempPR001(:,:)
!      REAL(KIND = 8) :: keyValue = -9,TrainingRate
!      REAL(KIND = 8),ALLOCATABLE :: tempPR(:,:)
!      REAL(KIND = 8), ALLOCATABLE :: FactorPrcp(:),StudyPrcp(:)
!      REAL(KIND = 8), ALLOCATABLE :: tempFactorPrcp(:),tempStudyPrcp(:)
!      REAL(KIND = 8),ALLOCATABLE :: tempPredictedPrcp(:,:),tempKFPredictedPrcp(:),tempKFPredictedPrcpCp(:),X_Value
!      REAL(KIND = 8) Pc,Pm,Fc,Fm
!      REAL(KIND = 8), ALLOCATABLE :: Pk(:),Pb(:)
!      REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:),RealArrayTemp2D(:,:)  !GhcnPrcp��Ÿ�ʽ��׼�����ݿ�
!      REAL(KIND = 8), ALLOCATABLE :: GhcnTavgStandardDB(:,:)
!      TYPE( CLS_CMD_Progress ) ::Progress                                          !������
!      LOGICAL :: lstatus,lrelase,llstatus
!      LOGICAL :: alive001,alive005,alive01,alive
!      
!      NAMELIST /PLODBT/ GhcnPrcpColNum,StartMonth,MonthNum,RankNum,StartYear,EndYear,ClimateStatus,MissVal,TraceVal,TrainingRate
!    
!100   FORMAT(a60:,i)  
!200   FORMAT(f15.0,f10.0,3f10.6)
!300   FORMAT(f13.0,f3.0,f13.0,f3.0,3f10.6)
!400   FORMAT(i13,i3,i13,i3,3f10.6)
!500   FORMAT(f20.0,1320f10.1)
!600   FORMAT(i13,i3,i13,i3,3f15.8,3f15.8,2f20.10)
!700   FORMAT(f10.3,f10.3,f15.3)
!800   FORMAT(i13,i3,i13,i3,3f15.8,3f15.8)
!900   FORMAT(f20.0,1320f10.1)
!1000  FORMAT(i13,i3,i13,i3,3f10.2)
!    
!      istatus = GETCWD(Path)
!      GhcnPrcpFile = TRIM(Path)//'\GhcnData\v2.prcp.dat'
!      PredicteAndCheckDataPath = TRIM(Path)//'\BaseOnTavg\PredicteAndCheck\'
!      DataPath = TRIM(Path)//'\BaseOnTavg\CalculateStationPrcpRP\'                   !���ݴ��·�����������ݣ�
!      PrcpDataPath = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\'
!      StationPRPath = TRIM(Path)//'\BaseOnTavg\CalculateStationPrcpRP\StationList\'  !Station����P��R��R2�Ĵ��·��
!      PredictedPrcpPath = 'PredictedAndStudyPrcp'
!      KFPredictedPrcpPath = 'PredictedAndStudyPrcpKF'
!      StationInfoFile = TRIM(Path)//'\GhcnData\v2.prcp.inv'                          !station��Ϣ�ļ�
!      istatus = CHDIR(TRIM(PredicteAndCheckDataPath))
!      OPEN(UNIT = fid,FILE = './PredictLastOneData_BT.namelist')
!      READ (fid,NML = PLODBT,ERR = 8089)
!      CLOSE(fid)
!      PRINT *,'Main parameters as below:'
!      WRITE (*,NML = PLODBT)
!    
!      !****************************************************************************
!      ! !                 Read ValidPrcpStationCode
!      !****************************************************************************
!      istatus = CHDIR(TRIM(PrcpDataPath))  ! �л�·�������ݿ�·��
!      lstatus = CHANGEDIRQQ('DataBase')  !�л�����׼���ݿ⣨��
!      PRINT *,">>��ȡ�о�ʱ���վ����..."
!      CALL Inqire_Text_Row('ValidStationCodes.dat',LEN('ValidStationCodes.dat'),ValidPrcpStationNum)
!      ALLOCATE(ValidPrcpStationCodesIndex(ValidPrcpStationNum))
!      OPEN(fid,FILE = 'ValidStationCodes.dat')
!      READ(fid,'(i20)') ValidPrcpStationCodesIndex
!      CLOSE(fid)
!      PRINT *,">>��ȡ�о�ʱ���վ������ɣ�"
!      !****************************************************************************
!      ! !                 Read ValidTavgStationCode
!      !****************************************************************************
!      istatus = CHDIR(TRIM(DataPath))  ! �л�·�������ݿ�·��
!      lstatus = CHANGEDIRQQ('DataBase')  !�л�����׼���ݿ⣨��
!      PRINT *,">>��ȡ�о�ʱ���վ����..."
!      CALL Inqire_Text_Row('ValidStationCodes.dat',LEN('ValidStationCodes.dat'),ValidTavgStationNum)
!      ALLOCATE(ValidTavgStationCodesIndex(ValidTavgStationNum))
!      OPEN(fid,FILE = 'ValidStationCodes.dat')
!      READ(fid,'(i20)') ValidTavgStationCodesIndex
!      CLOSE(fid)
!      PRINT *,">>��ȡ�о�ʱ���վ������ɣ�"
!      !****************************************************************************
!      ! !                Read P less than 0.01 Station Data
!      !****************************************************************************
!      ilen = 47
!      ALLOCATE(P_R_001RankData(MonthNum*RankNum*ValidPrcpStationNum,7))
!      ALLOCATE(tempPR001(7,MonthNum*RankNum*ValidPrcpStationNum))
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
!          !PRINT *,'>>Change Directory Successful! next->'
!          GOTO 40
!          CASE DEFAULT
!          WRITE (errmsg,*) 'Error with code ', istatus
!          PAUSE
!        END SELECT
!40      CALL Progress % Set( N = ValidPrcpStationNum , L = 30 )!// StationNum�Σ���ʾ����30
!        Progress % M = ">" !// ����ɲ��ֵ��ַ������Ǳ���
!        Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
!        Progress % Prefix = "Read P R Data(P.LT.0.01):  "  !// ǰ����ʾ���֣����Ǳ���
!        DO i = 1,ValidPrcpStationNum
!          WRITE(Str,'(i11)') ValidPrcpStationCodesIndex(i)
!          lstatus = CHANGEDIRQQ(Str)
!          IF (lstatus) THEN
!            P_R_001RankData((i-1)*MonthNum*RankNum+1:i*MonthNum*RankNum,1) = ValidPrcpStationCodesIndex(i)
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
!        DO i = 1,MonthNum*RankNum*ValidPrcpStationNum
!          WRITE(fid,400)INT(P_R_001RankData(i,1),8),INT(P_R_001RankData(i,2),8),INT(P_R_001RankData(i,3),8),INT(P_R_001RankData(i,4),8),&
!              P_R_001RankData(i,5),P_R_001RankData(i,6),P_R_001RankData(i,7)
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
!        ALLOCATE(tempArray(MonthNum*RankNum*ValidPrcpStationNum))
!        tempArray = 0
!        WHERE((P_R_001RankData(:,7)/=DataNumNotEnough).AND.(P_R_001RankData(:,7)/=P_Inf).AND.(P_R_001RankData(:,7)/=R_Inf))
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
!        ALLOCATE(tempArray(MonthNum*RankNum*ValidPrcpStationNum))
!        tempArray = 0
!        WHERE((P_R_001RankData(:,7)/=DataNumNotEnough).AND.(P_R_001RankData(:,7)/=P_Inf).AND.(P_R_001RankData(:,7)/=R_Inf))
!          tempArray = P_R_001RankData(:,3)
!        END WHERE
!        CALL Unique(tempArray,ValidStationList)
!        IF(ValidStationList(1) == 0)THEN
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
!        !DEALLOCATE(ValidStationLocation)
!        PRINT *,'>>��ȡFactorCodes�ɹ�������ΪFactorCode_RankNum15_P.LT.0.01.dat�ļ�!'
!      END IF
!      !***********************************************************************************************************
!      !         statistic detail information of each predictable station in each month
!      !***********************************************************************************************************
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
!          WRITE(fid,400)INT(P_R_001RankData_Predictable(i,1),8),INT(P_R_001RankData_Predictable(i,2),8),INT(P_R_001RankData_Predictable(i,3),8),&
!                        INT(P_R_001RankData_Predictable(i,4),8),P_R_001RankData_Predictable(i,5:7)
!        END DO
!        CLOSE(fid)
!        PRINT *,'>>��ȡR_RankNum15_P.LT.0.01_Predicted�ļ���ɣ������棡'
!      END IF
!      
!      !***********************************************************************************************************
!      !      statistic predictable of each month of each station(1 is can be predicted, -1 is can't be predicted)
!      !***********************************************************************************************************
!      CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum�Σ���ʾ����30
!      Progress % M = ">" !// ����ɲ��ֵ��ַ������Ǳ���
!      Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
!      Progress % Prefix = "Statistic Each Month Predictable:  "  !// ǰ����ʾ���֣����Ǳ���
!      keyValue = -9.0
!      ALLOCATE(StudyStations_Month_Predictable(StudyStationNum001*MonthNum,3))   !ÿ��վ��ÿ�¿�Ԥ����
!      DO i = 1,StudyStationNum001
!        StudyStations_Month_Predictable((i-1)*MonthNum+1:i*MonthNum,1) = StudyStationCodes001(i)
!        StudyStations_Month_Predictable((i-1)*MonthNum+1:i*MonthNum,2) = (/(k,k = 1,MonthNum)/)
!        DO j = 1,MonthNum
!          tempLength = COUNT((P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+(j-1)*RankNum+1:(i-1)*MonthNum*RankNum+j*RankNum,7)/= DataNumNotEnough).AND.&
!                             (P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+(j-1)*RankNum+1:(i-1)*MonthNum*RankNum+j*RankNum,7)/= R_Inf).AND.&
!                              P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+(j-1)*RankNum+1:(i-1)*MonthNum*RankNum+j*RankNum,7)/= P_Inf)
!          IF (tempLength>0) THEN
!            StudyStations_Month_Predictable((i-1)*MonthNum+j,3) = 1
!          ELSE
!            StudyStations_Month_Predictable((i-1)*MonthNum+j,3) = -1
!          END IF
!        END DO
!        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
!      END DO
!      OPEN(fid,FILE = 'StudyStations_Month_Predictable.dat')
!      DO i = 1,StudyStationNum001
!        DO j = 1,MonthNum
!          WRITE(fid,'(I11,I3,I3)')INT(StudyStations_Month_Predictable((i-1)*MonthNum+j,1),8),INT(StudyStations_Month_Predictable((i-1)*MonthNum+j,2),8),&
!                                  INT(StudyStations_Month_Predictable((i-1)*MonthNum+j,3),8)
!        END DO
!      END DO
!      CLOSE(fid)
!      !*************************************************************************************************************************
!      !   read ghcn precipitation and temperature standard database
!      !*************************************************************************************************************************
!      YearLen = EndYear - StartYear + 1
!      PRINT *,">>����Prcp��׼���洢���ݿ�..."
!      CALL SLEEP(1)
!      PRINT *,'YerarLen:', YearLen
!      ALLOCATE(GhcnPrcpStandardDB(ValidPrcpStationNum,1+MonthNum*YearLen))
!      PRINT *,">>����Prcp��׼���洢���ݿ�ɹ���"
!      CALL SLEEP(1)
!      PRINT *,">>��ѯPrcp��׼���洢���ݿ������ļ��Ƿ����..."
!      CALL SLEEP(1)
!      INQUIRE(FILE = TRIM( PrcpDataPath)//'DataBase\GhcnPrcpStandardDB.dat', EXIST = alive)
!      IF(alive) THEN
!        PRINT *, ">>Prcp��׼���洢���ݿ������ļ����ڣ�"
!        CALL SLEEP(1)
!        PRINT *, ">>��ȡPrcp��׼���洢���ݿ������ļ���Ϣ..."
!        CALL SLEEP(1)
!        ALLOCATE(RealArrayTemp2D(1+MonthNum*YearLen,ValidPrcpStationNum))
!        OPEN(UNIT = fid,FILE = TRIM( PrcpDataPath)//'DataBase\GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
!          & POSITION = 'REWIND') !RECL = 300,
!        READ(fid,500) RealArrayTemp2D
!        GhcnPrcpStandardDB = TRANSPOSE(RealArrayTemp2D)
!        PRINT *, ">>��ȡPrcp��׼���洢���ݿ������ļ���Ϣ��ɣ�"
!        CALL SLEEP(1)
!        CLOSE(fid)
!        DEALLOCATE(RealArrayTemp2D)
!      END IF
!      
!      PRINT *,">>����Tavg��׼���洢���ݿ�..."
!      CALL SLEEP(1)
!      ALLOCATE(GhcnTavgStandardDB(ValidTavgStationNum,1+MonthNum*YearLen))
!      PRINT *,">>����Tavg��׼���洢���ݿ�ɹ���"
!      CALL SLEEP(1)
!      PRINT *,">>��ѯTavg��׼���洢���ݿ������ļ��Ƿ����..."
!      CALL SLEEP(1)
!      INQUIRE(FILE = TRIM( DataPath)//'DataBase\GhcnTavgStandardDB.dat', EXIST = alive)
!      IF(alive) THEN
!        PRINT *, ">>Tavg��׼���洢���ݿ������ļ����ڣ�"
!        CALL SLEEP(1)
!        PRINT *, ">>��ȡTavg��׼���洢���ݿ������ļ���Ϣ..."
!        CALL SLEEP(1)
!        ALLOCATE(RealArrayTemp2D(1+MonthNum*YearLen,ValidTavgStationNum))
!        OPEN(UNIT = fid,FILE = TRIM( DataPath)//'DataBase\GhcnTavgStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
!          & POSITION = 'REWIND') !RECL = 300,
!        READ(fid,900) RealArrayTemp2D
!        GhcnTavgStandardDB = TRANSPOSE(RealArrayTemp2D)
!        PRINT *, ">>��ȡTavg��׼���洢���ݿ������ļ���Ϣ��ɣ�"
!        CALL SLEEP(1)
!        CLOSE(fid)
!        DEALLOCATE(RealArrayTemp2D)
!      END IF
!      !*************************************************************************************************************************
!      ALLOCATE(FactorPrcp(YearLen*MonthNum))
!      ALLOCATE(StudyPrcp(YearLen*MonthNum))
!      ALLOCATE(Pk(MonthNum*RankNum*StudyStationNum001))
!      ALLOCATE(Pb(MonthNum*RankNum*StudyStationNum001))
!      ALLOCATE(tempPredictedPrcp(MonthNum*RankNum*StudyStationNum001,7))
!      CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum�Σ���ʾ����30
!      !!!!CALL progress % set( n = 1000 , l = 30 )!// stationnum�Σ���ʾ����30
!      Progress % M = ">" !// ����ɲ��ֵ��ַ������Ǳ���
!      Progress % O = " " !// δ��ɲ��ֵ��ַ������Ǳ���
!      Progress % Prefix = "Predicted AND Statistic Each Month:  "  !// ǰ����ʾ���֣����Ǳ���
!      tempPredictedPrcp = -999
!      Pk = -999
!      Pb = -999
!      KFNum = 10
!      PRINT *,'������Ԥ��ʱ���õ�RankNum��'
!      READ(*,*) k
!      PRINT *,k
!      !OPEN(fidd,FILE = 'records.dat')
!      DO i = 1,StudyStationNum001
!        DO j = 1,MonthNum
!          !DO k = 1,RankNum
!            tempLength = (i-1)*MonthNum*RankNum + (j-1)*RankNum + k !Ԥ��ʱ���õĳ�ʼ��
!            tempLen = (i-1)*MonthNum + j
!            tempPredictedPrcp(tempLen,1) = P_R_001RankData_Predictable(tempLength,1)
!            tempPredictedPrcp(tempLen,2) = P_R_001RankData_Predictable(tempLength,2)
!            tempPredictedPrcp(tempLen,3) = P_R_001RankData_Predictable(tempLength,3)
!            tempPredictedPrcp(tempLen,4) = P_R_001RankData_Predictable(tempLength,4)
!            !print *,'R2:'P_R_001RankData_Predictable(tempLength,5)
!            !pause
!            IF (P_R_001RankData_Predictable(tempLength,5) == 1) THEN   !R2����1
!              Pc = P_R_001RankData_Predictable(tempLength,1)
!              Pm = P_R_001RankData_Predictable(tempLength,2)
!              Fc = P_R_001RankData_Predictable(tempLength,3)
!              Fm = P_R_001RankData_Predictable(tempLength,4)
!              CALL StudyMonthAndFactorPreData_BT(Pc,Pm,Fc,Fm,YearLen,MonthNum,RankNum,ValidPrcpStationNum,ValidTavgStationNum,StartMonth,GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth
!              ALLOCATE(ValidStationLocation(COUNT(tempFactorTavgMonth>-9998.AND.tempStudyPrcpMonth>=0)))
!              tempNum = 0
!              DO jj = 1,SIZE(tempFactorTavgMonth)
!                IF(tempFactorTavgMonth(jj)>-9998.AND.tempStudyPrcpMonth(jj)>=0) THEN
!                  tempNum = tempNum + 1
!                  ValidStationLocation(tempNum) = jj
!                END IF
!              END DO
!
!              lstatus = CHANGEDIRQQ('R2.EQ.1')   !/�л�·��
!              lstatus = CHANGEDIRQQ('CoupledStationsPrcp')
!              WRITE(FileName(1:11),'(i11)'),INT(Pc,8)
!              WRITE(FileName(12:12),'(a)'),'_'
!              WRITE(FileName(13:14),'(i2)'),INT(Pm)
!              WRITE(FileName(15:15),'(a)'),'_'
!              WRITE(FileName(16:26),'(i11)'),INT(Fc,8)
!              WRITE(FileName(27:27),'(a)'),'_'
!              WRITE(FileName(28:29),'(i2)'),INT(Fm)
!              WRITE(FileName(30:33),'(a4)'),'.dat'
!              OPEN(fiddd,FILE = TRIM(FileName))
!              DO ik = 1,tempNum
!                WRITE(fiddd,'(f15.8,f15.8)'),tempFactorTavgMonth(ValidStationLocation(ik)),tempStudyPrcpMonth(ValidStationLocation(ik)) !Ԥ�����ݣ�GHCN�۲�����
!              END DO
!              CLOSE(fid)
!              istatus = CHDIR(TRIM(PredicteAndCheckDataPath))   ! �л�����Ŀ¼
!              DEALLOCATE(ValidStationLocation)
!              DEALLOCATE(tempFactorTavgMonth)
!              DEALLOCATE(tempStudyPrcpMonth)
!            ELSE IF ((P_R_001RankData_Predictable(tempLength,7) /= DataNumNotEnough).AND.(P_R_001RankData_Predictable(tempLength,7) /= R_Inf).AND.(P_R_001RankData_Predictable(tempLength,7) /= P_Inf)) THEN
!              !print *,'Enter IF:'
!              !pause
!              Pc = P_R_001RankData_Predictable(tempLength,1)
!              Pm = P_R_001RankData_Predictable(tempLength,2)
!              Fc = P_R_001RankData_Predictable(tempLength,3)
!              Fm = P_R_001RankData_Predictable(tempLength,4)
!              !print *,Pc,Pm,Fc,Fm
!              !pause
!              CALL StudyMonthAndFactorPreData_BT(Pc,Pm,Fc,Fm,YearLen,MonthNum,RankNum,ValidPrcpStationNum,ValidTavgStationNum,StartMonth,GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorTavgMonth,tempStudyPrcpMonth
!              
!              ALLOCATE(ValidStationLocation(COUNT(tempFactorTavgMonth>-9998.AND.tempStudyPrcpMonth>=0)))
!              tempNum = 0
!              DO jj = 1,SIZE(tempFactorTavgMonth)
!                IF(tempFactorTavgMonth(jj)>-9998.AND.tempStudyPrcpMonth(jj)>=0) THEN
!                  tempNum = tempNum + 1
!                  ValidStationLocation(tempNum) = jj
!                END IF
!              END DO
!              !==============================================================================================
!              !               �ж��Ƿ�������10��������ޱ仯����������10�������ޱ仯ʱ������
!              !==============================================================================================
!              !IF((isContinuityGT_M(tempFactorTavgMonth(ValidStationLocation),ClimateStatus) .EQ. .false.).AND.&
!              !   (isContinuityGT_M(tempStudyPrcpMonth(ValidStationLocation),ClimateStatus) .EQ. .false.)) THEN    !ֻ���㵱20������̬��û���������ޱ仯��ֵʱ��ֵ
!                !===========================================================
!                !           Simple LinearRegression
!                !===========================================================
!                !print *,'start LR:'
!                !pause
!                CALL LinearRegression(tempFactorTavgMonth(ValidStationLocation(1:tempNum - 1)),&
!                                      tempStudyPrcpMonth(ValidStationLocation(1:tempNum - 1)),tempNum - 1,&
!                                      Pk(tempLength),Pb(tempLength))
!                !tempPredictedPrcp(tempLen,1) = Pc
!                !tempPredictedPrcp(tempLen,2) = Pm
!                !tempPredictedPrcp(tempLen,3) = Fc
!                !tempPredictedPrcp(tempLen,4) = Fm
!                tempPredictedPrcp(tempLen,5) = Pk(tempLength)*tempFactorTavgMonth(ValidStationLocation(tempNum))+Pb(tempLength)
!                IF(tempPredictedPrcp(tempLen,5)<0) THEN
!                tempPredictedPrcp(tempLen,6) = 0
!                ELSE
!                  tempPredictedPrcp(tempLen,6) = tempPredictedPrcp(tempLen,5)
!                END IF
!                tempPredictedPrcp(tempLen,7) = tempStudyPrcpMonth(ValidStationLocation(tempNum))
!              !END IF
!              DEALLOCATE(ValidStationLocation)
!              DEALLOCATE(tempFactorTavgMonth)
!              DEALLOCATE(tempStudyPrcpMonth)
!            END IF
!          !END DO
!        END DO
!        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
!        !!!!CALL Progress % Put( i-8000 , CMD_PROGRESS_ABSOLUTE ) !// ���Է�ʽ
!      END DO
!      !CLOSE(fidd)
!      CLOSE(fiddd)
!      !=========================================================================================
!      PRINT *,'Writing Simple LinearRegression Results...'
!      OPEN(fid,FILE = 'SimpleLinear_LastOneData_P0.01_FirstRank_BP.dat')
!      DO ii = 1,StudyStationNum001*MonthNum
!        !IF(tempPredictedPrcp(ii,1)==-999.0) THEN
!        !  WRITE(fid,900),INT(tempPredictedPrcp(ii,1),8),-9,INT(tempPredictedPrcp(ii,3),8),-9,tempPredictedPrcp(ii,5:7)
!        !ELSE
!        WRITE(fid,1000),INT(tempPredictedPrcp(ii,1),8),INT(tempPredictedPrcp(ii,2),8),INT(tempPredictedPrcp(ii,3),8),&
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
!
!      WRITE(*,*)'istatus:', istatus
!      WRITE(*,*)'lstatus:', lstatus
!      WRITE(*,*)'ierror:',ierror
!
!      PRINT *,'�����������...'
!      pause
!      STOP
!8089  PRINT *,'�ļ���ȡ����'
!      PAUSE
!      
!
!    
!End SUBROUTINE PredictLastOneData_BT
