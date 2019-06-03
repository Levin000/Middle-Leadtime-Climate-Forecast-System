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
!      CHARACTER(LEN = 500) :: Path  !调试文件exe路径
!      CHARACTER(LEN = 500) :: GhcnPrcpFile
!      CHARACTER(LEN = 500) :: StationPRPath  !站点数据库目录
!      CHARACTER(LEN = 500) :: PredicteAndCheckDataPath  !分析数据库
!      CHARACTER(LEN = 500) :: DataPath  !数据库
!      CHARACTER(LEN = 500) :: PredictedPrcpPath !预测的降水和观测降水
!      CHARACTER(LEN = 500) :: PredictedPrcpPathOne
!      CHARACTER(LEN = 500) :: KFPredictedPrcpPath  !K-Flod 预测的降水和观测降水
!      CHARACTER(LEN = 500) :: StationInfoFile  !station信息文件
!      CHARACTER(LEN = 500) :: DirName,FileName
!      CHARACTER(LEN = 500) :: errmsg
!      CHARACTER(LEN = 11) :: Str
!      INTEGER :: GhcnPrcpColNum   !GHCNPrcp数据列数
!      INTEGER :: StationInfoFileLen  ! GHCN Station信息的数量
!      INTEGER :: ValidStationNum,StartMonth,MonthNum,RankNum, MonthIndex
!      INTEGER :: StartYear,EndYear,YearLen  !预报研究的开始年份、结束年份、时间长度
!      INTEGER :: FactorStationNum001,FactorStationNum005,FactorStationNum01,StudyStationNum001,PredictedStationNum005,PredictedStationNum01
!      INTEGER :: i,j,k,jj,ii,ik
!      INTEGER :: istatus = -9999,ilen = -9999,ierror = -9999,iosval
!      INTEGER :: tempNum,ClimateStatus
!      INTEGER :: KFNum    !Group Number of K-Flod Cross Check
!      INTEGER :: fid = 10,fidd = 55,fiddd = 555
!      INTEGER :: MissVal!缺省值处理（1为处理，0为不处理）
!      INTEGER :: TraceVal !痕迹降水的处理（1为处理，0为不处理）
!      INTEGER :: GhcnPrcpRowNum  ! GHCNPrcp数据的行数
!      INTEGER(KIND = 8) :: tempLength,tempLen
!      INTEGER(KIND = 8),ALLOCATABLE :: ValidStationCode(:)  !站点数量
!      INTEGER(KIND = 8),ALLOCATABLE :: ValidStationLocation(:)  !未计算
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
!      REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:),RealArrayTemp2D(:,:)  !GhcnPrcp存放格式标准化数据库
!      LOGICAL :: lstatus,lrelase,llstatus
!      LOGICAL :: alive001,alive005,alive01,alive
!      TYPE( CLS_CMD_Progress ) ::Progress  !进度条
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
!      DataPath = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\'  !数据存放路径（所有数据）
!      StationPRPath = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\StationList\' !Station数据P、R、R2的存放路径
!      PredictedPrcpPath = 'PredictedAndStudyPrcp'
!      PredictedPrcpPathOne = 'PredictedAndStudyPrcpOne'
!      KFPredictedPrcpPath = 'PredictedAndStudyPrcpKF'
!      StationInfoFile = TRIM(Path)//'\GhcnData\v2.prcp.inv'  !station信息文件
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
!      istatus = CHDIR(TRIM(DataPath))  ! 切换路径到数据库路径
!      PRINT *,">>读取研究时间段站点编号..."
!      CALL Inqire_Text_Row('ValidStationCodes.dat',LEN('ValidStationCodes.dat'),ValidStationNum)
!      ALLOCATE(ValidStationCode(ValidStationNum))
!      OPEN(fid,FILE = 'ValidStationCodes.dat')
!      READ(fid,'(i20)') ValidStationCode
!      CLOSE(fid)
!      PRINT *,">>读取研究时间段站点编号完成！"
!      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!      !                             Read GhcnPrcp Data
!      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!      PRINT *,'>>读取GHCN数据...'
!      CALL SLEEP(1)
!      CALL Inqire_Text_Row(TRIM(GhcnPrcpFile),LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum)
!      ALLOCATE(GhcnPrcp(GhcnPrcpRowNum, GhcnPrcpColNum)) !动态分配GHCN原始降雨数据库大小
!      CALL Read_GHCN_Prcp(TRIM(GhcnPrcpFile),LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum, GhcnPrcpColNum, MissVal, TraceVal)!读取GHCN降水数据
!      PRINT *,'>>读取GHCN数据完成！'
!      CALL SLEEP(1)
!      !****************************************************************************
!      ! !                Read P_GT_0.01 Station Data
!      !****************************************************************************
!      ilen = 47
!      ALLOCATE(P_R_001RankData(MonthNum*RankNum*ValidStationNum,7))
!      ALLOCATE(tempPR001(7,MonthNum*RankNum*ValidStationNum))
!      istatus = CHDIR(TRIM(PredicteAndCheckDataPath))  ! 切换路径到数据库路径
!      INQUIRE(FILE = 'R_RankNum15_P.LT.0.01.dat', EXIST = alive001)
!      IF (alive001) THEN
!        PRINT *,'>>读取PValue<0.01数据...'
!        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01.dat')
!        READ(fid,300) tempPR001
!        CLOSE(fid)
!        P_R_001RankData = TRANSPOSE(tempPR001)
!        PRINT *,'>>读取PValue<0.01数据完成！'
!      ELSE
!        PRINT *,TRIM(PredicteAndCheckDataPath)//'R_RankNum15_P.LT.0.01.dat数据不存在...'
!        ALLOCATE(tempPR(5,MonthNum*RankNum))
!        PRINT *,'>>读取研究时间段站点P、R信息...'
!        istatus = CHDIR(TRIM(StationPRPath))  !切换数据库路径
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
!40      CALL Progress % Set( N = ValidStationNum , L = 30 )!// StationNum次，显示长度30
!        Progress % M = ">" !// 已完成部分的字符，不是必须
!        Progress % O = " " !// 未完成部分的字符，不是必须
!        Progress % Prefix = "Read P R Data(P.LT.0.01):  "  !// 前方提示文字，不是必须
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
!            PRINT *,">>切换站点数据库失败,站点编号："//Str
!            PAUSE
!          END IF
!          CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
!          istatus = CHDIR(TRIM(StationPRPath))
!          IF (istatus.NE.0) THEN
!            PRINT *,">>切换站点数据库列表文件夹失败！"
!            PAUSE
!          END IF
!        END DO
!        istatus = CHDIR(TRIM(PredicteAndCheckDataPath))  ! 切换路径到数据库路径
!        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01.dat')
!        DO i = 1,MonthNum*RankNum*ValidStationNum
!          WRITE(fid,400)INT(P_R_001RankData(i,1),8),INT(P_R_001RankData(i,2),8),INT(P_R_001RankData(i,3),8),INT(P_R_001RankData(i,4),8),&
!                            P_R_001RankData(i,5),P_R_001RankData(i,6),P_R_001RankData(i,7)
!        END DO
!        CLOSE(fid)
!        PRINT *,'>>读取研究时间段站点P、R信息完成！'
!      END IF
!      !*************************************************************************************************
!      ! !                分析可预报站点与预报站点的信息--Factor\Predicted站点编号及数量（通过 P = 0.01）
!      !*************************************************************************************************
!      INQUIRE(FILE = 'StudyCode_RankNum15_P.LT.0.01.dat', EXIST = alive001)
!      IF (alive001) THEN
!        PRINT *,'>>StudyCode_RankNum15_P.LT.0.01文件存在，读取文件...'
!        CALL Inqire_Text_Row('StudyCode_RankNum15_P.LT.0.01.dat',LEN('StudyCode_RankNum15_P.LT.0.01.dat'),tempNum)
!        ALLOCATE(StudyStationCodes001(tempNum))
!        OPEN(fid,FILE = 'StudyCode_RankNum15_P.LT.0.01.dat')
!        READ(fid,'(f15.0)')StudyStationCodes001
!        CLOSE(fid)
!        StudyStationNum001 = SIZE(StudyStationCodes001)
!        PRINT *,'StudyStationNum: ',StudyStationNum001
!        PRINT *,'>>读取StudyCode_RankNum15_P.LT.0.01文件完成!'
!      ELSE
!        PRINT *,'>>StudyCode_RankNum15_P.LT.0.01文件不存在，获取StudyCodes...'
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
!        PRINT *,'>>获取StudyCodes成功，保存为StudyCode_RankNum15_P.LT.0.01.dat文件!'
!      END IF
!      
!      INQUIRE(FILE = 'FactorCode_RankNum15_P.LT.0.01.dat', EXIST = alive001)
!      IF (alive001) THEN
!        PRINT *,'>>FactorCode_RankNum15_P.LT.0.01文件存在，读取文件...'
!        CALL Inqire_Text_Row('FactorCode_RankNum15_P.LT.0.01.dat',LEN('FactorCode_RankNum15_P.LT.0.01.dat'),tempNum)
!        ALLOCATE(FactorStationCodes001(tempNum))
!        OPEN(fid,FILE = 'FactorCode_RankNum15_P.LT.0.01.dat')
!        READ(fid,'(f15.0)')FactorStationCodes001
!        CLOSE(fid)
!        FactorStationNum001 = SIZE(FactorStationCodes001)
!        PRINT *,'FactorStationNum: ',FactorStationNum001
!        PRINT *,'>>读取FactorCode_RankNum15_P.LT.0.01文件完成!'
!      ELSE
!        PRINT *,'>>FactorCode_RankNum15_P.LT.0.01文件不存在，获取FactorCodes...'
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
!        PRINT *,'>>获取FactorCodes成功，保存为FactorCode_RankNum15_P.LT.0.01.dat文件!'
!      END IF
!      !***********************************************************************************************************
!      ! get and save the information of station which can be predicted 
!      ALLOCATE(P_R_001RankData_Predictable(StudyStationNum001*MonthNum*RankNum,7))
!      INQUIRE(FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat', EXIST = alive001)
!      IF (alive001) THEN
!        PRINT *,'>>R_RankNum15_P.LT.0.01_Predicted文件存在，读取文件...'
!        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat')
!        DO i = 1,StudyStationNum001*MonthNum*RankNum
!          READ(fid,300)P_R_001RankData_Predictable(i,:)
!        END DO
!        CLOSE(fid)
!        PRINT *,'>>读取R_RankNum15_P.LT.0.01_Predicted文件完成！'
!      ELSE
!        PRINT *,'>>R_RankNum15_P.LT.0.01_Predicted文件不存在,提取相关信息...'
!        ALLOCATE(ValidStationLocation(MonthNum*RankNum))
!        CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum次，显示长度30
!        Progress % M = ">" !// 已完成部分的字符，不是必须
!        Progress % O = " " !// 未完成部分的字符，不是必须
!        Progress % Prefix = "Build DataBase of P_R_001RankData_Predictable:  "  !// 前方提示文字，不是必须
!        DO i = 1,StudyStationNum001
!          ValidStationLocation = ArrayFind(P_R_001RankData(:,1),'=',StudyStationCodes001(i))
!          P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+1:i*MonthNum*RankNum,:)=P_R_001RankData(ValidStationLocation,:)
!          CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
!        END DO
!        DEALLOCATE(ValidStationLocation)
!        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat')
!        DO i = 1,StudyStationNum001*MonthNUM*RankNum
!          WRITE(fid,400)INT(P_R_001RankData_Predictable(i,1),8),INT(P_R_001RankData_Predictable(i,2),8),&
!            INT(P_R_001RankData_Predictable(i,3),8),INT(P_R_001RankData_Predictable(i,4),8),P_R_001RankData_Predictable(i,5:7)
!        END DO
!        CLOSE(fid)
!        PRINT *,'>>提取R_RankNum15_P.LT.0.01_Predicted文件完成，并保存！'
!      END IF
!      !*************************************************************************************************************************
!      !
!      istatus = CHDIR(TRIM(DataPath))!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      YearLen = EndYear - StartYear + 1
!      PRINT *,">>建立标准化存储数据库..."
!      CALL SLEEP(1)
!      ALLOCATE(GhcnPrcpStandardDB(ValidStationNum,1+MonthNum*YearLen))
!      GhcnPrcpStandardDB(:,2:) = -9998  !标准数据库中，原数据库没有的数据，全部设置为-9998，缺测为-9999，痕迹为-8888
!      PRINT *,">>建立标准化存储数据库成功！"
!      CALL SLEEP(1)
!      PRINT *,">>查询标准化存储数据库数据文件是否存在..."
!      CALL SLEEP(1)
!      INQUIRE(FILE = 'GhcnPrcpStandardDB.dat', EXIST = alive)
!      IF(alive) THEN
!        PRINT *, ">>标准化存储数据库数据文件存在！"
!        CALL SLEEP(1)
!        PRINT *, ">>读取标准化存储数据库数据文件信息..."
!        CALL SLEEP(1)
!        ALLOCATE(RealArrayTemp2D(1+MonthNum*YearLen,ValidStationNum))
!        OPEN(UNIT = fid,FILE = 'GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
!          & POSITION = 'REWIND') !RECL = 300,
!        READ(fid,500) RealArrayTemp2D
!        GhcnPrcpStandardDB = TRANSPOSE(RealArrayTemp2D)
!        PRINT *, ">>读取标准化存储数据库数据文件信息完成！"
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
!      CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum次，显示长度30
!      Progress % M = ">" !// 已完成部分的字符，不是必须
!      Progress % O = " " !// 未完成部分的字符，不是必须
!      Progress % Prefix = "Predicted and Statistic Each Month:  "  !// 前方提示文字，不是必须
!      tempPredictedPrcp = -999
!      Pk = -999
!      Pb = -999
!      KFNum = 10
!      PRINT *,'请输入预报时所用的RankNum：'
!      READ(*,*) k
!      PRINT *,k
!      DO i = 1,StudyStationNum001
!        DO j = 1,MonthNum
!          !PAUSE
!          !DO k = 1,1  !RankNum
!          tempLength = (i-1)*MonthNum*RankNum + (j-1)*RankNum + k !预报是采用的初始量
!          tempLen = (i-1)*MonthNum + j
!          tempPredictedPrcp(tempLen,1) = P_R_001RankData_Predictable(tempLength,1)
!          tempPredictedPrcp(tempLen,2) = P_R_001RankData_Predictable(tempLength,2)
!          tempPredictedPrcp(tempLen,3) = P_R_001RankData_Predictable(tempLength,3)
!          tempPredictedPrcp(tempLen,4) = P_R_001RankData_Predictable(tempLength,4)
!          IF (P_R_001RankData_Predictable(tempLength,5) == 1) THEN   !R2不等于1
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
!            !               判断是否有连续20年的数据无变化存在连续的20年数据无变化时舍弃，
!            !==============================================================================================
!            !IF((isContinuityGT_M(tempFactorPrcpMonth(ValidStationLocation),ClimateStatus) .EQ. .false.).AND. &
!            !   (isContinuityGT_M(tempStudyPrcpMonth(ValidStationLocation),ClimateStatus) .EQ. .false.)) THEN    !只计算当20年气候态内没有连续的无变化的值时的值
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
!        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
!        !!!!CALL Progress % Put( i-8000 , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
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
!      istatus = CHDIR(TRIM(Path))  ! 切换路径到主目录
!      !***************************************************************************************************************************************
!      
!      PRINT *,'按任意键继续...'
!      PAUSE
!      STOP
!8089  PRINT *,'文件读取错误'
!      PAUSE
!      
!END SUBROUTINE PredictLastOneData_BP