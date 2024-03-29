SUBROUTINE PredicteAndCheckSystem_BP
      !!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"PredicteAndCheckSystem_BP" :: PredicteAndCheckSystem_BP
      USE, INTRINSIC :: IEEE_ARITHMETIC
      USE IFPOSIX
      USE IFPORT
      USE global
      USE cmdProgress
      USE ghcnDefType
      USE ghcnPort
      USE arrayPort
      USE statisticPort
      IMPLICIT NONE

      CHARACTER(LEN = 500) :: Path  !调试文件exe路径
      CHARACTER(LEN = 500) :: GhcnPrcpFile
      CHARACTER(LEN = 500) :: StationPRPath  !站点数据库目录
      CHARACTER(LEN = 500) :: PredicteAndCheckDataPath  !分析数据库
      CHARACTER(LEN = 500) :: DataPath  !数据库
      CHARACTER(LEN = 500) :: PredictedPrcpPath !预测的降水和观测降水
      CHARACTER(LEN = 500) :: KFPredictedPrcpPath  !K-Flod 预测的降水和观测降水
      CHARACTER(LEN = 500) :: StationInfoFile  !station信息文件
      CHARACTER(LEN = 500) :: DirName,FileName
      CHARACTER(LEN = 500) :: errmsg
      CHARACTER(LEN = 11) :: Str
      INTEGER :: GhcnPrcpColNum  !GHCNPrcp数据列数
      INTEGER :: StationInfoFileLen  ! GHCN Station信息的数量
      INTEGER :: ValidStationNum,StartMonth ,MonthNum ,RankNum , MonthIndex
      integer :: StartYear,EndYear,YearLen  !预报研究的开始年份、结束年份、时间长度
      INTEGER :: FactorStationNum001,FactorStationNum005,FactorStationNum01,StudyStationNum001,PredictedStationNum005,PredictedStationNum01
      INTEGER :: i,j,k,jj,ii,ik
      INTEGER :: istatus = -9999,ilen = -9999,ierror = -9999,iosval
      INTEGER :: tempNum,ClimateStatus
      INTEGER :: KFNum    !Group Number of K-Flod Cross Check
      INTEGER :: fid = 10,fidd = 55,fiddd = 555
      INTEGER :: MissVal !缺省值处理（1为处理，0为不处理）
      INTEGER :: TraceVal !痕迹降水的处理（1为处理，0为不处理）
      INTEGER :: GhcnPrcpRowNum  ! GHCNPrcp数据的行数
      INTEGER(KIND = 8),ALLOCATABLE :: ValidStationCode(:)  !站点数量
      INTEGER(KIND = 8),ALLOCATABLE :: ValidStationLocation(:)  !未计算
      INTEGER(KIND = 8) :: tempLength
      REAL(KIND = 8) :: DataNumNotEnough = -9.0,P_Inf = -6.0,R_Inf = -5.0,Trash = -7   !R_EQ_NAN = -8.0,
      REAL(KIND = 8),ALLOCATABLE :: ValidStationList(:)
      REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData(:,:)
      REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData_Predictable(:,:)
      REAL(KIND = 8),ALLOCATABLE :: FactorStationCodes001(:),StudyStationCodes001(:)
      REAL(KIND = 8),ALLOCATABLE :: P_EQ_1_Codes(:)
      REAL(KIND = 8),ALLOCATABLE :: StudyStations_Month_Predictable(:,:)
      REAL(KIND = 8),ALLOCATABLE :: tempArray(:),tempPR001(:,:)
      REAL(KIND = 8) :: keyValue ,TrainingRate
      REAL(KIND = 8),ALLOCATABLE :: tempPR(:,:)
      REAL(KIND = 8), ALLOCATABLE :: FactorPrcp(:),StudyPrcp(:)
      REAL(KIND = 8), ALLOCATABLE :: tempFactorPrcp(:),tempStudyPrcp(:)
      REAL(KIND = 8),ALLOCATABLE :: tempPredictedPrcp(:),tempPredictedPrcpCP(:),tempKFPredictedPrcp(:),tempKFPredictedPrcpCp(:),X_Value
      REAL(KIND = 8) :: Pc,Pm,Fc,Fm
      REAL,ALLOCATABLE :: R(:),P(:),KFR(:),KFP(:)
      REAL(KIND = 8), ALLOCATABLE :: Pk(:),Pb(:)
      REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:),RealArrayTemp2D(:,:)  !GhcnPrcp存放格式标准化数据库
      LOGICAL :: lstatus,lrelase,llstatus
      LOGICAL :: alive001,alive005,alive01,alive
      TYPE( CLS_CMD_Progress ) ::Progress  !进度条
    
      NAMELIST /PACSBP/ GhcnPrcpColNum,StartMonth,MonthNum,RankNum,StartYear,EndYear,ClimateStatus,MissVal,TraceVal,TrainingRate
    
100   FORMAT(a60:,i)
200   FORMAT(f15.0,f10.0,3f6.2)
300   FORMAT(f13.0,f3.0,f13.0,f3.0,3f6.2)
400   FORMAT(i13,i3,i13,i3,3f6.2)
500   FORMAT(f20.0,1320f10.1)
600   FORMAT(i13,i3,i13,i3,3f15.8,3f15.8,2f30.5)
700   FORMAT(f10.3,f10.3,f15.3)
800   FORMAT(i13,i3,i13,i3,3f15.8,3f15.8)
    
      istatus = GETCWD(Path)
      GhcnPrcpFile = TRIM(Path)//'\GhcnData\v2.prcp.dat'
      PredicteAndCheckDataPath = TRIM(Path)//'\BaseOnPrcp\PredicteAndCheck\'
      DataPath = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\'  !数据存放路径（所有数据）
      StationPRPath = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\StationList\' !Station数据P、R、R2的存放路径
      PredictedPrcpPath = 'PredictedAndStudyPrcp'
      KFPredictedPrcpPath = 'PredictedAndStudyPrcpKF'
      StationInfoFile = TRIM(Path)//'\GhcnData\v2.prcp.inv'  !station信息文件
      istatus = CHDIR(TRIM(PredicteAndCheckDataPath))
      OPEN(UNIT = fid,FILE = './PredicteAndCheckSystem_BP.NAMELIST')
      READ (fid,NML = PACSBP,ERR = 8089)
      CLOSE(fid)
      PRINT *,'Main parameters as below:'
      WRITE (*,NML = PACSBP)
    
      !****************************************************************************
      ! !                 Read ValidStationCode
      !****************************************************************************
      istatus = CHDIR(TRIM(DataPath))  ! 切换路径到数据库路径
      !lstatus = CHANGEDIRQQ('DataBase')  !切换到标准数据库（）
      PRINT *,">>读取有效站点编号..."
      CALL Inqire_Text_Row('ValidStationCodes.dat',LEN('ValidStationCodes.dat'),ValidStationNum)
      ALLOCATE(ValidStationCode(ValidStationNum))
      OPEN(fid,FILE = 'ValidStationCodes.dat')
      READ(fid,'(i20)') ValidStationCode
      CLOSE(fid)
      PRINT *,">>读取有效站点编号完成！"
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !                             Read GhcnPrcp Data
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PRINT *,'>>读取GHCN数据...'
      CALL SLEEP(1)
      CALL Inqire_Text_Row(TRIM(GhcnPrcpFile),LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum)
      ALLOCATE(GhcnPrcp(GhcnPrcpRowNum, GhcnPrcpColNum)) !动态分配GHCN原始降雨数据库大小
      CALL Read_GHCN_Prcp(TRIM(GhcnPrcpFile),LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum, GhcnPrcpColNum, MissVal, TraceVal)!读取GHCN降水数据
      PRINT *,'>>读取GHCN数据完成！'
      CALL SLEEP(1)
      !****************************************************************************
      ! !                Read P_GT_0.01 0.05 0.1 Station Data
      !****************************************************************************
      ilen = 47
      ALLOCATE(P_R_001RankData(MonthNum*RankNum*ValidStationNum,7))
      ALLOCATE(tempPR001(7,MonthNum*RankNum*ValidStationNum))
      istatus = CHDIR(TRIM(PredicteAndCheckDataPath))  ! 切换路径到数据库路径
      INQUIRE(FILE = 'R_RankNum15_P.LT.0.01.dat', EXIST = alive001)
      IF (alive001) THEN
        PRINT *,'>>读取PValue<0.01数据...'
        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01.dat')
        READ(fid,300) tempPR001
        CLOSE(fid)
        P_R_001RankData = TRANSPOSE(tempPR001)
        PRINT *,'>>读取PValue<0.01数据完成！'
      ELSE
        PRINT *,TRIM(PredicteAndCheckDataPath)//'R_RankNum15_P.LT.0.01.dat数据不存在...'
        ALLOCATE(tempPR(5,MonthNum*RankNum))
        PRINT *,'>>读取可预报站点P、R信息...'
        istatus = CHDIR(TRIM(StationPRPath))  !切换数据库路径
        SELECT CASE (istatus)
        CASE (2)  ! ENOENT
          errmsg = 'The directory '//TRIM(StationPRPath)//' does not exist'
          PAUSE
        CASE (20) ! ENOTDIR
          errmsg = TRIM(StationPRPath)//' is not a directory'
          PAUSE
        CASE (0) ! NO error
          !PRINT *,'>>Change Directory Successful! next->'
          !PAUSE
          goto 40
          CASE DEFAULT
          WRITE (errmsg,*) 'Error with code ', istatus
          PAUSE
        END SELECT
40      CALL Progress % Set( N = ValidStationNum , L = 30 )!// StationNum次，显示长度30
        Progress % M = ">" !// 已完成部分的字符，不是必须
        Progress % O = " " !// 未完成部分的字符，不是必须
        Progress % Prefix = "Read P R Data(P.GT.0.01):  "  !// 前方提示文字，不是必须
        DO i = 1,ValidStationNum
          WRITE(Str,'(i11)') ValidStationCode(i)
          lstatus = CHANGEDIRQQ(Str)
          IF (lstatus) THEN
!88882       PRINT *,i,Str
!88883       PAUSE
!88884       istatus = getcwd(DirName)
!88885       PRINT *,TRIM(DirName)
!88886       PAUSE
            P_R_001RankData((i-1)*MonthNum*RankNum+1:i*MonthNum*RankNum,1) = ValidStationCode(i)
            DO j = 1,MonthNum
              P_R_001RankData((i-1)*MonthNum*RankNum+(j-1)*RankNum+1:(i-1)*MonthNum*RankNum+j*RankNum,2) = j
            END DO
            OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01.dat')
            READ(fid,200) tempPR
            CLOSE(fid)
            P_R_001RankData((i-1)*MonthNum*RankNum+1:i*MonthNum*RankNum,3:) = TRANSPOSE(tempPR)
          ELSE
            PRINT *,">>切换站点数据库失败,站点编号："//Str
            PAUSE
          END IF
          CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
          istatus = CHDIR(TRIM(StationPRPath))
          IF (istatus.NE.0) THEN
            PRINT *,">>切换站点数据库列表文件夹失败！"
            PAUSE
          END IF
        END DO
        istatus = CHDIR(TRIM(PredicteAndCheckDataPath))  ! 切换路径到数据库路径
        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01.dat')
        DO i = 1,MonthNum*RankNum*ValidStationNum
          WRITE(fid,400)INT(P_R_001RankData(i,1),8),INT(P_R_001RankData(i,2),8),INT(P_R_001RankData(i,3),8),&
            INT(P_R_001RankData(i,4),8),P_R_001RankData(i,5),P_R_001RankData(i,6),P_R_001RankData(i,7)
        END DO
        CLOSE(fid)
        PRINT *,'>>读取可预报站点P、R信息完成！'
      END IF

      !*************************************************************************************************
      ! !                分析可预报站点与预报站点的信息--Factor\Predicted站点编号及数量（通过 P = 0.01）
      !*************************************************************************************************
      INQUIRE(FILE = 'FactorCode_RankNum15_P.LT.0.01.dat', EXIST = alive001)
      IF (alive001) THEN
        PRINT *,'>>FactorCode_RankNum15_P.LT.0.01文件存在，读取文件...'
        CALL Inqire_Text_Row('FactorCode_RankNum15_P.LT.0.01.dat',LEN('FactorCode_RankNum15_P.LT.0.01.dat'),tempNum)
        ALLOCATE(FactorStationCodes001(tempNum))
        OPEN(fid,FILE = 'FactorCode_RankNum15_P.LT.0.01.dat')
        READ(fid,'(f15.0)')FactorStationCodes001
        CLOSE(fid)
        FactorStationNum001 = SIZE(FactorStationCodes001)
        PRINT *,'FactorStationNum: ',FactorStationNum001
        PRINT *,'>>读取FactorCode_RankNum15_P.LT.0.01文件完成!'
      ELSE
        PRINT *,'>>FactorCode_RankNum15_P.LT.0.01文件不存在，获取FactorCodes...'
        ALLOCATE(tempArray(MonthNum*RankNum*ValidStationNum))
        tempArray = 0
        WHERE((P_R_001RankData(:,7)/=DataNumNotEnough).AND.(P_R_001RankData(:,7)/=P_Inf).AND.(P_R_001RankData(:,7)/=R_Inf)) !.AND.(P_R_001RankData(:,7)/=R_EQ_NAN).AND.(P_R_001RankData(:,7)/=P_OV_PV)
          tempArray = P_R_001RankData(:,3)
        END WHERE
        CALL Unique(tempArray,ValidStationList)
        IF(ValidStationList(1) == 0)THEN
          ALLOCATE(FactorStationCodes001(SIZE(ValidStationList)-1))
          FactorStationCodes001 = ValidStationList(2:)
          FactorStationNum001 = SIZE(FactorStationCodes001)
        ELSE
          ALLOCATE(FactorStationCodes001(SIZE(ValidStationList)))
          FactorStationCodes001 = ValidStationList
          FactorStationNum001 = SIZE(FactorStationCodes001)
        END IF
        PRINT *,'FactorStationNum: ',FactorStationNum001
        OPEN(fid,FILE = 'FactorCode_RankNum15_P.LT.0.01.dat')
        WRITE(fid,'(i15)')INT(FactorStationCodes001,8)
        CLOSE(fid)
        DEALLOCATE(tempArray)
        DEALLOCATE(ValidStationList)
        PRINT *,'>>获取FactorCodes成功，保存为FactorCode_RankNum15_P.LT.0.01.dat文件!'
      END IF
      
      INQUIRE(FILE = 'StudyCode_RankNum15_P.LT.0.01.dat', EXIST = alive001)
      IF (alive001) THEN
        PRINT *,'>>StudyCode_RankNum15_P.LT.0.01文件存在，读取文件...'
        CALL Inqire_Text_Row('StudyCode_RankNum15_P.LT.0.01.dat',LEN('StudyCode_RankNum15_P.LT.0.01.dat'),tempNum)
        ALLOCATE(StudyStationCodes001(tempNum))
        OPEN(fid,FILE = 'StudyCode_RankNum15_P.LT.0.01.dat')
        READ(fid,'(f15.0)')StudyStationCodes001
        CLOSE(fid)
        StudyStationNum001 = SIZE(StudyStationCodes001)
        PRINT *,'StudyStationNum: ',StudyStationNum001
        PRINT *,'>>读取StudyCode_RankNum15_P.LT.0.01文件完成!'
      ELSE
        PRINT *,'>>StudyCode_RankNum15_P.LT.0.01文件不存在，获取StudyCodes...'
        ALLOCATE(tempArray(MonthNum*RankNum*ValidStationNum))
        tempArray = 0
        WHERE((P_R_001RankData(:,7)/=DataNumNotEnough).AND.(P_R_001RankData(:,7)/=P_Inf).AND.(P_R_001RankData(:,7)/=R_Inf))  !.AND.(P_R_001RankData(:,7)/=R_EQ_NAN).AND.(P_R_001RankData(:,7)/=P_OV_PV)
          tempArray = P_R_001RankData(:,1)
        END WHERE
        CALL Unique(tempArray,ValidStationList)
        IF(ValidStationList(1) == 0)THEN
          ALLOCATE(StudyStationCodes001(SIZE(ValidStationList)-1))
          StudyStationCodes001 = ValidStationList(2:)
          StudyStationNum001 = SIZE(StudyStationCodes001)
        ELSE
          ALLOCATE(StudyStationCodes001(SIZE(ValidStationList)))
          StudyStationCodes001 = ValidStationList
          StudyStationNum001 = SIZE(StudyStationCodes001)
        END IF
        PRINT *,'StudyStationNum: ',StudyStationNum001
        OPEN(fid,FILE = 'StudyCode_RankNum15_P.LT.0.01.dat')
        WRITE(fid,'(i15)')INT(StudyStationCodes001,8)
        CLOSE(fid)
        DEALLOCATE(tempArray)
        DEALLOCATE(ValidStationList)
        PRINT *,'>>获取StudyCodes成功，保存为StudyCode_RankNum15_P.LT.0.01.dat文件!'
      END IF
      !***********************************************************************************************************
      ! get AND save R2 eq 1 codes
      keyValue = 1.0  !P Value = 1
      tempNum = COUNT(P_R_001RankData(:,5)==keyValue)
      IF (tempNum.GT.0) THEN
        PRINT *,'>>处理R2=1的站点，提取并保存站点号...'
        lstatus = CHANGEDIRQQ('R2.EQ.1')
        ALLOCATE(ValidStationLocation(tempNum))
        ValidStationLocation = ArrayFind(P_R_001RankData(:,5),'=',keyValue)
        CALL Unique(P_R_001RankData(ValidStationLocation,1),P_EQ_1_Codes)
        OPEN(fid,FILE = 'R2_EQ_1_PredictedCodes.dat')
        WRITE(fid,'(i15)')INT(P_EQ_1_Codes,8)
        CLOSE(fid)
        OPEN(fid,FILE = 'R2_EQ_1_R_RankNum15_P.LT.0.01.dat')
        DO i = 1,SIZE(ValidStationLocation)
          WRITE(fid,400)INT(P_R_001RankData(ValidStationLocation(i),1),8),INT(P_R_001RankData(ValidStationLocation(i),2),8),&
            INT(P_R_001RankData(ValidStationLocation(i),3),8),INT(P_R_001RankData(ValidStationLocation(i),4),8),&
            P_R_001RankData(ValidStationLocation(i),5:7)
        END DO
        CLOSE(fid)
        istatus = CHDIR(TRIM(PredicteAndCheckDataPath))
        DEALLOCATE(ValidStationLocation)
        PRINT *,'>>处理R2=1的站点，提取并保存站点号完成！'
      END IF
      !***********************************************************************************************************
      ! get and save the information of station which can be predicted 
      ALLOCATE(P_R_001RankData_Predictable(StudyStationNum001*MonthNum*RankNum,7))
      INQUIRE(FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat', EXIST = alive001)
      IF (alive001) THEN
        PRINT *,'>>R_RankNum15_P.LT.0.01_Predicted文件存在，读取文件...'
        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat')
        DO i = 1,StudyStationNum001*MonthNum*RankNum
          READ(fid,300)P_R_001RankData_Predictable(i,:)
        END DO
        CLOSE(fid)
        PRINT *,'>>读取R_RankNum15_P.LT.0.01_Predicted文件完成！'
      ELSE
        PRINT *,'>>R_RankNum15_P.LT.0.01_Predicted文件不存在,提取相关信息...'
        ALLOCATE(ValidStationLocation(MonthNum*RankNum))
        CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum次，显示长度30
        Progress % M = ">" !// 已完成部分的字符，不是必须
        Progress % O = " " !// 未完成部分的字符，不是必须
        Progress % Prefix = "Build DataBase of P_R_001RankData_Predictable:  "  !// 前方提示文字，不是必须
        DO i = 1,StudyStationNum001
          ValidStationLocation = ArrayFind(P_R_001RankData(:,1),'=',StudyStationCodes001(i))
          P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+1:i*MonthNum*RankNum,:)=P_R_001RankData(ValidStationLocation,:)
          CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
        END DO
        DEALLOCATE(ValidStationLocation)
        OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat')
        DO i = 1,StudyStationNum001*MonthNUM*RankNum
          WRITE(fid,400)INT(P_R_001RankData_Predictable(i,1),8),INT(P_R_001RankData_Predictable(i,2),8),INT(P_R_001RankData_Predictable(i,3),8),&
            INT(P_R_001RankData_Predictable(i,4),8),P_R_001RankData_Predictable(i,5:7)
        END DO
        CLOSE(fid)
        PRINT *,'>>提取R_RankNum15_P.LT.0.01_Predicted文件完成，并保存！'
      END IF
!88883 PAUSE
      !***********************************************************************************************************
      !   statistic predictable of each month of each station(1 is can be predicted, -1 is can't be predicted)
      !***********************************************************************************************************
      CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum次，显示长度30
      Progress % M = ">" !// 已完成部分的字符，不是必须
      Progress % O = " " !// 未完成部分的字符，不是必须
      Progress % Prefix = "Statistic Each Month Predictable:  "  !// 前方提示文字，不是必须
      keyValue = -9.0
      ALLOCATE(StudyStations_Month_Predictable(StudyStationNum001*MonthNum,3))   !每个站点每月可预报性
      DO i = 1,StudyStationNum001
        StudyStations_Month_Predictable((i-1)*MonthNum+1:i*MonthNum,1) = StudyStationCodes001(i)
        StudyStations_Month_Predictable((i-1)*MonthNum+1:i*MonthNum,2) = (/(k,k = 1,MonthNum)/)
        DO j = 1,MonthNum
          tempLength = COUNT((P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+(j-1)*RankNum+1:(i-1)*MonthNum*RankNum+j*RankNum,7)/= DataNumNotEnough).AND.&
            (P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+(j-1)*RankNum+1:(i-1)*MonthNum*RankNum+j*RankNum,7)/= R_Inf).AND.&
            (P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+(j-1)*RankNum+1:(i-1)*MonthNum*RankNum+j*RankNum,7)/= P_Inf))
          IF (tempLength>0) THEN
            StudyStations_Month_Predictable((i-1)*MonthNum+j,3) = 1
          ELSE
            StudyStations_Month_Predictable((i-1)*MonthNum+j,3) = -1
          END IF
        END DO
        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
      END DO
      OPEN(fid,FILE = 'StudyStations_Month_Predictable.dat')
      DO i = 1,StudyStationNum001
        DO j = 1,MonthNum
          WRITE(fid,'(I11,I3,I3)')INT(StudyStations_Month_Predictable((i-1)*MonthNum+j,1),8),&
            INT(StudyStations_Month_Predictable((i-1)*MonthNum+j,2),8),INT(StudyStations_Month_Predictable((i-1)*MonthNum+j,3),8)
        END DO
      END DO
      CLOSE(fid)
      !*************************************************************************************************************************
      ! read ghcn precipitation standard database
      !*************************************************************************************************************************
      istatus = CHDIR(TRIM(DataPath))!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      YearLen = EndYear - StartYear + 1
      PRINT *,">>建立标准化存储数据库..."
      CALL SLEEP(1)
      ALLOCATE(GhcnPrcpStandardDB(ValidStationNum,1+MonthNum*YearLen))
      GhcnPrcpStandardDB(:,2:) = -9998  !标准数据库中，原数据库没有的数据，全部设置为-9998，缺测为-9999，痕迹为-8888
      PRINT *,">>建立标准化存储数据库成功！"
      CALL SLEEP(1)
      PRINT *,">>查询标准化存储数据库数据文件是否存在..."
      CALL SLEEP(1)
      INQUIRE(FILE = 'GhcnPrcpStandardDB.dat', EXIST = alive)
      IF(alive) THEN
        PRINT *, ">>标准化存储数据库数据文件存在！"
        CALL SLEEP(1)
        PRINT *, ">>读取标准化存储数据库数据文件信息..."
        CALL SLEEP(1)
        ALLOCATE(RealArrayTemp2D(1+MonthNum*YearLen,ValidStationNum))
        OPEN(UNIT = fid,FILE = 'GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') !RECL = 300,
        READ(fid,500) RealArrayTemp2D
        GhcnPrcpStandardDB = TRANSPOSE(RealArrayTemp2D)
        PRINT *, ">>读取标准化存储数据库数据文件信息完成！"
        CALL SLEEP(1)
        CLOSE(fid)
      END IF
      ! 为了将异常值排除在外，计算可预报性时我们将所有的异常值置为-9996
      !WHERE (GhcnPrcpStandardDB < 0)
      !  GhcnPrcpStandardDB = -9996
      !END WHERE
      !istatus = getcwd(Path)
      !PRINT *,TRIM(Path)
      !PAUSE
      !PRINT *,GhcnPrcpStandardDB(1,:)
      ALLOCATE(R(MonthNum*RankNum*StudyStationNum001))
      ALLOCATE(P(MonthNum*RankNum*StudyStationNum001))
      ALLOCATE(KFR(MonthNum*RankNum*StudyStationNum001))
      ALLOCATE(KFP(MonthNum*RankNum*StudyStationNum001))
      ALLOCATE(FactorPrcp(YearLen*MonthNum))
      ALLOCATE(StudyPrcp(YearLen*MonthNum))
      ALLOCATE(Pk(MonthNum*RankNum*StudyStationNum001))
      ALLOCATE(Pb(MonthNum*RankNum*StudyStationNum001))
      istatus = CHDIR(TRIM(PredicteAndCheckDataPath))!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// StationNum次，显示长度30
      Progress % M = ">" !// 已完成部分的字符，不是必须
      Progress % O = " " !// 未完成部分的字符，不是必须
      Progress % Prefix = "Predicted and Statistic Each Month:  "  !// 前方提示文字，不是必须
      R = DataNumNotEnough
      P = DataNumNotEnough
      KFR = DataNumNotEnough
      KFP = DataNumNotEnough
      Pk = -999
      Pb = -999
      KFNum = 10
      !OPEN(fidd,FILE = 'records.dat')
      DO i = 1,StudyStationNum001
        DO j = 1,MonthNum
          DO k = 1,RankNum
            tempLength = (i-1)*MonthNum*RankNum + (j-1)*RankNum + k !预报是采用的初始量
            IF (P_R_001RankData_Predictable(tempLength,5) == 1) THEN   !R2不等于1
              Pc = P_R_001RankData_Predictable(tempLength,1)
              Pm = P_R_001RankData_Predictable(tempLength,2)
              Fc = P_R_001RankData_Predictable(tempLength,3)
              Fm = P_R_001RankData_Predictable(tempLength,4)
              !PRINT *,tempLength
              CALL StudyMonthAndFactorPreData_BP(Pc,Pm,Fc,Fm,YearLen,MonthNum,RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB) !,tempFactorPrcpMonth,tempStudyPrcpMonth
              !PRINT *,'Finish:',tempLength
              ALLOCATE(ValidStationLocation(COUNT(tempFactorPrcpMonth>=0.AND.tempStudyPrcpMonth>=0)))
              tempNum = 0
              DO jj = 1,SIZE(tempFactorPrcpMonth)
                IF(tempFactorPrcpMonth(jj)>=0.AND.tempStudyPrcpMonth(jj)>=0) THEN
                  tempNum = tempNum + 1
                  ValidStationLocation(tempNum) = jj
                END IF
              END DO

              lstatus = CHANGEDIRQQ('R2.EQ.1')   !/切换路径
              lstatus = CHANGEDIRQQ('CoupledStationsPrcp')
              WRITE(FileName(1:11),'(i11)'),INT(Pc,8)
              WRITE(FileName(12:12),'(a)'),'_'
              WRITE(FileName(13:14),'(i2)'),INT(Pm)
              WRITE(FileName(15:15),'(a)'),'_'
              WRITE(FileName(16:26),'(i11)'),INT(Fc,8)
              WRITE(FileName(27:27),'(a)'),'_'
              WRITE(FileName(28:29),'(i2)'),INT(Fm)
              WRITE(FileName(30:33),'(a4)'),'.dat'
              OPEN(fiddd,FILE = TRIM(FileName))
              DO ik = 1,tempNum
                WRITE(fiddd,'(f15.8,f15.8)'),tempFactorPrcpMonth(ValidStationLocation(ik)),tempStudyPrcpMonth(ValidStationLocation(ik)) !预报数据，GHCN观测数据
              END DO
              CLOSE(fid)
              istatus = CHDIR(TRIM(PredicteAndCheckDataPath))   ! 切换回主目录


              DEALLOCATE(ValidStationLocation)
              DEALLOCATE(tempFactorPrcpMonth)
              DEALLOCATE(tempStudyPrcpMonth)
            ELSE IF((P_R_001RankData_Predictable(tempLength,7) /= DataNumNotEnough).AND.(P_R_001RankData_Predictable(tempLength,7) /= R_Inf).AND.(P_R_001RankData_Predictable(tempLength,7) /= P_Inf) ) THEN
              Pc = P_R_001RankData_Predictable(tempLength,1)
              Pm = P_R_001RankData_Predictable(tempLength,2)
              Fc = P_R_001RankData_Predictable(tempLength,3)
              Fm = P_R_001RankData_Predictable(tempLength,4)
              !print *,tempLength
              !pause
              CALL StudyMonthAndFactorPreData_BP(Pc,Pm,Fc,Fm,YearLen,MonthNum,RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB) !,tempFactorPrcpMonth,tempStudyPrcpMonth
              !PRINT *,tempFactorPrcpMonth
              !PRINT *,tempStudyPrcpMonth
              !print *,'finished!'
              !pause
              !  record the location of valid prcp data
              ALLOCATE(ValidStationLocation(COUNT(tempFactorPrcpMonth>=0.AND.tempStudyPrcpMonth>=0)))
              !print *,'allocate space successful'
              !pause
              !PRINT *,'tempLength = ',tempLength
              !PRINT *,'Pc = ',Pc,'Pm = ',Pm,'Fc = ',Fc,'Fm = ',Fm
              !pause
              !PRINT *,tempFactorPrcpMonth
              !PRINT *,tempStudyPrcpMonth
              !PAUSE
              tempNum = 0
              DO jj = 1,SIZE(tempFactorPrcpMonth)
                IF(tempFactorPrcpMonth(jj)>=0.AND.tempStudyPrcpMonth(jj)>=0) THEN
                  tempNum = tempNum + 1
                  ValidStationLocation(tempNum) = jj
                  !if(tempLength == 18) then
                  !  print *,'j:',j
                  !  print *,'tempNum:',tempNum
                  !  pause
                  !end if
                END IF
              END DO
              !print *,'FactorPrcp'
              !print *,tempFactorPrcpMonth
              !print *,'StudyPrcp'
              !print *,tempStudyPrcpMonth
              !print *,'StudyStationNum',i,'Month:',j,'RankNum:',k
              !print *,'R2:',P_R_001RankData_Predictable(tempLength,5)
              !print *,'tempNum:',tempNum
              !print *,'length of factor prcp:',size(tempFactorPrcpMonth)
              !print *,'length of prcp:',size(tempStudyPrcpMonth)
              !pause
              !==============================================================================================
              !               判断是否有连续20年的数据无变化存在连续的20年数据无变化时舍弃，
              !==============================================================================================
              !IF((isContinuityGT_M(tempFactorPrcpMonth(ValidStationLocation),ClimateStatus,prcp_anomaly_value) .EQ. .false.) .AND.&
              !   (isContinuityGT_M(tempStudyPrcpMonth(ValidStationLocation),ClimateStatus,prcp_anomaly_value) .EQ. .false.) ) THEN    !只计算当20年气候态内没有连续的无变化的值时的值
                
                !===========================================================
                !           K-flod CrossCheck
                !===========================================================
                ALLOCATE(tempKFPredictedPrcp(tempNum))
                ALLOCATE(tempKFPredictedPrcpCP(tempNum))
                KFNum = tempNum
                !PRINT *,tempLength,KFNum,COUNT(tempFactorPrcpMonth>=0.AND.tempStudyPrcpMonth>=0)
                CALL KFoldCrossCheck(tempFactorPrcpMonth(ValidStationLocation),tempStudyPrcpMonth(ValidStationLocation),tempKFPredictedPrcp,&
                                     tempKFPredictedPrcpCP,tempNum,KFNum,KFR(tempLength),KFP(tempLength))
                !=========================================================
                !   保存K-Fold Cross Check数据
                !=========================================================
                lstatus = CHANGEDIRQQ(TRIM(KFPredictedPrcpPath))   !/切换路径
                WRITE(FileName(1:11),'(i11)'),INT(Pc,8)
                WRITE(FileName(12:12),'(a)'),'_'
                WRITE(FileName(13:14),'(i2)'),INT(Pm)
                WRITE(FileName(15:15),'(a)'),'_'
                WRITE(FileName(16:26),'(i11)'),INT(Fc,8)
                WRITE(FileName(27:27),'(a)'),'_'
                WRITE(FileName(28:29),'(i2)'),INT(Fm)
                WRITE(FileName(30:33),'(a4)'),'.dat'
                OPEN(fid ,FILE = TRIM(FileName))  ! 保存原始的预报数据
                DO ii = 1,tempNum
                  WRITE(fid,700),tempKFPredictedPrcp(ii),tempKFPredictedPrcpCp(ii),tempStudyPrcpMonth(ValidStationLocation(ii)) !预报数据，GHCN观测数据
                END DO
                CLOSE(fid)
                istatus = CHDIR(TRIM(PredicteAndCheckDataPath))   ! 切换回主目录
                !===========================================================
                !           Simple LinearRegression
                !===========================================================
                CALL LinearRegression(tempFactorPrcpMonth(ValidStationLocation(1:FLOOR(tempNum*TrainingRate))),&
                                      tempStudyPrcpMonth(ValidStationLocation(1:FLOOR(tempNum*TrainingRate))),&
                                      FLOOR(tempNum*TrainingRate),Pk(tempLength),Pb(tempLength))
                ALLOCATE(tempPredictedPrcp(tempNum - FLOOR(tempNum*TrainingRate)))
                ALLOCATE(tempPredictedPrcpCP(tempNum - FLOOR(tempNum*TrainingRate)))
                tempPredictedPrcp = Pk(tempLength)*tempFactorPrcpMonth(ValidStationLocation(FLOOR(tempNum*TrainingRate)+1:tempNum))+Pb(tempLength)
                tempPredictedPrcpCP = tempPredictedPrcp
                WHERE(tempPredictedPrcp <0 )
                  tempPredictedPrcpCP = 0 !预报值小于0的时候都等于0
                END WHERE
                CALL correlation(tempNum - FLOOR(tempNum*TrainingRate),tempPredictedPrcpCP,&
                                 tempStudyPrcpMonth(ValidStationLocation(FLOOR(tempNum*TrainingRate)+1:tempNum)),R(tempLength))
                IF(R(tempLength) == R_Inf) THEN
                  P(tempLength) = R_Inf
                ELSE
                  CALL Pvalue(tempNum - FLOOR(tempNum*TrainingRate),R(tempLength),P(tempLength))
                END IF
                !IF(isnan(R(tempLength))) THEN  !处理R中的NaN值，计算中
                !    R(tempLength) = R_EQ_NAN
                !    P(tempLength) = R_EQ_NAN
                !END IF
                !llstatus = IEEE_IS_FINITE(R(tempLength))
                !IF(llstatus == .false.) THEN
                !    R(tempLength) = R_Inf
                !    P(tempLength) = R_Inf
                !END IF

                !=========================================================
                !   保存 Simple LinearRegression 数据
                !=========================================================
                lstatus = CHANGEDIRQQ(TRIM(PredictedPrcpPath))   !/切换路径
                WRITE(FileName(1:11),'(i11)'),INT(Pc,8)
                WRITE(FileName(12:12),'(a)'),'_'
                WRITE(FileName(13:14),'(i2)'),INT(Pm)
                WRITE(FileName(15:15),'(a)'),'_'
                WRITE(FileName(16:26),'(i11)'),INT(Fc,8)
                WRITE(FileName(27:27),'(a)'),'_'
                WRITE(FileName(28:29),'(i2)'),INT(Fm)
                WRITE(FileName(30:33),'(a4)'),'.dat'
                OPEN(fid ,FILE = TRIM(FileName))  ! 保存原始的预报数据
                DO ii = 1,tempNum - FLOOR(tempNum*TrainingRate)
                  WRITE(fid,700),tempPredictedPrcp(ii),tempPredictedPrcpCP(ii),tempStudyPrcpMonth(ValidStationLocation(FLOOR(tempNum*TrainingRate)+ii)) !预报数据，GHCN观测数据
                END DO
                CLOSE(fid)
                istatus = CHDIR(TRIM(PredicteAndCheckDataPath))   ! 切换回主目录
                DEALLOCATE(tempPredictedPrcp)
                DEALLOCATE(tempPredictedPrcpCP)
                DEALLOCATE(tempKFPredictedPrcp)
                DEALLOCATE(tempKFPredictedPrcpCP)
              !ELSE
              !  R(tempLength) = Trash
              !  P(tempLength) = Trash
              !  KFR(tempLength) = Trash
              !  KFP(tempLength) = Trash
              !END IF
              DEALLOCATE(ValidStationLocation)
              DEALLOCATE(tempFactorPrcpMonth)
              DEALLOCATE(tempStudyPrcpMonth)
            END IF
          END DO
        END DO
        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
        !!!!CALL Progress % Put( i-8000 , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
      END DO
      CLOSE(fiddd)
      !=========================================================================================
      PRINT *,'Writing Simple LinearRegression Results...'
      OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01_K_and_B_PredictedS-L.dat')
      DO ii = 1,StudyStationNum001*MonthNum*RankNum
        !IF(P_R_001RankData_Predictable(ii,5)/=1) THEN
        !IF(R(ii)==P_Inf) THEN
        !    WRITE(fid,600),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),INT(P_R_001RankData_Predictable(ii,4),8),&
        !        &P_R_001RankData_Predictable(ii,5:7),P_Inf,R(ii),P(ii),P_Inf,P_Inf
        !ELSE IF(R(ii)==R_EQ_NAN) THEN
        !    WRITE(fid,600),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),INT(P_R_001RankData_Predictable(ii,4),8),&
        !        &P_R_001RankData_Predictable(ii,5:7),R_EQ_NAN,R(ii),P(ii),R_EQ_NAN,R_EQ_NAN
        !ELSE
        IF(R(ii) == DataNumNotEnough) THEN
          WRITE(fid,600),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),&
              INT(P_R_001RankData_Predictable(ii,4),8),P_R_001RankData_Predictable(ii,5:7),DataNumNotEnough,R(ii),P(ii),DataNumNotEnough,DataNumNotEnough
        ELSE IF(R(ii) == R_Inf) THEN
          WRITE(fid,600),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),&
              INT(P_R_001RankData_Predictable(ii,4),8),P_R_001RankData_Predictable(ii,5:7),R_Inf,R(ii),P(ii),R_Inf,R_Inf
        ELSE IF(R(ii) == Trash) THEN
          WRITE(fid,600),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),&
              INT(P_R_001RankData_Predictable(ii,4),8),P_R_001RankData_Predictable(ii,5:7),Trash,R(ii),P(ii),Trash,Trash
        ELSE
          WRITE(fid,600),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),&
              INT(P_R_001RankData_Predictable(ii,4),8),P_R_001RankData_Predictable(ii,5:7),R(ii)**2,R(ii),P(ii),Pk(ii),Pb(ii)
        END IF
        !END IF
      END DO
      CLOSE(fid)
      PRINT *,'Writing Simple LinearRegression Results Success!'
      !==========================================================================================
      PRINT *,'Writing K-Flod Cross Check Results...'
      OPEN(fid,FILE = 'R_RankNum15_P.LT.0.01_K_and_B_PredictedK-Flod.dat')
      DO ii = 1,StudyStationNum001*MonthNum*RankNum
        !IF(P_R_001RankData_Predictable(ii,5)/=1) THEN
        !IF(KFR(ii)==R_EQ_NAN) THEN
        !    WRITE(fid,600),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),INT(P_R_001RankData_Predictable(ii,4),8),&
        !        &P_R_001RankData_Predictable(ii,5:7),R_EQ_NAN,KFR(ii),KFP(ii)
        !ELSE
        !IF(KFR(ii) == P_Inf) THEN
        !    WRITE(fid,600),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),INT(P_R_001RankData_Predictable(ii,4),8),&
        !        &P_R_001RankData_Predictable(ii,5:7),P_Inf,KFR(ii),KFP(ii)
        !ELSE
        IF(KFR(ii) == DataNumNotEnough) THEN
          WRITE(fid,800),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),&
              INT(P_R_001RankData_Predictable(ii,4),8),P_R_001RankData_Predictable(ii,5:7),DataNumNotEnough,KFR(ii),KFP(ii)
        ELSE IF(KFR(ii) == R_Inf) THEN
          WRITE(fid,800),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),&
              INT(P_R_001RankData_Predictable(ii,4),8),P_R_001RankData_Predictable(ii,5:7),R_Inf,KFR(ii),KFP(ii)
        ELSE IF(KFR(ii) == Trash) THEN
          WRITE(fid,800),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),&
              INT(P_R_001RankData_Predictable(ii,4),8),P_R_001RankData_Predictable(ii,5:7),Trash,KFR(ii),KFP(ii)
        ELSE
          WRITE(fid,800),INT(P_R_001RankData_Predictable(ii,1),8),INT(P_R_001RankData_Predictable(ii,2),8),INT(P_R_001RankData_Predictable(ii,3),8),&
          INT(P_R_001RankData_Predictable(ii,4),8),P_R_001RankData_Predictable(ii,5:7),KFR(ii)**2,KFR(ii),KFP(ii)
        END IF
        !END IF
      END DO
      CLOSE(fid)
      PRINT *,'Writing K-Flod Cross Check Results Success!'
      !***************************************************************************************************************************************
      istatus = CHDIR(TRIM(Path))  ! 切换路径到主目录
      !***************************************************************************************************************************************


      WRITE(*,*)'istatus:', istatus
      WRITE(*,*)'lstatus:', lstatus
      WRITE(*,*)'ierror:',ierror
      PRINT *,'按任意键继续...'
      STOP
8089  PRINT *,'文件读取错误'
      PAUSE
      
End SUBROUTINE PredicteAndCheckSystem_BP