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
  CHARACTER(LEN = 10) :: DateT                                    !记录日期
  CHARACTER(LEN = 8) :: NowTime                                   !记录时间
  CHARACTER(LEN = 500) :: Path                                    !调试文件exe路径       , Name
  CHARACTER(LEN = 500) :: GhcnPrcpFile                            !Prcp文件绝对地址
  CHARACTER(LEN = 500) :: StationInfoFile                         !station信息文件
  CHARACTER(LEN = 11) :: StationDirName                           !每一个Station子文件夹临时名
  CHARACTER(LEN = 5)  :: StationNumStr,TotalAnalysisStationNumStr ! 进度条中显示的站点数
  CHARACTER(LEN = 500) :: WorkSpace
  CHARACTER(LEN = 100) :: dirname
  CHARACTER(LEN = 5) :: PressKey
  CHARACTER(LEN = 5) :: StartStationStr,EndStationStr
  CHARACTER(LEN = 5) :: monthStr,RankNumStr

  CHARACTER(LEN = 500) :: PredicteAndCheckDataPath  !分析数据库

  INTEGER :: times                                                !
  INTEGER :: GhcnPrcpColNum                                       !GHCNPrcp数据列数
  INTEGER :: MissVal                                              !缺省值处理（1为处理，0为不处理）
  INTEGER :: TraceVal                                             !痕迹降水的处理（1为处理，0为不处理）
  INTEGER :: AheadMonthNum                                        !最大提前预报的月份数
  INTEGER :: StartMonth                                           !降雨数据记录（GhcnPrcp）开始的月份
  INTEGER :: MonthNum                                             !一年的月份数
  INTEGER :: pTandMonth                                           !预报量站点的当前月份（在AheadMonth和MonthRoll控制下），
  INTEGER :: StartYear,EndYear,YearLen                            !预报研究的开始年份、结束年份、时间长度
  INTEGER :: RankNum                                              !站点有效排名
  INTEGER :: saveRankNum                                          !保存数据时，站点有效排名，即只保存前saveRankNum个站点的信息
  INTEGER :: II,JJ,KK                                             !随机使用的变量
  INTEGER :: N                                                   !拟合时自变量的最高次方数
  INTEGER :: CoverYears                                           !设置北京站点和预报因子站点降水月值数据能成Couple的最少数量
  INTEGER :: StationInfoFileLen                                   ! GHCN Station信息的数量
  INTEGER :: StationNum                                           ! GhcnPrcp数据中站点数量
  INTEGER :: GhcnPrcpRowNum                                       ! GHCNPrcp数据的行数
  INTEGER :: ValidStationNum                                      !GhcnPrcp数据库中起始终止年在StartYear-EndYear之间的站点数
  INTEGER :: fileID = 10, iosval                                  !文件ID，文件操作状态
  INTEGER :: savePrcpFileID = 200
  INTEGER :: istatus,istatus_dir_ch                               ! 改变当前工作目录成功与否的状态，新建文件夹成功与否的状态
  INTEGER :: tempNum ,tempCount,ColStart                            !临时变量
  INTEGER :: FactorPrcpLen                                        !预报因子StartYear->EndYear期间数据长度
  INTEGER :: i,j,k,ic,im,StartStationNum,EndStationNum,analysisStationNum
  INTEGER :: ClimateStatus
  INTEGER :: R2CountTotal                                          !单站点单月份有效R2的总数量

  INTEGER :: trainLen, saveTrainLen, leaveOut

  INTEGER(KIND = 8)  StudyCode
  INTEGER(KIND = 8), ALLOCATABLE :: CodesIndex(:)                 !提取出研究的站点编号
  INTEGER(KIND = 8), ALLOCATABLE :: CodesIndexLocation(:)         !存储某一个站点再数据库中的索引值(行位置)
  INTEGER(KIND = 8), ALLOCATABLE :: CIL(:)                           !功能同CodesIndexLocation
  INTEGER(KIND = 8), ALLOCATABLE :: GhcnPrcpYear(:,:)             !GhcnPrcp站点数据的开始年份和结束年份信息
  INTEGER(KIND = 8), ALLOCATABLE :: IntegerArrayTemp2D(:,:)       ! 2D整型变量临时存储
  INTEGER(KIND = 8), ALLOCATABLE :: ValidStationCoupled(:,:)      !通过限制条件的站点，及通过情况，1通过，0不通过

  INTEGER :: StudyStationNum001

  REAL(KIND = 8) :: DataNumNotEnough = -9.0    !, R_Inf = -5.0, P_Inf = -6.0   不在存在该情况
  REAL(KIND = 8) :: prcpRightLimit,prcp_anomaly_missing,prcp_anomaly_trace
  REAL(KIND = 8), ALLOCATABLE :: StudyPrcp(:),TempStudyPrcp(:),TempMonthStudyPrcp(:)    !研究区站点的降水数据
  REAL(KIND = 8), ALLOCATABLE :: FactorPrcp(:),TempFactorPrcp(:),TempMonthFactorPrcp(:) !预报因子站点的降水数据

  REAL(KIND = 8), ALLOCATABLE :: TempMonthFactorPrcpModify(:),TempMonthStudyPrcpModify(:) !修订后与pstor1ID保持相同长度的pstor2ID、pstandID站点的降水数据
  REAL(KIND = 8), ALLOCATABLE :: tempFactorPrcpMonthModify(:)
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcp(:), ptor1Prcp(:), ptor2Prcp(:)   !保存预报量站点、第一预报因子、第二预报因子降雨量数据,
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY1(:), ptandPrcpY1Residual(:)     !保存根据线性回归预报的预报量站点降雨量数据，只包含第一预报因子、再次情况下的残差
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY2(:)                             !保存根据线性回归预报的预报量残差，第二预报因子与第一预报因子预报残差之间的线性关系
  REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY(:)                              !包含第一预报因子和第二预报因子的总预报降雨量
  REAL(KIND = 8), ALLOCATABLE :: savePtror1Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror2Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand1Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand2Prcp(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror1PrcpModify(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtror2PrcpModify(:)
  REAL(KIND = 8), ALLOCATABLE :: savePtand1PrcpModify(:)

  REAL(KIND = 8), ALLOCATABLE :: StationCodesUnique(:)            !GhcnPrcp中Codes编号
  REAL(KIND = 8), ALLOCATABLE :: RealArrayTemp2D(:,:)             !读取GhcnPrcp站点数据的临时数据库
  REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:)          !GhcnPrcp存放格式标准化数据库
  REAL, ALLOCATABLE :: P(:,:),R(:,:),TempR2(:,:),TempP(:,:),R2OneMonth(:,:)        ! 预报每一个站点时，P\R信息

  REAL(KIND = 8), ALLOCATABLE :: ptor2k(:,:), ptor2b(:,:)
  REAL(KIND = 8) :: ptor1k, ptor1b, tempPtor2k, tempPtor2b

  REAL :: StartRate,EndRate
  REAL :: PPvalue                     !显著性水平
  REAL :: TrainingRate                !训练数据占总数据的比例
  REAL :: maxR2, minR2,avgR2
  REAL :: R2Total                                          !单站点单月份有效R2的总和

  REAL(KIND = 8),ALLOCATABLE :: StudyStationCodes001(:)
  REAL(KIND = 8),ALLOCATABLE :: P_R_001RankData_Predictable(:,:)
  REAL(KIND = 8) :: pstandID, pstor1ID, pstor1LM, pstor2ID, pstor2LM
  REAL :: RptandY, PptandY, Rtandtor1, R2tandtor1

  TYPE( CLS_CMD_Progress ) ::Progress  !进度条
  LOGICAL(4) :: istatus_dir_mk,alive                              !文件存在状态
  NAMELIST /CSPRPBP/ prcp_anomaly_missing,prcp_anomaly_trace,times,GhcnPrcpColNum,MissVal,&
    TraceVal,AheadMonthNum,StartMonth,StartYear,EndYear,MonthNum,ClimateStatus,&
    RankNum,PPvalue,TrainingRate,CoverYears,saveRankNum,leaveOut
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
  PRINT "(1X,A17/)", '>>程序开始运行...'
  CALL Display_Time_Current(DateT,NowTime)
  WRITE(*,100)'当前时间：',DateT(1:4),'/',DateT(5:6),'/',DateT(7:8),'   ',NowTime
  ! get current direction and set work direction
  PRINT "(1X,A19)", '>>程序参数初始化...'
  CALL SLEEP(1)
  istatus = GETCWD(Path)

  PredicteAndCheckDataPath = TRIM(Path)//'\BaseOnPrcp\PredicteAndCheck\'

  GhcnPrcpFile = TRIM(Path)//'\GhcnData\v2.prcp.dat'
  StationInfoFile = TRIM(Path)//'\GhcnData\v2.prcp.inv'
  WorkSpace = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\'
  istatus_dir_ch = CHDIR(TRIM(WorkSpace)) !设置默认的工作目录
  ! read default namelist file which define default parameters
  OPEN(UNIT = fileID,FILE = './CalStationPrcpRP_BP_add_next.namelist')
  READ (fileID,NML = CSPRPBP,ERR = 8089)
  CLOSE(fileID)
  PRINT "(1X,A18)", ">>参数初始化完成！"
  PRINT *,'Main parameters as below:'
  WRITE (*,NML = CSPRPBP)
  CALL SLEEP(1)
  !****************************************************************************
  ! !                        Body of CalStationPrcpRP
  !****************************************************************************

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !                             Read GhcnPrcp Data
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  PRINT *,'>>读取GHCN数据...'
  CALL SLEEP(1)
  CALL Inqire_Text_Row(TRIM(GhcnPrcpFile),LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum)
  ALLOCATE(GhcnPrcp(GhcnPrcpRowNum, GhcnPrcpColNum)) !动态分配GHCN原始降雨数据库大小
  CALL Read_GHCN_Prcp(TRIM(GhcnPrcpFile), LEN(TRIM(GhcnPrcpFile)), GhcnPrcpRowNum, GhcnPrcpColNum, MissVal, TraceVal)!读取GHCN降水数据
  PRINT *,'>>读取GHCN数据完成！'
  CALL SLEEP(1)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         get and save ghcnPrcp original station codes information
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  PRINT *,">>查询GhcnPrcp站点编号信息文件是否存在..."
  CALL SLEEP(1)
  INQUIRE(DIRECTORY = TRIM(WorkSpace)//'DataBase', EXIST = alive)
  IF (alive == .false.) THEN
    istatus_dir_mk = MAKEDIRQQ(TRIM('DataBase'))
  END IF
  INQUIRE(FILE = './DataBase/StationCodes.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *, ">>GhcnPrcp站点编号信息文件存在！"
    CALL SLEEP(1)
    PRINT *, ">>读取GhcnPrcp站点编号信息..."
    CALL SLEEP(1)
    CALL Inqire_Text_Row('./DataBase/StationCodes.dat',LEN('./DataBase/StationCodes.dat'),StationNum)
    ALLOCATE(StationCodesUnique(StationNum))
    OPEN(UNIT = fileID,FILE = './DataBase/StationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') ! RECL = 20,
    READ(fileID,'(F20.0)') StationCodesUnique
    CLOSE(fileID)
    PRINT *, ">>读取GhcnPrcp站点编号信息成功！"
    CALL SLEEP(1)
  ELSE
    PRINT *, ">>GhcnPrcp站点编号信息文件不存在"
    CALL SLEEP(1)
    PRINT *, ">>提取GhcnPrcp中站点编号信息..."
    CALL SLEEP(1)
    CALL Unique(GhcnPrcp(1:GhcnPrcpRowNum,1),StationCodesUnique)
    PRINT *, ">>提取GhcnPrcp中站点编号信息完成！"
    StationNum = SIZE(StationCodesUnique)
    PRINT *,'Station number of original ghcn Precipitation data is :',StationNum
    CALL SLEEP(1)
    PRINT *,">>保存GhcnPrcp中站点编号信息..."
    CALL SLEEP(1)
    OPEN(UNIT = fileID,FILE = './DataBase/StationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') ! RECL = 20,
    WRITE(fileID,'(I20)') INT(StationCodesUnique,8)
    CLOSE(fileID)
    PRINT *,">>保存GhcnPrcp中站点编号信息完成(当前exe目录下\database\StationCodes.dat文件)！"
    CALL SLEEP(1)
  END IF
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !           get and save ghcnPrcp station start-end year information
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  PRINT *,">>查询GhcnPrcp Station Start-End Year文件是否存在..."
  CALL SLEEP(1)
  INQUIRE(FILE = './DataBase/StationStartEndYear.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *, ">>GhcnPrcp Station Start-End Year文件存在！"
    CALL SLEEP(1)
    PRINT *, ">>读取GhcnPrcp Station Start-End Year文件信息..."
    CALL SLEEP(1)
    ALLOCATE(GhcnPrcpYear(StationNum,3))
    ALLOCATE(IntegerArrayTemp2D(3,StationNum))
    OPEN(UNIT = 50,FILE = './DataBase/StationStartEndYear.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') !RECL = 60,
    READ(50,300) IntegerArrayTemp2D
    CLOSE(fileID)
    GhcnPrcpYear = TRANSPOSE(IntegerArrayTemp2D)
    PRINT *, ">>读取GhcnPrcp Station Start-End Year文件成功！"
    CALL SLEEP(1)
  ELSE
    PRINT *, ">>GhcnPrcp Station Start-End Year文件不存在..."
    CALL SLEEP(1)
    PRINT *, ">>提取GhcnPrcp Station Start-End Year信息..."
    CALL SLEEP(1)
    ALLOCATE(GhcnPrcpYear(StationNum,3))
    GhcnPrcpYear(1:StationNum,1) = INT(StationCodesUnique,8)
    !print *,GhcnPrcpYear(1:StationNum,1)
    GhcnPrcpYear(1:StationNum,2:3) = 0
    CALL Progress % Set( N = StationNum , L = 30 )!// StationNum次，显示长度30
    Progress % Prefix = "Station Start-End Year:  "  !// 前方提示文字，不是必须
    Progress % M = "|" !// 已完成部分的字符，不是必须
    Progress % O = " " !// 未完成部分的字符，不是必须
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
      CALL Progress % Put( II , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
    END DO
    PRINT *, ">>提取GhcnPrcp Station Start-End Year信息完成！"
    CALL SLEEP(1)
    PRINT *,">>保存GhcnPrcp Station Start-End Year文件..."
    CALL SLEEP(1)
    OPEN(UNIT = fileID,FILE = './DataBase/StationStartEndYear.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') ! RECL = 60,
    WRITE(fileID,300) TRANSPOSE(GhcnPrcpYear)
    CLOSE(fileID)
    PRINT *,">>保存GhcnPrcp Station Start-End Year文件完成(当前exe目录下StationStartEnd.dat文件)！"
    CALL SLEEP(1)
  END IF
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         select station which in 1901-2010 to build standard prcp database(ValidStationNum,1+MonthNum*YearLen)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  YearLen = EndYear - StartYear + 1
  WRITE(*,'(1x,a,i4,a,i4,a)')">>查询",StartYear,"-",EndYear,"年间的站点编号文件是否存在..."
  CALL SLEEP(1)
  INQUIRE(FILE = './DataBase/ValidStationCodes.dat', EXIST = alive)
  IF(alive) THEN
    WRITE(*,'(1x,a,i4,a,i4,a)')">>",StartYear,"-",EndYear,"年间的站点编号文件存在..."
    CALL SLEEP(1)
    WRITE(*,'(1x,a,i4,a,i4,a)')">>读取",StartYear,"-",EndYear,"年间的站点编号文件中..."
    CALL SLEEP(1)
    CALL Inqire_Text_Row('./DataBase/ValidStationCodes.dat',LEN('./DataBase/ValidStationCodes.dat'),tempNum)
    ALLOCATE(CodesIndex(tempNum))
    OPEN(UNIT = fileID,FILE = './DataBase/ValidStationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') !RECL = 300,
    READ(fileID,'(I20)') CodesIndex
    WRITE(*,'(1x,a,i4,a,i4,a)')">>读取",StartYear,"-",EndYear,"年间的站点编号文件完成！"
    CALL SLEEP(1)
    CLOSE(fileID)
  ELSE
    WRITE(*,'(1x,a,i4,a,i4,a)')">>提取",StartYear,"-",EndYear,"年间的站点编号，并建立标准化存储数据库..."
    CALL SLEEP(1)
    tempNum = COUNT(GhcnPrcpYear(:,2)<=EndYear.AND.GhcnPrcpYear(:,3)>=StartYear)
    tempCount = 0
    ALLOCATE(CodesIndex(tempNum))
    PRINT *,">>提取站点编号..."
    CALL SLEEP(1)
    DO II = 1,StationNum
      IF(GhcnPrcpYear(II,2)<=EndYear.AND.GhcnPrcpYear(II,3)>=StartYear) THEN
        tempCount = tempCount + 1
        CodesIndex(tempCount) = GhcnPrcpYear(II,1)
      END IF
    END DO
    ! save valid station codes to file
    PRINT *,">>保存有效站点编号数据..."
    OPEN(UNIT = fileID,FILE = './DataBase/ValidStationCodes.dat') ! RECL = 20,
    WRITE(fileID,'(I20)') INT(CodesIndex,8)
    CLOSE(fileID)
    PRINT *,">>保存有效站点编号数据成功！"
    PRINT *,">>提取站点编号成功！"
    CALL SLEEP(1)
  END IF
  ! Build standard Precipitation DataBase
  PRINT *,">>建立标准化存储数据库..."
  CALL SLEEP(1)
  ValidStationNum = SIZE(CodesIndex)
  ALLOCATE(GhcnPrcpStandardDB(ValidStationNum,1+MonthNum*YearLen))
  GhcnPrcpStandardDB(:,2:) = -9998  !标准数据库中，原数据库没有的数据，全部设置为-9998，原始缺测为-9999，原始痕迹为-8888
  PRINT *,">>建立标准化存储数据库成功！"
  CALL SLEEP(1)
  PRINT *,">>查询标准化存储数据库数据文件是否存在..."
  CALL SLEEP(1)
  INQUIRE(FILE = './DataBase/GhcnPrcpStandardDB.dat', EXIST = alive)
  IF(alive) THEN
    PRINT *, ">>标准化存储数据库数据文件存在！"
    CALL SLEEP(1)
    PRINT *, ">>读取标准化存储数据库数据文件信息..."
    CALL SLEEP(1)
    ALLOCATE(RealArrayTemp2D(1+YearLen*MonthNum,ValidStationNum))
    OPEN(UNIT = fileID,FILE = './DataBase/GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') !RECL = 300,
    READ(fileID,500) RealArrayTemp2D
    GhcnPrcpStandardDB = TRANSPOSE(RealArrayTemp2D)
    PRINT *, ">>读取标准化存储数据库数据文件信息完成！"
    CALL SLEEP(1)
    CLOSE(fileID)
  ELSE
    PRINT *,">>标准化存储数据库数据文件不存在..."
    CALL SLEEP(1)
    CALL Progress % Set( N = ValidStationNum , L = 30 )!// size(CodesIndex)次，显示长度30
    Progress % Prefix = "获取标准化存储数据库数据:  "  !// 前方提示文字，不是必须
    Progress % M = "#" !// 已完成部分的字符，不是必须
    Progress % O = " " !// 未完成部分的字符，不是必须
    DO II = 1,ValidStationNum
      GhcnPrcpStandardDB(II,1) = CodesIndex(II)
      tempNum = COUNT(GhcnPrcp(:,1) == CodesIndex(II))
      ALLOCATE(CodesIndexLocation(tempNum))
      CodesIndexLocation = ArrayFind(GhcnPrcp(:,1),'=',REAL(CodesIndex(II),8))
      DO KK = 1,tempNum
        IF(GhcnPrcp(CodesIndexLocation(KK),3)>=StartYear.and.GhcnPrcp(CodesIndexLocation(KK),3)<=EndYear) THEN
          ColStart = (GhcnPrcp(CodesIndexLocation(KK),3) - StartYear)*MonthNum +1  !GhcnPrcpStandardDB中的行号
          GhcnPrcpStandardDB(II,1+ColStart:1+ColStart+MonthNum) = GhcnPrcp(CodesIndexLocation(KK),4:15)
        END IF
      END DO
      DEALLOCATE(CodesIndexLocation)
      CALL Progress % Put( II , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
    END DO
    PRINT *,">>保存GhcnPrcpStandardDB文件..."
    CALL SLEEP(1)
    OPEN(UNIT = fileID,FILE = './DataBase/GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
      POSITION = 'REWIND') !RECL = 300,
    DO i = 1,ValidStationNum!*(EndYear - StartYear + 1)
      WRITE(fileID,400) INT(GhcnPrcpStandardDB(i,1),8),GhcnPrcpStandardDB(i,2:)
    END DO
    CLOSE(fileID)
    PRINT *,">>保存GhcnPrcpStandardDB文件完成(当前exe目录下GhcnPrcpStandardDB.dat文件)!"
    CALL SLEEP(1)
    !-------------------------------------------------------------------------
    !     筛选降水文每月小于   μ+timesσ (右边界)部分
    !-------------------------------------------------------------------------
    !print *,'>> 进行μ+3σ处理'
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
    !PRINT *,">>保存GhcnPrcpStandardDB文件..."
    !CALL SLEEP(1)
    !OPEN(UNIT = fileID,FILE = './DataBase/GhcnPrcpStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
    !     POSITION = 'REWIND') !RECL = 300,
    !DO i = 1,ValidStationNum!*(EndYear - StartYear + 1)
    !  WRITE(fileID,400) INT(GhcnPrcpStandardDB(i,1),8),GhcnPrcpStandardDB(i,2:)
    !END DO
    !CLOSE(fileID)
    !PRINT *,">>保存GhcnPrcpStandardDB文件完成(当前exe目录下GhcnPrcpStandardDB.dat文件)!"
    !CALL SLEEP(1)
  END IF

  !***********************************************************************************************************
  ! 查询可预报站点列表文件是否存在，存在则读取
  !*****************************************************************************************
  ! 切换路径到结果分析路径
  istatus = CHDIR(TRIM(PredicteAndCheckDataPath))

  INQUIRE(FILE = 'StudyCode_RankNum15_P.LT.0.01.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *,'>>StudyCode_RankNum15_P.LT.0.01文件存在，读取文件...'
    CALL Inqire_Text_Row('StudyCode_RankNum15_P.LT.0.01.dat',LEN('StudyCode_RankNum15_P.LT.0.01.dat'),tempNum)
    ALLOCATE(StudyStationCodes001(tempNum))
    OPEN(fileID,FILE = 'StudyCode_RankNum15_P.LT.0.01.dat')
    READ(fileID,'(f15.0)')StudyStationCodes001
    CLOSE(fileID)
    StudyStationNum001 = SIZE(StudyStationCodes001)
    PRINT *,'StudyStationNum: ',StudyStationNum001
    PRINT *,'>>读取StudyCode_RankNum15_P.LT.0.01文件完成!'
  END IF
  !*****************************************************************************************
  ! get and save the information of station which can be predicted
  !*****************************************************************************************
  ALLOCATE(P_R_001RankData_Predictable(StudyStationNum001*MonthNum*RankNum,7))
  INQUIRE(FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat', EXIST = alive)
  IF (alive) THEN
    PRINT *,'>>R_RankNum15_P.LT.0.01_Predicted文件存在，读取文件...'
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.01_Predicted.dat')
    DO i = 1,StudyStationNum001*MonthNum*RankNum
      READ(fileID,3000)P_R_001RankData_Predictable(i,:)
    END DO
    CLOSE(fileID)
    PRINT *,'>>读取R_RankNum15_P.LT.0.01_Predicted文件完成！'
  END IF

  !*****************************************************************************************
  ! calculate partial R R2 P of each predicted station in 12 month in max aheadmonth 24,
  ! controlling by the strogest correlation predictor station
  !*****************************************************************************************
  !切换到默认工作路径
  istatus_dir_ch = CHDIR(TRIM(WorkSpace))
  !动态分配变量
  ALLOCATE(StudyPrcp(YearLen*MonthNum))
  ALLOCATE(FactorPrcp(YearLen*MonthNum))
  ALLOCATE(R(ValidStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(TempR2(ValidStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(P(ValidStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(TempP(ValidStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(R2OneMonth(ValidStationNum,AheadMonthNum))
  ALLOCATE(ptor2k(ValidStationNum*MonthNum,AheadMonthNum))
  ALLOCATE(ptor2b(ValidStationNum*MonthNum,AheadMonthNum))
  !查询结果保存目录是否存在
  INQUIRE(DIRECTORY = TRIM(WorkSpace)//'add_next_StationList', EXIST = alive)
  IF (alive == .false.) THEN
    istatus_dir_mk = makedirqq(TRIM('add_next_StationList'))
  END IF
  istatus_dir_ch = CHDIR(TRIM(WorkSpace)//TRIM('add_next_StationList\'))
  !运行前是否需要运行某一段站点，若采用此策略，则会保存所有的预报站点R\R2\P信息，占用存储大量空间
  PRINT *,'是否需要计算某一个或者某一段站点？ y/n'
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
  !根据最终的预报站点信息，确定预报站点数量等信息
  analysisStationNum = EndStationNum - StartStationNum + 1
  ALLOCATE(ValidStationCoupled(analysisStationNum,13))
  ValidStationCoupled = 0
  !进度条中总站点数量更新
  WRITE(TotalAnalysisStationNumStr,'(I5)') StudyStationNum001
  PRINT *,analysisStationNum
  ! 为了将异常值排除在外，计算可预报性时我们将-9998转换为-9999
  WHERE (GhcnPrcpStandardDB == -9998)
    GhcnPrcpStandardDB = -9999
  END WHERE

  !初始化命令行进度条
  CALL Progress % Set( N = StudyStationNum001 , L = 30 )!// size(CodesIndex)次，显示长度30
  Progress % M = "=" !// 已完成部分的字符，不是必须
  Progress % O = "." !// 未完成部分的字符，不是必须

  DO i = StartStationNum,EndStationNum
    !获取可预报站点及其最强相关的预报因子站点信息
    pstandID = P_R_001RankData_Predictable((i-1)*MonthNum*RankNum+1,1)

    !print*,pstandID
    !pause

    ValidStationCoupled(i - StartStationNum + 1,1) = pstandID
    R = DataNumNotEnough
    P = DataNumNotEnough
    ptor2k = DataNumNotEnough
    ptor2B = DataNumNotEnough
    WRITE(StationDirName,'(I11)') INT(pstandID,KIND=8)!StudyCode

    !查询站点目录是否存在
    INQUIRE(DIRECTORY = TRIM(WorkSpace)//'add_next_StationList\'//TRIM(StationDirName), EXIST = alive)
    IF (alive == .false.) THEN
      istatus_dir_mk = MAKEDIRQQ(TRIM(StationDirName))
    END IF
    istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'add_next_StationList\'//TRIM(StationDirName))

    !istatus_dir_mk = MAKEDIRQQ(TRIM(StationDirName))
    !istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'add_next_StationList\'//TRIM(StationDirName))
    WRITE(StationNumStr,'(I5)') i
    !更新命令行进度条，// 前方提示文字，不是必须
    Progress % Prefix = "StationNum:"//TRIM(StationNumStr)//" of "//TotalAnalysisStationNumStr
    !预报量站点降雨数据
    !需要首先确定预报量站点数据在什么位置
    DO tempNum = 1,ValidStationNum
      IF (GhcnPrcpStandardDB(tempNum,1) == pstandID) EXIT
    END DO
    StudyPrcp = GhcnPrcpStandardDB(tempNum,2:)
    !print *,tempNum,"=======",pstandID,GhcnPrcpStandardDB(tempNum,1)
    !pause

    DO j = 1,ValidStationNum
      pstor2ID = GhcnPrcpStandardDB(j,1)
      FactorPrcp = GhcnPrcpStandardDB(j,2:)
      FactorPrcpLen = MonthNum*YearLen
      DO k = 1,AheadMonthNum
        pstor2LM = REAL(k,KIND=8)
        ALLOCATE(TempFactorPrcp(FactorPrcpLen - k))
        ALLOCATE(TempStudyPrcp(FactorPrcpLen - k))
        TempFactorPrcp = FactorPrcp(1:FactorPrcpLen - k)
        TempStudyPrcp = StudyPrcp(k+1:)
        DO ii = 1,MonthNum
          !print *,"enter"

          CALL pTandMonthAtAheadMonthAndMonthRoll(k, ii, StartMonth, MonthNum, pTandMonth)
          !与ii月份对应得最强预报因子站点信息
          pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,3)
          pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,4)
          R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,5)
          Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (pTandMonth-1)*RankNum + 1,6)
          IF (((pstor1ID == pstor2ID) .AND. ((pstor1LM == pstor2LM))) .OR. (Rtandtor1 < -1)) THEN
            !空白行，当第一个因子站点号与lead time与第二个因子相同时执行，或者第一个因子的相关系数为-9时执行
          ELSE

            !获取与pstandID、month、pstor1ID、pstor1LM对应得预报量及预报因子站点数据
            !变量为全局变量tempFactorPrcpMonth和tempStudyPrcpMonth
            CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(pTandMonth,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,&
              &RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB) !,tempFactorPrcpMonth,tempStudyPrcpMonth

            !获取与pstor2ID相应的预报量站点和预报因子站点数据
            ALLOCATE(TempMonthFactorPrcp(SIZE(TempFactorPrcp(ii:SIZE(TempFactorPrcp):MonthNum))))
            ALLOCATE(TempMonthStudyPrcp(SIZE(TempStudyPrcp(ii:SIZE(TempStudyPrcp):MonthNum))))
            TempMonthFactorPrcp = TempFactorPrcp(ii:SIZE(TempFactorPrcp):MonthNum)
            TempMonthStudyPrcp = TempStudyPrcp(ii:SIZE(TempStudyPrcp):MonthNum)

            !print *,"deal..."

            !根据pstor1ID和pstor2ID的情况，修订预报量、预报因子1及预报因子2的降雨量数据
            IF (SIZE(tempFactorPrcpMonth)>SIZE(TempMonthFactorPrcp)) THEN
              ALLOCATE(tempFactorPrcpMonthModify(SIZE(TempMonthFactorPrcp)))
              tempFactorPrcpMonthModify = tempFactorPrcpMonth(SIZE(tempFactorPrcpMonth)-SIZE(TempMonthFactorPrcp)+1:)
              !另一个不需要修改，原封不动的copy即可
              ALLOCATE(TempMonthFactorPrcpModify(SIZE(TempMonthFactorPrcp)))
              TempMonthFactorPrcpModify = TempMonthFactorPrcp
              !此时预报量站点使用与pstor2ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(TempMonthFactorPrcp)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp
            ELSE IF(SIZE(tempFactorPrcpMonth)<SIZE(TempMonthFactorPrcp)) THEN
              ALLOCATE(TempMonthFactorPrcpModify(SIZE(tempFactorPrcpMonth)))
              TempMonthFactorPrcpModify = TempMonthFactorPrcp(SIZE(TempMonthFactorPrcp)-SIZE(tempFactorPrcpMonth)+1:)
              !另一个不需要修改，原封不动的copy即可
              ALLOCATE(tempFactorPrcpMonthModify(SIZE(tempFactorPrcpMonth)))
              tempFactorPrcpMonthModify = tempFactorPrcpMonth
              !此时预报量站点使用与pstor2ID对应的预报量降雨的修订值，此时需要修灯
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(tempFactorPrcpMonth)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp(SIZE(TempMonthFactorPrcp)-SIZE(tempFactorPrcpMonth)+1:)
            ELSE
              !此时，均不需要修订
              ALLOCATE(TempMonthFactorPrcpModify(SIZE(TempMonthFactorPrcp)))
              TempMonthFactorPrcpModify = TempMonthFactorPrcp
              !另一个不需要修改，原封不动的copy即可
              ALLOCATE(tempFactorPrcpMonthModify(SIZE(tempFactorPrcpMonth)))
              tempFactorPrcpMonthModify = tempFactorPrcpMonth
              !此时预报量站点使用与pstor2ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
              ALLOCATE(TempMonthStudyPrcpModify(SIZE(TempMonthFactorPrcp)))
              TempMonthStudyPrcpModify = TempMonthStudyPrcp
            END IF

            !print *,SIZE(TempMonthFactorPrcpModify),SIZE(TempMonthStudyPrcpModify),SIZE(tempFactorPrcpMonthModify)
            !print *,"judage"

            IF((isContinuityGT_M(TempMonthStudyPrcpModify,ClimateStatus,prcp_anomaly_missing) == .false.) .AND.&
              (isContinuityGT_M(TempMonthStudyPrcpModify,ClimateStatus,prcp_anomaly_trace) == .false.) .AND.&
              (isContinuityGT_M(TempMonthFactorPrcpModify,ClimateStatus,prcp_anomaly_missing) == .false.) .AND.&
              (isContinuityGT_M(TempMonthFactorPrcpModify,ClimateStatus,prcp_anomaly_trace) == .false.).AND.&
              (isContinuityGT_M(tempFactorPrcpMonthModify,ClimateStatus,prcp_anomaly_missing) == .false.) .AND.&
              (isContinuityGT_M(tempFactorPrcpMonthModify,ClimateStatus,prcp_anomaly_trace) == .false.))then

              !print *,"enter IF"

              tempCount = COUNT(TempMonthFactorPrcpModify>=0 .AND. TempMonthStudyPrcpModify>=0 .AND. tempFactorPrcpMonthModify>=0)
              ALLOCATE(CodesIndexLocation(tempCount))
              tempNum = 0
              !print *,"deal do"
              DO jj = 1,SIZE(TempMonthFactorPrcpModify)
                IF(TempMonthFactorPrcpModify(jj)>=0 .AND. TempMonthStudyPrcpModify(jj)>=0 .AND. tempFactorPrcpMonthModify(jj)>=0) THEN
                  tempNum = tempNum + 1
                  CodesIndexLocation(tempNum) = jj
                END IF
              END DO

              IF(tempCount >= CoverYears) THEN
                !统计站点是否存在配对记录，由于此次只是为了统计是否存在记录
                ValidStationCoupled(i - StartStationNum + 1 ,pTandMonth+1) = 1

                !设置trainLen的长度
                IF (leaveOut < 0) THEN
                  trainLen = FLOOR(tempCount*TrainingRate)
                ELSE
                  trainLen = tempCount - leaveOut
                END IF

                ALLOCATE(ptandPrcp(trainLen))
                ALLOCATE(ptor1Prcp(trainLen))
                ALLOCATE(ptor2Prcp(trainLen))
                ptandPrcp = TempMonthStudyPrcpModify(CodesIndexLocation(1:trainLen))
                ptor1Prcp = tempFactorPrcpMonthModify(CodesIndexLocation(1:trainLen))
                ptor2Prcp = TempMonthFactorPrcpModify(CodesIndexLocation(1:trainLen))

                !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
                CALL LinearRegression(ptor1Prcp,ptandPrcp,trainLen,ptor1k,ptor1b)
                ALLOCATE(ptandPrcpY1(trainLen))
                ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
                !CALL Correlation(trainLen,ptandPrcpY1,ptandPrcp,RptandY)
                !print *,"k:",Rtandtor1,",  b:",RptandY
                !pause

                !计算预报量与第一预报因子的“残差”
                ALLOCATE(ptandPrcpY1Residual(trainLen))
                ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
                !线性回归拟合预报量与第一预报因子的“残差”和第二预报因子之间的线性关系
                CALL LinearRegression(ptor2Prcp,ptandPrcpY1Residual,trainLen,tempPtor2k, tempPtor2b)
                ptor2k(j+(pTandMonth-1)*ValidStationNum,k) = tempPtor2k
                ptor2b(j+(pTandMonth-1)*ValidStationNum,k) = tempPtor2b

                ALLOCATE(ptandPrcpY2(trainLen))
                ptandPrcpY2 = ptor2Prcp*tempPtor2k + tempPtor2b
                !计算总预报降雨量
                ALLOCATE(ptandPrcpY(trainLen))
                ptandPrcpY = ptandPrcpY1+ptandPrcpY2
                !当总预报方程中Y < 0时，直接设为0即可
                WHERE (ptandPrcpY < 0)
                  ptandPrcpY = 0
                END WHERE
                !计算预报量站点与第二个因子的相关系数
                CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)

                !如若上一行未生效，这里借用一下RptandY做为临时变量
                !CALL Correlation(trainLen,ptor2Prcp,ptandPrcpY1Residual,RptandY)
                R(j+(pTandMonth-1)*ValidStationNum,k) = RptandY
                !计算显著性水平
                !CALL Pvalue(trainLen, RptandY, PptandY)

                !如若上一行未生效，这里借用一下RptandY做为临时变量
                CALL Pvalue(trainLen, RptandY, PptandY)
                P(j+(pTandMonth-1)*ValidStationNum,k) = PptandY
                !pause

                DEALLOCATE(ptandPrcp)
                DEALLOCATE(ptor1Prcp)
                DEALLOCATE(ptor2Prcp)
                DEALLOCATE(ptandPrcpY)
                DEALLOCATE(ptandPrcpY1)
                DEALLOCATE(ptandPrcpY2)
                DEALLOCATE(ptandPrcpY1Residual)

              END IF


              DEALLOCATE(CodesIndexLocation)
            END IF

            !print *,RptandY,PptandY,"del vars",Rtandtor1
            !pause

            DEALLOCATE(TempMonthFactorPrcp)
            DEALLOCATE(TempMonthStudyPrcp)
            DEALLOCATE(tempFactorPrcpMonth)
            DEALLOCATE(tempStudyPrcpMonth)
            DEALLOCATE(tempFactorPrcpMonthModify)
            DEALLOCATE(TempMonthFactorPrcpModify)
            DEALLOCATE(TempMonthStudyPrcpModify)
            !print *,"del vars finished"
            !pause
          END IF
        END DO
        DEALLOCATE(TempFactorPrcp)
        DEALLOCATE(TempStudyPrcp)
      END DO

      !print *,j

    END DO


    ALLOCATE(CodesIndexLocation(2))


    IF(TRIM(PressKey) == 'y') THEN
      open(fileID,FILE = 'R.txt',IOSTAT = iosval)
      do ii = 1,MonthNum * ValidStationNum
        if(mod(ii,ValidStationNum) /= 0) then
          write(fileID, '(I15,24F6.2)' ) CodesIndex(mod(ii,ValidStationNum)), R(ii,:)  !CodesIndex(jj),
        else
          write(fileID, '(I15,24F6.2)' ) CodesIndex(ValidStationNum), R(ii,:)  !CodesIndex(jj),
        endif
      end do
      close(fileID)
      open(fileID,FILE = 'P.txt',IOSTAT = iosval)
      do ii = 1,MonthNum*ValidStationNum
        if(mod(ii,ValidStationNum) /= 0) then
          write(fileID, '(I15,24F6.2)') CodesIndex(mod(ii,ValidStationNum)),P(ii, :)  !CodesIndex(jj),
        else
          write(fileID, '(I15,24F6.2)') CodesIndex(ValidStationNum),P(ii, :)  !CodesIndex(jj),
        endif
      end do
      close(fileID)
    END IF

    ! Save into binary file in order to save save hard disk space
    !open(fileID,FILE = 'R.bin',IOSTAT = iosval, FORM = 'UNFORMATTED', ACCESS = 'DIRECT',RECL = 26, STATUS = 'REPLACE')
    !do ii = 1,MonthNum * ValidStationNum
    !    if(mod(ii,ValidStationNum) ==0 ) then
    !        write(fileID, rec = ii ) CodesIndex(ValidStationNum),(R(ii,k), k = 1, AheadMonthNum)  !CodesIndex(jj),
    !    else
    !        write(fileID, rec = ii ) CodesIndex(mod(ii,ValidStationNum)),(R(ii,k), k = 1, AheadMonthNum)  !CodesIndex(jj),
    !    endif
    !end do
    !close(fileID)
    !open(fileID,FILE = 'P.bin',IOSTAT = iosval, FORM = 'UNFORMATTED', ACCESS = 'DIRECT',RECL = 26, STATUS = 'REPLACE')
    !do ii = 1,MonthNum*ValidStationNum
    !    if(mod(ii,ValidStationNum)) then
    !        write(fileID, rec = ii) CodesIndex(mod(ii,ValidStationNum)),(P(ii, k),k = 1, AheadMonthNum)  !CodesIndex(jj),
    !    else
    !        write(fileID, rec = ii) CodesIndex(ValidStationNum),(P(ii, k), k = 1, AheadMonthNum)  !CodesIndex(jj),
    !    endif
    !end do
    !close(fileID)

    ! 保存前15名,P<0.1

    !print *,"保存P<0.1结果"

    PPvalue = 0.1
    TempR2 = R**2
    !TempP = P
    WHERE(P >= PPvalue)   !删除显著水平低于0.1的值
      TempR2 = 0
    END WHERE

    WHERE(P == DataNumNotEnough)   !R .EQ. NaN
      TempR2 = DataNumNotEnough
    END WHERE
    !WHERE(TempR2 == R_Inf)   !R .EQ. Inf
    !  TempR2 = 0
    !END WHERE
    !WHERE(TempP == P_Inf)   !R .EQ. Infinity
    !  TempR2 = 0
    !END WHERE
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.1.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum
      !print *,"获取第一因子的相关信息"
      !与ii月份对应得最强预报因子站点信息
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"读取与第一个因子对应的Prcp数据"
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
        R2OneMonth = TempR2(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:)
        maxR2 = MAXVAL(R2OneMonth)
        CodesIndexLocation = MAXLOC(R2OneMonth)
        IF (Rtandtor1 < -1) THEN
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,DataNumNotEnough,DataNumNotEnough, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
        ELSE IF (maxR2 <= 0) THEN

          !print *,"调整Prcp数据"
          !根据pstor1ID和pstor2ID的情况，修订预报量、预报因子1及预报因子2的降雨量数据
          !另一个不需要修改，原封不动的copy即可
          ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
          savePtror1PrcpModify = savePtror1Prcp
          !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2PrcpModify
          !pause

          !print *,"统计不为0的数据量及位置"

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

          !print *,"重新调整ptand，ptor1，ptor2数据"
          saveTrainLen = FLOOR(tempCount*1.0)
          !设置trainLen的长度
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Prcp(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:saveTrainLen))

          !print *,"拟合第一预报因子与预报量"

          !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
          CALL LinearRegression(ptor1Prcp(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !计算预报量与第一预报因子的“残差”
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"拟合第二预报因子与预报量"

          !计算总预报降雨量
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY = ptandPrcpY1
          !当总预报方程中Y < 0时，直接设为0即可
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !计算预报量站点观测记录与预报的相关系数
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)

          !print *,"第一个因子信息："
          !print *,"原始记录：",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"第二个因子信息："
          !print *,"记录：",CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
          !print *,"本次计算信息："
          !print *,CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "保存文件"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))

          IF (TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"保存完成"

          !保存预测变量
          !ptandPrcp
          !ptor1Prcp
          !ptor2Prcp
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          !print *,"转换文件名称"
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          !print *,"转换文件名称完成"

          !print *,'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat'
          !pause

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.1.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Prcp(kk), &
              prcp_anomaly_missing, &
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
          !print *,"读取与第二个因子相关的数据，第",jj,"次"
          CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(ii,KIND=8),REAL(CodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8),YearLen,MonthNum,RankNum,&
            ValidStationNum,StartMonth,GhcnPrcpStandardDB)
          ALLOCATE(savePtror2Prcp(size(tempFactorPrcpMonth)))
          savePtror2Prcp = tempFactorPrcpMonth
          ALLOCATE(savePtand2Prcp(size(tempStudyPrcpMonth)))
          savePtand2Prcp = tempStudyPrcpMonth

          DEALLOCATE(tempFactorPrcpMonth)
          DEALLOCATE(tempStudyPrcpMonth)

          !print *,savePtand2Prcp
          !pause
          !print *,savePtror2Prcp
          !pause

          !print *,"调整Prcp数据"
          !根据pstor1ID和pstor2ID的情况，修订预报量、预报因子1及预报因子2的降雨量数据
          IF (SIZE(savePtror1Prcp)>SIZE(savePtror2Prcp)) THEN
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror2Prcp)))
            savePtror1PrcpModify = savePtror1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Prcp)+1:)
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror2Prcp)))
            savePtror2PrcpModify = savePtror2Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时需要修灯
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Prcp)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Prcp)+1:)
          ELSE IF(SIZE(savePtror1Prcp)<SIZE(savePtror2Prcp)) THEN
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror1Prcp)))
            savePtror2PrcpModify = savePtror2Prcp(SIZE(savePtror2Prcp)-SIZE(savePtror1Prcp)+1:)
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !此时，均不需要修订
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror2Prcp)))
            savePtror2PrcpModify = savePtror2Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2PrcpModify
          !pause

          !print *,"统计不为0的数据量及位置"

          tempCount = COUNT(savePtror2PrcpModify>=0 .AND. savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2PrcpModify)
            IF(savePtror2PrcpModify(kk)>=0 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"重新调整ptand，ptor1，ptor2数据"
          saveTrainLen = FLOOR(tempCount*1.0)
          !设置trainLen的长度
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Prcp(saveTrainLen))
          ALLOCATE(ptor2Prcp(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:saveTrainLen))
          ptor2Prcp = savePtror2PrcpModify(CIL(1:saveTrainLen))

          !print *,"拟合第一预报因子与预报量"

          !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
          CALL LinearRegression(ptor1Prcp(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !计算预报量与第一预报因子的“残差”
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"拟合第二预报因子与预报量"
          !线性回归拟合预报量与第一预报因子的“残差”和第二预报因子之间的线性关系
          CALL LinearRegression(ptor2Prcp(1:trainLen),ptandPrcpY1Residual(1:trainLen),trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(saveTrainLen))
          ptandPrcpY2 = -9999
          ptandPrcpY2 = ptor2Prcp*tempPtor2k + tempPtor2b
          !计算总预报降雨量
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !当总预报方程中Y < 0时，直接设为0即可
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !计算预报量站点观测记录与预报的相关系数
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"第一个因子信息："
          !print *,"原始记录：",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"第二个因子信息："
          !print *,"记录：",CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
          !print *,"本次计算信息："
          !print *,CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "保存文件"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2), tempPtor2k, tempPtor2b, maxR2,& !RptandY*RptandY, RptandY,PptandY,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))

          IF (TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"保存完成"

          !保存预测变量
          !ptandPrcp
          !ptor1Prcp
          !ptor2Prcp
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          !print *,"转换文件名称"
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          !print *,"转换文件名称完成"

          !print *,'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat'
          !pause

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.1.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Prcp(kk), &
              ptor2Prcp(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror2Prcp)

          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtror2PrcpModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptor2Prcp)
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




    ! 保存前15名,P<0.05
    PPvalue = 0.05
    TempR2 = R**2
    WHERE(P >= PPvalue)   !删除显著水平低于0.05的值
      TempR2 = 0
    END WHERE
    WHERE(P == DataNumNotEnough)   !R .EQ. NaN
      TempR2 = DataNumNotEnough
    END WHERE
    !WHERE(TempR2 == R_Inf)   !
    !  TempR2 = 0
    !END WHERE
    !WHERE(TempP == P_Inf)   !
    !  TempR2 = 0
    !END WHERE
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.05.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum

      !print *,"获取第一因子的相关信息"
      !与ii月份对应得最强预报因子站点信息
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"读取与第一个因子对应的Prcp数据"
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
        R2OneMonth = TempR2(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:)
        maxR2 = MAXVAL(R2OneMonth)
        CodesIndexLocation = MAXLOC(R2OneMonth)
        IF (Rtandtor1 < -1) THEN
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,DataNumNotEnough,DataNumNotEnough, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
        ELSE IF (maxR2 <= 0) THEN

          !print *,"调整Prcp数据"
          !根据pstor1ID和pstor2ID的情况，修订预报量、预报因子1及预报因子2的降雨量数据
          !另一个不需要修改，原封不动的copy即可
          ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
          savePtror1PrcpModify = savePtror1Prcp
          !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2PrcpModify
          !pause

          !print *,"统计不为0的数据量及位置"

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

          !print *,"重新调整ptand，ptor1，ptor2数据"
          saveTrainLen = FLOOR(tempCount*1.0)
          !设置trainLen的长度
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Prcp(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:saveTrainLen))

          !print *,"拟合第一预报因子与预报量"

          !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
          CALL LinearRegression(ptor1Prcp(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !计算预报量与第一预报因子的“残差”
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"拟合第二预报因子与预报量"

          !计算总预报降雨量
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY(1:trainLen) = ptandPrcpY1(1:trainLen)
          !当总预报方程中Y < 0时，直接设为0即可
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !计算预报量站点观测记录与预报的相关系数
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)

          !print *,"第一个因子信息："
          !print *,"原始记录：",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"第二个因子信息："
          !print *,"记录：",CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
          !print *,"本次计算信息："
          !print *,CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "保存文件"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))

          IF (TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"保存完成"

          !保存预测变量
          !ptandPrcp
          !ptor1Prcp
          !ptor2Prcp
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          !print *,"转换文件名称"
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          !print *,"转换文件名称完成"

          !print *,'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat'
          !pause

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.05.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Prcp(kk), &
              prcp_anomaly_missing, &
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
          !print *,"读取与第二个因子相关的数据，第",jj,"次"
          CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(ii,KIND=8),REAL(CodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8),YearLen,MonthNum,RankNum,&
            ValidStationNum,StartMonth,GhcnPrcpStandardDB)
          ALLOCATE(savePtror2Prcp(size(tempFactorPrcpMonth)))
          savePtror2Prcp = tempFactorPrcpMonth
          ALLOCATE(savePtand2Prcp(size(tempStudyPrcpMonth)))
          savePtand2Prcp = tempStudyPrcpMonth

          DEALLOCATE(tempFactorPrcpMonth)
          DEALLOCATE(tempStudyPrcpMonth)

          !print *,savePtand2Prcp
          !pause
          !print *,savePtror2Prcp
          !pause

          !print *,"调整Prcp数据"
          !根据pstor1ID和pstor2ID的情况，修订预报量、预报因子1及预报因子2的降雨量数据
          IF (SIZE(savePtror1Prcp)>SIZE(savePtror2Prcp)) THEN
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror2Prcp)))
            savePtror1PrcpModify = savePtror1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Prcp)+1:)
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror2Prcp)))
            savePtror2PrcpModify = savePtror2Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时需要修灯
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Prcp)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Prcp)+1:)
          ELSE IF(SIZE(savePtror1Prcp)<SIZE(savePtror2Prcp)) THEN
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror1Prcp)))
            savePtror2PrcpModify = savePtror2Prcp(SIZE(savePtror2Prcp)-SIZE(savePtror1Prcp)+1:)
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !此时，均不需要修订
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror2Prcp)))
            savePtror2PrcpModify = savePtror2Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2PrcpModify
          !pause

          !print *,"统计不为0的数据量及位置"

          tempCount = COUNT(savePtror2PrcpModify>=0 .AND. savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2PrcpModify)
            IF(savePtror2PrcpModify(kk)>=0 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"重新调整ptand，ptor1，ptor2数据"
          saveTrainLen = FLOOR(tempCount*1.0)
          !设置trainLen的长度
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Prcp(saveTrainLen))
          ALLOCATE(ptor2Prcp(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:saveTrainLen))
          ptor2Prcp = savePtror2PrcpModify(CIL(1:saveTrainLen))

          !print *,"拟合第一预报因子与预报量"

          !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
          CALL LinearRegression(ptor1Prcp(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !计算预报量与第一预报因子的“残差”
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"拟合第二预报因子与预报量"
          !线性回归拟合预报量与第一预报因子的“残差”和第二预报因子之间的线性关系
          CALL LinearRegression(ptor2Prcp(1:trainLen),ptandPrcpY1Residual(1:trainLen),trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(saveTrainLen))
          ptandPrcpY2 = -9999
          ptandPrcpY2 = ptor2Prcp*tempPtor2k + tempPtor2b
          !计算总预报降雨量
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !当总预报方程中Y < 0时，直接设为0即可
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !计算预报量站点观测记录与预报的相关系数
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"第一个因子信息："
          !print *,"原始记录：",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"第二个因子信息："
          !print *,"记录：",CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
          !print *,"本次计算信息："
          !print *,CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "保存文件"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2), tempPtor2k, tempPtor2b, maxR2,& !RptandY*RptandY, RptandY,PptandY,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))

          IF (TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
          END IF

          !保存预测变量
          !ptandPrcp
          !ptor1Prcp
          !ptor2Prcp
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.05.dat')
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Prcp(kk), &
              ptor2Prcp(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)



          DEALLOCATE(savePtror2Prcp)

          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtror2PrcpModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptor2Prcp)
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


    ! 保存前15名,P<0.01
    PPvalue = 0.01
    TempR2 = R**2
    WHERE(P >= PPvalue)   !删除显著水平低于0.05的值
      TempR2 = 0
    END WHERE
    WHERE(P == DataNumNotEnough)   !R .EQ. NaN
      TempR2 = DataNumNotEnough
    END WHERE
    !WHERE(TempR2 == R_Inf)   !R .EQ. NaN
    !  TempR2 = 0
    !END WHERE
    !WHERE(TempP == P_Inf)   !R .EQ. Infinity
    !  TempR2 = 0
    !END WHERE
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.01.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum

      !print *,"获取第一因子的相关信息"
      !与ii月份对应得最强预报因子站点信息
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"读取与第一个因子对应的Prcp数据"
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
        R2OneMonth = TempR2(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:)
        maxR2 = MAXVAL(R2OneMonth)
        CodesIndexLocation = MAXLOC(R2OneMonth)

        IF (Rtandtor1 < -1) THEN
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,DataNumNotEnough,DataNumNotEnough, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            maxR2,& !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
        ELSE IF (maxR2 <= 0) THEN

          !print *,"调整Prcp数据"
          !根据pstor1ID和pstor2ID的情况，修订预报量、预报因子1及预报因子2的降雨量数据
          !另一个不需要修改，原封不动的copy即可
          ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
          savePtror1PrcpModify = savePtror1Prcp
          !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2PrcpModify
          !pause

          !print *,"统计不为0的数据量及位置"

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

          !print *,"重新调整ptand，ptor1，ptor2数据"
          saveTrainLen = FLOOR(tempCount*1.0)
          !设置trainLen的长度
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Prcp(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:saveTrainLen))

          !print *,"拟合第一预报因子与预报量"

          !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
          CALL LinearRegression(ptor1Prcp(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !计算预报量与第一预报因子的“残差”
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"拟合第二预报因子与预报量"

          !计算总预报降雨量
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY(1:trainLen) = ptandPrcpY1(1:trainLen)
          !当总预报方程中Y < 0时，直接设为0即可
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !计算预报量站点观测记录与预报的相关系数
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)

          !print *,"第一个因子信息："
          !print *,"原始记录：",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"第二个因子信息："
          !print *,"记录：",CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
          !print *,"本次计算信息："
          !print *,CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "保存文件"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))

          IF (TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"保存完成"

          !保存预测变量
          !ptandPrcp
          !ptor1Prcp
          !ptor2Prcp
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          !print *,"转换文件名称"
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj
          !print *,"转换文件名称完成"

          !print *,'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat'
          !pause

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Prcp(kk), &
              prcp_anomaly_missing, &
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

          !print *,REAL(CodesIndex(CodesIndexLocation(1)),KIND=8),REAL(CodesIndexLocation(2),KIND=8)
          !pause

          !print *,"读取与第二个因子相关的数据，第",jj,"次"
          CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(ii,KIND=8),REAL(CodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8),YearLen,MonthNum,RankNum,&
            ValidStationNum,StartMonth,GhcnPrcpStandardDB)
          ALLOCATE(savePtror2Prcp(size(tempFactorPrcpMonth)))
          savePtror2Prcp = tempFactorPrcpMonth
          ALLOCATE(savePtand2Prcp(size(tempStudyPrcpMonth)))
          savePtand2Prcp = tempStudyPrcpMonth

          DEALLOCATE(tempFactorPrcpMonth)
          DEALLOCATE(tempStudyPrcpMonth)

          !print *,savePtror1Prcp, savePtand1Prcp, savePtror2Prcp, savePtand2Prcp
          !pause
          !print *,savePtand2Prcp
          !pause
          !print *,savePtror2Prcp
          !pause

          !print *,"调整Prcp数据"
          !根据pstor1ID和pstor2ID的情况，修订预报量、预报因子1及预报因子2的降雨量数据
          IF (SIZE(savePtror1Prcp)>SIZE(savePtror2Prcp)) THEN
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror2Prcp)))
            savePtror1PrcpModify = savePtror1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Prcp)+1:)
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror2Prcp)))
            savePtror2PrcpModify = savePtror2Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时需要修灯
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Prcp)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Prcp)-SIZE(savePtror2Prcp)+1:)
          ELSE IF(SIZE(savePtror1Prcp)<SIZE(savePtror2Prcp)) THEN
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror1Prcp)))
            savePtror2PrcpModify = savePtror2Prcp(SIZE(savePtror2Prcp)-SIZE(savePtror1Prcp)+1:)
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !此时，均不需要修订
            ALLOCATE(savePtror1PrcpModify(SIZE(savePtror1Prcp)))
            savePtror1PrcpModify = savePtror1Prcp
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror2Prcp)))
            savePtror2PrcpModify = savePtror2Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1PrcpModify
          !print *,savePtror2PrcpModify
          !pause

          !print *,"统计不为0的数据量及位置"

          tempCount = COUNT(savePtror2PrcpModify>=0 .AND. savePtand1PrcpModify>=0 .AND. savePtror1PrcpModify>=0)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2PrcpModify)
            IF(savePtror2PrcpModify(kk)>=0 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1PrcpModify(kk)>=0) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"重新调整ptand，ptor1，ptor2数据"
          saveTrainLen = FLOOR(tempCount*1.0)
          !设置trainLen的长度
          IF (leaveOut < 0) THEN
            trainLen = FLOOR(tempCount*TrainingRate)
          ELSE
            trainLen = tempCount - leaveOut
          END IF
          
          ALLOCATE(ptandPrcp(saveTrainLen))
          ALLOCATE(ptor1Prcp(saveTrainLen))
          ALLOCATE(ptor2Prcp(saveTrainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:saveTrainLen))
          ptor1Prcp = savePtror1PrcpModify(CIL(1:saveTrainLen))
          ptor2Prcp = savePtror2PrcpModify(CIL(1:saveTrainLen))

          !print *,"拟合第一预报因子与预报量"

          !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
          CALL LinearRegression(ptor1Prcp(1:trainLen),ptandPrcp(1:trainLen),trainLen,ptor1k,ptor1b)

          !print *,ptor1Prcp,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(saveTrainLen))
          ptandPrcpY1 = -9999
          ptandPrcpY1 = ptor1Prcp*ptor1k + ptor1b
          !计算预报量与第一预报因子的“残差”
          ALLOCATE(ptandPrcpY1Residual(saveTrainLen))
          ptandPrcpY1Residual = -9999
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"拟合第二预报因子与预报量"
          !线性回归拟合预报量与第一预报因子的“残差”和第二预报因子之间的线性关系
          CALL LinearRegression(ptor2Prcp(1:trainLen),ptandPrcpY1Residual(1:trainLen),trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(saveTrainLen))
          ptandPrcpY2 = -9999
          ptandPrcpY2 = ptor2Prcp*tempPtor2k + tempPtor2b
          !计算总预报降雨量
          ALLOCATE(ptandPrcpY(saveTrainLen))
          ptandPrcpY = -9999
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !当总预报方程中Y < 0时，直接设为0即可
          WHERE (ptandPrcpY < 0)
            ptandPrcpY = 0
          END WHERE
          !计算预报量站点观测记录与预报的相关系数
          CALL Correlation(trainLen,ptandPrcpY(1:trainLen),ptandPrcp(1:trainLen),RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"第一个因子信息："
          !print *,"原始记录：",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"第二个因子信息："
          !print *,"记录：",CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
          !print *,"本次计算信息："
          !print *,CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "保存文件"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2), tempPtor2k, tempPtor2b, maxR2,&  ! RptandY*RptandY, RptandY,PptandY,
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
            
          !print *,"k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
          !        "r:",R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
                  
          !print *,tempPtor2k,tempPtor2b,RptandY
          
          !pause

          IF (TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR2(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
          END IF

          !保存预测变量
          !ptandPrcp
          !ptor1Prcp
          !ptor2Prcp
          !ptandPrcpY1
          !ptandPrcpY2
          !ptandPrcpY
          WRITE(monthStr,'(I5)') ii
          WRITE(RankNumStr,'(I5)') jj

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.0.01.dat')
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Prcp(kk), &
              ptor2Prcp(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror2Prcp)
          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1PrcpModify)
          DEALLOCATE(savePtror2PrcpModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Prcp)
          DEALLOCATE(ptor2Prcp)
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
    istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'add_next_StationList\')

    CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式


  END DO
  !================================================================================================================
  istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'add_next_StationList') !设置默认的工作目录
  WRITE(StartStationStr,'(I5)') StartStationNum
  WRITE(EndStationStr,'(I5)') EndStationNum!StudyCode
  OPEN(fileID,FILE = TRIM(ADJUSTL(StartStationStr))//'-'//TRIM(ADJUSTL(EndStationStr))//'.dat',IOSTAT = iosval)
  DO i = 1, analysisStationNum
    WRITE(fileID,'(I15,12I3)') ValidStationCoupled(i,:)
  END DO
  CLOSE(fileID)
  !================================================================================================================
  PRINT "(1X,A17/)", '>>程序运行结束！'
  CALL Display_Time_Current(DateT,NowTime)
  WRITE(*,100)'当前时间：',DateT(1:4),'/',DateT(5:6),'/',DateT(7:8),'   ',NowTime      ! 写出时间字符串
  PRINT "(/1X,A19)",'按任意键结束程序...'
  PAUSE
  STOP
8089 PRINT *,'文件读取错误'

  END SUBROUTINE CalStationPrcpRP_BP_add_next



