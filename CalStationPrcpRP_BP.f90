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

SUBROUTINE CalStationPrcpRP_BP(StartRate,EndRate)
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
      CHARACTER(LEN = 5)  :: StationNumStr                            ! 进度条中显示的站点数
      CHARACTER(LEN = 500) :: WorkSpace
      CHARACTER(LEN = 100) :: dirname
      CHARACTER(LEN = 5) :: PressKey
      CHARACTER(LEN = 5) :: StartStationStr,EndStationStr
      INTEGER :: times                                                !
      INTEGER :: GhcnPrcpColNum                                       !GHCNPrcp数据列数
      INTEGER :: MissVal                                              !缺省值处理（1为处理，0为不处理）
      INTEGER :: TraceVal                                             !痕迹降水的处理（1为处理，0为不处理）
      INTEGER :: AheadMonthNum                                        !最大提前预报的月份数
      INTEGER :: StartMonth                                           !降雨数据记录（GhcnPrcp）开始的月份
      INTEGER :: MonthNum                                             !一年的月份数
      INTEGER :: StartYear,EndYear,YearLen                            !预报研究的开始年份、结束年份、时间长度
      INTEGER :: RankNum                                              !站点有效排名
      INTEGER :: II,JJ,KK                                             !随机使用的变量
      INTEGER :: N                                                    !拟合时自变量的最高次方数
      INTEGER :: CoverYears                                           !设置北京站点和预报因子站点降水月值数据能成Couple的最少数量
      INTEGER :: StationInfoFileLen                                   ! GHCN Station信息的数量
      INTEGER :: StationNum                                           ! GhcnPrcp数据中站点数量
      INTEGER :: GhcnPrcpRowNum                                       ! GHCNPrcp数据的行数
      INTEGER :: ValidStationNum                                      !GhcnPrcp数据库中起始终止年在StartYear-EndYear之间的站点数
      INTEGER :: fileID = 10, iosval                                  !文件ID，文件操作状态
      INTEGER :: istatus,istatus_dir_ch                               ! 改变当前工作目录成功与否的状态，新建文件夹成功与否的状态
      INTEGER :: tempNum ,tempCount,ColStart                            !临时变量
      INTEGER :: FactorPrcpLen                                        !预报因子StartYear->EndYear期间数据长度
      INTEGER :: i,j,k,ic,im,StartStationNum,EndStationNum,analysisStationNum
      INTEGER :: ClimateStatus
      INTEGER :: R2CountTotal                                          !单站点单月份有效R2的总数量
      INTEGER(KIND = 8)  StudyCode
      INTEGER(KIND = 8), ALLOCATABLE :: CodesIndex(:)                 !提取出研究的站点编号
      INTEGER(KIND = 8), ALLOCATABLE :: CodesIndexLocation(:)         !存储某一个站点再数据库中的索引值(行位置)
      INTEGER(KIND = 8), ALLOCATABLE :: GhcnPrcpYear(:,:)             !GhcnPrcp站点数据的开始年份和结束年份信息
      INTEGER(KIND = 8), ALLOCATABLE :: IntegerArrayTemp2D(:,:)       ! 2D整型变量临时存储
      INTEGER(KIND = 8), ALLOCATABLE :: ValidStationCoupled(:,:)      !通过限制条件的站点，及通过情况，1通过，0不通过
      REAL(KIND = 8) :: DataNumNotEnough = -9.0    !, R_Inf = -5.0, P_Inf = -6.0   不在存在该情况
      REAL(KIND = 8) :: prcpRightLimit,prcp_anomaly_missing,prcp_anomaly_trace
      REAL(KIND = 8), ALLOCATABLE :: StudyPrcp(:),TempStudyPrcp(:),TempMonthStudyPrcp(:)    !研究区站点的降水数据
      REAL(KIND = 8), ALLOCATABLE :: FactorPrcp(:),TempFactorPrcp(:),TempMonthFactorPrcp(:) !预报因子站点的降水数据
      REAL(KIND = 8), ALLOCATABLE :: StationCodesUnique(:)            !GhcnPrcp中Codes编号
      REAL(KIND = 8), ALLOCATABLE :: RealArrayTemp2D(:,:)             !读取GhcnPrcp站点数据的临时数据库
      REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:)          !GhcnPrcp存放格式标准化数据库
      REAL, ALLOCATABLE :: P(:,:),R(:,:),TempR(:,:),TempP(:,:),R2Count(:,:)        ! 预报每一个站点时，P\R信息
      REAL :: StartRate,EndRate
      REAL :: PPvalue                     !显著性水平
      REAL :: TrainingRate                !训练数据占总数据的比例
      REAL :: maxR2, minR2,avgR2
      REAL :: R2Total                                          !单站点单月份有效R2的总和
      TYPE( CLS_CMD_Progress ) ::Progress  !进度条
      LOGICAL(4) :: istatus_dir_mk,alive                              !文件存在状态
      NAMELIST /CSPRPBP/ prcp_anomaly_missing,prcp_anomaly_trace,times,GhcnPrcpColNum,MissVal,&
                         TraceVal,AheadMonthNum,StartMonth,StartYear,EndYear,MonthNum,ClimateStatus,&
                         RankNum,PPvalue,TrainingRate,CoverYears
      !****************************************************************************
      ! !                        Formatting
      !****************************************************************************

100   FORMAT(1X,A10,A4,A1,A2,A1,A2,A3,A8/)
200   FORMAT(I11,1X,A20,A10,F7.2,1X,F7.2,I5/)
300   FORMAT(I20,I20,I20)
400   FORMAT(I20,1320f10.1)
500   FORMAT(f20.0,1320f10.1)
600   FORMAT(I15,24F8.4)

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
      GhcnPrcpFile = TRIM(Path)//'\GhcnData\v2.prcp.dat'
      StationInfoFile = TRIM(Path)//'\GhcnData\v2.prcp.inv'
      WorkSpace = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\'
      istatus_dir_ch = CHDIR(TRIM(WorkSpace)) !设置默认的工作目录
      ! read default namelist file which define default parameters
      OPEN(UNIT = fileID,FILE = './CalStationPrcpRP_BP.namelist')
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
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !    calculate R R2 P of each station in 12 month in max aheadmonth 24 use other station
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ALLOCATE(StudyPrcp(YearLen*MonthNum))
      ALLOCATE(FactorPrcp(YearLen*MonthNum))
      INQUIRE(DIRECTORY = TRIM(WorkSpace)//'StationList', EXIST = alive)
      IF (alive == .false.) THEN
        istatus_dir_mk = makedirqq(TRIM('StationList'))
      END IF
      istatus_dir_ch = CHDIR(TRIM(WorkSpace)//TRIM('StationList\'))
      ALLOCATE(R(ValidStationNum*MonthNum,AheadMonthNum))
      ALLOCATE(TempR(ValidStationNum*MonthNum,AheadMonthNum))
      ALLOCATE(P(ValidStationNum*MonthNum,AheadMonthNum))
      ALLOCATE(TempP(ValidStationNum*MonthNum,AheadMonthNum))
      ALLOCATE(R2Count(ValidStationNum*MonthNum,AheadMonthNum))
      CALL Progress % Set( N = ValidStationNum , L = 30 )!// size(CodesIndex)次，显示长度30
      Progress % M = "#" !// 已完成部分的字符，不是必须
      Progress % O = " " !// 未完成部分的字符，不是必须
      
      PRINT *,'是否需要计算某一个或者某一段站点？ y/n'
      READ (*,*),PressKey
      IF(TRIM(PressKey) == 'n') THEN
          StartStationNum = FLOOR(ValidStationNum*StartRate)+1
          EndStationNum = FLOOR(ValidStationNum*EndRate)
      ELSE
          PRINT *,' '
96        PRINT *,'Please input StartNum(as integer format ,StartNum should >= 1 and <= 20547):'
          PRINT *,' '
          READ (*,*) StartStationNum
          PRINT *,' '
          PRINT *,'Please input StartNum(as integer format ,StartNum should >= 1 and <= 20547):'
          PRINT *,' '
          READ (*,*) EndStationNum
          PRINT *,' '
      END IF
      
      analysisStationNum = EndStationNum - StartStationNum + 1
      ALLOCATE(ValidStationCoupled(analysisStationNum,13))
      ValidStationCoupled = 0
      
      PRINT *,analysisStationNum
      
      ! 为了将异常值排除在外，计算可预报性时我们将-9998转换为-9999
      WHERE (GhcnPrcpStandardDB == -9998)
        GhcnPrcpStandardDB = -9999
      END WHERE
      
      DO i = StartStationNum,EndStationNum
        ValidStationCoupled(i - StartStationNum + 1,1) = CodesIndex(i)
        R = DataNumNotEnough
        P = DataNumNotEnough
        WRITE(StationDirName,'(I11)') CodesIndex(i)!StudyCode
        istatus_dir_mk = MAKEDIRQQ(TRIM(StationDirName))
        istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'StationList\'//TRIM(StationDirName))
        StudyPrcp = GhcnPrcpStandardDB(i,2:)
        WRITE(StationNumStr,'(I5)') i
        Progress % Prefix = "StationNum:"//TRIM(StationNumStr)//'/20547   '  !// 前方提示文字，不是必须
        
        DO j = 1,ValidStationNum
          FactorPrcp = GhcnPrcpStandardDB(j,2:)
          FactorPrcpLen = MonthNum*YearLen
          DO k = 1,AheadMonthNum
            ALLOCATE(TempFactorPrcp(FactorPrcpLen - k))
            ALLOCATE(TempStudyPrcp(FactorPrcpLen - k))
            TempFactorPrcp = FactorPrcp(1:FactorPrcpLen - k)
            TempStudyPrcp = StudyPrcp(k+1:)
            DO ii = 1,MonthNum
              ALLOCATE(TempMonthFactorPrcp(SIZE(TempFactorPrcp(ii:SIZE(TempFactorPrcp):MonthNum))))
              ALLOCATE(TempMonthStudyPrcp(SIZE(TempStudyPrcp(ii:SIZE(TempStudyPrcp):MonthNum))))
              TempMonthFactorPrcp = TempFactorPrcp(ii:SIZE(TempFactorPrcp):MonthNum)
              TempMonthStudyPrcp = TempStudyPrcp(ii:SIZE(TempStudyPrcp):MonthNum)
              
              if((isContinuityGT_M(TempMonthStudyPrcp,ClimateStatus,prcp_anomaly_missing) == .false.) .AND.&
                 (isContinuityGT_M(TempMonthStudyPrcp,ClimateStatus,prcp_anomaly_trace) == .false.) .AND.&
                 (isContinuityGT_M(TempMonthFactorPrcp,ClimateStatus,prcp_anomaly_missing) == .false.) .AND.&
                 (isContinuityGT_M(TempMonthFactorPrcp,ClimateStatus,prcp_anomaly_trace) == .false.))then
                tempCount = COUNT(TempMonthFactorPrcp>=0.and.TempMonthStudyPrcp>=0)
                ALLOCATE(CodesIndexLocation(tempCount))
                tempNum = 0
                DO jj = 1,SIZE(TempMonthFactorPrcp)
                  IF(TempMonthFactorPrcp(jj)>=0.AND.TempMonthStudyPrcp(jj)>=0) THEN
                    tempNum = tempNum + 1
                    CodesIndexLocation(tempNum) = jj
                  END IF
                END DO

                IF(tempCount >= CoverYears) THEN
                  IF(MOD(k + ii + StartMonth - 1,MonthNum) == 0) THEN
                    ValidStationCoupled(i - StartStationNum + 1 ,MonthNum+1) = 1  !统计站点是否存在配对记录，由于此次只是为了统计是否存在记录
                    CALL Correlation(FLOOR(tempCount*TrainingRate),TempMonthFactorPrcp(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),&
                      TempMonthStudyPrcp(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),R(j+(MonthNum-1)*ValidStationNum,k))
                    !IF(R(j+(MonthNum-1)*ValidStationNum,k) == R_Inf) THEN
                    !  P(j+(MonthNum-1)*ValidStationNum,k) = R_Inf
                    !ELSE
                    CALL Pvalue(FLOOR(tempCount*TrainingRate),R(j+(MonthNum-1)*ValidStationNum,k),P(j+(MonthNum-1)*ValidStationNum,k))
                    !END IF

                    !print *,'codsID:',CodesIndex(j),'ahead:',k,',month:',MonthNum,',r:',R(j+(MonthNum-1)*ValidStationNum,k),',p:',P(j+(MonthNum-1)*ValidStationNum,k)

                  ELSE
                    ValidStationCoupled(i - StartStationNum + 1 ,MOD(k + ii + StartMonth - 1,MonthNum)+1) = 1  !统计站点是否存在配对记录，由于此次只是为了统计是否存在记录
                    CALL Correlation(FLOOR(tempCount*TrainingRate),TempMonthFactorPrcp(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),&
                      TempMonthStudyPrcp(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),&
                      R(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidStationNum,k))
                    !IF(R(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidStationNum,k) == R_Inf) THEN
                    !  P(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidStationNum,k) = R_Inf
                    !ELSE
                    CALL Pvalue(FLOOR(tempCount*TrainingRate),R(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidStationNum,k),&
                      P(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidStationNum,k))
                    !END IF

                    !print *,'codsID:',CodesIndex(j),'ahead:',k,',month:',MOD(k + ii + StartMonth - 1,MonthNum),',r:',R(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidStationNum,k),',p:',P(j+(MOD(k + ii + StartMonth - 1,MonthNum)-1)*ValidStationNum,k)

                  END IF

                  !pause

                END IF
                DEALLOCATE(CodesIndexLocation)
              endif
              DEALLOCATE(TempMonthFactorPrcp)
              DEALLOCATE(TempMonthStudyPrcp)
            END DO
            DEALLOCATE(TempFactorPrcp)
            DEALLOCATE(TempStudyPrcp)
          END DO
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
        PPvalue = 0.1
        TempR = R**2
        !TempP = P
        WHERE(P >= PPvalue)   !删除显著水平低于0.1的值
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
          DO jj = 1,RankNum
            maxR2 = MAXVAL(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
            CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
            WRITE(fileID,'(I15,I10,3F6.2)') CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),maxR2,&
                  R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
                  P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
            IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
              TempR(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
            END IF
          END DO
        END DO
        CLOSE(fileID)
        
        ! 保存最差R2,P<0.1
        PPvalue = 0.1
        TempR = R**2
        !TempP = P
        WHERE(P >= PPvalue)   !删除显著水平低于0.1的值
          TempR = 9
        END WHERE
        
        WHERE(P == DataNumNotEnough)   !R .EQ. NaN
          TempR = 9
        END WHERE
        
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.1_minimum.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          minR2 = MINVAL(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
          CodesIndexLocation = MINLOC(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
          WRITE(fileID,'(I15,I10,3F8.4)') CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),minR2,&
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
        END DO
        CLOSE(fileID)
        
        ! 保存平均R2,P<0.1
        PPvalue = 0.1
        TempR = R**2
        R2Count = 0
        R2CountTotal = 0
        R2Total = 0
        !TempP = P
        WHERE((P < PPvalue) .AND. (P /= DataNumNotEnough))   !R有效值计数
          R2Count = 1
        END WHERE
        
        WHERE((P >= PPvalue) .OR. (P == DataNumNotEnough))
          TempR = 0
        END WHERE
        
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.1_averge.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          !统计相应月份的有效值的r2及有效r2的数量以便计算平均值
          DO jj = 1+(ii-1)*ValidStationNum, ii*ValidStationNum
            DO kk = 1,AheadMonthNum
              IF (R2Count(jj,kk) > 0) THEN
                R2CountTotal = R2CountTotal + 1
                R2Total = R2Total + TempR(jj,kk)
              END IF
            END DO
          END DO
          
          IF (R2Total > 0) THEN
            avgR2 = R2Total/R2CountTotal
            WRITE(fileID,'(I5,2F8.4)') ii, avgR2, 0.0
          ELSE
            WRITE(fileID,'(I5,2F8.4)') ii, -9.0, -9.0
          END IF
        END DO
        CLOSE(fileID)
        
        ! 保存前15名,P<0.05
        PPvalue = 0.05
        TempR = R**2
        WHERE(P >= PPvalue)   !删除显著水平低于0.05的值
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
          DO jj = 1,RankNum
            maxR2 = MAXVAL(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
            CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
            WRITE(fileID,'(I15,I10,3F6.2)') CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),maxR2,&
                  R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
                  P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
            IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
              TempR(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
            END IF
          END DO
        END DO
        CLOSE(fileID)
        ! 保存最差R2,P<0.05
        PPvalue = 0.05
        TempR = R**2
        !TempP = P
        WHERE(P >= PPvalue)   !删除显著水平低于0.1的值
          TempR = 9
        END WHERE
        
        WHERE(P == DataNumNotEnough)   !R .EQ. NaN
          TempR = 9
        END WHERE
        
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.05_minimum.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          minR2 = MINVAL(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
          CodesIndexLocation = MINLOC(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
          WRITE(fileID,'(I15,I10,3F8.4)') CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),minR2,&
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
        END DO
        CLOSE(fileID)
        
        ! 保存平均R2,P<0.05
        PPvalue = 0.05
        TempR = R**2
        R2Count = 0
        R2CountTotal = 0
        R2Total = 0
        !TempP = P
        WHERE((P < PPvalue) .AND. (P /= DataNumNotEnough))   !R有效值计数
          R2Count = 1
        END WHERE
        
        WHERE((P >= PPvalue) .OR. (P == DataNumNotEnough))
          TempR = 0
        END WHERE
        
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.05_averge.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          !统计相应月份的有效值的r2及有效r2的数量以便计算平均值
          DO jj = 1+(ii-1)*ValidStationNum, ii*ValidStationNum
            DO kk = 1,AheadMonthNum
              IF (R2Count(jj,kk) > 0) THEN
                R2CountTotal = R2CountTotal + 1
                R2Total = R2Total + TempR(jj,kk)
              END IF
            END DO
          END DO
          IF (R2Total > 0) THEN
            avgR2 = R2Total/R2CountTotal
            WRITE(fileID,'(I5,2F8.4)') ii, avgR2, 0.0
          ELSE
            WRITE(fileID,'(I5,2F8.4)') ii, -9.0, -9.0
          END IF
        END DO
        CLOSE(fileID)

        ! 保存前15名,P<0.01
        PPvalue = 0.01
        TempR = R**2
        WHERE(P >= PPvalue)   !删除显著水平低于0.05的值
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
          DO jj = 1,RankNum
            maxR2 = MAXVAL(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
            CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
            WRITE(fileID,'(I15,I10,3F6.2)') CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),maxR2,&
                R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
                P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
            IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
              TempR(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)) = 0
            END IF
          END DO
        END DO
        CLOSE(fileID)
        ! 保存最差R2,P<0.01
        PPvalue = 0.01
        TempR = R**2
        !TempP = P
        WHERE(P >= PPvalue)   !删除显著水平低于0.1的值
          TempR = 9
        END WHERE
        
        WHERE(P == DataNumNotEnough)   !R .EQ. NaN
          TempR = 9
        END WHERE
        
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.01_minimum.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          minR2 = MINVAL(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
          CodesIndexLocation = MINLOC(TempR(1+(ii-1)*ValidStationNum:ii*ValidStationNum,:))
          WRITE(fileID,'(I15,I10,3F8.4)') CodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),minR2,&
            R(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidStationNum,CodesIndexLocation(2))
        END DO
        CLOSE(fileID)
        
        ! 保存平均R2,P<0.01
        PPvalue = 0.01
        TempR = R**2
        R2Count = 0
        R2CountTotal = 0
        R2Total = 0
        !TempP = P
        WHERE((P < PPvalue) .AND. (P /= DataNumNotEnough))   !R有效值计数
          R2Count = 1
        END WHERE
        
        WHERE((P >= PPvalue) .OR. (P == DataNumNotEnough))
          TempR = 0
        END WHERE
        
        OPEN(fileID,FILE = 'R_RankNum15_P.LT.0.01_averge.dat',IOSTAT = iosval)
        DO ii = 1,MonthNum
          !统计相应月份的有效值的r2及有效r2的数量以便计算平均值
          DO jj = 1+(ii-1)*ValidStationNum, ii*ValidStationNum
            DO kk = 1,AheadMonthNum
              IF (R2Count(jj,kk) > 0) THEN
                R2CountTotal = R2CountTotal + 1
                R2Total = R2Total + TempR(jj,kk)
              END IF
            END DO
          END DO
          IF (R2Total > 0) THEN
            avgR2 = R2Total/R2CountTotal
            WRITE(fileID,'(I5,2F8.4)') ii, avgR2, 0.0
          ELSE
            WRITE(fileID,'(I5,2F8.4)') ii, -9.0, -9.0
          END IF
        END DO
        CLOSE(fileID)
        
        DEALLOCATE(CodesIndexLocation)
        istatus_dir_ch = CHDIR(TRIM(WorkSpace)//'StationList\')
        
        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
        
      END DO
      !================================================================================================================
      istatus_dir_ch = CHDIR(TRIM(WorkSpace)) !设置默认的工作目录
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
8089  PRINT *,'文件读取错误'

END SUBROUTINE CalStationPrcpRP_BP


    
