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
      CHARACTER(LEN = 10) :: DateT                         !记录日期
      CHARACTER(LEN = 8) :: NowTime                        !记录时间
      CHARACTER(LEN = 500) :: Path                         !调试文件exe路径, Name
      CHARACTER(LEN = 500) :: GhcnTavgFile                 !Temperature average文件绝对地址
      CHARACTER(LEN = 500) :: StationInfoFile              !station信息文件
      CHARACTER(LEN = 11) :: StationDirName                !每一个Station子文件夹临时名
      CHARACTER(LEN = 5)  :: StationNumStr                 !进度条中显示的站点数
      CHARACTER(LEN = 500) :: WorkSpace
      CHARACTER(LEN = 500) :: PrcpWorkSpace
      CHARACTER(LEN = 500) :: GhcnPrcpStandardDBFile
      CHARACTER(LEN = 500) :: dirname
      CHARACTER(LEN = 5) :: PressKey
      CHARACTER(LEN = 5) :: StartStationStr,EndStationStr
      INTEGER :: times
      INTEGER :: GhcnTavgColNum = 14                       !GHCNTavg数据列数
      INTEGER :: MissVal                                   !缺省值处理（1为处理，0为不处理）
      INTEGER :: TraceVal                                  !痕迹降水的处理（1为处理，0为不处理）
      INTEGER :: AheadMonthNum                             !最大提前预报的月份数
      INTEGER :: StartMonth                                !降雨数据记录（Ghcn Temperature average）开始的月份
      INTEGER :: MonthNum                                  !一年的月份数
      INTEGER :: StartYear,EndYear,YearLen                 !预报研究的开始年份、结束年份、时间长度
      INTEGER :: RankNum                                   !站点有效排名
      INTEGER :: II,JJ,KK                                  !随机使用的变量
      INTEGER :: N                                         !拟合时自变量的最高次方数
      INTEGER :: CoverYears                                !设置北京站点和预报因子站点降水月值数据能成Couple的最少数量
      INTEGER :: StationInfoFileLen                        !GHCN Station信息的数量
      INTEGER :: StationNum                                !Ghcn Temperature average数据中站点数量
      INTEGER :: GhcnTavgRowNum                            ! GhcnTemperature average数据的行数
      INTEGER :: ValidTavgStationNum                       !GhcnTavg数据库中起始终止年在StartYear-EndYear之间的站点数
      INTEGER :: ValidPrcpStationNum                       !GhcnPrcp数据库中起始终止年在StartYear-EndYear之间的站点数
      INTEGER :: fileID = 10, iosval                       !文件ID，文件操作状态
      INTEGER :: istatus,istatus_dir_ch                    ! 改变当前工作目录成功与否的状态，新建文件夹成功与否的状态
      INTEGER :: tempNum ,tempCount,ColStart                 !临时变量
      INTEGER :: FactorTavgLen                             !预报因子StartYear->EndYear期间数据长度
      INTEGER :: i,j,k,ic,im,StartStationNum,EndStationNum,analysisStationNum
      INTEGER :: ClimateStatus
      INTEGER(KIND = 8)  StudyCode
      INTEGER(KIND = 8), ALLOCATABLE :: ValidTavgStationCodesIndex(:)   !提取出Factor的站点编号
      INTEGER(KIND = 8), ALLOCATABLE :: ValidPrcpStationCodesIndex(:)   !提取出研究的站点编号
      INTEGER(KIND = 8), ALLOCATABLE :: CodesIndexLocation(:)           !存储某一个站点再数据库中的索引值(行位置)
      INTEGER(KIND = 8), ALLOCATABLE :: GhcnTavgYear(:,:)               !GhcnTavg站点数据的开始年份和结束年份信息
      INTEGER(KIND = 8), ALLOCATABLE :: IntegerArrayTemp2D(:,:)         ! 2D整型变量临时存储
      INTEGER(KIND = 8), ALLOCATABLE :: ValidStationCoupled(:,:)      !通过限制条件的站点，及通过情况，1通过，0不通过
      REAL(KIND = 8) :: DataNumNotEnough = -9.0    !,R_Inf = -5.0, P_Inf = -6.0 ,tg
      REAL(KIND = 8) :: tavgLeftLimit,tavgRightLimit,prcp_anomaly_value,tavg_anomaly_value
      REAL(KIND = 8), ALLOCATABLE :: StudyPrcp(:),TempStudyPrcp(:),TempMonthStudyPrcp(:)     !研究区站点的降水数据
      REAL(KIND = 8), ALLOCATABLE :: FactorTavg(:),TempFactorTavg(:),TempMonthFactorTavg(:)  !预报因子站点的降水数据
      REAL(KIND = 8), ALLOCATABLE :: StationCodesUnique(:)                                   !GhcnTavg中Codes编号
      REAL(KIND = 8), ALLOCATABLE :: RealArrayTemp2D(:,:)                                    !读取GhcnTavg站点数据的临时数据库
      REAL(KIND = 8), ALLOCATABLE :: GhcnTavgStandardDB(:,:)                                 !GhcnTavg存放格式标准化数据库
      REAL(KIND = 8), ALLOCATABLE :: GhcnPrcpStandardDB(:,:)                                 !GhcnPrcp存放格式标准化数据库
      REAL, ALLOCATABLE :: P(:,:),R(:,:),TempR(:,:),TempP(:,:)                               ! 预报每一个站点时，P\R信息
      REAL :: StartRate,EndRate
      REAL :: PPvalue                                       !显著性水平
      REAL :: TrainingRate                                  !训练数据占总数据的比例
      REAL :: maxR2
      TYPE( CLS_CMD_Progress ) :: Progress                  !Commands line process bar
      LOGICAL(4) :: istatus_dir_mk,alive                                !文件存在状态
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
      PRINT "(1X,A17/)", '>>程序开始运行...'
      CALL Display_Time_Current(DateT,NowTime)
      WRITE(*,100)'当前时间：',DateT(1:4),'/',DateT(5:6),'/',DateT(7:8),'   ',NowTime      ! 写出时间字符串
      ! get current direction and set work direction
      PRINT "(1X,A19)", '>>程序参数初始化...'
      CALL SLEEP(1)
      istatus = GETCWD(Path)
      GhcnTavgFile = TRIM(Path)//'\GhcnData\ghcnm.tavg.v3.3.0.20170515.qcu.dat'
      StationInfoFile = TRIM(Path)//'\GhcnData\ghcnm.tavg.v3.3.0.20161009.qcu.inv'
      WorkSpace = TRIM(Path)//'\BaseOnTavg\CalculateStationPrcpRP\'
      PrcpWorkSpace = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\'
      GhcnPrcpStandardDBFile = TRIM(Path)//'\BaseOnPrcp\CalculateStationPrcpRP\DataBase\GhcnPrcpStandardDB.dat'
      istatus_dir_ch = CHDIR(TRIM(WorkSpace)) !设置默认的工作目录
      ! read default namelist file which define default parameters
      OPEN(UNIT = FileID,FILE = './CalStationPrcpRP_BT.namelist')
      READ (fileID,NML = CSPRPBT,ERR = 8089)
      CLOSE(fileID)
      PRINT "(1X,A18)", ">>参数初始化完成！"
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
      PRINT *,'>>读取GhcnTavg数据...'
      CALL SLEEP(1)
      CALL Inqire_Text_Row(TRIM(GhcnTavgFile),LEN(TRIM(GhcnTavgFile)), GhcnTavgRowNum)
      ALLOCATE(GhcnTavg(GhcnTavgRowNum,GhcnTavgColNum)) !动态分配GHCN原始月平均温度数据库大小
      CALL Read_GHCN_Tavg(TRIM(GhcnTavgFile), LEN(TRIM(GhcnTavgFile)), GhcnTavgRowNum, GhcnTavgColNum, MissVal)!读取GHCN降水数据
      PRINT *,'>>读取GhcnTavg数据完成！'
      CALL SLEEP(1)
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !           get and save ghcnTavg original station codes information 
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PRINT *,">>查询GhcnTavg站点编号信息文件是否存在..."
      CALL SLEEP(1)
      INQUIRE(DIRECTORY = TRIM(WorkSpace)//'DataBase', EXIST = alive)
      IF (alive == .false.) THEN
        istatus_dir_mk = makedirqq(TRIM('DataBase'))
      END IF
      INQUIRE(FILE = './DataBase/StationCodes.dat', EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>GhcnTavg站点编号信息文件存在！"
        CALL SLEEP(1)
        PRINT *, ">>读取GhcnTavg站点编号信息..."
        CALL SLEEP(1)
        CALL Inqire_Text_Row('./DataBase/StationCodes.dat',LEN('./DataBase/StationCodes.dat'),StationNum)
        ALLOCATE(StationCodesUnique(StationNum))
        OPEN(UNIT = fileID,FILE = './DataBase/StationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          &POSITION = 'REWIND') ! RECL = 20,
        READ(fileID,'(F20.0)') StationCodesUnique
        CLOSE(fileID)
        PRINT *, ">>读取GhcnTavg站点编号信息成功！"
        CALL SLEEP(1)
      ELSE
        PRINT *, ">>GhcnTavg站点编号信息文件不存在"
        CALL SLEEP(1)
        PRINT *, ">>提取GhcnTavg中站点编号信息..."
        CALL SLEEP(1)
        CALL Unique(GhcnTavg(1:GhcnTavgRowNum,1),StationCodesUnique)
        StationNum = SIZE(StationCodesUnique)
        PRINT *, ">>提取GhcnTavg中站点编号信息完成！"
        CALL SLEEP(1)
        PRINT *,">>保存GhcnTavg中站点编号信息..."
        CALL SLEEP(1)
        OPEN(UNIT = fileID,FILE = './DataBase/StationCodes.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') ! RECL = 20,
        WRITE(fileID,'(I20)') INT(StationCodesUnique,8)
        CLOSE(fileID)
        PRINT *,">>保存GhcnTavg中站点编号信息完成(当前exe目录下\StationPrediction\databaseStationCodes.dat文件)！"
        CALL SLEEP(1)
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !        get and save ghcnTavg station start-end year information
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PRINT *,">>查询GhcnTavg Station Start-End Year文件是否存在..."
      CALL SLEEP(1)
      inquire(FILE = './DataBase/StationStartEndYear.dat', EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>GhcnTavg Station Start-End Year文件存在！"
        CALL SLEEP(1)
        PRINT *, ">>读取GhcnTavg Station Start-End Year文件信息..."
        CALL SLEEP(1)
        ALLOCATE(GhcnTavgYear(StationNum,3))
        ALLOCATE(IntegerArrayTemp2D(3,StationNum))
        OPEN(UNIT = 50,FILE = './DataBase/StationStartEndYear.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          &  POSITION = 'REWIND') !RECL = 60,
        READ(50,300) IntegerArrayTemp2D
        CLOSE(fileID)
        GhcnTavgYear = TRANSPOSE(IntegerArrayTemp2D)
        PRINT *, ">>读取GhcnTavg Station Start-End Year文件成功！"
        CALL SLEEP(1)
      ELSE
        PRINT *, ">>GhcnTavg Station Start-End Year文件不存在..."
        CALL SLEEP(1)
        PRINT *, ">>提取GhcnTavg Station Start-End Year信息..."
        CALL SLEEP(1)
        ALLOCATE(GhcnTavgYear(StationNum,3))
        GhcnTavgYear(1:StationNum,1) = INT(StationCodesUnique,8)
        GhcnTavgYear(1:StationNum,2:3) = 0
        CALL Progress % Set( N = StationNum , L = 30 )!// StationNum次，显示长度30
        Progress % Prefix = "Station Start-End Year:  "  !// 前方提示文字，不是必须
        Progress % M = "|" !// 已完成部分的字符，不是必须
        Progress % O = " " !// 未完成部分的字符，不是必须
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
          CALL Progress % Put( II , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
        END DO
        PRINT *, ">>提取GhcnTavg Station Start-End Year信息完成！"
        CALL SLEEP(1)
        PRINT *,">>保存GhcnTavg Station Start-End Year文件..."
        CALL SLEEP(1)
        OPEN(UNIT = fileID,FILE = './DataBase/StationStartEndYear.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') ! RECL = 60,
        WRITE(fileID,300) TRANSPOSE(GhcnTavgYear)
        CLOSE(fileID)
        PRINT *,">>保存GhcnTavg Station Start-End Year文件完成(当前exe目录下StationStartEnd.dat文件)！"
        CALL SLEEP(1)
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !         选取站点数据时间跨度在1901-2010年的站点,建立标准化存储数据库
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      YearLen = EndYear - StartYear + 1
      WRITE(*,'(1x,a,i4,a,i4,a)')">>提取",StartYear,"-",EndYear,"年间的有效站点，并建立标准化存储数据库..."
      CALL SLEEP(1)
      ! get and save valid station codes to file
      INQUIRE(FILE = './DataBase/ValidStationCodes.dat', EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>ValidStationCodes.dat文件存在！"
        CALL SLEEP(1)
        PRINT *, ">>读取ValidStationCodes.dat文件中..."
        CALL SLEEP(1)
        CALL Inqire_Text_Row('./DataBase/ValidStationCodes.dat',LEN('./DataBase/ValidStationCodes.dat'),ValidTavgStationNum)
        ALLOCATE(ValidTavgStationCodesIndex(ValidTavgStationNum))
        OPEN(UNIT = fileID,FILE = './DataBase/ValidStationCodes.dat')
        READ(fileID,'(I20)') ValidTavgStationCodesIndex
        CLOSE(fileID)
        PRINT *, ">>读取ValidStationCodes.dat文件成功！"
        CALL SLEEP(1)
      ELSE
        tempNum = COUNT(GhcnTavgYear(:,2)<=EndYear.AND.GhcnTavgYear(:,3)>=StartYear)
        tempCount = 0
        ALLOCATE(ValidTavgStationCodesIndex(tempNum))
        PRINT *,">>提取站点编号..."
        CALL SLEEP(1)
        DO II = 1,StationNum
          IF(GhcnTavgYear(II,2)<=EndYear.AND.GhcnTavgYear(II,3)>=StartYear) THEN
            tempCount = tempCount + 1
            ValidTavgStationCodesIndex(tempCount) = GhcnTavgYear(II,1)
          END IF
        END DO
        ! 保存有效站点编号数据
        PRINT *,">>保存有效站点编号数据..."
        OPEN(UNIT = fileID,FILE = './DataBase/ValidStationCodes.dat') ! RECL = 20,
        WRITE(fileID,'(I20)') INT(ValidTavgStationCodesIndex,8)
        CLOSE(fileID)
        ValidTavgStationNum = SIZE(ValidTavgStationCodesIndex)
        PRINT *,">>保存有效站点编号数据成功！"
        PRINT *,">>提取站点编号成功！"
        CALL SLEEP(1)
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !          Build standard Precipitation DataBase
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PRINT *,">>建立标准化存储数据库..."
      CALL SLEEP(1)

      ALLOCATE(GhcnTavgStandardDB(ValidTavgStationNum,1+MonthNum*YearLen))
      GhcnTavgStandardDB(:,2:) = -9998  !标准数据库中，原数据库没有的数据，全部设置为-9998，缺测为-9999
      PRINT *,">>建立标准化存储数据库成功！"
      CALL SLEEP(1)
      PRINT *,">>查询标准化存储数据库数据文件是否存在..."
      CALL SLEEP(1)
      INQUIRE(FILE = './DataBase/GhcnTavgStandardDB.dat', EXIST = alive)
      IF(alive) THEN
        PRINT *, ">>标准化存储数据库数据文件存在！"
        CALL SLEEP(1)
        PRINT *, ">>读取标准化存储数据库数据文件信息..."
        CALL SLEEP(1)
        ALLOCATE(RealArrayTemp2D(1+YearLen*MonthNum,ValidTavgStationNum))
        OPEN(UNIT = fileID,FILE = './DataBase/GhcnTavgStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') !RECL = 300,
        READ(fileID,500) RealArrayTemp2D
        GhcnTavgStandardDB = TRANSPOSE(RealArrayTemp2D)
        PRINT *, ">>读取标准化存储数据库数据文件信息完成！"
        CALL SLEEP(1)
        CLOSE(fileID)
        DEALLOCATE(RealArrayTemp2D)
      ELSE
        PRINT *,">>标准化存储数据库数据文件不存在..."
        CALL SLEEP(1)
        CALL Progress % Set( N = ValidTavgStationNum , L = 30 )!// size(ValidTavgStationCodesIndex)次，显示长度30
        Progress % Prefix = "获取标准化存储数据库数据:  "  !// 前方提示文字，不是必须
        Progress % M = "#" !// 已完成部分的字符，不是必须
        Progress % O = " " !// 未完成部分的字符，不是必须
        DO II = 1,ValidTavgStationNum
          GhcnTavgStandardDB(II,1) = ValidTavgStationCodesIndex(II)
          tempNum = COUNT(GhcnTavg(:,1) == ValidTavgStationCodesIndex(II))
          ALLOCATE(CodesIndexLocation(tempNum))
          CodesIndexLocation = ArrayFind(GhcnTavg(:,1),'=',REAL(ValidTavgStationCodesIndex(II),8))
          DO KK = 1,tempNum
            IF(GhcnTavg(CodesIndexLocation(KK),2)>=StartYear.AND.GhcnTavg(CodesIndexLocation(KK),2)<=EndYear) THEN
              ColStart =(GhcnTavg(CodesIndexLocation(KK),2) - StartYear)*MonthNum +1  !GhcnTavgStandardDB中的行号
              GhcnTavgStandardDB(II,1+ColStart:1+ColStart+MonthNum) = GhcnTavg(CodesIndexLocation(KK),3:14)
            END IF
          END DO
          DEALLOCATE(CodesIndexLocation)
          CALL Progress % Put( II , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
        END DO
        PRINT *,">>保存GhcnTavgStandardDB文件..."
        CALL SLEEP(1)
        OPEN(UNIT = fileID,FILE = './DataBase/GhcnTavgStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
          & POSITION = 'REWIND') !RECL = 300,
        DO i = 1,ValidTavgStationNum!*(EndYear - StartYear + 1)
          WRITE(fileID,400) INT(GhcnTavgStandardDB(i,1),8),GhcnTavgStandardDB(i,2:)
        END DO
        CLOSE(fileID)
        PRINT *,">>保存GhcnTavgStandardDB文件完成(当前exe目录下GhcnTavgStandardDB.dat文件)!"
        CALL SLEEP(1)
        !!-------------------------------------------------------------------------
        !!     筛选温度每月小于   μ+timesσ (左、右边界)部分
        !!-------------------------------------------------------------------------
        !print *,'>> 进行μ+/-3σ处理'
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
        !PRINT *,">>保存GhcnTavgStandardDB文件..."
        !CALL SLEEP(1)
        !OPEN(UNIT = fileID,FILE = './DataBase/GhcnTavgStandardDB.dat',IOSTAT = iosval, FORM = 'FORMATTED',&
        !  & POSITION = 'REWIND') !RECL = 300,
        !DO i = 1,ValidTavgStationNum!*(EndYear - StartYear + 1)
        !  WRITE(fileID,400) INT(GhcnTavgStandardDB(i,1),8),GhcnTavgStandardDB(i,2:)
        !END DO
        !CLOSE(fileID)
        !PRINT *,">>保存GhcnTavgStandardDB文件完成(当前exe目录下GhcnTavgStandardDB.dat文件)!"
        !CALL SLEEP(1)
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !         读取降水数据有效站点编号  ValidPrcpStationCodesIndex
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INQUIRE(FILE = TRIM(PrcpWorkSpace)//'\ValidStationCodes.dat', EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>GhcnPrcp的ValidStationCodes.dat文件存在！"
        CALL SLEEP(1)
        PRINT *, ">>读取GhcnPrcp的ValidStationCodes.dat中..."
        CALL SLEEP(1)
        CALL Inqire_Text_Row(TRIM(PrcpWorkSpace)//'\ValidStationCodes.dat',LEN(TRIM(PrcpWorkSpace)//'\ValidStationCodes.dat'),ValidPrcpStationNum)
        ALLOCATE(ValidPrcpStationCodesIndex(ValidPrcpStationNum))
        OPEN(UNIT = fileID,FILE = TRIM(PrcpWorkSpace)//'\ValidStationCodes.dat')
        READ(fileID,'(I20)') ValidPrcpStationCodesIndex
        CLOSE(fileID)
        PRINT *, ">>读取GhcnPrcpStandardDBFile信息成功！"
        CALL SLEEP(1)
      ELSE
        PRINT *, ">>GhcnPrcpStandardDBFile文件不存在！"
        PAUSE
      END IF
      !PRINT *,'length of array:',size(ValidPrcpStationCodesIndex)
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !         读取降水数据1901-2010标准化存储数据库
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INQUIRE(FILE = TRIM(GhcnPrcpStandardDBFile), EXIST = alive)
      IF (alive) THEN
        PRINT *, ">>GhcnPrcpStandardDBFile文件存在！"
        CALL SLEEP(1)
        PRINT *, ">>读取GhcnPrcpStandardDBFile中..."
        CALL SLEEP(1)
        ALLOCATE(RealArrayTemp2D(1+YearLen*MonthNum,ValidPrcpStationNum))
        ALLOCATE(GhcnPrcpStandardDB(ValidPrcpStationNum,1+YearLen*MonthNum))
        OPEN(UNIT = fileID,FILE = TRIM(GhcnPrcpStandardDBFile))
        READ(fileID,700) RealArrayTemp2D
        CLOSE(fileID)
        GhcnPrcpStandardDB = TRANSPOSE(RealArrayTemp2D)
        PRINT *, ">>读取GhcnPrcpStandardDBFile信息成功！"
        CALL SLEEP(1)
        DEALLOCATE(RealArrayTemp2D)
      ELSE
        PRINT *, ">>GhcnPrcpStandardDBFile文件不存在！"
      END IF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !         逐站点进行预报分析
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ALLOCATE(StudyPrcp(YearLen*MonthNum))
      ALLOCATE(FactorTavg(YearLen*MonthNum))
      INQUIRE(DIRECTORY = TRIM(WorkSpace)//'StationList', EXIST = alive)
      IF (alive == .false.) THEN
        istatus_dir_mk = makedirqq(TRIM('StationList'))
      END IF
      istatus_dir_ch = CHDIR(TRIM(WorkSpace)//TRIM('StationList\'))
      ALLOCATE(R(ValidTavgStationNum*MonthNum,AheadMonthNum))      !用ValidTavgStationNum 个温度站点做预报
      ALLOCATE(TempR(ValidTavgStationNum*MonthNum,AheadMonthNum))
      ALLOCATE(P(ValidTavgStationNum*MonthNum,AheadMonthNum))
      ALLOCATE(TempP(ValidTavgStationNum*MonthNum,AheadMonthNum))
      CALL Progress % Set( N = ValidPrcpStationNum , L = 30 )!// size(ValidTavgStationCodesIndex)次，显示长度30
      Progress % M = "-" !// 已完成部分的字符，不是必须
      Progress % O = " " !// 未完成部分的字符，不是必须
      
      PRINT *,'是否需要计算某一个或者某一段站点？ y/n'
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

      ! 为了将异常值排除在外，计算可预报性时我们将所有的降水异常值置为-9996，温度的异常值置为-9996
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
        Progress % Prefix = "StationNum:"//TRIM(StationNumStr)//'/20547   '  !// 前方提示文字，不是必须
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
                    ValidStationCoupled(i - StartStationNum + 1 ,MonthNum+1) = 1  !统计站点是否存在配对记录，由于此次只是为了统计是否存在记录
                    CALL Correlation(FLOOR(tempCount*TrainingRate),TempMonthFactorTavg(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),&
                      TempMonthStudyPrcp(CodesIndexLocation(1:FLOOR(tempCount*TrainingRate))),R(j+(MonthNum-1)*ValidTavgStationNum,k))
                    !IF(R(j+(MonthNum-1)*ValidTavgStationNum,k) == R_Inf) THEN
                    !  P(j+(MonthNum-1)*ValidTavgStationNum,k) = R_Inf
                    !ELSE
                    CALL Pvalue(FLOOR(tempCount*TrainingRate),R(j+(MonthNum-1)*ValidTavgStationNum,k),P(j+(MonthNum-1)*ValidTavgStationNum,k))
                    !END IF
                  ELSE
                    ValidStationCoupled(i - StartStationNum + 1 ,MOD(k + ii + StartMonth - 1,MonthNum)+1) = 1  !统计站点是否存在配对记录，由于此次只是为了统计是否存在记录
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
        CALL Progress % Put( i , CMD_PROGRESS_ABSOLUTE ) !// 绝对方式
        
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


        ! 保存前15名,P<0.01
        PPvalue = 0.01
        TempR = R**2
        WHERE(P >= PPvalue)   !删除显著水平低于0.01的值
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
        ! 保存前15名,P<0.05
        PPvalue = 0.05
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

        ! 保存前15名,P<0.1
        PPvalue = 0.1
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

END SUBROUTINE CalStationPrcpRP_BT


    
