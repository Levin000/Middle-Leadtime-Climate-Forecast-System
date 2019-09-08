  MODULE ghcnPort
  IMPLICIT NONE
  PUBLIC :: Read_GHCN_Prcp
  PUBLIC :: Read_GHCN_Prcp_Station_Info
  PUBLIC :: Read_GHCN_Tavg
  PUBLIC :: Read_GHCN_Tavg_Station_Info
  PUBLIC :: StudyMonthAndFactorPreData_BP
  PUBLIC :: StudyMonthAndFactorPreData_BT
  PUBLIC :: isContinuityGT_M

  CONTAINS
  !****************************************************************************
  !
  !  SUBROUTINE: Inqire_Text_Row
  !
  !  PURPOSE:  This SUBROUTINE can READ GHCN precipitation DAT data
  !
  !****************************************************************************
  SUBROUTINE Inqire_Text_Row(FileName,FileNameLen,RowNum)
  IMPLICIT NONE
  INTEGER :: RowNum
  INTEGER :: FileNameLen, val = 0, FileID = 10
  CHARACTER*FileNameLen FileName

  CHARACTER(LEN = 76) line
  RowNum = 0
  OPEN(UNIT = FileID,FILE = FileName,IOSTAT = val)
  !PRINT *,"打开文件：   ", val
  IF (val >0 ) THEN
    PRINT *,"读取文件出错！！！"
    RETURN
  ELSE
    DO WHILE(val == 0)
      READ(UNIT = FileID,FMT = '(A76)',IOSTAT = val) line
      !PRINT *,line
      RowNum = RowNum + 1
      IF (val > 0 ) THEN
        PRINT *,"读取文件出错，行号： ", RowNum
        RETURN
      END IF

    END DO
  END IF
  CLOSE(FileID)
  RowNum = RowNum - 1
  !PRINT *, RowNum
  END SUBROUTINE  Inqire_Text_Row
  !****************************************************************************
  !
  !  SUBROUTINE: Read_GHCN_Prcp
  !
  !  PURPOSE:  This SUBROUTINE can READ GHCN precipitation DAT data, mssing value is -9999, trace value is -8888
  !
  !****************************************************************************
  SUBROUTINE Read_GHCN_Prcp(FileName,NameLen,RowNum,ColNum,MissVal,TraceVal)
  USE global
  IMPLICIT NONE
  INTEGER ::  NameLen,RowNum,ColNum, MissVal, TraceVal, FileID = 10, val = 0, num = 0
  CHARACTER(LEN = NameLen) :: FileName
100 FORMAT(f11.0,f1.0,f4.0,f5.0,f5.0,f5.0,f5.0,f5.0,f5.0,f5.0,f5.0,f5.0,f5.0,f5.0,f5.0)
  OPEN(UNIT = FileID, FILE = FileName, RECL = 76, IOSTAT = val, POSITION = 'REWIND')
  IF ( val > 0) THEN
    PRINT *,"打开文件出错！！！"
    RETURN
  ELSE
    DO WHILE(val == 0 .AND. num < RowNum)
      num = num + 1
      READ(FileID,100,IOSTAT = val) GhcnPrcp(num,1),GhcnPrcp(num,2),GhcnPrcp(num,3),GhcnPrcp(num,4),&
        GhcnPrcp(num,5),GhcnPrcp(num,6),GhcnPrcp(num,7),GhcnPrcp(num,8),GhcnPrcp(num,9),&
        GhcnPrcp(num,10),GhcnPrcp(num,11),GhcnPrcp(num,12),GhcnPrcp(num,13),GhcnPrcp(num,14),&
        GhcnPrcp(num,15)
      IF (val > 0 ) THEN
        PRINT *,num
        PRINT *,"读取文件出错！！！"
        RETURN
      END IF
    END DO
  END IF
  CLOSE(FileID)
  IF (MissVal == 1 .AND. TraceVal==1) THEN
    WHERE(GhcnPrcp == -9999 .OR. GhcnPrcp == -8888)  !对缺测值-9999和缺省值-8888全部处理成0
      GhcnPrcp = GhcnPrcp*0
    END WHERE
  ELSE IF (MissVal == 1) THEN
    WHERE(GhcnPrcp == -9999 )  !对缺测值-9999处理成0
      GhcnPrcp = GhcnPrcp*0
    END WHERE
  ELSE IF (TraceVal == 1) THEN
    WHERE(GhcnPrcp == -8888)  !对痕迹降水-8888处理成0
      GhcnPrcp = GhcnPrcp*0
    END WHERE
  END IF
  !GhcnPrcp(1:RowNum,4:ColNum) = GhcnPrcp(1:RowNum,4:ColNum)/10  !将降雨的单位设置为mm
  END SUBROUTINE Read_GHCN_Prcp


  !****************************************************************************
  !
  !  SUBROUTINE: Read_GHCN_Prcp_Station_Info
  !  PURPOSE:  This SUBROUTINE can READ GHCN precipitation DAT data
  !
  !****************************************************************************
  SUBROUTINE Read_GHCN_Prcp_Station_Info(StationFile,NameLen,StationNum)
  USE global
  IMPLICIT NONE

  INTEGER :: StationNum,NameLen
  INTEGER :: FileID = 10, val = 0, num = 0
  CHARACTER(LEN = NameLen) :: StationFile
200 FORMAT(I11,1X,A20,A10,F7.2,1X,F7.2,I5)
  OPEN(UNIT = FileID,FILE = StationFile,RECL = 62,IOSTAT = val,POSITION = 'REWIND')
  IF (val > 0) THEN
    PRINT *,'打开文件错误!!!'
    RETURN
  ELSE
    DO WHILE(val == 0 .AND. num < StationNum)
      num = num + 1
      READ(FileID, 200, IOSTAT = val) StationInfo(num)%Code,StationInfo(num)%City,&
        StationInfo(num)%Country,StationInfo(num)%Lat,StationInfo(num)%Lon,&
        StationInfo(num)%Ele
      IF (val > 0) THEN
        PRINT *,'读取文件出错...'
        RETURN
      END IF
    END DO
  END IF
  CLOSE(FileID)
  END SUBROUTINE Read_GHCN_Prcp_Station_Info

  !****************************************************************************
  !
  !  SUBROUTINE: Read_GHCN_temperature
  !
  !  PURPOSE:  This SUBROUTINE can READ GHCN temperature DAT data， mssing value is -9999
  !
  !****************************************************************************
  SUBROUTINE Read_GHCN_Tavg(FileName,NameLen,RowNum,ColNum,MissVal)
  USE global
  IMPLICIT NONE
  INTEGER ::  NameLen,RowNum,ColNum, MissVal, FileID = 10, val = 0, num = 0
  CHARACTER(LEN = NameLen) :: FileName
300 FORMAT(f11.0,f4.0,4xf5.0,3xf5.0,3xf5.0,3xf5.0,3xf5.0,3xf5.0,3xf5.0,3xf5.0,3xf5.0,3xf5.0,3xf5.0,3xf5.0,3x)
  OPEN(UNIT = FileID, FILE = FileName, RECL = 76, IOSTAT = val, POSITION = 'REWIND')
  IF ( val > 0) THEN
    PRINT *,"打开文件出错！！！"
    RETURN
  ELSE
    DO WHILE(val == 0 .AND. num < RowNum)
      num = num + 1
      READ(FileID,300,IOSTAT = val) GhcnTavg(num,1),GhcnTavg(num,2),GhcnTavg(num,3),GhcnTavg(num,4),&
        GhcnTavg(num,5),GhcnTavg(num,6),GhcnTavg(num,7),GhcnTavg(num,8),GhcnTavg(num,9),&
        GhcnTavg(num,10),GhcnTavg(num,11),GhcnTavg(num,12),GhcnTavg(num,13),GhcnTavg(num,14)
      IF (val > 0 ) THEN
        PRINT *,num
        PRINT *,"读取文件出错！！！"
        RETURN
      END IF
    END DO
  END IF
  CLOSE(FileID)
  IF (MissVal == 1) THEN
    WHERE(GhcnTavg == -9999 )  !对缺测值-9999全部处理成0
      GhcnTavg = GhcnTavg*0
    END WHERE
  END IF
  ! GhcnTavg(1:RowNum,3:ColNum) = GhcnTavg(1:RowNum,3:ColNum)/100  !将温度的单位设置为℃
  END SUBROUTINE Read_GHCN_Tavg

  !****************************************************************************
  !
  !  SUBROUTINE: Read_GHCN_Station_Info
  !  PURPOSE:  This SUBROUTINE can READ GHCN precipitation DAT data
  !
  !****************************************************************************
  SUBROUTINE Read_GHCN_Tavg_Station_Info(StationFile,NameLen,StationNum)
  USE global
  IMPLICIT NONE

  INTEGER :: StationNum,NameLen
  INTEGER :: FileID = 10, val = 0, num = 0
  CHARACTER(LEN = NameLen) :: StationFile
400 FORMAT(I11,1X,A20,A10,F7.2,1X,F7.2,I5)
  OPEN(UNIT = FileID,FILE = StationFile,RECL = 62,IOSTAT = val,POSITION = 'REWIND')
  IF (val > 0) THEN
    PRINT *,'打开文件错误!!!'
    RETURN
  ELSE
    DO WHILE(val == 0 .AND. num < StationNum)
      num = num + 1
      READ(FileID, 400, IOSTAT = val)  GhcnTavgStationInfo(num)%Code, GhcnTavgStationInfo(num)%City,&
        GhcnTavgStationInfo(num)%Country, GhcnTavgStationInfo(num)%Lat, GhcnTavgStationInfo(num)%Lon,&
        GhcnTavgStationInfo(num)%Ele
      IF (val > 0) THEN
        PRINT *,'读取文件出错...'
        RETURN
      END IF
    END DO
  END IF
  CLOSE(FileID)
  END SUBROUTINE Read_GHCN_Tavg_Station_Info
  !****************************************************************************
  !
  !  SUBROUTINE: StudyMonthAndFactorPreData_BP
  !  PURPOSE:
  !
  !****************************************************************************
  SUBROUTINE StudyMonthAndFactorPreData_BP(Pc,Pm,Fc,Fm,YearLen,MonthNum,RankNum,ValidStationNum,StartMonth,GhcnPrcpStandardDB)
  USE global
  USE arrayPort
  IMPLICIT NONE
  INTEGER :: YearLen,RankNum,ValidStationNum,StartMonth
  INTEGER :: MonthNum
  INTEGER :: MonthIndex,tempNum,jj,ii
  INTEGER :: codeIndex;
  REAL(KIND = 8) :: Pc,Pm,Fc,Fm,TrainingRate
  REAL(KIND = 8) :: GhcnPrcpStandardDB(ValidStationNum,1+YearLen*MonthNum)
  REAL(KIND = 8) :: StudyPrcp(YearLen*MonthNum),FactorPrcp(YearLen*MonthNum)
  REAL(KIND = 8) :: tempFactorPrcp(YearLen*MonthNum-INT(Fm)),tempStudyPrcp(YearLen*MonthNum-INT(Fm))
  !PRINT *,'StartRun'
  DO ii = 1,ValidStationNum
    IF(GhcnPrcpStandardDB(ii,1) .EQ. Pc) THEN
      codeIndex = ii
      EXIT
    END IF
  END DO
  !PRINT *,SIZE(ValidStationLocation)
  !pause
  StudyPrcp = GhcnPrcpStandardDB(codeIndex,2:)
  !PRINT *,StudyPrcp
  !pause
  DO ii = 1,ValidStationNum
    IF(GhcnPrcpStandardDB(ii,1) .EQ. Fc) THEN
      codeIndex = ii
      EXIT
    END IF
  END DO
  !pause
  FactorPrcp = GhcnPrcpStandardDB(codeIndex,2:)
  !PRINT *,FactorPrcp
  !pause
  tempFactorPrcp = FactorPrcp(1:YearLen*MonthNum-INT(Fm))  !提前Fm月的数据
  tempStudyPrcp = StudyPrcp(1+INT(Fm):YearLen*MonthNum)  !提前Fm月的数据
  MonthIndex = MOD(INT(Fm) + StartMonth,12)  !处理后的开始月份
  !PRINT *,'SMAFPD'
  !pause
  !PRINT *,'RunHere',MonthIndex
  IF ( MonthIndex == 0) THEN
    IF(INT(Pm) == 12) THEN
      ALLOCATE(tempFactorPrcpMonth(SIZE(tempFactorPrcp(1:YearLen*MonthNum-INT(Fm):12))))  !提取预报因子站点的月值数据
      ALLOCATE(tempStudyPrcpMonth(SIZE(tempStudyPrcp(1:YearLen*MonthNum-INT(Fm):12))))     !提取预报研究站点的月值数据
      tempFactorPrcpMonth = tempFactorPrcp(1:(YearLen*MonthNum-INT(Fm)):12)
      tempStudyPrcpMonth = tempStudyPrcp(1:(YearLen*MonthNum-INT(Fm)):12)
    ELSE
      ALLOCATE(tempFactorPrcpMonth(SIZE(tempFactorPrcp((1 + INT(Pm)):(YearLen*MonthNum-INT(Fm)):12))))
      ALLOCATE(tempStudyPrcpMonth(SIZE(tempStudyPrcp((1 + INT(Pm)):(YearLen*MonthNum-INT(Fm)):12))))
      tempFactorPrcpMonth = tempFactorPrcp((1 + INT(Pm)):(YearLen*MonthNum-INT(Fm)):12)
      tempStudyPrcpMonth = tempStudyPrcp((1 + INT(Pm)):(YearLen*MonthNum-INT(Fm)):12)
    END IF
  ELSE
    IF(MonthIndex <= INT(Pm)) THEN
      ALLOCATE(tempFactorPrcpMonth(SIZE(tempFactorPrcp((1 + INT(Pm)-MonthIndex):(YearLen*MonthNum-INT(Fm)):12))))
      ALLOCATE(tempStudyPrcpMonth(SIZE(tempStudyPrcp((1 + INT(Pm)-MonthIndex):(YearLen*MonthNum-INT(Fm)):12))))
      tempFactorPrcpMonth = tempFactorPrcp((1+INT(Pm)-MonthIndex):(YearLen*MonthNum-INT(Fm)):12)
      tempStudyPrcpMonth = tempStudyPrcp((1+INT(Pm)-MonthIndex):(YearLen*MonthNum-INT(Fm)):12)
    ELSE
      ALLOCATE(tempFactorPrcpMonth(SIZE(tempFactorPrcp((1 + INT(Pm)+MonthNum-MonthIndex):(YearLen*MonthNum-INT(Fm)):12))))
      ALLOCATE(tempStudyPrcpMonth(SIZE(tempStudyPrcp((1 + INT(Pm)+MonthNum-MonthIndex):(YearLen*MonthNum-INT(Fm)):12))))
      tempFactorPrcpMonth = tempFactorPrcp((1+INT(Pm)+MonthNum-MonthIndex):(YearLen*MonthNum-INT(Fm)):12)
      tempStudyPrcpMonth = tempStudyPrcp((1+INT(Pm)+MonthNum-MonthIndex):(YearLen*MonthNum-INT(Fm)):12)
    END IF
  END IF
  END SUBROUTINE StudyMonthAndFactorPreData_BP
  !****************************************************************************
  !
  !  SUBROUTINE: StudyMonthAndFactorPreData_BT
  !  PURPOSE:
  !
  !****************************************************************************
  SUBROUTINE StudyMonthAndFactorPreData_BT(Pc,Pm,Fc,Fm,YearLen,MonthNum,RankNum,ValidPrcpStationNum,ValidTavgStationNum,StartMonth,GhcnPrcpStandardDB,GhcnTavgStandardDB)
  USE global
  USE arrayPort
  IMPLICIT NONE
  INTEGER :: YearLen,RankNum,ValidPrcpStationNum,ValidTavgStationNum,StartMonth
  INTEGER :: MonthNum
  INTEGER :: MonthIndex,tempNum,jj,ii,codeIndex
  REAL(KIND = 8) :: Pc,Pm,Fc,Fm,TrainingRate
  REAL(KIND = 8) :: GhcnPrcpStandardDB(ValidPrcpStationNum,1+MonthNum*YearLen)
  REAL(KIND = 8) :: GhcnTavgStandardDB(ValidTavgStationNum,1+MonthNum*YearLen)
  REAL(KIND = 8) :: StudyPrcp(YearLen*MonthNum),FactorTavg(YearLen*MonthNum)
  REAL(KIND = 8) :: tempFactorTavg(YearLen*MonthNum-INT(Fm)),tempStudyPrcp(YearLen*MonthNum-INT(Fm))
  !PRINT *,'StartRun'
  !PAUSE
  !print *,'Enter subroutine:'
  !pause
  DO ii = 1,ValidPrcpStationNum
    IF(GhcnPrcpStandardDB(ii,1) .EQ. Pc)THEN
      codeIndex = ii
      EXIT
    END IF
  END DO
  !print *,'Line number in standard database:',codeIndex
  !pause
  StudyPrcp = GhcnPrcpStandardDB(codeIndex,2:)
  !print *,StudyPrcp
  !print *,'size of StudyPrcp:',size(StudyPrcp)
  !pause
  DO ii = 1,ValidTavgStationNum
    IF(GhcnTavgStandardDB(ii,1) .EQ. Fc)THEN
      codeIndex = ii
      EXIT
    END IF
  END DO
  !print *,'Line number in standard database:',codeIndex
  !pause
  FactorTavg = GhcnTavgStandardDB(codeIndex,2:)
  !print *,FactorTavg
  !print *,'size of FactorTavg:',size(FactorTavg)
  !pause
  tempFactorTavg = FactorTavg(1:YearLen*MonthNum-INT(Fm))  !提前Fm月的数据
  tempStudyPrcp = StudyPrcp(1+INT(Fm):YearLen*MonthNum)  !提前Fm月的数据
  MonthIndex = MOD(INT(Fm) + StartMonth,12)  !处理后的开始月份
  !print *,'MonthIndex:',MonthIndex
  !pause
  !PRINT *,'RunHere',MonthIndex
  IF ( MonthIndex == 0) THEN
    IF(INT(Pm) == 12) THEN
      ALLOCATE(tempFactorTavgMonth(SIZE(tempFactorTavg(1:YearLen*MonthNum-INT(Fm):12))))  !提取预报因子站点的月值数据
      ALLOCATE(tempStudyPrcpMonth(SIZE(tempStudyPrcp(1:YearLen*MonthNum-INT(Fm):12))))     !提取预报研究站点的月值数据
      !print *,'allocate data success'
      !pause
      tempFactorTavgMonth = tempFactorTavg(1:(YearLen*MonthNum-INT(Fm)):12)
      tempStudyPrcpMonth = tempStudyPrcp(1:(YearLen*MonthNum-INT(Fm)):12)
    ELSE
      ALLOCATE(tempFactorTavgMonth(SIZE(tempFactorTavg((1 + INT(Pm)):(YearLen*MonthNum-INT(Fm)):12))))
      ALLOCATE(tempStudyPrcpMonth(SIZE(tempStudyPrcp((1 + INT(Pm)):(YearLen*MonthNum-INT(Fm)):12))))
      !print *,'allocate data success'
      !pause
      tempFactorTavgMonth = tempFactorTavg((1 + INT(Pm)):(YearLen*MonthNum-INT(Fm)):12)
      tempStudyPrcpMonth = tempStudyPrcp((1 + INT(Pm)):(YearLen*MonthNum-INT(Fm)):12)
    END IF
  ELSE
    IF(MonthIndex <= INT(Pm)) THEN
      ALLOCATE(tempFactorTavgMonth(SIZE(tempFactorTavg((1 + INT(Pm)-MonthIndex):(YearLen*MonthNum-INT(Fm)):12))))
      ALLOCATE(tempStudyPrcpMonth(SIZE(tempStudyPrcp((1 + INT(Pm)-MonthIndex):(YearLen*MonthNum-INT(Fm)):12))))
      !print *,'allocate data success'
      !pause
      tempFactorTavgMonth = tempFactorTavg((1+INT(Pm)-MonthIndex):(YearLen*MonthNum-INT(Fm)):12)
      tempStudyPrcpMonth = tempStudyPrcp((1+INT(Pm)-MonthIndex):(YearLen*MonthNum-INT(Fm)):12)
    ELSE
      ALLOCATE(tempFactorTavgMonth(SIZE(tempFactorTavg((1 + INT(Pm)+MonthNum-MonthIndex):(YearLen*MonthNum-INT(Fm)):12))))
      ALLOCATE(tempStudyPrcpMonth(SIZE(tempStudyPrcp((1 + INT(Pm)+MonthNum-MonthIndex):(YearLen*MonthNum-INT(Fm)):12))))
      !print *,'allocate data success'
      !pause
      tempFactorTavgMonth = tempFactorTavg((1+INT(Pm)+MonthNum-MonthIndex):(YearLen*MonthNum-INT(Fm)):12)
      tempStudyPrcpMonth = tempStudyPrcp((1+INT(Pm)+MonthNum-MonthIndex):(YearLen*MonthNum-INT(Fm)):12)
    END IF
  END IF
  !print *,'END StudyMonthAndFactorPreData_BT'
  !pause
  END SUBROUTINE StudyMonthAndFactorPreData_BT
  !****************************************************************************
  !
  !  SUBROUTINE: isContinuityGT_M(array,M)
  !  PURPOSE:  justify whether the array has the continuity elements which number more than M
  !
  !****************************************************************************
  LOGICAL FUNCTION isContinuityGT_M(array,M,anomaly_value)
  REAL(KIND = 8),DIMENSION(:) :: array
  REAL(KIND = 8) :: X_Value,anomaly_value
  INTEGER :: M,X_Num,X_Len,X_i

  X_Value = array(1)
  X_Num = 1
  X_Len = SIZE(array)
  DO X_i = 2,X_Len

    IF(array(X_i) == X_Value) THEN
      X_Num = X_Num + 1
      IF(X_Num>=M) THEN
        if(X_Value > anomaly_value) then ! 排除缺省值和痕迹值 -9999、-8888
          isContinuityGT_M = .true.
          RETURN
        else
          X_Value = array(X_i)
          X_Num = 1
        endif
      END IF
    ELSE
      X_Value = array(X_i)
      X_Num = 1
    END IF
  END DO
  isContinuityGT_M = .false.
  RETURN
  END FUNCTION

  END MODULE ghcnPort


  !****************************************************************************
  !
  !  SUBROUTINE: StudyMonthAndFactorPreData_BT
  !  PURPOSE:
  !
  !****************************************************************************
  SUBROUTINE pTandMonthAtAheadMonthAndMonthRoll(AheadMonth, MonthRoll, StartMonth, MonthNum, Month)
  USE global
  USE arrayPort
  IMPLICIT NONE
  INTEGER :: AheadMonth,MonthRoll,StartMonth,MonthNum  !输入变量，提前时间、滚动月数、数据开始月份、每年月数（12）
  INTEGER :: Month                                     !输出变量,具体的当前月份

  IF(MOD(AheadMonth + MonthRoll + StartMonth - 1,MonthNum) == 0) THEN
    Month = 12
  ELSE
    Month = MOD(AheadMonth + MonthRoll + StartMonth - 1,MonthNum)
  END IF

  END SUBROUTINE pTandMonthAtAheadMonthAndMonthRoll
  
  !****************************************************************************
  !
  !  SUBROUTINE: saveResultsBTBP
  !  PURPOSE:
  !
  !****************************************************************************
  
  subroutine saveResultsBTBP(R, P, PPvalue, MonthNum, RankNum, AheadMonthNum, YearLen,&
                           ValidPrcpStationNum, ValidTavgStationNum, StudyStationNum001, &
                           StartMonth, saveRankNum, ValidPrcpStationCodesIndex, &
                           TrainingRate, GhcnPrcpStandardDB, GhcnTavgStandardDB)
    use ghcnPort
    use global
    use statisticPort

    implicit none

    CHARACTER(LEN = 5) :: monthStr,RankNumStr
    CHARACTER(LEN = 4) :: filePvalueStr

    integer :: fileID = 10, iosval                       !文件ID，文件操作状态
    integer :: savePrcpFileID = 200
    integer :: StartMonth, MonthNum, RankNum, YearLen, ValidPrcpStationNum, &
               ValidTavgStationNum, AheadMonthNum, StudyStationNum001
    integer :: i, ii, jj, kk, saveRankNum, tempCount, tempNum, trainLen

    ! 此类数组的大小在处理过程中设置
    INTEGER(KIND = 8), ALLOCATABLE :: CIL(:)
    INTEGER(KIND = 8), ALLOCATABLE :: CodesIndexLocation(:)           !存储某一个站点再数据库中的索引值(行位置)
    ! 需要提前指定此类数组的大小
    INTEGER(KIND = 8) :: ValidPrcpStationCodesIndex(ValidPrcpStationNum)   !提取出研究的站点编号


    real :: PPvalue, maxR2, TrainingRate
    real(kind = 8) :: DataNumNotEnough = -9.0 
    ! 需要提前指定此类数组的大小
    real :: R(ValidPrcpStationNum*MonthNum, AheadMonthNum), &
            P(ValidPrcpStationNum*MonthNum, AheadMonthNum), &
            TempR(ValidPrcpStationNum*MonthNum, AheadMonthNum), &
            tempP(ValidPrcpStationNum*MonthNum, AheadMonthNum), &
            ptor2k(ValidPrcpStationNum*MonthNum, AheadMonthNum), &
            ptor2b(ValidPrcpStationNum*MonthNum, AheadMonthNum)
    real(kind = 8) :: P_R_001RankData_Predictable(StudyStationNum001*MonthNum*RankNum, 7)
    real(kind = 8) :: GhcnPrcpStandardDB(ValidPrcpStationNum,1+YearLen*MonthNum), &
                                   GhcnTavgStandardDB(ValidTavgStationNum,1+MonthNum*YearLen)

    real(kind = 8) :: ptor1k, ptor1b, tempPtor2k, tempPtor2b
    real(kind = 8) :: pstandID, pstor1ID, pstor1LM, pstor2ID, pstor2LM
    real(kind = 8) ::   Rtandtor1, R2tandtor1
    real :: PptandY, RptandY
    REAL(KIND = 8) :: prcp_anomaly_missing,prcp_anomaly_trace,tavg_anomaly_missing
    ! 以下变量的大小将在处理过程中进行计算
    REAL(KIND = 8), ALLOCATABLE :: ptandPrcp(:), ptor1Tavg(:), ptor2Prcp(:)   !保存预报量站点、第一预报因子、第二预报因子降雨量数据,
    REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY1(:), ptandPrcpY1Residual(:)     !保存根据线性回归预报的预报量站点降雨量数据，只包含第一预报因子、再次情况下的残差
    REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY2(:)                             !保存根据线性回归预报的预报量残差，第二预报因子与第一预报因子预报残差之间的线性关系
    REAL(KIND = 8), ALLOCATABLE :: ptandPrcpY(:)                              !包含第一预报因子和第二预报因子的总预报降雨量
    REAL(KIND = 8), ALLOCATABLE :: savePtror1Tavg(:)
    REAL(KIND = 8), ALLOCATABLE :: savePtror2Prcp(:)
    REAL(KIND = 8), ALLOCATABLE :: savePtand1Prcp(:)
    REAL(KIND = 8), ALLOCATABLE :: savePtand2Prcp(:)
    REAL(KIND = 8), ALLOCATABLE :: savePtror1TavgModify(:)
    REAL(KIND = 8), ALLOCATABLE :: savePtror2PrcpModify(:)
    REAL(KIND = 8), ALLOCATABLE :: savePtand1PrcpModify(:)

    
    !******************************************************************************
    !  处理保存文件过程
    !******************************************************************************
    ! 此参数子函数参数中已经给出   !PPvalue = 0.1
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

    ALLOCATE(CodesIndexLocation(2))
    WRITE(filePvalueStr,'(F4.2)') PPvalue
    OPEN(fileID,FILE = 'R_RankNum15_P.LT.'//TRIM(adjustl(filePvalueStr))//'.dat',IOSTAT = iosval)
    DO ii = 1,MonthNum
      !print *,"获取第一因子的相关信息"
      !与ii月份对应得最强预报因子站点信息
      pstor1ID = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,3)
      pstor1LM = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,4)
      R2tandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,5)
      Rtandtor1 = P_R_001RankData_Predictable( (i-1)*MonthNum*RankNum + (ii-1)*RankNum + 1,6)
      !print *,"读取与第一个因子对应的Prcp数据"

      CALL StudyMonthAndFactorPreData_BT(pstandID,REAL(ii,KIND=8),pstor1ID,pstor1LM,YearLen,MonthNum,RankNum,&
        ValidPrcpStationNum,ValidTavgStationNum,StartMonth,&
        GhcnPrcpStandardDB,GhcnTavgStandardDB) !,tempFactorPrcpMonth,tempStudyPrcpMonth

      ALLOCATE(savePtror1Tavg(size(tempFactorPrcpMonth)))
      savePtror1Tavg = tempFactorPrcpMonth
      ALLOCATE(savePtand1Prcp(size(tempStudyPrcpMonth)))
      savePtand1Prcp = tempStudyPrcpMonth
      DEALLOCATE(tempFactorPrcpMonth)
      DEALLOCATE(tempStudyPrcpMonth)

      !print *,savePtand1Prcp
      !pause
      !print *,savePtror1Tavg
      !pause

      DO jj = 1,saveRankNum
        maxR2 = MAXVAL(TempR(1+(ii-1)*ValidPrcpStationNum:ii*ValidPrcpStationNum,:))
        CodesIndexLocation = MAXLOC(TempR(1+(ii-1)*ValidPrcpStationNum:ii*ValidPrcpStationNum,:))
        IF (Rtandtor1 < -1) THEN
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,DataNumNotEnough,DataNumNotEnough, &
            ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2))
        ELSE IF (maxR2 <= 0) THEN

          !print *,"调整Prcp数据"
          !根据pstor1ID和pstor2ID的情况，修订预报量、预报因子1及预报因子2的降雨量数据
          !另一个不需要修改，原封不动的copy即可
          ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
          savePtror1TavgModify = savePtror1Tavg
          !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
          ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Tavg)))
          savePtand1PrcpModify = savePtand1Prcp

          !print *,savePtand1PrcpModify
          !print *,savePtror1TavgModify
          !print *,savePtror2PrcpModify
          !pause

          !print *,"统计不为0的数据量及位置"

          tempCount = COUNT(savePtand1PrcpModify>=0 .AND. savePtror1TavgModify> -9998)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror1TavgModify)
            IF(savePtand1PrcpModify(kk)>=0 .AND. savePtror1TavgModify(kk)> -9998) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"重新调整ptand，ptor1，ptor2数据"

          trainLen = FLOOR(tempCount*TrainingRate)
          ! 这里ptor1Tavg
          ALLOCATE(ptandPrcp(trainLen))
          ALLOCATE(ptor1Tavg(trainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:trainLen))
          ptor1Tavg = savePtror1TavgModify(CIL(1:trainLen))

          !print *,"拟合第一预报因子与预报量"

          !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
          CALL LinearRegression(ptor1Tavg,ptandPrcp,trainLen,ptor1k,ptor1b)

          !print *,ptor1Tavg,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(trainLen))
          ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
          !计算预报量与第一预报因子的“残差”
          ALLOCATE(ptandPrcpY1Residual(trainLen))
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"拟合第二预报因子与预报量"

          !计算总预报降雨量
          ALLOCATE(ptandPrcpY(trainLen))
          ptandPrcpY = ptandPrcpY1
          !计算预报量站点观测记录与预报的相关系数
          CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)

          !print *,"第一个因子信息："
          !print *,"原始记录：",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"第二个因子信息："
          !print *,"记录：",ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2))
          !print *,"本次计算信息："
          !print *,ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "保存文件"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
            ptor2k(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
            ptor2b(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
            maxR2,&  !DataNumNotEnough,DataNumNotEnough,DataNumNotEnough,
            R(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2))

          IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"保存完成"

          !保存预测变量
          !ptandPrcp
          !ptor1Tavg
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

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.'//TRIM(adjustl(filePvalueStr))//'.dat')
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
          !print *,"读取与第二个因子相关的数据，第",jj,"次"
          CALL StudyMonthAndFactorPreData_BP(pstandID,REAL(ii,KIND=8),REAL(ValidPrcpStationCodesIndex(CodesIndexLocation(1)),KIND=8),&
            REAL(CodesIndexLocation(2),KIND=8), YearLen,MonthNum,RankNum,&
            ValidPrcpStationNum,StartMonth, GhcnPrcpStandardDB) !,tempFactorPrcpMonth,tempStudyPrcpMonth
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
          IF (SIZE(savePtror1Tavg)>SIZE(savePtror2Prcp)) THEN
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror2Prcp)))
            savePtror1TavgModify = savePtror1Tavg(SIZE(savePtror1Tavg)-SIZE(savePtror2Prcp)+1:)
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror2Prcp)))
            savePtror2PrcpModify = savePtror2Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时需要修灯
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror2Prcp)))
            savePtand1PrcpModify = savePtand1Prcp(SIZE(savePtror1Tavg)-SIZE(savePtror2Prcp)+1:)
          ELSE IF(SIZE(savePtror1Tavg)<SIZE(savePtror2Prcp)) THEN
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror1Tavg)))
            savePtror2PrcpModify = savePtror2Prcp(SIZE(savePtror2Prcp)-SIZE(savePtror1Tavg)+1:)
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
            savePtror1TavgModify = savePtror1Tavg
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtror1Tavg)))
            savePtand1PrcpModify = savePtand1Prcp
          ELSE
            !此时，均不需要修订
            ALLOCATE(savePtror1TavgModify(SIZE(savePtror1Tavg)))
            savePtror1TavgModify = savePtror1Tavg
            !另一个不需要修改，原封不动的copy即可
            ALLOCATE(savePtror2PrcpModify(SIZE(savePtror2Prcp)))
            savePtror2PrcpModify = savePtror2Prcp
            !此时预报量站点使用与pstor1ID对应的预报量降雨的修订值，此时不需要修灯，直接copy即可
            ALLOCATE(savePtand1PrcpModify(SIZE(savePtand1Prcp)))
            savePtand1PrcpModify = savePtand1Prcp
          END IF

          !print *,savePtand1PrcpModify
          !print *,savePtror1TavgModify
          !print *,savePtror2PrcpModify
          !pause

          !print *,"统计不为0的数据量及位置"

          tempCount = COUNT(savePtror2PrcpModify>=0 .AND. savePtand1PrcpModify>=0 .AND. savePtror1TavgModify> -9998)
          ALLOCATE(CIL(tempCount))
          tempNum = 0
          !print *,"deal do"
          DO kk = 1,SIZE(savePtror2PrcpModify)
            IF(savePtror2PrcpModify(kk)>=0 .AND. savePtand1PrcpModify(kk)>=0 .AND. savePtror1TavgModify(kk)>-9998) THEN
              tempNum = tempNum + 1
              CIL(tempNum) = kk
            END IF
          END DO

          !print *,"重新调整ptand，ptor1，ptor2数据"

          trainLen = FLOOR(tempCount*TrainingRate)
          ALLOCATE(ptandPrcp(trainLen))
          ALLOCATE(ptor1Tavg(trainLen))
          ALLOCATE(ptor2Prcp(trainLen))
          ptandPrcp = savePtand1PrcpModify(CIL(1:trainLen))
          ptor1Tavg = savePtror1TavgModify(CIL(1:trainLen))
          ptor2Prcp = savePtror2PrcpModify(CIL(1:trainLen))

          !print *,"拟合第一预报因子与预报量"

          !线性回归拟合第一预报因子站点与预报量站点之间的线性关系
          CALL LinearRegression(ptor1Tavg,ptandPrcp,trainLen,ptor1k,ptor1b)

          !print *,ptor1Tavg,ptandPrcp,ptor1k,ptor1b

          ALLOCATE(ptandPrcpY1(trainLen))
          ptandPrcpY1 = ptor1Tavg*ptor1k + ptor1b
          !计算预报量与第一预报因子的“残差”
          ALLOCATE(ptandPrcpY1Residual(trainLen))
          ptandPrcpY1Residual = ptandPrcp-ptandPrcpY1
          !print *,"拟合第二预报因子与预报量"
          !线性回归拟合预报量与第一预报因子的“残差”和第二预报因子之间的线性关系
          CALL LinearRegression(ptor2Prcp,ptandPrcpY1Residual,trainLen,tempPtor2k, tempPtor2b)
          ALLOCATE(ptandPrcpY2(trainLen))
          ptandPrcpY2 = ptor2Prcp*tempPtor2k + tempPtor2b
          !计算总预报降雨量
          ALLOCATE(ptandPrcpY(trainLen))
          ptandPrcpY = ptandPrcpY1+ptandPrcpY2
          !计算预报量站点观测记录与预报的相关系数
          CALL Correlation(trainLen,ptandPrcpY,ptandPrcp,RptandY)
          CALL Pvalue(trainLen, RptandY, PptandY)
          !print *,"第一个因子信息："
          !print *,"原始记录：",pstor1ID,pstor1LM,Rtandtor1,"k:",ptor1k,"b:",ptor1b
          !print *,"第二个因子信息："
          !print *,"记录：",ValidTavgStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),&
          !        R(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
          !        "k:",ptor2k(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
          !        "b:",ptor2b(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2))
          !print *,"本次计算信息："
          !print *,ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2),RptandY,"k:",tempPtor2k,"b:",tempPtor2b
          !pause
          !print *, "保存文件"
          WRITE(fileID,'(I15,I10,2F6.2,2F15.4,I15,I10,2F15.4,3F10.2)')INT(pstor1ID,KIND=8),INT(pstor1LM),R2tandtor1,Rtandtor1,ptor1k,ptor1b, &
            ValidPrcpStationCodesIndex(CodesIndexLocation(1)),CodesIndexLocation(2), tempPtor2k, tempPtor2b, maxR2,& !RptandY*RptandY, RptandY,PptandY,
            R(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)),&
            P(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2))

          IF (TempR(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)) /= DataNumNotEnough) THEN
            TempR(CodesIndexLocation(1)+(ii-1)*ValidPrcpStationNum,CodesIndexLocation(2)) = 0
          END IF

          !print *,"保存完成"

          !保存预测变量
          !ptandPrcp
          !ptor1Tavg
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

          OPEN(savePrcpFileID,FILE = 'PRCP_month_'//TRIM(ADJUSTL(monthStr))//'_RankNum_'//TRIM(ADJUSTL(RankNumStr))//'_P.LT.'//TRIM(adjustl(filePvalueStr))//'.dat')
          !pause

          !print *,"enter"
          !pause
          DO kk = 1,SIZE(ptandPrcp)
            WRITE(savePrcpFileID,'(7F10.2)') ptandPrcp(kk), &
              ptor1Tavg(kk), &
              ptor2Prcp(kk), &
              ptandPrcpY1(kk), &
              ptandPrcpY2(kk), &
              ptandPrcpY(kk)
          END DO
          CLOSE(savePrcpFileID)

          DEALLOCATE(savePtror2Prcp)

          DEALLOCATE(savePtand2Prcp)
          DEALLOCATE(savePtror1TavgModify)
          DEALLOCATE(savePtror2PrcpModify)
          DEALLOCATE(savePtand1PrcpModify)
          DEALLOCATE(ptandPrcp)
          DEALLOCATE(ptor1Tavg)
          DEALLOCATE(ptor2Prcp)
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



end subroutine saveResultsBTBP