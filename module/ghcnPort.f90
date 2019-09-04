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
  
  