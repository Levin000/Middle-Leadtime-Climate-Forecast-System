MODULE arrayPort
  USE IFPORT
  USE arraySortModule
  IMPLICIT NONE
  PUBLIC :: Array_Unique_Number
  PUBLIC :: ArrayFind
  PUBLIC :: ArrayUnique
  PUBLIC :: Unique
CONTAINS
  !****************************************************************************
  !
  !  SUBROUTINE: ARRAY_UNIQUE_Number
  !  PURPOSE:  Get the Number of unique value of the input array
  !
  !****************************************************************************
  INTEGER FUNCTION Array_Unique_Number(inputArray)
    !USE IFPORT
    IMPLICIT NONE
    INTEGER :: inputArrayLen,uniqueNumber
    INTEGER :: i
    REAL(KIND = 8) inputArray(:)
    REAL(KIND = 8),ALLOCATABLE :: tempInputArray(:)
    REAL(KIND = 8) temp
    inputArrayLen = SIZE(inputArray)
    ALLOCATE(tempInputArray(inputArrayLen))
    tempInputArray = InputArray
    CALL shell_sort(tempInputArray,inputArrayLen)
    temp = tempInputArray(1)
    uniqueNumber = 1
    DO i = 2,inputArrayLen
      IF(temp /= tempInputArray(i)) THEN
        uniqueNumber = uniqueNumber + 1
        temp = tempInputArray(i)
      END IF
    END DO
    Array_Unique_Number = uniqueNumber
    !implicit none
    !REAL(KIND = 8) :: InputArray(:)
    !INTEGER :: InputArrayLen = 0, num = 1, accountNum
    !REAL(KIND = 8), ALLOCATABLE :: ArrayOneDim(:)  !输入数组转换成一维数组
    !REAL(KIND = 8), ALLOCATABLE :: ArrayOneDimUnique(:)  !输入数组转换成一维数组
    !INTEGER :: II,JJ
    !InputArrayLen = SIZE(InputArray);
    !ALLOCATE(ArrayOneDim(InputArrayLen)) !为一维数组分配存储空间
    !ALLOCATE(ArrayOneDimUnique(InputArrayLen)) !为一维数组unique分配存储空间
    !ArrayOneDim = InputArray
    !ArrayOneDimUnique = 0
    !ArrayOneDimUnique(1) = ArrayOneDim(1)
    !do II = 2,InputArrayLen
    !    accountNum = 0  !重置计数器为0
    !    do JJ = 1,num
    !        if (ArrayOneDim(II) == ArrayOneDimUnique(JJ)) then
    !            accountNum = accountNum + 1
    !        end if
    !    end do
    !    if (accountNum == 0) then
    !        num = num+1
    !        ArrayOneDimUnique(num) = ArrayOneDim(II)
    !    end if
    !end do
    !Array_Unique_Number = num
  END FUNCTION Array_Unique_Number
  !****************************************************************************
  !
  !  FUNCTION: ArrayFind(Array,condition,a)
  !
  !  PURPOSE:  This FUNCTION can return data and loction information in Array where Array data have relationship
  !            with a as condition. Only for Array with 1 Dim
  !            主要目的是为了求一维数组（Arrya）中等于、大于或小于(condition)某一值（a）的数以及所在数组下标
  !****************************************************************************
  FUNCTION ArrayFind(Array,condition,a)
    IMPLICIT NONE
    CHARACTER(LEN = 1) :: condition
    INTEGER :: ArrayLen
    INTEGER :: num, II, JJ, Index
    REAL(KIND = 8) :: a
    REAL(KIND = 8) :: Array(:)
    INTEGER(KIND = 8),ALLOCATABLE :: ArrayFind(:)
    
    ArrayLen = SIZE(Array)
    Index = 0

    SELECT CASE(condition)
    CASE('/')
      num = COUNT(Array /= a)
      ALLOCATE(ArrayFind(num))
      DO II = 1,ArrayLen
        IF(Array(II)/=a) THEN
          Index = Index + 1
          ArrayFind(Index) = II
        END IF
      END DO
    CASE('=')
      num = COUNT(Array == a)
      ALLOCATE(ArrayFind(num))
      DO II = 1,ArrayLen
        IF(Array(II)==a) THEN
          Index = Index + 1
          ArrayFind(Index) = II
        END IF
      END DO
    CASE('>')
      num = COUNT(Array > a)
      ALLOCATE(ArrayFind(num))
      DO II = 1,ArrayLen
        IF(Array(II) > a) THEN
          Index = Index + 1
          ArrayFind(Index) = II
        END IF
      END DO
    CASE('<')
      num = COUNT(Array < a)
      ALLOCATE(ArrayFind(num))
      DO II = 1,ArrayLen
        IF(Array(II) < a) THEN
          Index = Index + 1
          ArrayFind(Index) = II
        END IF
      END DO
    END SELECT
  END FUNCTION ArrayFind
  !****************************************************************************
  !
  !  SUBROUTINE: ArrayUnique
  !  PURPOSE:  Get the unique data of the input array
  !
  !****************************************************************************
  FUNCTION ArrayUnique(InputArray, UniqueNum)
    IMPLICIT NONE
    INTEGER :: InputArrayLen
    INTEGER :: num = 1, accountNum, UniqueNum
    INTEGER :: II,JJ
    INTEGER :: S = 1
    REAL(KIND = 8) :: InputArray(:)
    REAL(KIND = 8), ALLOCATABLE :: ArrayOneDim(:)  !输入数组转换成一维数组
    REAL(KIND = 8), ALLOCATABLE :: ArrayOneDimUnique(:)  !输入数组转换成一维数组
    REAL(KIND = 8) :: ArrayUnique(UniqueNum)
    !print *,"进入Unique"
    InputArrayLen = SIZE(InputArray);
    !print *,InputArrayLen
    ALLOCATE(ArrayOneDim(InputArrayLen)) !为一维数组分配存储空间
    ALLOCATE(ArrayOneDimUnique(InputArrayLen)) !为一维数组unique分配存储空间
    ArrayOneDim = InputArray
    ArrayUnique(1) = ArrayOneDim(1)
    DO II = 2,InputArrayLen
        accountNum = 0  !重置计数器为0
        DO JJ = 1,num
            IF (ArrayOneDim(II) == ArrayUnique(JJ)) THEN
                accountNum = accountNum + 1
            END IF
        END DO
        IF (accountNum == 0) THEN
            num = num+1
            ArrayUnique(num) = ArrayOneDim(II)
        END IF
    END DO
    !print *,"进行排序"
    !print *,num
    CALL shell_sort(ArrayUnique,UniqueNum)
  END FUNCTION ArrayUnique
  
  !****************************************************************************
  !
  !  SUBROUTINE: UNIQUE
  !  PURPOSE:  Get the unique data of the input array without known UniqueNum
  !
  !****************************************************************************
  SUBROUTINE Unique(InputArray,InputArrayUnique)   !!!, UniqueNum
    !USE IFPORT
    IMPLICIT NONE
    INTEGER :: InputArrayLen,inputArrayUniqueNumber
    INTEGER :: num = 1, accountNum !!!, UniqueNum
    INTEGER :: II,JJ
    INTEGER :: S = 1
    REAL(KIND = 8) :: InputArray(:)
    REAL(KIND = 8), ALLOCATABLE :: ArrayOneDim(:)  !输入数组转换成一维数组
    REAL(KIND = 8), ALLOCATABLE :: ArrayOneDimUnique(:)  !输入数组转换成一维数组
    REAL(KIND = 8), ALLOCATABLE :: InputArrayUnique(:)  !!!UniqueNum
    !print *,"进入Unique"
    !pause
    InputArrayLen = SIZE(InputArray);
    ALLOCATE(ArrayOneDim(InputArrayLen)) !为一维数组分配存储空间
    ALLOCATE(ArrayOneDimUnique(InputArrayLen)) !为一维数组unique分配存储空间
    ArrayOneDim = InputArray
    ArrayOneDimUnique(1) = ArrayOneDim(1)!!!ArrayUnique(1) = ArrayOneDim(1)
    accountNum = 1 !!!
    DO II = 2,InputArrayLen
        IF (COUNT(ArrayOneDimUnique(1:accountNum) == ArrayOneDim(II))==0) THEN
            accountNum = accountNum + 1
            ArrayOneDimUnique(accountNum) = ArrayOneDim(II)
        END IF
    END DO
    ALLOCATE(InputArrayUnique(accountNum))
    InputArrayUnique(:) = ArrayOneDimUnique(1:accountNum)
    !PRINT *,'SIZE of InpurArrayUnique:',SIZE(InputArrayUnique)
    !PRINT *,'Array Sort started: running...'
    CALL shell_sort(InputArrayUnique,accountNum)
    !PRINT *,'Array Quick_Sort finished!'
  END SUBROUTINE Unique
END MODULE arrayPort