MODULE arraySortModule
  IMPLICIT NONE
  PUBLIC :: shell_sort,bubble_sort
  PRIVATE :: quick_sort
CONTAINS
  !****************************************************************************
  !
  !  SUBROUTINE: Shell_Sort(array,n)
  !  PURPOSE:  sort the array
  !
  !****************************************************************************
  SUBROUTINE shell_sort(a,n)
    IMPLICIT NONE
    INTEGER :: n ! 传入数组的大小
    INTEGER i,j       ! 循环计数器
    INTEGER k         ! k 值
    REAL(KIND = 8) :: a(n)   !传入的数组
    REAL(KIND = 8) :: temp   !交换变量
    k=n/2             ! k 的初值
    DO WHILE( k>0 )
      DO i=k+1,n
        j=i-k
        DO WHILE( j>0 )
          ! 如果a(j)>a(j+k),要交换它们的数值,并往回取出
          ! a(j-k)\a(j)为新的一组来比较。
          IF ( a(j) .gt. a(j+k) ) THEN
            temp=a(j)
            a(j)=a(j+k)
            a(j+k)=temp
            j=j-k
          ELSE
            EXIT ! a(j)<a(j+k)时可跳出循环
          END IF
        END DO
      END DO
      k=k/2 ! 设定新的k值
    END DO
    RETURN
  END SUBROUTINE shell_sort
  !****************************************************************************
  !
  !  SUBROUTINE: Bubble_Sort(array,n)
  !  PURPOSE:  sort the array
  !
  !****************************************************************************
  SUBROUTINE bubble_sort(a,n)
    IMPLICIT NONE
    INTEGER :: n
    INTEGER :: i,j
    REAL(KIND = 8) :: a(n),temp
    DO i=n-1,1,-1   ! 开始做n-1次的扫瞄  1,n=1
      DO j=1,i      ! 一对一对的来比较，i之后的数字不用比较 1,n-i
        IF ( a(j) > a(j+1) ) THEN  ! 如果a(j) > a(j+1) 就把这两个数值交换
          temp=a(j)
          a(j)=a(j+1)
          a(j+1)=temp
        END IF
      END DO
    END DO
    RETURN
  END SUBROUTINE bubble_sort
  !****************************************************************************
  !
  !  SUBROUTINE: Quick_Sort(InputArray,N,S,E)
  !  PURPOSE:  sort the array
  !
  !****************************************************************************
  RECURSIVE SUBROUTINE quick_sort(a,n,s,e)
    IMPLICIT NONE
    INTEGER :: n    ! 表示类型的大小
    INTEGER :: s    ! 传入的参数, 这一组的类型起始位置
    INTEGER :: e    ! 传入的参数, 这一组的类型结束位置
    INTEGER :: l,r  ! 用来找a(l)>k及a(r)<k时用的
    REAL(KIND = 8) :: a(n) ! 存放数据的类型
    REAL(KIND = 8) :: k    ! 记录键值a(s)
    REAL(KIND = 8) :: temp ! 交换两个数值时用的
    ! 首先要先给定l,r的初值. l要从头开始,e则要从尾开始
    l=s
    r=e+1
    ! right值 > left值 时才有必要进行排序
    IF ( r<=l ) RETURN
    k=a(s)  ! 设定键值
    DO WHILE(.true.)
      ! 找出a(l)<k的所在
      DO WHILE( .true. )
        l=l+1
        IF ( (a(l) > k) .or. (l>=e) ) EXIT
      END DO
      ! 找出a(r)>k的所在
      DO WHILE( .true. )
        r=r-1
        IF ( (a(r) < k) .or. (r<=s) ) EXIT
      END DO
      ! 如果right 跑到 left的左边时, 循环就该结束了
      IF ( r <= l ) EXIT
      ! 交换a(l),a(r)的数值
      temp=a(l)
      a(l)=a(r)
      a(r)=temp
    END DO
    ! 交换a(s),a(r)的数值
    temp=a(s)
    a(s)=a(r)
    a(r)=temp
    ! 把r之前的数据重新分组,再做排序
    CALL quick_sort(a,n,s,r-1)
    ! 把r之后的数据重新分组,再做排序
    CALL quick_sort(a,n,r+1,e)
    RETURN
  END SUBROUTINE quick_sort
END MODULE arraySortModule