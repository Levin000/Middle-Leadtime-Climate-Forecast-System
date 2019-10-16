MODULE ArraySortModule
  IMPLICIT NONE
  INTERFACE shell_sort
    module procedure shell_sort_int
    module procedure shell_sort_real
    module procedure shell_sort_int_l
    module procedure shell_sort_real_d
  end interface
  INTERFACE bubble_sort
    module procedure bubble_sort_int
    module procedure bubble_sort_real
    module procedure bubble_sort_int_l
    module procedure bubble_sort_real_d
  end interface
  INTERFACE quick_sort
    module procedure quick_sort_int
    module procedure quick_sort_real
    module procedure quick_sort_int_l
    module procedure quick_sort_real_d
  end interface
  
  PUBLIC :: shell_sort,bubble_sort
  PRIVATE :: quick_sort
CONTAINS
  !****************************************************************************
  !  subroutine: shell_sort_int(array,n), deal with integer 4bytes type array
  !  purpose:  sort the array by shell sort method
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  SUBROUTINE shell_sort_int(a)
    implicit none
    integer :: n
    integer i,j
    integer k
    integer :: a(:)
    integer :: temp
    ! get the size of array
    n = size(a)
    k=n/2
    do while( k>0 )
      do i=k+1,n
        j=i-k
        do while( j>0 )
          if ( a(j) .gt. a(j+k) ) then
            temp=a(j)
            a(j)=a(j+k)
            a(j+k)=temp
            j=j-k
          else
            exit 
          end if
        end do
      end do
      k=k/2
    end do
    return
  end subroutine shell_sort_int
  !****************************************************************************
  !  subroutine: shell_sort_real(array,n), deal with real 4bytes type array
  !  purpose:  sort the array by shell sort method
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  SUBROUTINE shell_sort_real(a)
    implicit none
    integer :: n
    integer i,j
    integer k
    real :: a(:)
    real :: temp
    ! get the size of array
    n = size(a)
    k=n/2
    do while( k>0 )
      do i=k+1,n
        j=i-k
        do while( j>0 )
          if ( a(j) .gt. a(j+k) ) then
            temp=a(j)
            a(j)=a(j+k)
            a(j+k)=temp
            j=j-k
          else
            exit 
          end if
        end do
      end do
      k=k/2
    end do
    return
  end subroutine shell_sort_real
  !****************************************************************************
  !  subroutine: shell_sort_int_l(array,n), deal with integer 8bytes type array
  !  purpose:  sort the array by shell sort method
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  SUBROUTINE shell_sort_int_l(a)
    implicit none
    integer :: n
    integer i,j
    integer k
    integer(kind = 8) :: a(:)
    integer(kind = 8) :: temp
    ! get the size of array
    n = size(a)
    k=n/2
    do while( k>0 )
      do i=k+1,n
        j=i-k
        do while( j>0 )
          if ( a(j) .gt. a(j+k) ) then
            temp=a(j)
            a(j)=a(j+k)
            a(j+k)=temp
            j=j-k
          else
            exit 
          end if
        end do
      end do
      k=k/2
    end do
    return
  end subroutine shell_sort_int_l
  !****************************************************************************
  !  subroutine: shell_sort_real_d(array,n), deal with real 8bytes type array
  !  purpose:  sort the array by shell sort method
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  SUBROUTINE shell_sort_real_d(a)
    implicit none
    integer :: n
    integer i,j
    integer k
    real(kind = 8) :: a(:)
    real(kind = 8) :: temp
    ! get the size of array
    n = size(a)
    k=n/2
    do while( k>0 )
      do i=k+1,n
        j=i-k
        do while( j>0 )
          if ( a(j) .gt. a(j+k) ) then
            temp=a(j)
            a(j)=a(j+k)
            a(j+k)=temp
            j=j-k
          else
            exit 
          end if
        end do
      end do
      k=k/2
    end do
    return
  end subroutine shell_sort_real_d
 
  !****************************************************************************
  !  subroutine: bubble_sort_int(array,n), deal with integer 4bytes type array
  !  purpose:  sort the array
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  SUBROUTINE bubble_sort_int(a)
    implicit none
    integer :: n
    integer :: i,j
    integer :: a(:)
    integer :: temp
    
    ! get the size of array
    n = size(a)
    
    do i=n-1,1,-1
      do j=1,i
        if ( a(j) > a(j+1) ) then
          temp=a(j)
          a(j)=a(j+1)
          a(j+1)=temp
        end if
      end do
    end do
    return
  end subroutine bubble_sort_int
  
  !****************************************************************************
  !  subroutine: bubble_sort_real(array,n), deal with real 4bytes type array
  !  purpose:  sort the array
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  SUBROUTINE bubble_sort_real(a)
    implicit none
    integer :: n
    integer :: i,j
    real :: a(:)
    real :: temp
    
    ! get the size of array
    n = size(a)
    
    do i=n-1,1,-1
      do j=1,i
        if ( a(j) > a(j+1) ) then
          temp=a(j)
          a(j)=a(j+1)
          a(j+1)=temp
        end if
      end do
    end do
    return
  end subroutine bubble_sort_real
  !****************************************************************************
  !  subroutine: bubble_sort_int_l(array,n), deal with integer 8bytes type array
  !  purpose:  sort the array
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  SUBROUTINE bubble_sort_int_l(a)
    implicit none
    integer :: n
    integer :: i,j
    integer(kind = 8) :: a(:)
    integer(kind = 8) :: temp
    
    ! get the size of array
    n = size(a)
    
    do i=n-1,1,-1
      do j=1,i
        if ( a(j) > a(j+1) ) then
          temp=a(j)
          a(j)=a(j+1)
          a(j+1)=temp
        end if
      end do
    end do
    return
  end subroutine bubble_sort_int_l
  
  !****************************************************************************
  !  subroutine: bubble_sort_real_d(array,n), deal with real 8bytes type array
  !  purpose:  sort the array
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  SUBROUTINE bubble_sort_real_d(a)
    implicit none
    integer :: n
    integer :: i,j
    real(kind = 8) :: a(:)
    real(kind = 8) :: temp
    
    ! get the size of array
    n = size(a)
    
    do i=n-1,1,-1
      do j=1,i
        if ( a(j) > a(j+1) ) then
          temp=a(j)
          a(j)=a(j+1)
          a(j+1)=temp
        end if
      end do
    end do
    return
  end subroutine bubble_sort_real_d
  !****************************************************************************
  !  subroutine: quick_sort_real_d(inputarray,n,s,e), deal with int 4bytes type array
  !  purpose:  sort the array
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  RECURSIVE SUBROUTINE quick_sort_int(a,s,e)
    implicit none
    integer :: n
    integer :: s    ! 传入的参数, 这一组的类型起始位置
    integer :: e    ! 传入的参数, 这一组的类型结束位置
    integer :: l,r  
    integer :: a(:) ! 存放数据的类型
    integer :: k    ! 记录键值a(s)
    integer :: temp ! 交换两个数值时用的
    
    n = size(a)
    l=s
    r=e+1
    if ( r<=l ) return
    k=a(s)  
    do while(.true.)
      do while( .true. )
        l=l+1
        if ( (a(l) > k) .or. (l>=e) ) exit
      end do
      do while( .true. )
        r=r-1
        if ( (a(r) < k) .or. (r<=s) ) exit
      end do
      if ( r <= l ) exit
      temp=a(l)
      a(l)=a(r)
      a(r)=temp
    end do
    temp=a(s)
    a(s)=a(r)
    a(r)=temp
    call quick_sort_int(a,s,r-1)
    call quick_sort_int(a,r+1,e)
    return
  end subroutine quick_sort_int
  !****************************************************************************
  !  subroutine: quick_sort_real_d(inputarray,n,s,e), deal with real 4bytes type array
  !  purpose:  sort the array
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  RECURSIVE SUBROUTINE quick_sort_real(a,s,e)
    implicit none
    integer :: n
    integer :: s    ! 传入的参数, 这一组的类型起始位置
    integer :: e    ! 传入的参数, 这一组的类型结束位置
    integer :: l,r  
    real :: a(:) ! 存放数据的类型
    real :: k    ! 记录键值a(s)
    real :: temp ! 交换两个数值时用的
    
    n = size(a)
    l=s
    r=e+1
    if ( r<=l ) return
    k=a(s)  
    do while(.true.)
      do while( .true. )
        l=l+1
        if ( (a(l) > k) .or. (l>=e) ) exit
      end do
      do while( .true. )
        r=r-1
        if ( (a(r) < k) .or. (r<=s) ) exit
      end do
      if ( r <= l ) exit
      temp=a(l)
      a(l)=a(r)
      a(r)=temp
    end do
    temp=a(s)
    a(s)=a(r)
    a(r)=temp
    call quick_sort_real(a,s,r-1)
    call quick_sort_real(a,r+1,e)
    return
  end subroutine quick_sort_real
  !****************************************************************************
  !  subroutine: quick_sort_real_d(inputarray,n,s,e), deal with integer 8bytes type array
  !  purpose:  sort the array
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  RECURSIVE SUBROUTINE quick_sort_int_l(a,s,e)
    implicit none
    integer :: n
    integer :: s    ! 传入的参数, 这一组的类型起始位置
    integer :: e    ! 传入的参数, 这一组的类型结束位置
    integer :: l,r  
    integer(kind = 8) :: a(:) ! 存放数据的类型
    integer(kind = 8) :: k    ! 记录键值a(s)
    integer(kind = 8) :: temp ! 交换两个数值时用的
    
    n = size(a)
    l=s
    r=e+1
    if ( r<=l ) return
    k=a(s)  
    do while(.true.)
      do while( .true. )
        l=l+1
        if ( (a(l) > k) .or. (l>=e) ) exit
      end do
      do while( .true. )
        r=r-1
        if ( (a(r) < k) .or. (r<=s) ) exit
      end do
      if ( r <= l ) exit
      temp=a(l)
      a(l)=a(r)
      a(r)=temp
    end do
    temp=a(s)
    a(s)=a(r)
    a(r)=temp
    call quick_sort_int_l(a,s,r-1)
    call quick_sort_int_l(a,r+1,e)
    return
  end subroutine quick_sort_int_l
  !****************************************************************************
  !  subroutine: quick_sort_real_d(inputarray,n,s,e), deal with real 8bytes type array
  !  purpose:  sort the array
  !  parameters:
  !      a is the array which will be sorted
  !****************************************************************************
  RECURSIVE SUBROUTINE quick_sort_real_d(a,s,e)
    implicit none
    integer :: n
    integer :: s    ! 传入的参数, 这一组的类型起始位置
    integer :: e    ! 传入的参数, 这一组的类型结束位置
    integer :: l,r  
    real(kind = 8) :: a(:) ! 存放数据的类型
    real(kind = 8) :: k    ! 记录键值a(s)
    real(kind = 8) :: temp ! 交换两个数值时用的
    
    n = size(a)
    l=s
    r=e+1
    if ( r<=l ) return
    k=a(s)  
    do while(.true.)
      do while( .true. )
        l=l+1
        if ( (a(l) > k) .or. (l>=e) ) exit
      end do
      do while( .true. )
        r=r-1
        if ( (a(r) < k) .or. (r<=s) ) exit
      end do
      if ( r <= l ) exit
      temp=a(l)
      a(l)=a(r)
      a(r)=temp
    end do
    temp=a(s)
    a(s)=a(r)
    a(r)=temp
    call quick_sort_real_d(a,s,r-1)
    call quick_sort_real_d(a,r+1,e)
    return
  end subroutine quick_sort_real_d
END MODULE ArraySortModule