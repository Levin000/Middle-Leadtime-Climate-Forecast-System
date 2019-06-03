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
    INTEGER :: n ! ��������Ĵ�С
    INTEGER i,j       ! ѭ��������
    INTEGER k         ! k ֵ
    REAL(KIND = 8) :: a(n)   !���������
    REAL(KIND = 8) :: temp   !��������
    k=n/2             ! k �ĳ�ֵ
    DO WHILE( k>0 )
      DO i=k+1,n
        j=i-k
        DO WHILE( j>0 )
          ! ���a(j)>a(j+k),Ҫ�������ǵ���ֵ,������ȡ��
          ! a(j-k)\a(j)Ϊ�µ�һ�����Ƚϡ�
          IF ( a(j) .gt. a(j+k) ) THEN
            temp=a(j)
            a(j)=a(j+k)
            a(j+k)=temp
            j=j-k
          ELSE
            EXIT ! a(j)<a(j+k)ʱ������ѭ��
          END IF
        END DO
      END DO
      k=k/2 ! �趨�µ�kֵ
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
    DO i=n-1,1,-1   ! ��ʼ��n-1�ε�ɨ��  1,n=1
      DO j=1,i      ! һ��һ�Ե����Ƚϣ�i֮������ֲ��ñȽ� 1,n-i
        IF ( a(j) > a(j+1) ) THEN  ! ���a(j) > a(j+1) �Ͱ���������ֵ����
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
    INTEGER :: n    ! ��ʾ���͵Ĵ�С
    INTEGER :: s    ! ����Ĳ���, ��һ���������ʼλ��
    INTEGER :: e    ! ����Ĳ���, ��һ������ͽ���λ��
    INTEGER :: l,r  ! ������a(l)>k��a(r)<kʱ�õ�
    REAL(KIND = 8) :: a(n) ! ������ݵ�����
    REAL(KIND = 8) :: k    ! ��¼��ֵa(s)
    REAL(KIND = 8) :: temp ! ����������ֵʱ�õ�
    ! ����Ҫ�ȸ���l,r�ĳ�ֵ. lҪ��ͷ��ʼ,e��Ҫ��β��ʼ
    l=s
    r=e+1
    ! rightֵ > leftֵ ʱ���б�Ҫ��������
    IF ( r<=l ) RETURN
    k=a(s)  ! �趨��ֵ
    DO WHILE(.true.)
      ! �ҳ�a(l)<k������
      DO WHILE( .true. )
        l=l+1
        IF ( (a(l) > k) .or. (l>=e) ) EXIT
      END DO
      ! �ҳ�a(r)>k������
      DO WHILE( .true. )
        r=r-1
        IF ( (a(r) < k) .or. (r<=s) ) EXIT
      END DO
      ! ���right �ܵ� left�����ʱ, ѭ���͸ý�����
      IF ( r <= l ) EXIT
      ! ����a(l),a(r)����ֵ
      temp=a(l)
      a(l)=a(r)
      a(r)=temp
    END DO
    ! ����a(s),a(r)����ֵ
    temp=a(s)
    a(s)=a(r)
    a(r)=temp
    ! ��r֮ǰ���������·���,��������
    CALL quick_sort(a,n,s,r-1)
    ! ��r֮����������·���,��������
    CALL quick_sort(a,n,r+1,e)
    RETURN
  END SUBROUTINE quick_sort
END MODULE arraySortModule