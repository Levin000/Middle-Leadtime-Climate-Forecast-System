MODULE statisticPort
      IMPLICIT NONE
      PUBLIC :: LinearRegression
      PUBLIC :: Pvalue
      PUBLIC :: correlation
      PRIVATE :: betai
      PRIVATE :: betacf
      PRIVATE :: gammln
      PRIVATE ::meanvar
      
  CONTAINS
  !****************************************************************************
      !  SUBROUTINE: getPrcpValidRight
      !  PURPOSE:  calculate right limit of x, base on the normal distribution μ+3σ 
      !****************************************************************************
      subroutine getTavgValidLimit(x,x_size,x_valid_left,x_valid_right,times)
      implicit none
        integer :: times
        integer :: x_size,x_ln_valid_size
        integer :: i
        real(kind = 8) :: x(x_size)
        real(kind = 8), allocatable :: x_valid(:)
        real(kind = 8) :: x_sum,x_mean,x_std,x_valid_right,x_valid_left

        x_sum = sum(x)
        x_mean = x_sum/x_size
        x_std = sqrt(sum((x-x_mean)**2)/(x_size-1))
        x_valid_right = x_mean + times * x_std
        x_valid_left  = x_mean - times * x_std
        return
      end subroutine    
      !****************************************************************************
      !  SUBROUTINE: getPrcpValidRight
      !  PURPOSE:  calculate right limit of x, base on the normal distribution μ+3σ 
      !****************************************************************************
      subroutine getPrcpValidRight(x,x_size,x_valid_right,times)
      implicit none
        integer :: times
        integer :: x_size,x_ln_valid_size
        integer :: i
        real(kind = 8) :: x(x_size),x_ln(x_size)
        real(kind = 8), allocatable :: x_valid(:)
        real(kind = 8) :: x_ln_sum,x_ln_mean,x_ln_std,x_ln_valid_right,x_valid_right

        where (x > 0)
          x_ln = log(x)
        elsewhere
          x_ln = 0
        end where

        x_ln_sum = sum(x_ln)
        x_ln_mean = x_ln_sum/x_size
        x_ln_std = sqrt(sum((x_ln-x_ln_mean)**2)/(x_size-1))
        x_ln_valid_right = x_ln_mean + times * x_ln_std
        x_valid_right = exp(x_ln_valid_right)
        return
      end subroutine    
      !****************************************************************************
      !
      !  SUBROUTINE: KFlodCrossCheck
      !  PURPOSE:  gain the results of K-Fold Cross Check
      !****************************************************************************
      SUBROUTINE KFoldCrossCheck(tempKFFactorMonth,tempKFStudyPrcpMonth,PredictedStudyPrcpMonth,PredictedStudyPrcpMonthCP,Num,KNum,R,P)
        !USE STATISTICPORT
        IMPLICIT NONE
        INTEGER :: Num,LocationNum,MissLocationNum
        INTEGER :: KNum   !Group Number of K-Flod CrossCheck, Here KNum is 10
        INTEGER,ALLOCATABLE :: Location(:),MissLocation(:)
        INTEGER :: i,ii,j,m,mk   ! m is the number of each group in K-Flod CrossCheck, mk is the Mod Number of (Num,KNum)
        REAL :: R,P
        REAL(KIND = 8) :: K,B
        REAL(KIND = 8) :: tempKFFactorMonth(Num),tempKFStudyPrcpMonth(Num),ModNum
        REAL(KIND = 8) :: PredictedStudyPrcpMonth(Num),PredictedStudyPrcpMonthCP(Num)
        REAL(KIND = 8),ALLOCATABLE :: tempPredictedStudyPrcpMonth(:)
        REAL(KIND = 8) :: R_Inf = -5.0, P_Inf = -6.0  !  P_OV_PV = -7.0,R_NAN = -8.0,
        m = Num/KNum
        mk = Num - m*KNum
        DO i = 1,KNum    !做KNum次预报，每次预报m个月数据（最后一次预报m+mk个数据）
          IF(i<KNum) THEN
            ALLOCATE(Location(Num-m))   !剔除m个值后剩余值得位置信息
            ALLOCATE(MissLocation(m))   !剔除值得位置信息
            ALLOCATE(tempPredictedStudyPrcpMonth(m))
            LocationNum = 0
            MissLocationNum = 0
            DO j = 1,Num
              IF(j<((i-1)*m+1).OR.j>i*m) THEN
                LocationNum = LocationNum + 1
                Location(LocationNum) = j
              ELSE
                MissLocationNum = MissLocationNum + 1
                MissLocation(MissLocationNum) = j
              END IF
            END DO
            !ModNum = tempKFFactorMonth(MissLocation(1))
            !print *,'MissValue:',ModNum
            CALL LinearRegression(tempKFFactorMonth(Location),tempKFStudyPrcpMonth(Location),Num - m,K,B)!,i
            PredictedStudyPrcpMonth(MissLocation) = K*tempKFFactorMonth(MissLocation) + B
            !tempPredictedStudyPrcpMonth = K*tempKFFactorMonth(MissLocation) + B
            !print *,MissLocation,tempPredictedStudyPrcpMonth,K,B
            !PAUSE
            !DO ii = 1,mMissLocationNum
            !    PredictedStudyPrcpMonth(MissLocation(ii)) = tempPredictedStudyPrcpMonth(ii)
            !END DO
            DEALLOCATE(Location)
            DEALLOCATE(missLocation)   !剔除值得位置信息
            DEALLOCATE(tempPredictedStudyPrcpMonth)
          ELSE
            ALLOCATE(Location(Num-m-mk))   !剔除m个值后剩余值得位置信息
            ALLOCATE(missLocation(m+mk))   !剔除值得位置信息
            ALLOCATE(tempPredictedStudyPrcpMonth(m+mk))
            LocationNum = 0
            MissLocationNum = 0
            DO j = 1,Num
              IF(j<((i-1)*m+1)) THEN
                LocationNum = LocationNum + 1
                Location(LocationNum) = j
              ELSE
                MissLocationNum = MissLocationNum + 1
                MissLocation(MissLocationNum) = j
              END IF
            END DO
            CALL LinearRegression(tempKFFactorMonth(Location),tempKFStudyPrcpMonth(Location),Num - m,K,B)!,i

            PredictedStudyPrcpMonth(MissLocation) = K*tempKFFactorMonth(MissLocation) + B
            DEALLOCATE(Location)
            DEALLOCATE(missLocation)   !剔除值得位置信息
            DEALLOCATE(tempPredictedStudyPrcpMonth)
          END IF
        END DO
        PredictedStudyPrcpMonthCP = PredictedStudyPrcpMonth
        WHERE(PredictedStudyPrcpMonth<0)
          PredictedStudyPrcpMonthCP = 0
        END WHERE
        CALL correlation(Num,PredictedStudyPrcpMonthCP,tempKFStudyPrcpMonth,R)
        IF(R == R_Inf) THEN
          P = R_Inf
        ELSE
          CALL Pvalue(Num,R,P)
        END IF

      END SUBROUTINE KFoldCrossCheck
      !****************************************************************************
      !
      !  SUBROUTINE: LinearRegression
      !  PURPOSE:  gain the parameters of simple linear regression
      !  x is independent variable
      !  y is dependent variable
      !  n is the number of array x
      !  k is slope of linear
      !  b is intercept distance
      !  equation is     Y = k*X + b
      !****************************************************************************
      SUBROUTINE LinearRegression(x,y,n,k,b)  ! iii
        IMPLICIT NONE
        INTEGER :: n
        INTEGER :: i,j!,iii
        REAL(KIND = 8) :: x(n),y(n)
        REAL(KIND = 8) :: k,b
        REAL(KIND = 8) :: Xmean,Ymean,Lxy,Lxx

        Lxy = 0.
        Lxx = 0.
        Xmean = SUM(x)/n
        Ymean = SUM(y)/n
        DO i = 1,n
          Lxy = Lxy + x(i)*y(i)
        END DO
        Lxy = Lxy -n*Xmean*Ymean
        DO i = 1,n
          Lxx = Lxx + x(i)*x(i)
        END DO
        Lxx = Lxx -n*Xmean*Xmean
        !IF(Lxx == 0) THEN !================================================
        !    print *,'========',iii
        !    print *,'x:', x
        !    print *,'y:', y
        !    PAUSE
        !END IF!================================================
        k = Lxy/Lxx
        b = Ymean - k*Xmean

      END SUBROUTINE LinearRegression
      !****************************************************************************
      !
      !  FUNCTION:  correlation(n,x,y,r)
      !
      !  PURPOSE:   For the correlation coefficient r between two series
      !             x(i) and y(i), WHERE i=1,...,n.
      !             input: n,x(n),y(n)
      !             n: number of time series
      !             x(n): raw series
      !             y(n): raw series
      !             output: r
      !             r: correlation coefficient between x and y
      !             By Dr. LI Jianping, January 5, 2000.
      !
      !             Modified By PHD WANG Leibin, March 27,2017
      !****************************************************************************
      SUBROUTINE correlation(n,x,y,r)
        IMPLICIT NONE
        INTEGER :: n,i
        REAL(KIND = 8) :: x(n),y(n)
        REAL :: r
        REAL(KIND = 8) :: ax,sx,vx
        REAL(KIND = 8) :: ay,sy,vy
        REAL(KIND = 8) :: sxy
        REAL(KIND = 8) :: R_Inf = -5
        CALL meanvar(n,x,ax,sx,vx)
        CALL meanvar(n,y,ay,sy,vy)
        sxy = 0.
        DO i = 1,n
          sxy = sxy+(x(i)-ax)*(y(i)-ay)
        END DO
        sxy = sxy/REAL(n,8)
        IF(sx*sy .EQ. 0) THEN
          r = R_Inf
        ELSE
          r = sxy/(sx*sy)
        END IF
        RETURN

      END SUBROUTINE correlation
      !****************************************************************************
      !
      !  FUNCTION:      meanvar(n,x,ax,sx,vx)
      !
      !  PURPOSE:       Computing the mean ax, standard deviation sx
      !                 and variance vx of a series x(i) (i=1,...,n).
      !                 input: n and x(n)
      !                 n: number of raw series
      !                 x(n): raw series
      !                 output: ax, sx and vx
      !                 ax: the mean value of x(n)
      !                 sx: the standard deviation of x(n)
      !                 vx: the variance of x(n)
      !                 By Dr. LI Jianping, May 6, 1998.
      !                 Modified By PHD WANG Leibin, March 27,2017
      !****************************************************************************
      SUBROUTINE meanvar(n,x,ax,sx,vx)
        IMPLICIT NONE
        INTEGER :: n, i
        REAL(KIND = 8) :: x(n)
        REAL(KIND = 8) :: ax,sx,vx
        ax=0.
        sx=0.
        vx=0.
        ax = SUM(x)/REAL(n,8)
        vx = SUM((x - ax)**2)/REAL(n,8)
        sx=SQRT(vx)
        RETURN
      END SUBROUTINE meanvar
      
      !****************************************************************************
      !
      !  FUNCTION:      Pvalue(N,R,P)
      !
      !  PURPOSE:       !!!! Note: 本程序为t-检验.
      !                 !!!! 1. 针对一元线性回归和相关系数进行显著性检验。
      !                 !!!! 2. 加入P 值大于1，此时P为-6.0
      !                 Modified By Dr WANG Leibin, March 27,2017
      !****************************************************************************
      SUBROUTINE Pvalue(N,R,P)
        !USE STATISTICPORT
        IMPLICIT NONE
        ! N is sample size. R is pearson coefficient.sig=*/**/NAN,表显著/极显著/不显著。
        ! t is T统计量。prob是t统计量在显著性水平为0.05/0.01时的临界值。prob<0.05/0.01时显著。
        INTEGER :: N
        REAL :: df, df0_01, df0_05, b0_01, b0_05, R, P
        REAL(KIND = 8) :: t, prob005, prob001   !,P_Inf = -6.0
        CHARACTER(LEN = 3) :: sig
        !REAL(KIND = 8),external :: betai
        LOGICAL(4) :: istatus

        df=N-2    !30 is sample size
        t=(ABS(R)*SQRT(df))/SQRT(1-R**2)
        df0_01 = 0.01*df
        df0_05 = 0.5*df
        b0_01 = 0.01
        b0_05 = 0.5
        prob001 = betai(df0_01,b0_01,df/(df+t**2))
        prob005 = betai(df0_05,b0_05,df/(df+t**2))
        IF(prob001<0.01) THEN
          sig=CHAR(42)//CHAR(42)
          P = prob001
        ELSE IF(prob005<0.05) THEN
          sig=CHAR(42)
          P = prob005
        ELSE IF(prob005>=0.05) THEN
          sig=CHAR(78)//CHAR(65)//CHAR(78)
          !IF(ABS(prob005)>=1) THEN
          !  P = P_Inf
          !ELSE
          P = prob005
          !END IF
        END IF


      END SUBROUTINE Pvalue
      FUNCTION betai(a,b,x)
        IMPLICIT NONE
        REAL :: a,b
        REAL(KIND = 8) :: betai,x
        REAL(KIND = 8) :: bt
        !REAL(KIND = 8),external :: betacf,gammln
        IF(x<0..OR.x>1.) PAUSE 'bad argument x in betai'
        IF(x==0..OR.x==1.) THEN
          bt=0.
        ELSE
          bt=EXP(gammln(a+b)-gammln(a)-gammln(b)+a*LOG(x)+b*LOG(1.-x))
        END IF
        IF(x<(a+1.)/(a+b+2.)) THEN
          betai=bt*betacf(a,b,x)/a
          RETURN
        ELSE
          betai=1.-bt*betacf(b,a,1.-x)/b
        END IF
        RETURN
      END FUNCTION betai
      FUNCTION betacf(a,b,x)
        IMPLICIT NONE
        INTEGER maxit
        REAL :: a,b
        REAL(KIND = 8) :: betacf,x,EPS,fpmin
        PARAMETER (maxit=100,EPS=3.e-7,fpmin=1.e-30)
        INTEGER m,m2
        REAL(KIND = 8) :: aa,c,d,del,h,qab,qam,qap
        qab=a+b
        qap=a+1.
        qam=a-1.
        c=1.
        d=1.-qab*x/qap
        IF(ABS(d)/=0) d=1./d
        h=d
        DO m=1,maxit
          m2=2*m
          aa=m*(b-m)*x/((qam+m2)*(a+m2))
          d=1.+aa*d
          IF(ABS(c)/=0)   c=1.+aa/c
          IF(ABS(d)/=0)   d=1./d
          h=h*d*c
          aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
          d=1.+aa*d
          IF(ABS(d)/=0)   c=1.+aa/c
          IF(ABS(c)/=0)   d=1./d
          del=d*c
          h=h*del
          IF(ABS(del-1.)/=0)     betacf=h
        END DO
        RETURN
        PAUSE 'a or b too big, or maxit too small in betacf'
      END FUNCTION betacf
      FUNCTION gammln(xx)
        IMPLICIT NONE
        REAL :: xx
        REAL(KIND = 8) gammln
        INTEGER j
        DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
        SAVE cof,stp
        DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,&
          24.01409824083091d0,-1.231739572450155d0,&
          .1208650973866179d-2,-.5395239384953d-5,&
          2.5066282746310005d0/
        x=xx
        y=x
        tmp=x+5.5d0
        tmp=(x+0.5d0)*LOG(tmp)-tmp
        ser=1.000000000190015d0
        DO j=1,6
          y=y+1.d0
          ser=ser+cof(j)/y
        END DO
        gammln=tmp+LOG(stp*ser/x)
      END FUNCTION gammln
    
END MODULE