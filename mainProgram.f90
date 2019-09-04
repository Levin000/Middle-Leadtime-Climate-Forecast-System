Program mainProgram
      Implicit None
      integer :: PressKey
      integer :: ConfirmStrLen
      character(len = 200) :: ConfirmStr
      real :: StartRate,EndRate

      !############################################################################
      !                     Variables       Initialization
      !############################################################################
      StartRate = 0.0
      EndRate = 1.0
      !############################################################################
      !                        ReadMe      Information
      !############################################################################
      print *,'###################################################################'
      print *,'       Welcome To The Monthly Precipitation Forecast Model         '
      print *,'###################################################################'
      print *,' '
      print *,'  Design of this model is aim at forecase the monthly precipitation'
      print *,'of Meteorological Stations. Model include seven main process, as    '
      print *,'below:'
      print *,' '
      print *,'1.Calculate R^2\R\P of any two stations in GHCN data base on precip'
      print *,'  itaiton data. Time from 1901 to 2010. Train rate is 0.6(as defaul'
      print *,'  t). Couple number of valid data must >= 50(as default).'
      print *,' '
      print *,'2.Forecast and check the monthly precipitation use simple linear r-'
      print *,'  egression methods base on precipitation data. In this model, we '
      print *,'  we using another predictor station in this projecitons to reduce '
      print *,'  residual by only one predictor station. "Another Station" is def-'
      print *,'  ined as the "stationID" and "AheadMonth" not equal to the first '
      print *,'  predictor.'
      print *,' '
      print *,'3.Forecast and check the monthly precipitation use simple linear re'
      print *,'  gression methods and K-fold cross check methods base on precipita'
      print *,'  tion data. Time from 1901 to 2010. Train rate in simple linear re'
      print *,'  gression is 0.6(as default). K-fold num in K-fold cross check is '
      print *,'  the length of valid couple data. Couple number of valid data must'
      print *,'  >= 50(as default).'
      print *,' '
      print *,'4.Forecast and check the monthly precipitation Only use simple line'
      print *,'  ar regression methods base on precipitation data. Time from 1901 '
      print *,'  to 2010. Train num is the length of valid couple data minus 1.(as'
      print *,'  default). Couple number of valid data must >= 50(as default).'
      print *,' '
      print *,'5.Calculate R^2\R\P of any two stations in GHCN data base on precip'
      print *,'  itaiton data and temperature data. Time from 1901 to 2010. Train '
      print *,'  rate is 0.6(as default). Couple number of valid data must >= 50(a'
      print *,'  s default).'
      print *,' '
      print *,'6.Forecast and check the monthly precipitation use simple linear re'
      print *,'  gression methods and K-fold cross check methods base on temperatu'
      print *,'  re  data.  Time from 1901 to 2010. Train rate in simple linear re'
      print *,'  gression is 0.6(as default). K-fold num in K-fold cross check is '
      print *,'  the length of valid couple data. Couple number of valid data must'
      print *,'  >= 50(as default).'
      print *,' '
      print *,'7.Forecast and check the monthly precipitation Only use simple line'
      print *,'  ar regression methods base on temperature data. Time from 1901 '
      print *,'  to 2010. Train num is the length of valid couple data minus 1.(as'
      print *,'  default). Couple number of valid data must >= 50(as default).'
      print *,' '
      print *,'The basic data used in this model from GHCN V2 stations precipitati'
      print *,'on data and V3 stations temperature data.'
      print *,''
      print *,'###################################################################'
      print *,'NOTES: If you are a new user, please read Important Information above'
      print *,'NOTES: If you are a new user, please read Important Information above'
      print *,'NOTES: If you are a new user, please read Important Information above'
      print *,'###################################################################'
      print *,'Press any key to Start model!'
      read (*,*)
      !##########################################################################################
      !                                   Main Program
      !##########################################################################################
      print *,'Please entry the code of the module you want use £º'
      print *,'1.CalStationPrcpRP_BP'
      print *,'2.CalStationPrcpRP_BP_add_next'
      print *,'3.PredicteAndCheckSystem_BP'
      print *,'4.PredictLastOneData_BP'
      print *,'5.CalStationPrcpRP_BT'
      print *,'6.PredicteAndCheckSystem_BT'
      print *,'7.PredictLastOneData_BT'
      

      read (*,*),PressKey
      do while(PressKey)
        print *,'Please confirm the module you want use :'
        select case(PressKey)
        case(1)
          print *,'>> CalStationPrcpRP_BP      yes/no?'
        case(2)
          print *,'>> CalStationPrcpRP_BP_add_next      yes/no?'
        case(3)
          print *,'>> PredicteAndCheckSystem_BP      yes/no?'
        case(4)
          print *,'>> PredictLastOneData_BP      yes/no?'
        case(5)
          print *,'>> CalStationPrcpRP_BT      yes/no?'
        case(6)
          print *,'>> PredicteAndCheckSystem_BT      yes/no?'
        case(7)
          print *,'>> PredictLastOneData_BT      yes/no?'
        case default
          print *,'>> Incorrect Module Code!'
        end select
        print *,' '
        read (*,*),ConfirmStr
        print *,' '
        if(trim(ConfirmStr)=='yes') then
          exit
        else if(trim(ConfirmStr)=='no') then
          print *,'Please entry the code of the module you want use £º'
          print *,'1.CalStationPrcpRP_BP'
          print *,'2.CalStationPrcpRP_BP_add_next'
          print *,'3.PredicteAndCheckSystem_BP'
          print *,'4.PredictLastOneData_BP'
          print *,'5.CalStationPrcpRP_BT'
          print *,'6.PredicteAndCheckSystem_BT'
          print *,'7.PredictLastOneData_BT'
          read (*,*),PressKey
        end if
      end do

      select case(PressKey)
      case(1)
        !Base On Precipitation
        print *,' '
        print *,'Default StartRate = 0.0\EndRate = 1.0, if need please modify those value.'
        print *,' '
        print *,'yes/no?'
        print *,' '
        read (*,*),ConfirmStr
        if(trim(ConfirmStr)=='yes') then
          do while(trim(ConfirmStr)=='yes')
            print *,' '
881         print *,'Please input StartRate(as format 0.x,StartRate should >= 0.0 and less than 1.0):'
            print *,' '
            read (*,*) StartRate
            print *,' '
            print *,'Please input EndRate(as format 0.x,EndRate should great than 0.0 and <= 1.0):'
            print *,' '
            read (*,*) EndRate
            print *,' '
            print *,'Modified StartRate is',StartRate,'EndRate is',EndRate
            print *,' '
            print *,'yes/no?'
            print *,' '
            read (*,*),ConfirmStr
            print *,'  '
            if(trim(ConfirmStr)=='yes') then
              exit
            else if(trim(ConfirmStr)=='no') then
              goto 881
            end if
          end do
        end if
        call CalStationPrcpRP_BP(StartRate,EndRate)
        pause
      case(2)
        !Base On Precipitation
        print *,' '
        print *,'Default StartRate = 0.0\EndRate = 1.0, if need please modify those value.'
        print *,' '
        print *,'yes/no?'
        print *,' '
        read (*,*),ConfirmStr
        if(trim(ConfirmStr)=='yes') then
          do while(trim(ConfirmStr)=='yes')
            print *,' '
882         print *,'Please input StartRate(as format 0.x,StartRate should >= 0.0 and less than 1.0):'
            print *,' '
            read (*,*) StartRate
            print *,' '
            print *,'Please input EndRate(as format 0.x,EndRate should great than 0.0 and <= 1.0):'
            print *,' '
            read (*,*) EndRate
            print *,' '
            print *,'Modified StartRate is',StartRate,'EndRate is',EndRate
            print *,' '
            print *,'yes/no?'
            print *,' '
            read (*,*),ConfirmStr
            print *,'  '
            if(trim(ConfirmStr)=='yes') then
              exit
            else if(trim(ConfirmStr)=='no') then
              goto 882
            end if
          end do
        end if
        call CalStationPrcpRP_BP_add_next(StartRate,EndRate)
        pause
      case(3)
        !Base On Precipitation
        call PredicteAndCheckSystem_BP()
        pause
      CASE(4)
        !Base On Precipitation
        call PredictLastOneData_BP()
        pause
      case(5)
        !Base On Temperature
        print *,' '
        print *,'Default StartRate = 0.0\EndRate = 1.0, if need please modify those value.'
        print *,' '
        print *,'yes/no?'
        print *,' '
        read (*,*),ConfirmStr
        if(trim(ConfirmStr)=='yes') then
          do while(trim(ConfirmStr)=='yes')
            print *,' '
885         print *,'Please input StartRate(as format 0.x,StartRate should >= 0.0 and less than 1.0):'
            print *,' '
            read (*,*) StartRate
            print *,' '
            print *,'Please input EndRate(as format 0.x,EndRate should great than 0.0 and <= 1.0):'
            print *,' '
            read (*,*) EndRate
            print *,' '
            print *,'Modified StartRate is',StartRate,'EndRate is',EndRate
            print *,' '
            print *,'yes/no?'
            print *,' '
            read (*,*),ConfirmStr
            if(trim(ConfirmStr)=='yes') then
              exit
            else if(trim(ConfirmStr)=='no') then
              goto 885
            end if
          end do
        end if
        call CalStationPrcpRP_BT(StartRate,EndRate)
        pause
      case(6)
        !Base On Temperature
        call PredicteAndCheckSystem_BT()
        pause
      case(7)
        !Base On Temperature
        call PredictLastOneData_BT()
        pause
        case default
        print *,'>> Incorrect Module Code!'
        pause
      end select






end Program mainProgram