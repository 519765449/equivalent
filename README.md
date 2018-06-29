# equivalent
      subroutine GTPFL(*) 
         use DEFTYPE
         use COMVAL
         use GENERALSUB
         implicit none
         integer :: err = 0
         integer pflun,si,i,rd1,rd2     
         integer slun(14),ctno(14) 
         character(len=220) rdstr,mesg,rdstr1,rdstr2
         character(len=80)  FMT
         character(len=8)   voltname,pgqgname
         logical error,alive
         real pgqgbkv,diff2,diff1,voltbkv
        !---------------------------------
        !   pflun             PFL file
        !   slun(1)     ctno(1)  bus data
        !   slun(2)     ctno(2)  transmission line data
        !   slun(3)     ctno(3)  transformer data
        !   slun(4)     ctno(4)  UTLC transformer data
        !   slun(5)     ctno(5)  generator data
        !   slun(6)     ctno(6)  load data
        !   slun(7)     ctno(7)  shunt data
        !   slun(8)     ctno(8)  BX shunt data 
        !   slun(9)     ctno(9) two-terminal DC data
        !   slun(10)    ctno(10) Area information 
        !   slun(11)    ctno(11) Unsch Q
        !   slun(12)    ctno(12) L+
        !   slun(13)    ctno(13) 
        !---------------------------------
          error = .false.
          pflnam = trim(pflnam)//'.PFL'
          inquire(file = trim(pflnam),exist = alive)
          if(.not.alive)then
              write(*,'(A)') 'THE FILE ' ,trim(pflnam),' DOES NOT EXIST '
              fterr = .true.
              return 1
          end if
          do si = 11,70
              if(.NOT.LUNMAP(si))then
                  pflun = si
                  LUNMAP(si) = .TRUE.
                  exit
              end if
          end do 
          call gtlun(slun,error)
          if (error) then
              fterr = .true.
              return 1
          end if
          ctno = 0
          mesg = '----    READING PFL FILE   ----'
          call WRCDF(mesg,'(/,T25,A)')
          open(pflun,file = trim(pflnam),action = 'read')
          do si = 1,size(slun)
            open(slun(si),status = 'scratch') 
         end do

         continue
         do i = 1, 1
           read(pflun,'(A)')rdstr
         end do
         read(pflun,'(A)')rdstr
         rdstr1 = adjustl(rdstr(3:7))
         rdstr2 = adjustl(rdstr(21:25))
         read(rdstr1,"(I5)")rd1
         TNBUS = rd1
         read(rdstr2,"(I5)")rd2
         TNGEN = rd2
         read(pflun,'(A)')rdstr
         goto 1000


1000    continue
        read(pflun,'(A)',end=1004)rdstr
        if(rdstr(2:3) == '0/')then
          goto 1004
        end if
        if(rdstr(2:2) == '.')then
          goto 1000 
        else 
          write(slun(1),'(A)') rdstr
          ctno(1) = ctno(1) + 1
        end if  
        goto 1000
1004    continue
        read(pflun,'(A)',end=1005)rdstr
        if(rdstr(2:3) == '0/')then
        goto 1005
        end if
        if(rdstr(2:2)=='.')then
        goto 1004 
        end if  
        goto 1004
1005    continue
        read(pflun,'(A)',end=1006)rdstr
        if(rdstr(2:3) == '0/')then
        goto 1006
        end if
        if(rdstr(2:2)=='.')then
        goto 1005 
        end if  
        goto 1005        
 1006   continue
        read(pflun,'(A)',end=1007)rdstr
        if(rdstr(2:3) == '0/')then
        goto 1007
        end if
        if(rdstr(2:2)=='.')then
        goto 1006 
        end if  
        goto 1006         
1007    continue
        read(pflun,'(A)',end=1014)rdstr
        if(rdstr(2:3) == '0/')then
        goto 1014
        end if
        if(rdstr(2:2)=='.')then
        goto 1007     
        else 
        write(slun(5),'(A)') rdstr
        ctno(5) = ctno(5) + 1
        end if  
        goto 1007 

        
1014    continue
        call RDPBUS                          
        call RDPG
        SBASE = 100                   
        call RDBV(mbus.volt)
        call RDGEN(mbus.pg,mbus.qg)  
        LUNMAP(pflun) = .FALSE. 
        call CLSF(slun)                  
        if(fterr) return 1
        return
        
        
        
        
        
        
        
        
1111    contains
        subroutine RDPBUS 
                implicit none
                integer stat,si
                logical alive
                character(len=220) str , string,string0
                if(ctno(1) == 0) return
                allocate(pbus(ctno(1)))            
                inquire(unit=slun(1),iostat = stat,exist = alive)        
         if(alive)then
                rewind(slun(1))                   
                do si = 1,ctno(1)
                    read(slun(1),'(A)',iostat = stat) str
                    pbus(si).busname = trim(str(2:9))
                    string0= adjustl(str(13:18)) 
                    read(string0,'(F4.0)') pbus(si).buskv                        
                    read(str(87:104),'(F19.16)')pbus(si).vx
                    string =adjustl(str(114:132))
                    read(string,'(F19.16)')pbus(si).vy
                    pbus(si).owner = trim(str(23:24))
                    pbus(si).zone = str(29:30)
                    pbus(si).vt = trim(str(36:53))
                    pbus(si).vangel = adjustl(str(62:81))
                    pbus(si).num = adjustl(str(138:142))
                    pbus(si).tp = trim(str(147:147))
                end do
         end if
         return
        end subroutine RDPBUS
        
        
     
         
     subroutine RDPG 
                implicit none
                integer stat,si
                logical alive
                character(len=220) str,string0,string1,string2
                if(ctno(5) == 0) return
                allocate(pg(ctno(5)))           
                inquire(unit=slun(5),iostat = stat,exist = alive)        
         if(alive)then
                rewind(slun(5))              
                do si = 1,ctno(5)
                    read(slun(5),'(A)',iostat = stat) str
                    pg(si).genname = trim(str(2:9))
                    string0= adjustl(str(14:19)) 
                    read(string0,'(F4.0)') pg(si).genkv             
                    pg(si).id = str(26:26)
                    string1 = adjustl(str(36:44))
                    string2 = adjustl(str(51:59))
                    read(string1,'(F19.16)')pg(si).pgmw
                    read(string2,'(F19.16)')pg(si).qgmvar
                    pg(si).pgmax = adjustl(str(69:76))
                    pg(si).qgmax = adjustl(str(84:91))
                    pg(si).qgmin = adjustl(str(98:106))
                    pg(si).no = adjustl(str(112:116))
                    pg(si).num = adjustl(str(112:116))
                end do
         end if
         return
     end subroutine RDPG
     
     
     
     subroutine RDBV(BVLT)
                use COMVAL
                use DEFTYPE
                implicit none
                integer si , sj 
                complex BVLT(:) 
                 do si = 1,TMBUS
                   do sj = 1 , TNBUS
                     voltname=trim(mbus(si).bnam(1:8))
                    read(mbus(si).bnam(9:12),'(F4.0)')voltbkv
                     if(voltname == pbus(sj).busname )then
                        diff1=  pbus(sj).buskv - voltbkv
                        if(diff1<=0.001 ) then
                            mbus(si).volt = cmplx(pbus(sj).vx,pbus(sj).vy)
                            EXIT 
                        else 
                            write(*,*)si
                            write(*,*)'error'
                        end if
                     end if  
                   end do  
            end do
                return
            end subroutine RDBV
            
            subroutine RDGEN(PGEN,QGEN)      
                use COMVAL
                use DEFTYPE
                implicit none
                integer si,sj
                real PGEN(:),QGEN(:)
                do si = 1,TMBUS
                    do sj = 1 , TNGEN
                     pgqgname=mbus(si).bnam(1:8)
                     read(mbus(si).bnam(9:12),'(F4.0)') pgqgbkv
                     if(pgqgname == pg(sj).genname )then
                         diff2=pg(sj).genkv-pgqgbkv
                        if(abs(diff2)<=0.001 ) then
                        mbus(si).pg = pg(sj).pgmw
                        mbus(si).qg = pg(sj).qgmvar
                        EXIT  
                        end if
                     end if                   
                    end do       
                end do
                return
        end subroutine RDGEN

     
     end subroutine GTPFL      