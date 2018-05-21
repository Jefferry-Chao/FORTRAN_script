!*************************************************************************
!***File Name: struct_field.f90
!***Author: Zhonghai Zhao
!***Mail: zhaozhonghi@126.com 
!***Created Time: 2018年02月04日 星期日 14时51分46秒
!*************************************************************************

PROGRAM main
    IMPLICIT NONE
    INTEGER, PARAMETER :: num = KIND(1.d0)
    INTEGER, PARAMETER :: nx = 100, ny = 600, nz = 600
    INTEGER, PARAMETER :: x_min = 21, x_max = 30
    INTEGER, PARAMETER :: y_min = 41, y_max = 60
    INTEGER :: i, j, k
    INTEGER :: istat
    INTEGER :: n = 0, total = 0
    REAL(num) :: values, max_value = 0
    REAL(num), DIMENSION(ny, nz) :: array
    !REAL(num), DIMENSION(nx, ny, nz) :: restruct_array

    ! read data
    OPEN (UNIT=1, FILE='by_0030.dat', ACCESS='DIRECT', FORM='UNFORMATTED', STATUS='OLD', &
          RECL=num, IOSTAT=istat)
    fileopen : IF (istat .EQ. 0) THEN
        readdata : DO
            n = n + 1
            READ (1, IOSTAT=istat, REC=n) values
            IF (values .GT. max_value) max_value = values
            IF (istat .NE. 0) THEN
                EXIT
            ELSE
                j = (n - 1)/ny + 1
                k = MOD(n-1, ny) + 1
                array(j, k) = values
            END IF
        END DO readdata
    ELSE
        WRITE (*, *) 'Open file failed!'
    END IF fileopen
    CLOSE (UNIT=1)
    WRITE (*, *) j, k, n, max_value
    ! write data
    OPEN (UNIT=1, FILE='bz.dat', ACCESS='DIRECT', STATUS='REPLACE', FORM='UNFORMATTED', &
         RECL=num)
    DO k = 1, nz, 1
        DO j = 1, nx, 1
            DO i = 1, ny, 1
                total = total + 1
                !IF ((i .GE. x_min) .AND. (i .LE. x_max)) THEN
                IF ((j .GE. y_min) .AND. (j .LE. y_max)) THEN
                    WRITE (1, REC=total) array(i, k)
                ELSE
                    WRITE (1, REC=total) 0._num
                END IF
            END DO
        END DO
    END DO
    CLOSE(UNIT=1)
END PROGRAM main

