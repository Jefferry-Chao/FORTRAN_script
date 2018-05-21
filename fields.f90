!*************************************************************************
!***File Name: binary.f90
!***Author: Zhonghai Zhao
!***Mail: zhaozhonghi@126.com 
!***Created Time: 2018年01月16日 星期二 13时04分33秒
!*************************************************************************

PROGRAM binary
    IMPLICIT NONE

    INTEGER, PARAMETER :: num = KIND(1.d0)
    INTEGER, PARAMETER :: nx = 300, ny = 200, nz = 200
    REAL(num), PARAMETER :: pi = 3.141592653589793238462643383279503_num
    REAL(num), PARAMETER :: n2 = 1.0e26_num
    REAL(num), PARAMETER :: qe = 1.602176565e-19_num
    REAL(num), PARAMETER :: me = 9.10938291e-31_num
    REAL(num), PARAMETER :: c = 2.99792458e8_num
    REAL(num), PARAMETER :: epsilon0 = 8.854187817620389850e-12_num
    REAL(num) :: wpi, di
    REAL(num), DIMENSION(nx, ny, nz) :: by, bz
    REAL(num), DIMENSION(ny) :: y, z
    REAL(num) :: sin_theta, cos_theta, r
    REAL(num) :: dx, b0 = 1000_num
    INTEGER :: i, j, k
    REAL(num) :: max_values = 0
    INTEGER(num) :: total = 1

    ! constants
    wpi = SQRT(n2*qe*qe/epsilon0/me/100.0)
    di = c/wpi
    dx = (5*di - (-5*di))/FLOAT(ny)
    ! array
    DO i = 1, ny, 1
        y(i) = -5*di + i*dx - dx/2.0
        z(i) = y(i)
    END DO
    DO k = 1, nz, 1
        DO j = 1, ny, 1
            r = SQRT(y(j)**2 + z(k)**2)
            IF((r .GT. 0.5*di) .AND. (r .LT. 1.5*di)) THEN
                sin_theta = z(k)/r
                cos_theta = y(j)/r
            ELSE
                sin_theta = 0._num
                cos_theta = 0._num
            END IF
            DO i = 1, nx, 1
                IF((i .GT. 100) .AND. (i .LE. 125)) THEN
                    by(i, j, k) = -b0*sin_theta
                    bz(i, j, k) = b0*cos_theta
                    !IF(by(i, j, k) .GT. max_values) THEN
                    !    max_values = by(i, j, k)
                    !END IF
                ELSE
                    by(i, j, k) = 0._num
                    bz(i, j, k) = 0._num
                END IF
            END DO
        END DO
    END DO
    OPEN(UNIT=1, FILE='bx.dat', ACCESS='DIRECT', STATUS='REPLACE', &
         FORM='UNFORMATTED', RECL=num)
    OPEN(UNIT=2, FILE='bz.dat', ACCESS='DIRECT', STATUS='REPLACE', &
         FORM='UNFORMATTED', RECL=num)
    DO k = 1, nz, 1
        DO j = 1, ny, 1
            DO i = 1, nx, 1
                WRITE(1, REC=total) by(i, j, k)
                !WRITE(1, REC=total) 1._num
                WRITE(2, REC=total) bz(i, j, k)
                total = total + 1
            END DO
        END DO
    END DO
        !WRITE(1, '(F30.28)', REC=i) datas
        !WRITE(1, REC=i) datas
        !WRITE(1, '(A, I3, A)', REC=i) 'This is record ', i, ' .'
    CLOSE(UNIT=1)
    CLOSE(UNIT=2)
    OPEN(UNIT=1, FILE='by3.dat', ACCESS='DIRECT', STATUS='REPLACE', &
         FORM='UNFORMATTED', RECL=num)
    total = 1
    DO i = 1, 300, 1
        DO j = 1, 200, 1
            DO k = 1, 100, 1
        !READ(1, REC=i) values
        !WRITE(*, '(f64.56)') values
        WRITE(1, REC=total) 1.2_num
        total = total + 1
            END DO
        END DO
    END DO
    CLOSE(UNIT=1)
!    INQUIRE(IOLENGTH = j) datas
    WRITE(*, *) max_values
!    INQUIRE(IOLENGTH = k) values
!    WRITE(*, *) k
!    WRITE(*, '(F64.56)') datas
END PROGRAM binary

