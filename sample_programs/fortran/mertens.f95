! implementation of the mertens function in fortran 

MODULE trashbin
    ! module to hold an interface so that pure functions can use each other
    implicit none

    INTERFACE 
        PURE LOGICAL FUNCTION isPrime(x)
            INTEGER, INTENT(IN) :: x
        END FUNCTION isPrime

        PURE LOGICAL FUNCTION isSquareFree(x)
            INTEGER, INTENT(IN) :: x
        END FUNCTION isSquareFree

        PURE INTEGER FUNCTION bigOmega(x, count)
          INTEGER, INTENT(IN) :: x, count
        END FUNCTION bigOmega

        PURE INTEGER FUNCTION mobius(x)
          INTEGER, INTENT(IN) :: x
        END FUNCTION mobius
    END INTERFACE 
END MODULE trashbin

PROGRAM mertens
    implicit none

    INTEGER, EXTERNAL :: mertensFunction, mobius
    INTEGER :: i, current
    INTEGER, PARAMETER :: high = 40, low = 0

    current = 0

    WRITE (*,*) "mertens function values up to", high, ":"
    DO i = low, high
        WRITE(*,*) "M(", i, ") ==", mertensFunction(i)
        ! WRITE(*,*) mertensFunction(i)
        ! current = current + mobius(i)
        ! WRITE(*,*) "M(", i, ") ==", current
    END DO

    STOP
END PROGRAM mertens 

PURE LOGICAL FUNCTION isSquareFree(x) 
    ! returns true if x is a square free integer
    implicit none

    INTEGER, INTENT(IN) :: x
    INTEGER :: i

    isSquareFree = .TRUE.
    IF (x < 1) THEN
        isSquareFree = .FALSE.
        RETURN
    END IF 
    DO i = 2, ceiling(sqrt(real(x)))
        IF (modulo(x,i**2) == 0) THEN
            ! i**2 divides x, so x isn't squarefree 
            isSquareFree = .FALSE. 
            RETURN 
        END IF
    END DO
    RETURN
END FUNCTION isSquareFree

PURE LOGICAL FUNCTION isPrime(x) 
    ! returns true if x is prime
    implicit none

    INTEGER, INTENT(IN) :: x
    INTEGER :: i

    IF (x < 2) THEN
        isPrime = .FALSE.
        RETURN 
    END IF
    DO i = 2, floor(sqrt(real(x)))
        IF (modulo(x, i) == 0) THEN
            isPrime = .FALSE.
            RETURN
        END IF
    END DO
    isPrime = .TRUE.
    RETURN 
END FUNCTION isPrime

PURE RECURSIVE INTEGER FUNCTION bigOmega(x, count) RESULT(res)
    ! returns the number of prime factors, not what they are (too complicated)
    USE trashbin, only: isPrime
    implicit none

    INTEGER, INTENT(IN) :: x, count
    INTEGER :: i

    res = count
    IF (isPrime(x)) THEN
        RETURN 
    END IF
    DO i = 2, x
        IF (isPrime(i) .AND. modulo(x, i) == 0) THEN
            res = bigOmega(int(x/i), count+1) 
            RETURN
        END IF
    END DO
    RETURN
END FUNCTION bigOmega

PURE INTEGER FUNCTION mobius(x) 
    USE trashbin, only: bigOmega, isSquareFree 
    implicit none

    INTEGER, INTENT(IN) :: x
    ! INTEGER, EXTERNAL :: bigOmega
    ! LOGICAL, EXTERNAL :: isSquareFree

    mobius = 0
    IF (x == 1) THEN
        mobius = 1
    ELSE
        IF (isSquareFree(x)) THEN
            IF (modulo(bigOmega(x, 1), 2) == 0) THEN
                mobius = 1
            ELSE
                mobius = -1
            END IF
        END IF
    END IF
    RETURN
END FUNCTION mobius

PURE INTEGER FUNCTION mertensFunction(x) 
    USE trashbin, only: mobius
    implicit none

    INTEGER :: res, i 
    INTEGER, INTENT(IN) :: x
    ! INTEGER, EXTERNAL :: mobius

    mertensFunction = 0
    DO i = 1, x
        mertensFunction = mertensFunction + mobius(i)
    END DO
    RETURN
END FUNCTION mertensFunction

