function lucol(n, lda, A, p)
        implicit none
        integer :: lucol
        integer, parameter :: E = 0.0000000001

        ! Scalar arguments:
        integer :: n
        integer :: lda

        ! Array arguments:
        integer, dimension(n) :: p
        real (kind = selected_real_kind(p=18)), dimension(lda, n) :: A

        !Local scalars:
        integer :: i, j, k, temp, i_max
        
        do k=1, n
                i_max = k
                do i=k+1, n
                        if(abs(A(i, k)) > A(i_max, k)) then
                                i_max = i     
                        end if
                end do 
                if(i_max /= k) then
                        do j=1, n
                                temp = A(i_max, j)
                                A(i_max, j) = A(k, j)
                                A(k, j) = temp
                        end do
                end if
                p(k) = i_max
        end do

        do k=1, n
                do i=1, k
                        do j=1, i
                                A(i, k) = A(i, k) - A(i, j)*A(j, i)
                        end do
                end do
                do i=k+1, n
                        do j=1, k
                                A(i, k) = A(i, k) - A(i, j)*A(j, k)
                        end do
                        if(abs(A(k, k)) <= E) then
                                lucol = -1
                                return
                        end if
                        A(i, k) = A(i, k)/A(k, k)
                end do
        end do
        
        lucol = 0
        return
end function lucol

function lurow(n, lda, A, p)
        implicit none
        integer :: lurow
        integer, parameter :: E = 0.0000000001

        !Scalar arguments:
        integer :: n
        integer :: lda

        !Array arguments:
        integer, dimension(n) :: p
        real (kind = selected_real_kind(p=18)), dimension(lda, n) :: A

        !Local scalars:
        integer :: i, j, k, temp, i_max

        do k=1, n
                i_max = k
                do i=k+1, n
                        if(abs(A(i, j)) > (A(i_max, k))) then
                                i_max = i
                        end if
                end do
                if(i_max /= k) then
                        do j=1, n
                                temp = A(i_max, j)
                                A(i_max, j) = A(k ,j)
                                A(k, j) = temp
                        end do
                end if
                p(k) = i_max
                do i=k+1, n
                        if(abs(A(k, k)) <= E) then
                                lurow = -1
                                return
                        end if
                        A(i, k) = A(i, k)/A(k, k)
                        do j=k+1, n
                                A(i, j) = A(i, j) - A(i, k)*A(k, j)
                        end do
                end do
        end do

        lurow = 0
        return
end function lurow

function sscol(n, lda, A, p, b)
        implicit none
        integer :: sscol

        integer, parameter :: prec = selected_real_kind(p=18)

        !Scalar arguments:
        integer :: n
        integer :: lda

        !Array arguments:
        integer, dimension(n) :: p
        real (kind = prec), dimension(n) :: b
        real (kind = prec), dimension(lda, n) :: A

        !Local scalars:
        integer :: i, j, temp

        do i=1, n
                temp = b(i)
                b(i) = b(p(i))
                b(p(i)) = temp
        end do

        do i=1, n
                do j=1, i-1
                        b(i) = b(i) - b(j)*A(i, j)
                end do
        end do

        do j=n, 1, -1
                if(A(j, j) == 0) then
                        sscol = -1
                        return
                end if
                b(j) = b(j)/A(j, j)
                do i=1, j-1
                        b(i) = b(i) - b(j)*A(i, j)
                end do
        end do
                        
        sscol = 0
        return
end function sscol

function ssrow(n, lda, A, p, b)
        implicit none
        integer :: ssrow

        integer, parameter :: prec = selected_real_kind(p=18)

        !Scalar arguments:
        integer :: n
        integer :: lda

        !Array arguments:
        integer, dimension(n) :: p
        real (kind = prec), dimension (n) :: b
        real (kind = prec), dimension (lda, n) :: A

        !Local scalars:
        integer :: i, j, temp

        do i=1, n
                temp = b(i)
                b(i) = b(p(i))
                b(p(i)) = temp
        end do        

        do i=1, n
                do j=1, i-1
                        b(i) = b(i) - b(j)*A(i, j)
                end do
        end do

        do i=n, 1, -1
                do j=i+1, n
                        b(i) = b(i) - b(j)*A(i, j)
                end do
                if(A(i, i) == 0) then
                        ssrow = -1
                        return
                end if
                b(i) = b(i)/A(i, i)
        end do

        ssrow = 0
        return
end function ssrow

program sistlin
        implicit none

        ! Functions:
        integer :: lucol
        integer :: lurow
        integer :: sscol
        integer :: ssrow

        ! Constants:
        integer, parameter :: FILENAME_LENGTH = 100
        integer, parameter :: NMAX = 700
        integer, parameter :: prec = selected_real_kind(p=18)

        ! I/O variables:
        integer :: eof, x, y, i, j
        character (len=FILENAME_LENGTH) :: filename
        real :: now, after

        ! Matrix, vectors and scalars:
        integer :: n, sing
        integer, dimension(NMAX) :: p
        real (kind = prec), dimension (NMAX) :: b
        real (kind = prec), dimension (NMAX, NMAX) :: A
        real :: temp

        ! Copies:
        real (kind = prec), dimension (NMAX) :: b_copy
        real (kind = prec), dimension (NMAX, NMAX) :: A_copy

        ! Ax = b
        ! pAx = b

        eof = 0

        do while(eof == 0)
                read(*, *, IOSTAT=eof) filename
                open(10, file=filename)
                
                read(10, *) n
                
                do i=1, n
                        do j=1, n
                                read(10, *) x, y, temp
                                A(y+1, x+1) = temp ! Fortran -> Column major
                        end do
                end do

                do i=1, n
                        read(10, *) x, temp
                        b(x+1) = temp
                end do

                A_copy = A
                b_copy = b

                print *, "Solucionando o sistema: Ax=b"
                print *, "=================="
                print *, "Pelo metodo orientado a linha:"
                
                call cpu_time(now)
                sing = lurow(n, NMAX, A_copy, p)
                call cpu_time(after)

                if(sing < 0) then
                        print *, "A e' provavelmente singular."
                        cycle
                end if
 
                print *, "Tempo de execucao do pivoteamento e decomposicao em LU: ", (after-now)
                
                call cpu_time(now)
                sing = ssrow(n, NMAX, A_copy, p, b_copy)
                call cpu_time(after)

                if(sing < 0) then
                        print *, "A e' provavelmente singular."
                        cycle
                end if
                
                print *, "Tempo de execucao para solucao do sistema por LUP: ", (after-now)

                print *, "=================="

                do i=1, n
                        write(*, "(a, $)") "x_", i, " = "
                        write(*, 1) b_copy(i)
                end do

                print *, "=================="
                print *, "Pelo metodo orientado a coluna:"

                A_copy = A
                b_copy = b

                call cpu_time(now)
                sing = lucol(n, NMAX, A_copy, p)
                call cpu_time(after)

                if(sing < 0) then 
                        print *, "A e' provavelmente singular."
                        cycle
                end if

                print *, "Tempo de execucao do pivoteamento e decomposicao em LU: ", (after-now)
                
                call cpu_time(now)
                sing = sscol(n, NMAX, A_copy, p, b_copy)
                call cpu_time(after)

                if(sing < 0) then
                        print *, "A e' provavelmente singular."
                        cycle
                end if
                
                print *, "Tempo de execucao para solucao do sistema por LUP: ", (after-now)

                print *, "=================="

                do i=1, n
                        print *, "x_", i, " = ", b_copy(i)
                end do

                print *, "=================="

                close(10)

                1 format(1f10.5)
        end do 
        
end program sistlin

