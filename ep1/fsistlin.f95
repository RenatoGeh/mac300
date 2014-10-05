function lucol(n, lda, A, p)
        implicit none
        integer :: lucol

        ! Scalar arguments:
        integer :: n
        integer :: lda

        ! Array arguments:
        integer, dimension(n) :: p
        real (kind = selected_real_kind(p=18)), dimension(lda, n) :: A

        !Local scalars:
        integer :: i, j, k

        
        
        

        lucol = 0
        return
end function lucol

function lurow(n, lda, A, p)
        implicit none
        integer :: lurow

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
                if(abs(A(i_max, k) <= EPSILON(A(i_max, k))) then
                        lurow = -1
                        return
                end if
                if(i_max /= k) then
                        do j=1, n
                                temp = A(i_max, j)
                                A(i_max, j) = A(k ,j)
                                A(k, j) = temp
                        end do
                end if
                p(k) = i_max
                do i=k+1, n
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
                do j=1, n
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
                do j=1, n
                        b(i) = b(i) - b(j)*A(i, j)
                end do
        end do

        do i=n, 1, -1
                do j=i+1, n
                        b(i) = b(i) - b(j)*A(i, j)
                end do
                if(A(i, i) == 0)
                        ssrow = -1
                        return
                end if
                b(i) /= A(i, i)
        end do

        ssrow = 0
        return
end function ssrow

program sistlin
        implicit none

        integer :: lucol
        integer :: lurow
        integer :: sscol
        integer :: ssrow

        

end program sistlin

