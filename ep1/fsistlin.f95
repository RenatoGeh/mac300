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
        integer :: i, j, k



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

