! Created by  on 9/26/2020.

module vector_algebra
    use vectors
    implicit none
    type(vector) :: a, b, c


contains
    type(vector) function vector_addition(this, that) result (c)
        class(vector), intent(in) :: this, that
        c = vector(this%x + that%x, this%y + that%y, this%z + that%z)
    end function vector_addition

    real function dot_product(this, that) result (dot_prod)
        class(vector), intent(in) :: this, that
        dot_prod = (this%x * that%x) + (this%y + that%y) + (this%z + that%z)
    end function dot_product

    type(vector) function cross_product(this, that) result(norm)
        class(vector), intent(in) :: this, that

        norm = vector((this%y * that%z) - (this%z * that%y), (this%z * that%x) - (this%x * that%z), &
                (this%x * that%y) - (this%y * that%x))
    end function cross_product

end module vector_algebra

program vectest
    use vector_algebra
    implicit none
    a = vector(4.0, 2.0, -5.0)
    b = vector(2.0, -3.0, 7.0)
    c = vector_addition(a, b)
    print *, c%x, c%y, c%z
    print *, c%magnitude()
    print *, dot_product(a, b)
    print *, cross_product(a, b)

end program vectest