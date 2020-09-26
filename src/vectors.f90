! Created by  on 9/23/2020.

module vectors
implicit none
        type, public :: Vector

            real :: x, y, z

        contains
            procedure :: theta => get_angle
            procedure :: magnitude => get_magnitude
    end type Vector

    contains
    function get_angle(this) result(theta)
        Class(Vector), intent(in) :: this
        real :: theta
        theta = atand(this%y/this%x)
    end function get_angle

    function get_magnitude(this) result(magnitude)
        Class(Vector), intent(in) :: this
        real :: magnitude
        magnitude = sqrt(((this%x)**2) +((this%y)**2))
    end function get_magnitude
end module vectors
