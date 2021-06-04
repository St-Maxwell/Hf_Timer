module test_suite
    use Hf_Timer
    use iso_fortran_env
    use iso_c_binding, only: c_int
    implicit none
    private
    public :: test1, test2, test3, &
              test4, test5

    interface
        subroutine usleep(us) bind(C)
            import c_int
            integer(kind=c_int), value :: us !! microsecond
        end subroutine
    end interface

contains

    subroutine test1()
        type(hftimer) :: clock
        real(kind=real64), dimension(:,:,:), allocatable :: A
        real(kind=real64), dimension(:,:,:), allocatable :: B
        real(kind=real64), dimension(:,:), allocatable :: C
        integer, parameter :: sz = 1000
        integer, parameter :: num = 20
        integer :: i

        allocate(A(sz, sz, num))
        allocate(B(sz, sz, num))
        allocate(C(sz, sz))

        call random_number(A)
        call random_number(B)

        call clock%start()

        do i = 1, num
            C(:,:) = matmul(transpose(A(:,:,i)), B(:,:,i))
        end do

        call clock%stop()

        write(*,"('TEST1: ',g0,'x',g0,' MATRIX MULTIPLICATION FOR ',g0,' TIMES')") sz, sz, num
        write(*,"('CPU TIME: ',f8.6,' s')") clock%get_cpu_time()
        write(*,"('ELAPSED TIME: ',f8.6,' s')") clock%get_elapsed_time()
        write(*,"('')")

    end subroutine test1


    subroutine test2()
        type(hftimer) :: clock
        integer(kind=c_int), parameter :: time = 1500000
        
        call clock%start()

        call usleep(time)

        call clock%stop()

        write(*,"('TEST2: CALL USLEEP(',g0,')')") time
        write(*,"('CPU TIME: ',f8.6,' s')") clock%get_cpu_time()
        write(*,"('ELAPSED TIME: ',f8.6,' s')") clock%get_elapsed_time()
        write(*,"('')")

    end subroutine test2


    subroutine test3()
        type(hftimer) :: clock

        call clock%start()
        write(*,"('TEST3: CALL CLOCK%START() AND NO CALLING STOP')")
        write(*,"('**EXPECT ZERO**')")
        write(*,"('CPU TIME: ',f8.6,' s')") clock%get_cpu_time()
        write(*,"('ELAPSED TIME: ',f8.6,' s')") clock%get_elapsed_time()
        write(*,"('')")

    end subroutine test3


    subroutine test4()
        type(hftimer) :: clock

        call clock%stop()
        write(*,"('TEST4: CALL CLOCK%STOP() DIRECTLY')")
        write(*,"('**EXPECT ZERO**')")
        write(*,"('CPU TIME: ',f8.6,' s')") clock%get_cpu_time()
        write(*,"('ELAPSED TIME: ',f8.6,' s')") clock%get_elapsed_time()
        write(*,"('')")

    end subroutine test4


    subroutine test5()
        type(hftimer) :: clock
        integer(kind=c_int), parameter :: time = 150000

        write(*,"('TEST5: CALL CLOCK%STOP() TWICE')")
        write(*,"('**EXPECT EQUAL ELAPSED TIME**')")

        call clock%start()
        call usleep(time)
        call clock%stop()

        write(*,"('ELAPSED TIME: ',f8.6,' s')") clock%get_elapsed_time()

        call usleep(time)
        call clock%stop()

        write(*,"('ELAPSED TIME: ',f8.6,' s')") clock%get_elapsed_time()

        write(*,"('')")

    end subroutine test5

end module test_suite