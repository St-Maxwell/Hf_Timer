module Hf_Timer
    use iso_fortran_env, only: int64, real64
    implicit none
    private
    public :: hftimer

    type :: hftimer
        private
        integer(kind=int64) :: tick = 0_int64
        integer(kind=int64) :: tock = 0_int64
        real(kind=real64) :: cpu_tick = 0._real64
        real(kind=real64) :: cpu_tock = 0._real64
        integer(kind=int64) :: count_rate = 0_int64
        integer(kind=int64) :: count_max = 0_int64
        logical :: started = .false.
        logical :: stopped = .false.
    contains
        private
        procedure, pass, public :: start => start_timer
        procedure, pass, public :: stop => stop_timer
        procedure, pass, public :: get_cpu_time
        procedure, pass, public :: get_elapsed_time
        procedure, pass :: reset => reset_timer
    end type

contains

    subroutine start_timer(this)
        class(hftimer), intent(inout) :: this

        call this%reset()

        this%started = .true.
        this%stopped = .false.

        call cpu_time(this%cpu_tick)
        call system_clock(count=this%tick)

    end subroutine start_timer


    subroutine stop_timer(this)
        class(hftimer), intent(inout) :: this

        if ((.not. this%started) .or. this%stopped) return

        call cpu_time(this%cpu_tock)
        call system_clock(count=this%tock)

        this%stopped = .true.

    end subroutine stop_timer


    function get_cpu_time(this) result(time)
        class(hftimer), intent(in) :: this
        real(real64) :: time

        if (this%started .and. this%stopped) then
            time = this%cpu_tock - this%cpu_tick
        else
            time = 0._real64
        end if

    end function get_cpu_time


    function get_elapsed_time(this) result(time)
        class(hftimer), intent(in) :: this
        real(real64) :: time

        !! locals
        integer(int64) :: ticks

        if (this%started .and. this%stopped) then
            ticks = this%tock - this%tick
            if (ticks > 0) then
                time = real(ticks, real64) / real(this%count_rate, real64)
            else
                time = real(ticks + this%count_max, real64) / real(this%count_rate, real64)
            end if
        else
            time = 0._real64
        end if

    end function get_elapsed_time


    subroutine reset_timer(this)
        class(hftimer), intent(inout) :: this

        this%tick = 0_int64
        this%tock = 0_int64
        this%cpu_tick = 0._real64
        this%cpu_tock = 0._real64
        this%started = .false.
        this%stopped = .false.

        call system_clock(count_rate=this%count_rate, count_max=this%count_max)

    end subroutine reset_timer

end module Hf_Timer
