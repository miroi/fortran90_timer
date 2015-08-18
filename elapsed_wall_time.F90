
module fortran_timer

integer, private :: time_array_start(8), time_array_end(8)
logical, save :: time_measurement_started=.false., time_measurement_ended=.true.
real*8, private :: elapsed_time

public start_time_measurement, get_elapsed_time

real*8 :: start_time, finish_time

logical, private :: year_changed, month_changed, day_changed

contains

  subroutine start_time_measurement
    if (.not.time_measurement_ended) stop "previous time measurement did not finish !"
    call date_and_time(values=time_array_start)
    time_measurement_started=.true.
  end subroutine start_time_measurement

  function get_elapsed_time() result(spent_time)
    real*8 :: elapsed_time
    if (.not.time_measurement_started) stop "time measurement did not start yet !"

    call date_and_time(values=time_array_end)

    year_changed  = ( time_array_end(1) /= time_array_start(1) )
    month_changed = ( time_array_end(2) /= time_array_start(2) )
    day_changed   = ( time_array_end(3) /= time_array_start(3) )

    if (.not. year_changed .and. .not. month_changed .and. .not.day_changed) then
      elapsed_time=(time_array_end(5)-time_array_start(5))*3600 + & 
                   (time_array_end(6)-time_array_start(6))*60   + & 
                   (time_array_end(7)-time_array_start(7))      + & 
                   (time_array_end(8)-time_array_start(8))*0.001
    else if (.not. year_changed .and. .not. month_changed .and. day_changed) then
      ! day changed (month, year the same)
      elapsed_time=(time_array_end(3)-time_array_start(3))*3600*24 + & 
                   (time_array_end(5)-time_array_start(5))*3600    + & 
                   (time_array_end(6)-time_array_start(6))*60      + & 
                   (time_array_end(7)-time_array_start(7))         + & 
                   (time_array_end(8)-time_array_start(8))*0.001
      
    else if (.not. year_changed .and. month_changed .and. day_changed) then
     ! day, month changed ( year the same)
      stop "day, month change not implemented !"
    else if (year_changed .and. month_changed .and. day_changed) then
     ! year, day, month changed 
      stop "day, month, year change not implemented !"
    endif

    spent_time=elapsed_time ! return value

    time_measurement_ended=.true.
    time_measurement_started=.false.
  end function get_elapsed_time

end module fortran_timer

program elapse_wall_time
  use fortran_timer
  integer :: n1=3,n2=2

  call start_time_measurement
  print *,' first batch of computations for ',n1,' seconds'
  call sleep(n1)
  print *,'wall time spent :', get_elapsed_time(),' seconds'

  call start_time_measurement
  print *,' second batch of computations for ',n2,' seconds'
  call sleep(n2)
  print *,'wall time spent :', get_elapsed_time(),' seconds'


end program 
