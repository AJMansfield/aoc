program scard

#ifdef f2003
  use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                            stdout=>output_unit, &
                                            stderr=>error_unit
#else
#define stdin  5
#define stdout 6
#define stderr 0
#endif

  implicit none

  write(stderr,*) "stderr output!"
  
end program scard