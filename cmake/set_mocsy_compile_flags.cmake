macro(configure_fortran_flags)
  if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")

    message(STATUS "Configuring Fortran flags for GNU")

    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcray-pointer -ffree-line-length-none -fno-range-check -Waliasing -Wampersand -Warray-bounds -Wcharacter-truncation -Wconversion -Wline-truncation -Wintrinsics-std -Wno-tabs -Wunderflow -Wunused-parameter -Wintrinsic-shadow -Wno-align-commons -Wsurprising")

    include(CheckFortranCompilerFlag)
    check_fortran_compiler_flag("-fallow-invalid-boz" _boz_flag)
    check_fortran_compiler_flag("-fallow-argument-mismatch" _argmis_flag)

    if(_boz_flag)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-invalid-boz")
    endif()
    if(_argmis_flag)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
    endif()

    set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g")
    set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -W -fbounds-check")

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")

    message(STATUS "Configuring Fortran flags for Intel/IntelLLVM")

    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-alias -safe-cray-ptr -fpe0 -ftz -assume byterecl -i4 -traceback -nowarn -check noarg_temp_created -assume nobuffered_io -convert big_endian -grecord-gcc-switches -fp-model precise -fp-model source -align all")
    set(CMAKE_Fortran_FLAGS_RELEASE "-g3 -O2 -xCORE-AVX2 -debug all -check none")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g3 -O2 -xCORE-AVX2 -debug all -check none")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g3 -O0 -debug all -check -check noarg_temp_created -check nopointer -warn -warn noerrors -ftrapuv")

  else()
    message(WARNING "Fortran compiler with ID '${CMAKE_Fortran_COMPILER_ID}' will be used with default flags")
  endif()

  message(STATUS "CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")
  string(TOUPPER "${CMAKE_BUILD_TYPE}" _build_type)
  if(_build_type STREQUAL "RELEASE")
  set(flags_to_print ${CMAKE_Fortran_FLAGS_RELEASE})
  elseif(_build_type STREQUAL "DEBUG")
  set(flags_to_print ${CMAKE_Fortran_FLAGS_DEBUG})
  elseif(_build_type STREQUAL "RELWITHDEBINFO")
  set(flags_to_print ${CMAKE_Fortran_FLAGS_RELWITHDEBINFO})
  endif()
  message(STATUS "The ${CMAKE_BUILD_TYPE} Fortran flags are: ${flags_to_print}")
endmacro()

