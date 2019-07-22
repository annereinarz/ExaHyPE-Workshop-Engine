! Exemplary stubs to connect to an ExaHyPE plotter.

subroutine ExaHyPETecplotWriter_init(filename,solverType,basisSize,writtenUnknowns)
  use iso_c_binding
  implicit none
  CHARACTER(LEN=1000), INTENT(IN) :: filename
  INTEGER, INTENT(IN) :: solverType, basisSize, writtenUnknowns
  !
  
  PRINT *, "ExaHyPETecplotWriter_init: Writing to >",filename,"<"
  PRINT *, "Writing for basisSize=", basisSize, ", writtenUnknowns=", writtenUnknowns
  
  SELECT CASE (solverType)
   CASE (1)
      PRINT *, "For an ADER-DG solver"
   CASE (2)
      PRINT *, "For an FV solver"
   CASE (3)
      PRINT *, "For an Limiting ADER-DG solver"
   CASE DEFAULT
      PRINT *, "Something weird, please abort."
  END SELECT

end subroutine

