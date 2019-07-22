#ifndef __EXAHYPE_PLOTTERS_ASCII__TIMESERIESREDUCTIONS__
#define __EXAHYPE_PLOTTERS_ASCII__TIMESERIESREDUCTIONS__

namespace exahype {
  namespace plotters {
    namespace ascii {
      class TimeSeriesReductions;
    }
  }
}

/**
 * Non-MPI aware single-core reductor.
 * 
 * TimeSeriesReductions is a C++ class to be used in UserOnTheFlyPostProcessing classes
 * to compute reductions of fields and write them out as ASCII files. The reductions are
 * 
 *   l1: L^1 norm of field (l1 = sum of all cells)
 *   l2: L^2 norm of field (l2 = sqrt(sum of cell[i]^2))
 *   max: Maximum value of field (equal to L^\inf norm)
 *   min: Minimum value of field
 *   avg: Average value of field
 *
 * Moved from CCZ4 to core, ncp2 branch at 01. April 2016.
 * 
 * @author SvenK
 * 
 **/
class exahype::plotters::ascii::TimeSeriesReductions {
  public:
    static constexpr int LEN=9;
    enum index       {                           tidx=0    , time=1, l1=2   , l2=3   , max=4, min=5, avg=6, nelem=7     , nnan=8 };
    const char * const colnames[LEN]  = {"plotindex","time" ,"l1norm","l2norm","max" ,"min" ,"avg" ,"numelements", "numnan" };
    const char * const colformat[LEN] = {"%.0f\t"   ,"%e\t" ,"%e\t"  ,"%e\t"  ,"%e\t","%e\t","%e\t","%.0f\t"     , "%.0f\t" };
  protected:
    double data[LEN];
  public:
    TimeSeriesReductions();

    void startRow(double current_time);
    void addValue(double val, double dx);
    void addValues(double input[LEN]);
    void addValues(TimeSeriesReductions& input);
    void finishRow();
};

#endif /* __EXAHYPE_PLOTTERS_ASCII__TIMESERIESREDUCTIONS__ */
