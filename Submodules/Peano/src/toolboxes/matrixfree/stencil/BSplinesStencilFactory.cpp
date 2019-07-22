#include "matrixfree/stencil/BSplinesStencilFactory.h"


tarch::la::Vector<3,double> matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP1() {
  tarch::la::Vector<3,double> result;
  result = 0.16666666666666669, 0.6666666666666666, 0.16666666666666666;
  return result;
}


tarch::la::Vector<5,double> matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP2() {
  tarch::la::Vector<5,double> result;
  result = 0.008333333316235017, 0.21666666666200657, 0.5499999997948198, 0.21666666666200657, 0.00833333331623502;
  return result;
}


tarch::la::Vector<7,double> matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP3() {
  tarch::la::Vector<7,double> result;
  result = 0.00019841269841269839, 0.023809523809523808, 0.2363095238095238, 0.4793650793650793, 0.2363095238095238, 0.023809523809523808, 0.00019841269841269836;
  return result;
}



tarch::la::Vector<9,double> matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP4() {
  tarch::la::Vector<9,double> result;
  result = 2.7557332248134565e-06, 0.0013833774195073723, 0.04025573206367054, 0.24314925047638827, 0.43041776925721903, 0.24314925047638827, 0.04025573206367054, 0.0013833774195073723, 2.7557332248134565e-06;
  return result;
}


tarch::la::Vector<11,double> matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP5() {
  tarch::la::Vector<11,double> result;
  result = 2.5052304450569587e-08, 5.1006092636848604e-05, 0.0038238786674069103, 0.05520202019999654, 0.243960287398345, 0.3939255651717376, 0.243960287398345, 0.05520202019999654, 0.0038238786674069107, 5.100609263684861e-05, 2.505230445056959e-08;
  return result;
}


tarch::la::Vector<13,double> matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP6() {
  tarch::la::Vector<13,double> result;
  result = 2.4094144075879413e-10, 1.3133086711151696e-06, 0.00023762984838819455, 0.007312236697946331, 0.06797496727064363, 0.24178841798351722, 0.3653708695288048, 0.24178841798351722, 0.06797496727064362, 0.00731223669794633, 0.00023762984838819455, 1.3133086711151694e-06, 2.409414407587941e-10;
  return result;
}


tarch::la::Vector<15,double> matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP7() {
  tarch::la::Vector<15,double> result;
  result = 1.0947193580436178e-12, 2.504599051941938e-08, 1.0571999465794245e-05, 0.0006485490055258489, 0.011502274482876591, 0.07859525386013605, 0.2381231949093699, 0.3422402613437703, 0.23812319490936987, 0.07859525386013605, 0.01150227448287659, 0.0006485490055258489, 1.0571999465794241e-05, 2.5045990519419365e-08, 1.0947193580436178e-12;
  return result;
}


tarch::la::Vector<17,double> matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP8() {
  tarch::la::Vector<17,double> result;
  result = 2.3373653415865523e-15, 3.24501468745525e-10, 3.5643941825441703e-07, 4.182154971459e-05, 0.0013308125720157594, 0.01607392199095226, 0.08731164076995032, 0.23373674923044221, 0.32300939415726554, 0.2337367492304422, 0.08731164076995032, 0.016073921990952256, 0.0013308125720157599, 4.182154971459001e-05, 3.5643941825441703e-07, 3.2450146874552373e-10, 2.3373653415865784e-15;
  return result;
}


tarch::la::Vector<3,double> matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP1() {
  tarch::la::Vector<3,double> result;
  result = -1.0, 2.0, -1.0;
  return result;
}


tarch::la::Vector<5,double> matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP2() {
  tarch::la::Vector<5,double> result;
  result = -0.16666666666666669, -0.3333333333333332, 1.0000000000000004, -0.3333333333333334, -0.16666666666666669;
  return result;
}


tarch::la::Vector<7,double> matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP3() {
  tarch::la::Vector<7,double> result;
  result = -0.033333333333333326, -0.7999999999999999, -0.4999999999999998, 2.6666666666666665, -0.4999999999999999, -0.7999999999999999, -0.033333333333333326;
  return result;
}


tarch::la::Vector<9,double> matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP4() {
  tarch::la::Vector<9,double> result;
  result = -0.00178571426809686, -0.2107142857070971, -1.7000000003129403, -0.2749999998371173, 4.374999999986392, -0.27499999983711726, -1.7000000003129403, -0.2107142857070971, -0.0017857142680968597;
  return result;
}


tarch::la::Vector<11,double> matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP5() {
  tarch::la::Vector<11,double> result;
  result = -4.40917125194486e-05, -0.022045855361508403, -0.5998677251325906, -2.624338625437523, 0.24999999970240092, 5.992592592315672, 0.24999999970240078, -2.6243386254375225, -0.5998677251325906, -0.02204585536150841, -4.409171251944858e-05;
  return result;
}


tarch::la::Vector<13,double> matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP6() {
  tarch::la::Vector<13,double> result;
  result = -6.263026951071867e-07, -0.0012738997050349508, -0.09304728786496695, -1.1901317241411467, -3.434503141404895, 0.9698247354247478, 7.498263889271339, 0.9698247354247472, -3.434503141404895, -1.1901317241411464, -0.09304728786496695, -0.0012738997050349506, -6.263026951071873e-07;
  return result;
}


tarch::la::Vector<15,double> matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP7() {
  tarch::la::Vector<15,double> result;
  result = -8.0167838891586e-09, -4.726755365152731e-05, -0.008460122053872053, -0.24617845117845116, -1.9291724536961081, -4.073425926009033, 1.8083159721446325, 8.897936507923742, 1.808315972144633, -4.073425926009033, -1.9291724536961086, -0.2461784511784512, -0.008460122053872053, -4.7267553651527287e-05, -8.0167838891586e-09;
  return result;
}


tarch::la::Vector<17,double> matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP8() {
  tarch::la::Vector<17,double> result;
  result = -3.3148981962750917e-11, -1.2271786081213804e-06, -0.0005155737360477914, -0.030744072146573372, -0.5005716754996019, -2.7557234410238713, -4.529313121562418, 2.715132855368815, 10.203472511572237, 2.715132855368814, -4.529313121562417, -2.7557234410238713, -0.5005716754996019, -0.030744072146573376, -0.0005155737360477913, -1.2271786081213804e-06, -3.3148981962751246e-11;
  return result;
}


tarch::la::Vector<3,double>  matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP1(int elementOfSupport) {
  assertion1(elementOfSupport<2 && elementOfSupport>=0, elementOfSupport);
  tarch::la::Vector<3,double> result;
  switch (elementOfSupport) {
    case 0:
      result = 0.16666666666666669, 0.33333333333333337, 0.0;
      break;
    case 1:
      result = 0.0, 0.3333333333333333, 0.16666666666666666;
      break;
    default:
      assertion(false);
  }
  return result;
}


tarch::la::Vector<5,double>  matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP2(int elementOfSupport) {
  assertion1(elementOfSupport<4 && elementOfSupport>=0, elementOfSupport);
  tarch::la::Vector<5,double> result;
  switch (elementOfSupport) {
    case 0:
      result = 0.0041666666666666675, 0.015104166666666665, 0.0015625000000000005, 0.0, 0.0;
      break;
    case 1:
      result = 0.004166666666666667, 0.18645833333333334, 0.2734375, 0.015104166666666667, 0.0;
      break;
    case 2:
      result = 0.0, 0.015104166666666667, 0.2734375, 0.18645833333333334, 0.004166666666666667;
      break;
    case 3:
      result = 0.0, 0.0, 0.0015624999999999997, 0.015104166666666669, 0.004166666666666667;
      break;
    default:
      assertion(false);
  }
  return result;
}


tarch::la::Vector<7,double>  matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP3(int elementOfSupport) {
  assertion1(elementOfSupport<4 && elementOfSupport>=0, elementOfSupport);
  tarch::la::Vector<7,double> result;
  switch (elementOfSupport) {
    case 0:
      result = 0.00019841269841269839, 0.011904761904761904, 0.025595238095238098, 0.00396825396825397, 0.0, 0.0, 0.0;
      break;
    case 1:
      result = 0.0, 0.011904761904761904, 0.18511904761904763, 0.2357142857142857, 0.025595238095238098, 0.0, 0.0;
      break;
    case 2:
      result = 0.0, 0.0, 0.025595238095238095, 0.2357142857142857, 0.18511904761904763, 0.011904761904761904, 0.0;
      break;
    case 3:
      result = 0.0, 0.0, 0.0, 0.003968253968253968, 0.025595238095238095, 0.011904761904761904, 0.00019841269841269836;
      break;
    default:
      assertion(false);
  }
  return result;
}


tarch::la::Vector<9,double>  matrixfree::stencil::BSplinesStencilFactory::get1DMassStencilP4(int elementOfSupport) {
  assertion1(elementOfSupport<6 && elementOfSupport>=0, elementOfSupport);
  tarch::la::Vector<9,double> result;
  switch (elementOfSupport) {
    case 0: result = 1.3778708896511514e-06, 6.21439227325405e-05, 0.00015407338739753, 4.2444725423179536e-05, 3.76760223765432e-07, 0.0, 0.0, 0.0, 0.0;
    break;
    case 1: result = 1.3778708896511516e-06, 0.0012590896095127865, 0.01997379253328495, 0.033460516845848644, 0.006981528464325303, 4.244472542317955e-05, 0.0, 0.0, 0.0;
    break;
    case 2: result = 0.0, 6.214392273254051e-05, 0.019973792533284948, 0.17614332733961638, 0.20822697930445327, 0.033460516845848644, 0.00015407338739753003, 0.0, 0.0;
    break;
    case 3: result = 0.0, 0.0, 0.00015407338739753, 0.03346051684584864, 0.20822697930445327, 0.17614332733961638, 0.01997379253328495, 6.214392273254049e-05, 0.0;
    break;
    case 4: result = 0.0, 0.0, 0.0, 4.244472542317952e-05, 0.006981528464325303, 0.03346051684584864, 0.01997379253328495, 0.0012590896095127865, 1.3778708896511523e-06;
    break;
    case 5: result = 0.0, 0.0, 0.0, 0.0, 3.767602237654318e-07, 4.244472542317953e-05, 0.00015407338739753006, 6.214392273254051e-05, 1.3778708896511523e-06;
    break;
    default:
      assertion(false);
  }
  return result;
}


tarch::la::Vector<3,double>  matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP1(int elementOfSupport) {
  assertion1(elementOfSupport<2 && elementOfSupport>=0, elementOfSupport);
  tarch::la::Vector<3,double> result;
  switch (elementOfSupport) {
    case 0: result = -1.0, 1.0, -0.0;
    break;
    case 1: result = 0.0, 1.0, -1.0;
    break;
    default:
      assertion(false);
  }
  return result;
}


tarch::la::Vector<5,double>  matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP2(int elementOfSupport) {
  assertion1(elementOfSupport<4 && elementOfSupport>=0, elementOfSupport);
  tarch::la::Vector<5,double> result;
  switch (elementOfSupport) {
    case 0: result = -0.08333333333333333, 0.04166666666666667, 0.04166666666666667, 0.0, 0.0;
    break;
    case 1: result = -0.08333333333333333, -0.41666666666666674, 0.45833333333333337, 0.041666666666666664, 0.0;
    break;
    case 2: result = -0.0, 0.041666666666666664, 0.45833333333333326, -0.41666666666666674, -0.08333333333333333;
    break;
    case 3: result = 0.0, 0.0, 0.041666666666666664, 0.04166666666666666, -0.08333333333333333;
    break;
    default:
      assertion(false);
  }
  return result;
}


tarch::la::Vector<7,double>  matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP3(int elementOfSupport) {
  assertion1(elementOfSupport<4 && elementOfSupport>=0, elementOfSupport);
  tarch::la::Vector<7,double> result; // @todo Shift in script is wrong. This version is manually fixed. In the script, I just forgot this h/2 shift of the interval borders
  switch (elementOfSupport) {
    case 0:
      result = -0.033333333333333326, -0.39999999999999997, 0.2333333333333333, 0.20000000000000004, 0.0, 0.0, 0.0;
      break;
    case 1:
      result = 0.0, -0.39999999999999997, -0.9666666666666665, 1.133333333333333, 0.2333333333333333, 0.0, 0.0;
      break;
    case 2:
      result = -0.0, -0.0, 0.2333333333333334, 1.133333333333333, -0.9666666666666666, -0.39999999999999997, -0.0;
      break;
    case 34:
      result = -0.0, -0.0, -0.0, 0.19999999999999998, 0.2333333333333334, -0.39999999999999997, -0.033333333333333326;
      break;
    default:
      assertion(false);
  }
  return result;
}


tarch::la::Vector<9,double>  matrixfree::stencil::BSplinesStencilFactory::get1DLaplaceStencilP4(int elementOfSupport) {
  assertion1(elementOfSupport<6 && elementOfSupport>=0, elementOfSupport);
  tarch::la::Vector<9,double> result;
  switch (elementOfSupport) {
    case 0:
      result = -0.0008928571428571428, -0.011662946428571429, 0.002845982142857143, 0.009430803571428574, 0.0002790178571428571, 0.0, 0.0, 0.0, 0.0;
      break;
    case 1:
      result = -0.0008928571428571428, -0.18738839285714284, -0.8528459821428571, 0.523046875, 0.5086495535714285, 0.009430803571428573, 0.0, 0.0, 0.0;
      break;
    case 2:
      result = 0.0, -0.011662946428571429, -0.8528459821428571, -1.3399553571428573, 1.6785714285714288, 0.523046875, 0.002845982142857143, 0.0, 0.0;
      break;
    case 3:
      result = -0.0, -0.0, 0.002845982142857143, 0.523046875, 1.6785714285714288, -1.3399553571428573, -0.8528459821428571, -0.011662946428571429, -0.0;
      break;
    case 4:
      result = -0.0, -0.0, -0.0, 0.009430803571428574, 0.5086495535714286, 0.523046875, -0.8528459821428571, -0.18738839285714284, -0.000892857142857143;
      break;
    case 5: result = 0.0, 0.0, 0.0, 0.0, 0.000279017857142857, 0.009430803571428581, 0.0028459821428571414, -0.011662946428571436, -0.000892857142857143;
      break;
    default:
      assertion(false);
  }
  return result;
}


tarch::la::Vector<FIVE_POWER_D,double> matrixfree::stencil::BSplinesStencilFactory::getLaplacianStencilP2( const tarch::la::Vector<DIMENSIONS,double>& scaling ) {
  tarch::la::Vector<FIVE_POWER_D,double> result;

  #if defined(Dim2)
  result =  scaling(0) *
            stencilProduct(
              get1DLaplaceStencilP2(),
              get1DMassStencilP2()
            )
            + scaling(1) *
            stencilProduct(
              get1DMassStencilP2(),
              get1DLaplaceStencilP2()
            );
  #elif defined(Dim3)
  result =  scaling(0) *
            stencilProduct(
              get1DLaplaceStencilP2(),
              get1DMassStencilP2(),
              get1DMassStencilP2()
            )
            + scaling(1) *
            stencilProduct(
              get1DMassStencilP2(),
              get1DLaplaceStencilP2(),
              get1DMassStencilP2()
            )
            + scaling(2) *
            stencilProduct(
              get1DMassStencilP2(),
              get1DMassStencilP2(),
              get1DLaplaceStencilP2()
            );
  #else
  assertionMsg( false, "dimension not supported" );
  #endif

  return result;
}


tarch::la::Vector<SEVEN_POWER_D,double> matrixfree::stencil::BSplinesStencilFactory::getLaplacianStencilP3( const tarch::la::Vector<DIMENSIONS,double>& scaling ) {
  tarch::la::Vector<SEVEN_POWER_D,double> result;

  #if defined(Dim2)
  result =  scaling(0) *
            stencilProduct(
              get1DLaplaceStencilP3(),
              get1DMassStencilP3()
            )
            + scaling(1) *
            stencilProduct(
              get1DMassStencilP3(),
              get1DLaplaceStencilP3()
            );
  #elif defined(Dim3)
  result =  scaling(0) *
            stencilProduct(
              get1DLaplaceStencilP3(),
              get1DMassStencilP3(),
              get1DMassStencilP3()
            )
            + scaling(1) *
            stencilProduct(
              get1DMassStencilP3(),
              get1DLaplaceStencilP3(),
              get1DMassStencilP3()
            )
            + scaling(2) *
            stencilProduct(
              get1DMassStencilP3(),
              get1DMassStencilP3(),
              get1DLaplaceStencilP3()
            );
  #else
  assertionMsg( false, "dimension not supported" );
  #endif

  return result;
}


tarch::la::Vector<NINE_POWER_D,double> matrixfree::stencil::BSplinesStencilFactory::getLaplacianStencilP4( const tarch::la::Vector<DIMENSIONS,double>& scaling ) {
  tarch::la::Vector<NINE_POWER_D,double> result;

  #if defined(Dim2)
  result =  scaling(0) *
            stencilProduct(
              get1DLaplaceStencilP4(),
              get1DMassStencilP4()
            )
            + scaling(1) *
            stencilProduct(
              get1DMassStencilP4(),
              get1DLaplaceStencilP4()
            );
  #elif defined(Dim3)
  result =  scaling(0) *
            stencilProduct(
              get1DLaplaceStencilP4(),
              get1DMassStencilP4(),
              get1DMassStencilP4()
            )
            + scaling(1) *
            stencilProduct(
              get1DMassStencilP4(),
              get1DLaplaceStencilP4(),
              get1DMassStencilP4()
            )
            + scaling(2) *
            stencilProduct(
              get1DMassStencilP4(),
              get1DMassStencilP4(),
              get1DLaplaceStencilP4()
            );
  #else
  assertionMsg( false, "dimension not supported" );
  #endif

  return result;
}


tarch::la::Vector<THREE_POWER_D,double> matrixfree::stencil::BSplinesStencilFactory::getLaplacianStencilP1( const tarch::la::Vector<DIMENSIONS,double>& scaling ) {
  tarch::la::Vector<THREE_POWER_D,double> result;

  #if defined(Dim2)
  result =  scaling(0) *
            stencilProduct(
              get1DLaplaceStencilP1(),
              get1DMassStencilP1()
            )
            + scaling(1) *
            stencilProduct(
              get1DMassStencilP1(),
              get1DLaplaceStencilP1()
            );
  #elif defined(Dim3)
  result =  scaling(0) *
            stencilProduct(
              get1DLaplaceStencilP1(),
              get1DMassStencilP1(),
              get1DMassStencilP1()
            )
            + scaling(1) *
            stencilProduct(
              get1DMassStencilP1(),
              get1DLaplaceStencilP1(),
              get1DMassStencilP1()
            )
            + scaling(2) *
            stencilProduct(
              get1DMassStencilP1(),
              get1DMassStencilP1(),
              get1DLaplaceStencilP1()
            );
  #else
  assertionMsg( false, "dimension not supported" );
  #endif

  return result;
}


tarch::la::Vector<THREE_POWER_D,double>  matrixfree::stencil::BSplinesStencilFactory::getMassStencilP1() {
  tarch::la::Vector<THREE_POWER_D,double> result;

  #if defined(Dim2)
  result =  stencilProduct(
              get1DMassStencilP1(),
              get1DMassStencilP1()
            );
  #elif defined(Dim3)
  result =  stencilProduct(
              get1DMassStencilP1(),
              get1DMassStencilP1(),
              get1DMassStencilP1()
            );
  #else
    assertionMsg( false, "dimension not supported" );
  #endif

  return result;
}


tarch::la::Vector<FIVE_POWER_D,double>  matrixfree::stencil::BSplinesStencilFactory::getMassStencilP2() {
  tarch::la::Vector<FIVE_POWER_D,double> result;

  #if defined(Dim2)
  result =  stencilProduct(
              get1DMassStencilP2(),
              get1DMassStencilP2()
            );
  #elif defined(Dim3)
  result =  stencilProduct(
              get1DMassStencilP2(),
              get1DMassStencilP2(),
              get1DMassStencilP2()
            );
  #else
    assertionMsg( false, "dimension not supported" );
  #endif

  return result;
}


tarch::la::Vector<SEVEN_POWER_D,double>  matrixfree::stencil::BSplinesStencilFactory::getMassStencilP3() {
  tarch::la::Vector<SEVEN_POWER_D,double> result;

  #if defined(Dim2)
  result =  stencilProduct(
              get1DMassStencilP3(),
              get1DMassStencilP3()
            );
  #elif defined(Dim3)
  result =  stencilProduct(
              get1DMassStencilP3(),
              get1DMassStencilP3(),
              get1DMassStencilP3()
            );
  #else
    assertionMsg( false, "dimension not supported" );
  #endif

  return result;
}


tarch::la::Vector<NINE_POWER_D,double>  matrixfree::stencil::BSplinesStencilFactory::getMassStencilP4() {
  tarch::la::Vector<NINE_POWER_D,double> result;

  #if defined(Dim2)
  result =  stencilProduct(
              get1DMassStencilP4(),
              get1DMassStencilP4()
            );
  #elif defined(Dim3)
  result =  stencilProduct(
              get1DMassStencilP4(),
              get1DMassStencilP4(),
              get1DMassStencilP4()
            );
  #else
    assertionMsg( false, "dimension not supported" );
  #endif

  return result;
}
