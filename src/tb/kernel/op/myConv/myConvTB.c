#include "myConvTB.h"
bool myConv_Kernel()
{
    /******************      8-2.1-4 : Quantization      ******************/
    // myQuantiInfo quantiInfo = {.scaling_factor = 0, .zero_point = 0};
    /**********************************************************************/
    int correct = 0;
    printf("[ TEST ] `CONV`  :               %4s\n", correct == 1 ? "Pass" : "Fail");

    // TODO
    return correct == 1 ? true : false;
}