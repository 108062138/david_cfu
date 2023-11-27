#ifndef _MY_GEMM_H_
#define _MY_GEMM_H_

#include "../../../simd/mySimdInst.h"
#include "../../includes/myQuanti.h"
#include "../../includes/myTensor.h"
#include "cfu.h"
#include "myGemmCfg.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus

extern "C"
{
#endif
    typedef struct MyGemmInfo
    {
        myTensorInfo weight;
        myTensorInfo bias;
    } myGemmInfo;

    void myGemm(myTensorInfo *, myTensorInfo *, myGemmInfo *, myQuantiInfo *);
    void myGemmScalar(myTensorInfo *, myTensorInfo *, myGemmInfo *, myQuantiInfo *);

#ifdef __cplusplus
}
#endif

#endif
