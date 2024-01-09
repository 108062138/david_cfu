#ifndef __OP_SIMD_RELU_H__
#define __OP_SIMD_RELU_H__

#include "acal_lab/includes/op/Op.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "acal_lab/includes/info/op/ReLUInfo.h"

namespace acal_lab
{
namespace simd
{
class ReLU : public Operator
{
  public:
    ReLU(tensorInfo *opt, tensorInfo *ipt, QauntiType qType) : Operator(opt, ipt, qType)
    {
        execFunction = reinterpret_cast<void (Operator::*)()>(&ReLU::exec);
    }
    void exec() override;
};
} // namespace simd
} // namespace acal_lab

#endif // __OP_SIMD_RELU_H__