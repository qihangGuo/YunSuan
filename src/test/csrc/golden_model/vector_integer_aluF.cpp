#include "../include/gm_common.h"
#include <assert.h>
#include <cstring>

VecOutput VGMIntegerALUF::get_expected_output(VecInput input)
{
  VecOutput output;
  output = get_output_vialuF(input);
  return output;
}

VecOutput VGMIntegerALUF::get_output_vialuF(VecInput input)
{
  VecOutput output;
  // get format
  int formatOH = (input.fuOpType >> 5) & 0x3;
  int is_vadd_vsub = ((input.fuOpType & 0x1f) == (VADD_VV & 0x1f) | (input.fuOpType & 0x1f) == (VSUB_VV & 0x1f));
  int is_vext = (input.fuOpType & 0x1f) == (VZEXT_VF2 & 0x1f);
  // get result
  if (is_vadd_vsub) {
    switch (formatOH)
    {
      case 0: output = vialuF_calculation_vvv(input); break; 
      case 1: output = vialuF_calculation_vvw(input); break; 
      case 2: output = vialuF_calculation_wvw(input); break; 
      case 3: output = vialuF_calculation_vvv(input); break; 
      default: printf("VGM IntegerALUV2, bad fuOpType %d\n", input.fuOpType); exit(1); 
    }
  } else if (is_vext) {
    switch (formatOH) {
      case 0: output = vialuF_calculation_22v(input); break; 
      case 1: output = vialuF_calculation_44v(input); break; 
      case 2: output = vialuF_calculation_88v(input); break; 
      default: printf("VGM IntegerALUV2, bad fuOpType %d\n", input.fuOpType); exit(1); 
    }
  } else  {
    switch (formatOH) {
      case 0: output = vialuF_calculation_vvv(input); break; 
      case 1: output = vialuF_calculation_vvm(input); break; 
      case 2: output = vialuF_calculation_mmm(input); break; 
      case 3: output = vialuF_calculation_wvv(input); break; 
      default: printf("VGM IntegerALUV2, bad fuOpType %d\n", input.fuOpType); exit(1); 
    }
  }
  return output;
}

// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_vvv(VecInput input)
{
  assert(input.vinfo.vlmul != 4);
  int lmul = input.vinfo.vlmul;
  int sew = input.sew;
  int num1_ = (VLEN / 8) >> sew; 
  int num1 = (lmul > 4) ? (num1_ >> (8 - lmul)) : num1_; // numeber of element per vreg
  int numMax = num1_ > num1 ? num1_ : num1;

  int mask_start_idx = input.uop_idx * num1;
  auto mask_bit = [&](int idx) -> bool {
    return (input.src4[idx / 64] >> (idx % 64)) & 0x1;
  };

  ElementOutput output_part[numMax];
  __uint64_t src1;
  __uint64_t src2;
  __uint64_t src3;
  __uint64_t result_t;

  for (size_t i = 0; i < numMax; i++) {
    src1 = 0;    src2 = 0;    src3 =0;
    output_part[i].fflags = 0; // DontCare
    output_part[i].vxsat = 0;  // DontCare
    switch (sew)
    {
      case 0: src1 = *((__uint8_t  *)input.src1 + i); src2 = *((__uint8_t  *)input.src2 + i); src3 = *((__uint8_t  *)input.src3 + i); break;
      case 1: src1 = *((__uint16_t *)input.src1 + i); src2 = *((__uint16_t *)input.src2 + i); src3 = *((__uint16_t *)input.src3 + i); break;
      case 2: src1 = *((__uint32_t *)input.src1 + i); src2 = *((__uint32_t *)input.src2 + i); src3 = *((__uint32_t *)input.src3 + i); break;
      case 3: src1 = *((__uint64_t *)input.src1 + i); src2 = *((__uint64_t *)input.src2 + i); src3 = *((__uint64_t *)input.src3 + i); break;
      default: printf("VPU ALU2 Modle, bad sew %d\n", input.sew); exit(1);
    }
    switch (input.fuOpType)
    {
      case VADD_VV: result_t = src2 + src1; break;
      case VSUB_VV: result_t = src2 - src1; break;
      // TODO: add more fuOpType
      default: printf("VPU ALU2 Modle, bad fuOpType %d\n", input.fuOpType); exit(1);
    }
    int elem_idx = mask_start_idx + i;
    if (elem_idx < input.vinfo.vstart && i < num1  ) {
      output_part[i].result = src3;
    }else if(elem_idx < input.vinfo.vl && i < num1 ){
      bool use_result = input.vinfo.vm || mask_bit(elem_idx);
      output_part[i].result = use_result ? result_t
                            : ((input.vinfo.ma          ? ~0ULL
                            :  src3 ));
    }else{
      output_part[i].result = input.vinfo.ta ? ~0ULL : src3;
    }

  }

  VecOutput output;
  int result_shift_len = 8 << sew;
  uint64_t result_element_mask = (result_shift_len == 64) ? ~0ULL : ((1ULL << result_shift_len) - 1);
  memset(output.result, 0, sizeof(output.result));
  memset(output.fflags, 0, sizeof(output.fflags));
  auto out_bytes = reinterpret_cast<uint8_t *>(output.result);
  int elem_bytes = result_shift_len / 8;
  for (int i = 0; i < numMax; i++) {
    uint64_t val = output_part[i].result & result_element_mask;
    for (int b = 0; b < elem_bytes; b++) {
      out_bytes[i * elem_bytes + b] = (val >> (8 * b)) & 0xff;
    }
  }
  output.vxsat = 0;
  return output;
}

// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_vvw(VecInput input)
{
  VecOutput output;
  return output;
}
// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_wvw(VecInput input)
{
  VecOutput output;
  return output;
}
// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_wvv(VecInput input)
{
  VecOutput output;
  return output;
}
// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_22v(VecInput input)
{
  VecOutput output;
  return output;
}
// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_44v(VecInput input)
{
  VecOutput output;
  return output;
}
// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_88v(VecInput input)
{
  VecOutput output;
  return output;
}
// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_vvm(VecInput input)
{
  VecOutput output;
  return output;
}
// TODO:
VecOutput VGMIntegerALUF ::vialuF_calculation_mmm(VecInput input)
{
  VecOutput output;
  return output;
}

ElementOutput VGMIntegerALUF::calculation_e8(ElementInput  input) {ElementOutput rs; return rs;}
ElementOutput VGMIntegerALUF::calculation_e16(ElementInput input) {ElementOutput rs; return rs;}
ElementOutput VGMIntegerALUF::calculation_e32(ElementInput input) {ElementOutput rs; return rs;}
ElementOutput VGMIntegerALUF::calculation_e64(ElementInput input) {ElementOutput rs; return rs;}