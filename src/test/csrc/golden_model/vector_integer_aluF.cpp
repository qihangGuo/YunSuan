#include "../include/gm_common.h"
#include "../include/vfpu_functions.h"
#include <assert.h>

#define GET_BIT(x, bit) ((x & (1ULL << bit)) >> bit)

ElementInput VGMIntegerALUF::select_vialuf_element(VecInput input, int idx, uint64_t mask, bool widenVd, bool widenVs2, bool isDstMask) {
  int sew = input.sew;
  int number = (VLEN / 8) >> sew;

  bool isVf2 = ((input.fuOpType == VZEXT_VF2) || (input.fuOpType == VSEXT_VF2));
  bool isVf4 = ((input.fuOpType == VZEXT_VF4) || (input.fuOpType == VSEXT_VF4));
  bool isVf8 = ((input.fuOpType == VZEXT_VF8) || (input.fuOpType == VSEXT_VF8));

  if (idx > number) { printf("Bad idx %d > %d at sew %d\n", idx, number, sew); exit(1); }
  
  ElementInput element;
  VecInputE8  *input8  = (VecInputE8  *) &input;
  VecInputE16 *input16 = (VecInputE16 *) &input;
  VecInputE32 *input32 = (VecInputE32 *) &input;
  VecInput    *input64 = (VecInput    *) &input;

  bool onlyWidenVs2 = !widenVd & widenVs2;
  bool onlyWidenVd = widenVd & !widenVs2;

  switch (sew) {
    case 0:
      element.src1 = (uint64_t)input8->src1[idx];
      element.src2 = onlyWidenVs2 ? (uint64_t)input16->src2[idx] : (uint64_t)input8->src2[idx];
      element.src3 = (uint64_t)input8->src3[idx];
      break;
    case 1:
      element.src1 = widenVd ? (uint64_t)input8->src1[idx] : (uint64_t)input16->src1[idx];
      element.src2 = onlyWidenVs2 ? (uint64_t)input32->src2[idx] : (onlyWidenVd || isVf2) ? (uint64_t)input8->src2[idx] : (uint64_t)input16->src2[idx];
      element.src3 = (uint64_t)input16->src3[idx];
      break;
    case 2:
      element.src1 = widenVd ? (uint64_t)input16->src1[idx] : (uint64_t)input32->src1[idx];
      if (onlyWidenVs2) {
        element.src2 = (uint64_t)input64->src2[idx];
      } else if (onlyWidenVd || isVf2) {
        element.src2 = (uint64_t)input16->src2[idx];
      } else if (isVf4) {
        if (idx < 2) {
          element.src2 = (uint64_t)input8->src2[idx];
        } else {
          element.src2 = (uint64_t)input8->src2[idx+2];
        }
      } else {
        element.src2 = (uint64_t)input32->src2[idx];
      }
      element.src3 = (uint64_t)input32->src3[idx];
      break;
    case 3:
      element.src1 = widenVd ? (uint64_t)input32->src1[idx] : (uint64_t)input64->src1[idx];
      if (onlyWidenVd || isVf2) {
        element.src2 = (uint64_t)input32->src2[idx];
      } else if (isVf4) {
        if (idx < 1) {
          element.src2 = (uint64_t)input16->src2[idx];
        } else {
          element.src2 = (uint64_t)input16->src2[idx+1];
        }
      } else if (isVf8) {
        if (idx < 1) {
          element.src2 = (uint64_t)input8->src2[idx];
        } else {
          element.src2 = (uint64_t)input8->src2[idx+3];
        }
      } else {
        element.src2 = (uint64_t)input64->src2[idx];
      }
      element.src3 = (uint64_t)input64->src3[idx];
      break;
    default:
      printf("select, VIAluF Golden Modle, fuOpTyp=%hx, bad sew %d\n", input.fuOpType, input.sew);
      exit(1);
  }


  switch (sew) {
    case 0:
      element.src4 = (uint64_t)GET_BIT(mask, idx);
      break;
    case 1: {
      if (idx < 4) {
        element.src4 = (uint64_t)GET_BIT(mask, idx);
      } else {
        element.src4 = (uint64_t)GET_BIT(mask, (idx+4));
      }
      break;
    }
    case 2:
      if (idx < 2) {
        element.src4 = (uint64_t)GET_BIT(mask, idx);
      } else {
        element.src4 = (uint64_t)GET_BIT(mask, (idx+6));
      }
      break;
    case 3:
      if (idx == 0) {
        element.src4 = (uint64_t)GET_BIT(mask, idx);
      } else {
        element.src4 = (uint64_t)GET_BIT(mask, (idx+7));
      }
      break;
    default:
      printf("no support sew\n");
      break;
  }

  element.fuOpType = input.fuOpType;
  element.src_widen = input.src_widen;
  element.widen = input.widen;
  element.rm = input.rm;
  element.rm_s = input.rm_s;
  element.uop_idx = input.uop_idx;
  return element;
}

uint16_t vec_data_to_mask_data(uint64_t srcMask, int sew, int idx) {
  uint16_t maskOut = 0;
    switch (sew) {
      case 0:
        maskOut = (srcMask & (0xFFFF << (idx*16))) >> (idx*16);
        break;
      case 1:
        maskOut = (srcMask & (0xFF << (idx*8))) >> (idx*8);
        break;
      case 2:
        maskOut = (srcMask & (0xF << (idx*4))) >> (idx*4);
        break;
      case 3:
        maskOut = (srcMask & (0x3 << (idx*2))) >> (idx*2);
        break;
      default: printf("bad sew can not split this vector\n"); break;
    }
  return maskOut;
}

uint16_t n_copy_x_to_16(uint16_t n, int x) {
  int size = 16 / x;
  uint16_t out = 0;
  for (int i = 0; i < size; i++) {
    bool value = (n & (0x1ULL << i)) >> i;
    for (int j = 0; j < x; j++) {
      out |= (value << (x*i+j));
    }
  }
  return out;
}


VecOutput VGMIntegerALUF::get_expected_output(VecInput input) {
  VecOutput output;
  constexpr int kVlenWords = VLEN / 64;
  constexpr int kVlenBytes = VLEN / 8;

  uint64_t allMaskFalse = 0;
  uint64_t allMaskTrue = ~allMaskFalse;

  uint64_t oldVd[kVlenWords];
  uint64_t maskIn[kVlenWords];
  for (int i = 0; i < kVlenWords; i++) {
    oldVd[i] = input.src3[i];
    maskIn[i] = input.src4[i];
  }
  uint16_t fuOpType = input.fuOpType;
  bool vm = input.vinfo.vm;
  bool ma = input.vinfo.ma;
  bool ta = input.vinfo.ta;
  int sew = input.sew;
  int vl = input.vinfo.vl;

  bool isAddCarry = ((fuOpType == VADC_VVM) || (fuOpType == VMADC_VVM) || (fuOpType == VMADC_VV) ||
                     (fuOpType == VSBC_VVM) || (fuOpType == VMSBC_VVM) || (fuOpType == VMSBC_VV));
  bool isSat = (fuOpType == VSADDU_VV) || (fuOpType == VSADD_VV) || (fuOpType == VSSUBU_VV) || (fuOpType == VSSUB_VV);
  bool isNclip = (fuOpType == VNCLIP_WV) || (fuOpType == VNCLIPU_WV);
  bool isNarrow = ((fuOpType == VNSRA_WV) || (fuOpType == VNSRL_WV) || (fuOpType == VNCLIP_WV) || (fuOpType == VNCLIPU_WV));
  bool isDstMask = ((fuOpType == VMADC_VV) || (fuOpType == VMADC_VVM) || (fuOpType == VMSBC_VV) || (fuOpType == VMSBC_VVM) ||
                    (fuOpType == VMAND_MM) || (fuOpType == VMNAND_MM) || (fuOpType == VMANDN_MM) || (fuOpType == VMXOR_MM) ||
                    (fuOpType == VMOR_MM) || (fuOpType == VMNOR_MM) || (fuOpType == VMORN_MM) || (fuOpType == VMXNOR_MM) ||
                    (fuOpType == VMSEQ_VV) || (fuOpType == VMSNE_VV) || (fuOpType == VMSLE_VV) || (fuOpType == VMSLEU_VV) ||
                    (fuOpType == VMSLT_VV) || (fuOpType == VMSLTU_VV) || (fuOpType == VMSGT_VV) || (fuOpType == VMSGTU_VV));
  bool isOpMask = ((fuOpType == VMAND_MM) || (fuOpType == VMNAND_MM) || (fuOpType == VMANDN_MM) || (fuOpType == VMXOR_MM) ||
                   (fuOpType == VMOR_MM) || (fuOpType == VMNOR_MM) || (fuOpType == VMORN_MM) || (fuOpType == VMXNOR_MM));

  bool widenVs2 = (fuOpType == VWADDU_WV) || (fuOpType == VWSUBU_WV) || (fuOpType == VWADD_WV) || (fuOpType == VWSUB_WV) || isNarrow;
  bool widenVd = (fuOpType == VWADDU_VV)  || (fuOpType == VWADD_VV)  ||(fuOpType == VWADDU_WV) || (fuOpType == VWADD_WV) ||
                  (fuOpType == VWSUBU_VV) || (fuOpType == VWSUB_VV)  || (fuOpType == VWSUBU_WV) || (fuOpType == VWSUB_WV) || (fuOpType == VWSLL_VV);

  bool needClearMask = (fuOpType == VMADC_VV) || (fuOpType == VMSBC_VV);
  uint64_t srcMask[kVlenWords];
  uint64_t activeMask[kVlenWords];
  for (int i = 0; i < kVlenWords; i++) {
    if (needClearMask) {
      srcMask[i] = allMaskFalse;
    } else if (vm) {
      srcMask[i] = allMaskTrue;
    } else {
      srcMask[i] = maskIn[i];
    }
    activeMask[i] = isAddCarry ? allMaskTrue : srcMask[i];
  }

  uint8_t elementMaskSrc[kVlenBytes];
  uint8_t elementMaskActive[kVlenBytes];
  uint8_t oldMaskBits[kVlenBytes];
  for (int i = 0; i < kVlenBytes; i++) {
    int word = i / 64;
    int bit = i % 64;
    elementMaskSrc[i] = (srcMask[word] >> bit) & 0x1ULL;
    elementMaskActive[i] = (activeMask[word] >> bit) & 0x1ULL;
    oldMaskBits[i] = (oldVd[word] >> bit) & 0x1ULL;
  }

  uint64_t maskToElement = 0;
  for (int i = 0; i < kVlenBytes; i++) {
    if (elementMaskSrc[i]) {
      maskToElement |= (0x1ULL << i);
    }
  }

  if (widenVd) {
    sew = sew + 1;
    input.sew = input.sew + 1;
  }

  int number = (VLEN / 8) >> sew;
  int words = kVlenWords;
  int elements_per_word = number / words;
  int result_shift_len = 8 << sew;

  ElementOutput output_part[number];
  for (int i = 0; i < number; i++) {
    ElementInput element = select_vialuf_element(input, i, maskToElement, widenVd, widenVs2, isDstMask);
    switch (sew) {
      case 0: output_part[i] = calculation_e8(element);  break;
      case 1: output_part[i] = calculation_e16(element); break;
      case 2: output_part[i] = calculation_e32(element); break;
      case 3: output_part[i] = calculation_e64(element); break;
      default:
        printf("VIAluF Golden Modle, bad sew %d\n", input.sew);
        exit(1);
    }
  }

  if (vl == 0) {
    for (int i = 0; i < kVlenWords; i++) {
      output.result[i] = input.src3[i];
      output.fflags[i] = 0;
    }
    output.vxsat = 0;
    return output;
  }

  uint64_t packedResult[kVlenWords];
  for (int i = 0; i < words; i++) {
    packedResult[i] = 0;
    output.fflags[i] = 0;
    for (int j = 0; j < elements_per_word; j++) {
      int idx = i * elements_per_word + j;
      packedResult[i] |= ((uint64_t)output_part[idx].result << (j * result_shift_len));
      if (verbose) {
        printf("%s::%s ResultJoint i:%d j:%d result:%lx fflags:%x\n", typeid(this).name(), __func__, i, j, packedResult[i], output.fflags[i]);
      }
    }
  }

  output.vxsat = 0;
  if (isSat || isNclip) {
    for (int i = 0; i < number; i++) {
      if ((i < vl) && elementMaskActive[i] && output_part[i].vxsat) {
        output.vxsat = 1;
        break;
      }
    }
  }

  if (isDstMask) {
    uint64_t maskOut[kVlenWords];
    for (int i = 0; i < kVlenWords; i++) {
      maskOut[i] = 0;
    }

    for (int bit = 0; bit < VLEN; bit++) {
      uint64_t bitval = 1;
      if (bit < number) {
        if (bit >= vl) {
          bitval = 1;
        } else if (isOpMask) {
          bitval = output_part[bit].result & 0x1ULL;
        } else if (elementMaskActive[bit]) {
          bitval = output_part[bit].result & 0x1ULL;
        } else {
          bitval = ma ? 1 : oldMaskBits[bit];
        }
      }

      if (bitval) {
        int word = bit / 64;
        int off = bit % 64;
        maskOut[word] |= (0x1ULL << off);
      }
    }

    for (int i = 0; i < kVlenWords; i++) {
      output.result[i] = maskOut[i];
      output.fflags[i] = 0;
    }
    return output;
  }

  uint8_t oldVdBytes[kVlenBytes];
  uint8_t resultBytes[kVlenBytes];
  uint8_t finalBytes[kVlenBytes];
  for (int i = 0; i < kVlenWords; i++) {
    for (int j = 0; j < 8; j++) {
      int byte_idx = i * 8 + j;
      oldVdBytes[byte_idx] = (oldVd[i] >> (8 * j)) & 0xFF;
      resultBytes[byte_idx] = (packedResult[i] >> (8 * j)) & 0xFF;
    }
  }

  int bytes_per_elem = 1 << sew;
  int vlBytes = vl * bytes_per_elem;
  if (vlBytes > kVlenBytes) {
    vlBytes = kVlenBytes;
  }

  for (int i = 0; i < kVlenBytes; i++) {
    bool body = (i < vlBytes);
    int elem_idx = i / bytes_per_elem;
    bool mask_on = elementMaskActive[elem_idx];
    bool active = body && mask_on;
    bool agnostic = (ma && body && !mask_on) || (ta && !body);

    if (active) {
      finalBytes[i] = resultBytes[i];
    } else if (agnostic) {
      finalBytes[i] = 0xFF;
    } else {
      finalBytes[i] = oldVdBytes[i];
    }
  }

  for (int i = 0; i < kVlenWords; i++) {
    output.result[i] = 0;
    for (int j = 0; j < 8; j++) {
      int byte_idx = i * 8 + j;
      output.result[i] |= ((uint64_t)finalBytes[byte_idx] << (8 * j));
    }
  }

  return output;
}

ElementOutput VGMIntegerALUF::calculation_e8(ElementInput input) {
  ElementOutput output;
  bool sat = false;
  
  switch (input.fuOpType) {
  case VADD_VV: {
    output.result = (uint8_t)((uint8_t)input.src1 + (uint8_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VSUB_VV: {
    output.result = (uint8_t)((uint8_t)input.src2 - (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VADC_VVM: {
    output.result = (uint8_t)((uint8_t)input.src2 + (uint8_t)input.src1 + (uint8_t)input.src4);
    output.vxsat = 0;
    break;
  }
  case VMADC_VVM: {
    uint16_t res = (uint8_t)input.src2 + (uint8_t)input.src1 + (uint8_t)input.src4;
    output.result = (uint8_t)((res >> 8) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VMADC_VV: {
    uint16_t res = (uint8_t)input.src2 + (uint8_t)input.src1;
    output.result = (uint8_t)((res >> 8) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VSBC_VVM: {
    output.result = (uint8_t)((uint8_t)input.src2 - (uint8_t)input.src1 - (uint8_t)input.src4);
    output.vxsat = 0;
    break;
  }
  case VMSBC_VVM: {
    uint16_t res = (uint8_t)input.src2 - (uint8_t)input.src1 - (uint8_t)input.src4;
    output.result = (uint8_t)((res >> 8) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VMSBC_VV: {
    uint16_t res = (uint8_t)input.src2 - (uint8_t)input.src1;
    output.result = (uint8_t)((res >> 8) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VAND_VV: {
    output.result = (uint8_t)((uint8_t)input.src1 & (uint8_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VOR_VV: {
    output.result = (uint8_t)((uint8_t)input.src1 | (uint8_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VXOR_VV: {
    output.result = (uint8_t)((uint8_t)input.src1 ^ (uint8_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VSLL_VV: {
    output.result = (uint8_t)((uint8_t)input.src2 << ((uint8_t)input.src1 & 0x7));
    output.vxsat = 0;
    break;
  }
  case VSRL_VV: {
    output.result = (uint8_t)((uint8_t)input.src2 >> ((uint8_t)input.src1 & 0x7));
    output.vxsat = 0;
    break;
  }
  case VSRA_VV: {
    output.result = (uint8_t)((int8_t)input.src2 >> ((uint8_t)input.src1 & 0x7));
    output.vxsat = 0;
    break;
  }
  case VNSRL_WV: {
    uint8_t mask = 0xf;
    uint16_t vs2 = input.src2;
    uint8_t vs1 = input.src1;
    output.result = (uint8_t)((uint16_t)(vs2 >> (vs1 & mask)));
    output.vxsat = 0;
    break;
  }
  case VNSRA_WV: {
    uint8_t mask = 0xf;
    int16_t vs2 = input.src2;
    uint8_t vs1 = input.src1;
    output.result = (uint8_t)((int16_t)(vs2 >> (vs1 & mask)));
    output.vxsat = 0;
    break;
  }
  case VMSEQ_VV:{
    output.result = (uint8_t)((uint8_t)input.src2 == (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSNE_VV: {
    output.result = (uint8_t)((uint8_t)input.src2 != (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLTU_VV: {
    output.result = (uint8_t)((uint8_t)input.src2 < (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLT_VV: {
    output.result = (uint8_t)((int8_t)input.src2 < (int8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLEU_VV: {
    output.result = (uint8_t)((uint8_t)input.src2 <= (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLE_VV: {
    output.result = (uint8_t)((int8_t)input.src2 <= (int8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSGTU_VV: {
    output.result = (uint8_t)((uint8_t)input.src2 > (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSGT_VV: {
    output.result = (uint8_t)((int8_t)input.src2 > (int8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMINU_VV: {
    output.result = (uint8_t)input.src2 <= (uint8_t)input.src1 ? (uint8_t)input.src2 : (uint8_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMIN_VV: {
    output.result = (int8_t)input.src2 <= (int8_t)input.src1 ? (uint8_t)input.src2 : (uint8_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMAXU_VV: {
    output.result = (uint8_t)input.src2 >= (uint8_t)input.src1 ? (uint8_t)input.src2 : (uint8_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMAX_VV: {
    output.result = (int8_t)input.src2 >= (int8_t)input.src1 ? (uint8_t)input.src2 : (uint8_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VSADDU_VV: {
    uint8_t vs2 = input.src2;
    uint8_t vs1 = input.src1;
    uint8_t vd = vs2 + vs1;
    sat = vd < vs2;
    vd |= -sat;
    output.result = vd;
    output.vxsat = sat;
    break;
  }
  case VSADD_VV: {
    output.result = (uint8_t)sat_add<int8_t, uint8_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VSSUBU_VV: {
    output.result = (uint8_t)sat_subu<uint8_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VSSUB_VV: {
    output.result = (uint8_t)sat_sub<int8_t, uint8_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VAADDU_VV: {
    uint16_t res = (uint8_t)input.src2 + (uint8_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint8_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VAADD_VV: {
    uint16_t res = (int8_t)input.src2 + (int8_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint8_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VASUBU_VV: {
    uint16_t res = (uint8_t)input.src2 - (uint8_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint8_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VASUB_VV: {
    int16_t res = (int8_t)input.src2 - (int8_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint8_t)(res >> 1);
    output.vxsat = ((input.rm_s == RM_S_RNU) || (input.rm_s == RM_S_RNE)) && ((uint8_t)input.src2 == 0x7F) && ((uint8_t)input.src1 == 0x80);
    break;
  }
  case VSSRL_VV: {
    int8_t sh = input.src1 & 0x7;
    uint16_t val = (uint8_t)input.src2;
    INT_ROUNDING(val, input.rm_s, sh);
    output.result = (uint8_t)(val >> sh);
    output.vxsat = 0;
    break;
  }
  case VSSRA_VV: {
    int8_t sh = input.src1 & 0x7;
    int16_t val = (int8_t)input.src2;
    INT_ROUNDING(val, input.rm_s, sh);
    output.result = (uint8_t)(val >> sh);
    output.vxsat = 0;
    break;
  }
  case VNCLIPU_WV: {
    uint8_t uint_max = 0xFF;
    uint16_t sign_mask = 0xFF00;

    uint32_t vs2 = (uint16_t)input.src2;
    uint8_t vs1 = (uint8_t)input.src1;
    unsigned shift = vs1 & 0xf;

    // rounding
    INT_ROUNDING(vs2, input.rm_s, shift);

    vs2 = vs2 >> shift;

    // saturation
    if (vs2 & sign_mask) {
      vs2 = uint_max;
      output.vxsat = 1;
    } else {
      output.vxsat = 0;
    }
    output.result = (uint8_t)vs2;
    break;
  }
  case VNCLIP_WV: {
    int8_t int_max = 0x7F;
    int8_t int_min = 0x80;

    int32_t vs2 = (int16_t)input.src2;  
    uint8_t vs1 = (uint8_t)input.src1;
    unsigned shift = vs1 & 0xf;
  
    // rounding
    INT_ROUNDING(vs2, input.rm_s, shift);

    vs2 = vs2 >> shift;

    // saturation
    if (vs2 < int_min) {
      vs2 = int_min;
      output.vxsat = 1;
    } else if (vs2 > int_max) {
      vs2 = int_max;
      output.vxsat = 1;
    } else {
      output.vxsat = 0;
    }

    output.result = (uint8_t)vs2;
    break;
  }
  case VMAND_MM: {
    output.result = (uint8_t)((uint8_t)input.src2 & (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMNAND_MM: {  
    output.result = (uint8_t)(~((uint8_t)input.src2 & (uint8_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMANDN_MM: {
    output.result = (uint8_t)((uint8_t)input.src2 & (~(uint8_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMXOR_MM: {
    output.result = (uint8_t)((uint8_t)input.src2 ^ (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMOR_MM: {
    output.result = (uint8_t)((uint8_t)input.src2 | (uint8_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMNOR_MM: {
    output.result = (uint8_t)(~((uint8_t)input.src2 | (uint8_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMORN_MM: {
    output.result = (uint8_t)((uint8_t)input.src2 | (~(uint8_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMXNOR_MM: {
    output.result = (uint8_t)(~((uint8_t)input.src2 ^ (uint8_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VANDN_VV: {
    output.result = (uint8_t)((uint8_t)input.src2 & (~(uint8_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VBREV_V: {
    uint8_t tmp = (uint8_t)input.src2;
    tmp = ((tmp & 0x55) << 1) | ((tmp & 0xAA) >> 1);
    tmp = ((tmp & 0x33) << 2) | ((tmp & 0xCC) >> 2);
    tmp = ((tmp & 0x0F) << 4) | ((tmp & 0xF0) >> 4);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VBREV8_V: {
    uint8_t tmp = (uint8_t)input.src2;
    tmp = ((tmp & 0x55) << 1) | ((tmp & 0xAA) >> 1);
    tmp = ((tmp & 0x33) << 2) | ((tmp & 0xCC) >> 2);
    tmp = ((tmp & 0x0F) << 4) | ((tmp & 0xF0) >> 4);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VREV8_V: {
    output.result = (uint8_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VCLZ_V: {
    uint8_t i = 0;
    for (; i < 8; i++) {
      if (1 & ((uint8_t)input.src2 >> (7 - i))) {
        break;
      }
    }
    output.result = i;
    output.vxsat = 0;
    break;
  }
  case VCTZ_V: {
    uint8_t i = 0;
    for (; i < 8; i++) {
      if (1 & ((uint8_t)input.src2 >> i)){
        break;
      }
    }
    output.result = i;
    output.vxsat = 0;
    break;
  }
  case VCPOP_V: {
    uint8_t cnt = 0;
    for (int i = 0; i < 8; i++) {
      if (1 & ((uint8_t)input.src2) >> i) {
        cnt++;
      }
    }
    output.result = cnt;
    output.vxsat = 0;
    break;
  }
  case VROL_VV: {
    uint8_t mask = 0x7;
    uint8_t lshift = (uint8_t)input.src1 & mask;
    uint8_t rshift = (-lshift) & mask;
    output.result = (uint8_t)(((uint8_t)input.src2 << lshift) | ((uint8_t)input.src2 >> rshift));
    output.vxsat = 0;
    break;
  }
  case VROR_VV: {
    uint8_t mask = 0x7;
    uint8_t rshift = (uint8_t)input.src1 & mask;
    uint8_t lshift = (-rshift) & mask;
    output.result = (uint8_t)(((uint8_t)input.src2 << lshift) | ((uint8_t)input.src2 >> rshift));
    output.vxsat = 0;
    break;
  }
  default:
    printf("VIntegerV2 no fuOpType\n");
    break;
  }

  output.fflags = 0;

  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }

  return output;
}

ElementOutput VGMIntegerALUF::calculation_e16(ElementInput input) {
  ElementOutput output;
  bool sat = false;

  switch (input.fuOpType) {
  case VADD_VV: {
    output.result = (uint16_t)((uint16_t)input.src1 + (uint16_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VSUB_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 - (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VWADDU_VV: {
    uint16_t vs2 = (uint8_t)input.src2;
    uint16_t vs1 = (uint8_t)input.src1;
    output.result = (uint16_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUBU_VV: {
    uint16_t vs2 = (uint8_t)input.src2;
    uint16_t vs1 = (uint8_t)input.src1;
    output.result = (uint16_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADD_VV: {
    uint16_t vs2 = (int8_t)input.src2;
    uint16_t vs1 = (int8_t)input.src1;
    output.result = (uint16_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUB_VV: {
    uint16_t vs2 = (int8_t)input.src2;
    uint16_t vs1 = (int8_t)input.src1;
    output.result = (uint16_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADDU_WV: {
    uint16_t vs2 = (uint16_t)input.src2;
    uint16_t vs1 = (uint8_t)input.src1;
    output.result = (uint16_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUBU_WV: {
    uint16_t vs2 = (uint16_t)input.src2;
    uint16_t vs1 = (uint8_t)input.src1;
    output.result = (uint16_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADD_WV: {
    int16_t vs2 = (int16_t)input.src2;
    int16_t vs1 = (int8_t)input.src1;
    output.result = (uint16_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUB_WV: {
    int16_t vs2 = (int16_t)input.src2;
    int16_t vs1 = (int8_t)input.src1;
    output.result = (uint16_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VZEXT_VF2: {
    output.result = (uint8_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VSEXT_VF2: {
    output.result = (uint16_t)(int8_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VADC_VVM: {
    output.result = (uint16_t)((uint16_t)input.src2 + (uint16_t)input.src1 + (uint16_t)input.src4);
    output.vxsat = 0;
    break;
  }
  case VMADC_VVM: {
    uint32_t res = (uint16_t)input.src2 + (uint16_t)input.src1 + (uint16_t)input.src4;
    output.result = (uint16_t)((res >> 16) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VMADC_VV: {
    uint32_t res = (uint16_t)input.src2 + (uint16_t)input.src1;
    output.result = (uint16_t)((res >> 16) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VSBC_VVM: {
    output.result = (uint16_t)((uint16_t)input.src2 - (uint16_t)input.src1 - (uint16_t)input.src4);
    output.vxsat = 0;
    break;
  }
  case VMSBC_VVM: {
    uint32_t res = (uint16_t)input.src2 - (uint16_t)input.src1 - (uint8_t)input.src4;
    output.result = (uint16_t)((res >> 16) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VMSBC_VV: {
    uint32_t res = (uint16_t)input.src2 - (uint16_t)input.src1;
    output.result = (uint16_t)((res >> 16) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VAND_VV: {
    output.result = (uint16_t)((uint16_t)input.src1 & (uint16_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VOR_VV: {
    output.result = (uint16_t)((uint16_t)input.src1 | (uint16_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VXOR_VV: {
    output.result = (uint16_t)((uint16_t)input.src1 ^ (uint16_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VSLL_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 << ((uint16_t)input.src1 & 0xf));
    output.vxsat = 0;
    break;
  }
  case VSRL_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 >> ((uint16_t)input.src1 & 0xf));
    output.vxsat = 0;
    break;
  }
  case VSRA_VV: {
    output.result = (uint16_t)((int16_t)input.src2 >> ((uint16_t)input.src1 & 0xf));
    output.vxsat = 0;
    break;
  }
  case VNSRL_WV: {
    uint8_t mask = 0x1f;
    uint32_t vs2 = input.src2;
    uint16_t vs1 = input.src1;
    output.result = (uint16_t)((uint32_t)(vs2 >> (vs1 & mask)));
    output.vxsat = 0;
    break;
  }
  case VNSRA_WV: {
    uint8_t mask = 0x1f;
    int32_t vs2 = input.src2;
    uint16_t vs1 = input.src1;
    output.result = (uint16_t)((int32_t)(vs2 >> (vs1 & mask)));
    output.vxsat = 0;
    break;
  }
  case VMSEQ_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 == (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSNE_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 != (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLTU_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 < (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLT_VV: {
    output.result = (uint16_t)((int16_t)input.src2 < (int16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLEU_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 <= (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLE_VV: {
    output.result = (uint16_t)((int16_t)input.src2 <= (int16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSGTU_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 > (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSGT_VV: {
    output.result = (uint16_t)((int16_t)input.src2 > (int16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMINU_VV: {
    output.result = (uint16_t)input.src2 <= (uint16_t)input.src1 ? (uint16_t)input.src2 : (uint16_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMIN_VV: {
    output.result = (int16_t)input.src2 <= (int16_t)input.src1 ? (uint16_t)input.src2 : (uint16_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMAXU_VV: {
    output.result = (uint16_t)input.src2 >= (uint16_t)input.src1 ? (uint16_t)input.src2 : (uint16_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMAX_VV: {
    output.result = (int16_t)input.src2 >= (int16_t)input.src1 ? (uint16_t)input.src2 : (uint16_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VSADDU_VV: {
    uint16_t vs2 = input.src2;
    uint16_t vs1 = input.src1;
    uint16_t vd = vs2 + vs1;
    sat = vd < vs2;
    vd |= -sat;
    output.result = vd;
    output.vxsat = sat;
    break;
  }
  case VSADD_VV: {
    output.result = (uint16_t)sat_add<int16_t, uint16_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VSSUBU_VV: {
    output.result = (uint16_t)sat_subu<uint16_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VSSUB_VV: {
    output.result = (uint16_t)sat_sub<int16_t, uint16_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VAADDU_VV: {
    uint32_t res = (uint16_t)input.src2 + (uint16_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint16_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VAADD_VV: {
    uint32_t res = (int16_t)input.src2 + (int16_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint16_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VASUBU_VV: {
    uint32_t res = (uint16_t)input.src2 - (uint16_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint16_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VASUB_VV: {
    int32_t res = (int16_t)input.src2 - (int16_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint16_t)(res >> 1);
    output.vxsat = ((input.rm_s == RM_S_RNU) || (input.rm_s == RM_S_RNE)) && ((uint16_t)input.src2 == 0x7FFF) && ((uint16_t)input.src1 == 0x8000);
    break;
  }
  case VSSRL_VV: {
    int8_t sh = input.src1 & 0xf;
    uint32_t val = (uint16_t)input.src2;
    INT_ROUNDING(val, input.rm_s, sh);
    output.result = (uint16_t)(val >> sh);
    output.vxsat = 0;
    break;
  }
  case VSSRA_VV: {
    int8_t sh = input.src1 & 0xf;
    int32_t val = (int16_t)input.src2;
    INT_ROUNDING(val, input.rm_s, sh);
    output.result = (uint16_t)(val >> sh);
    output.vxsat = 0;
    break;
  }
  case VNCLIPU_WV: {
    uint16_t uint_max = 0xFFFF;
    uint32_t sign_mask = 0xFFFF0000;

    uint64_t vs2 = (uint32_t)input.src2;
    uint16_t vs1 = (uint16_t)input.src1;
    unsigned shift = vs1 & 0x1f;

    // rounding
    INT_ROUNDING(vs2, input.rm_s, shift);

    vs2 = vs2 >> shift;

    // saturation
    if (vs2 & sign_mask) {
      vs2 = uint_max;
      output.vxsat = 1;
    } else {
      output.vxsat = 0;
    }
    output.result = (uint16_t)vs2;
    break;
  }
  case VNCLIP_WV: {
    int16_t int_max = 0x7FFF;
    int16_t int_min = 0x8000;

    int64_t vs2 = (int32_t)input.src2;  
    uint16_t vs1 = (uint16_t)input.src1;
    unsigned shift = vs1 & 0x1f;
  
    // rounding
    INT_ROUNDING(vs2, input.rm_s, shift);

    vs2 = vs2 >> shift;

    // saturation
    if (vs2 < int_min) {
      vs2 = int_min;
      output.vxsat = 1;
    } else if (vs2 > int_max) {
      vs2 = int_max;
      output.vxsat = 1;
    } else {
      output.vxsat = 0;
    }

    output.result = (uint16_t)vs2;
    break;
  }
  case VMAND_MM: {
    output.result = (uint16_t)((uint16_t)input.src2 & (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMNAND_MM: {
    output.result = (uint16_t)(~((uint16_t)input.src2 & (uint16_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMANDN_MM: {
    output.result = (uint16_t)((uint16_t)input.src2 & (~(uint16_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMXOR_MM: {
    output.result = (uint16_t)((uint16_t)input.src2 ^ (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMOR_MM: {
    output.result = (uint16_t)((uint16_t)input.src2 | (uint16_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMNOR_MM: {
    output.result = (uint16_t)(~((uint16_t)input.src2 | (uint16_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMORN_MM: {
    output.result = (uint16_t)((uint16_t)input.src2 | (~(uint16_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMXNOR_MM: {
    output.result = (uint16_t)(~((uint16_t)input.src2 ^ (uint16_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VANDN_VV: {
    output.result = (uint16_t)((uint16_t)input.src2 & (~(uint16_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VBREV_V: {
    uint16_t tmp = (uint16_t)input.src2;
    tmp = ((tmp & 0x5555) << 1) | ((tmp & 0xAAAA) >> 1);
    tmp = ((tmp & 0x3333) << 2) | ((tmp & 0xCCCC) >> 2);
    tmp = ((tmp & 0x0F0F) << 4) | ((tmp & 0xF0F0) >> 4);
    tmp = ((tmp & 0x00FF) << 8) | ((tmp & 0xFF00) >> 8);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VBREV8_V: {
    uint16_t tmp = (uint16_t)input.src2;
    tmp = ((tmp & 0x5555) << 1) | ((tmp & 0xAAAA) >> 1);
    tmp = ((tmp & 0x3333) << 2) | ((tmp & 0xCCCC) >> 2);
    tmp = ((tmp & 0x0F0F) << 4) | ((tmp & 0xF0F0) >> 4);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VREV8_V: {
    uint16_t tmp = (uint16_t)input.src2;
    tmp = ((tmp & 0x00FF) << 8) | ((tmp & 0xFF00) >> 8);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VCLZ_V: {
    uint8_t i = 0;
    for (; i < 16; i++) {
      if (1 & ((uint16_t)input.src2 >> (15 - i))) {
        break;
      }
    }
    output.result = i;
    output.vxsat = 0;
    break;
  }
  case VCTZ_V: {
    uint8_t i = 0;
    for (; i < 16; i++) {
      if (1 & ((uint16_t)input.src2 >> i)){
        break;
      }
    }
    output.result = i;
    output.vxsat = 0;
    break;
  }
  case VCPOP_V: {
    uint8_t cnt = 0;
    for (int i = 0; i < 16; i++) {
      if (1 & ((uint16_t)input.src2) >> i) {
        cnt++;
      }
    }
    output.result = cnt;
    output.vxsat = 0;
    break;
  }
  case VROL_VV: {
    uint8_t mask = 0xF;
    uint16_t lshift = (uint16_t)input.src1 & mask;
    uint16_t rshift = (-lshift) & mask;
    output.result = (uint16_t)(((uint16_t)input.src2 << lshift) | ((uint16_t)input.src2 >> rshift));
    output.vxsat = 0;
    break;
  }
  case VROR_VV: {
    uint8_t mask = 0xF;
    uint16_t rshift = (uint16_t)input.src1 & mask;
    uint16_t lshift = (-rshift) & mask;
    output.result = (uint16_t)(((uint16_t)input.src2 << lshift) | ((uint16_t)input.src2 >> rshift));
    output.vxsat = 0;
    break;
  }
  case VWSLL_VV: {
    uint8_t shift = (uint8_t)input.src1 & 0xF;
    output.result = (uint16_t)((uint8_t)input.src2 << shift);
    output.vxsat = 0;
    break;
  }
  default:
    printf("VIntegerV2 no fuOpType\n");
    break;
  }

  output.fflags = 0;
  
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  
  return output;
}
ElementOutput VGMIntegerALUF::calculation_e32(ElementInput input) {
  ElementOutput output;
  bool sat = false;

  switch (input.fuOpType) {
  case VADD_VV: {
    output.result = (uint32_t)((uint32_t)input.src1 + (uint32_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VSUB_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 - (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
case VWADDU_VV: {
    uint32_t vs2 = (uint16_t)input.src2;
    uint32_t vs1 = (uint16_t)input.src1;
    output.result = (uint32_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUBU_VV: {
    uint32_t vs2 = (uint16_t)input.src2;
    uint32_t vs1 = (uint16_t)input.src1;
    output.result = (uint32_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADD_VV: {
    uint32_t vs2 = (int16_t)input.src2;
    uint32_t vs1 = (int16_t)input.src1;
    output.result = (uint32_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUB_VV: {
    uint32_t vs2 = (int16_t)input.src2;
    uint32_t vs1 = (int16_t)input.src1;
    output.result = (uint32_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADDU_WV: {
    uint32_t vs2 = (uint32_t)input.src2;
    uint32_t vs1 = (uint16_t)input.src1;
    output.result = (uint32_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUBU_WV: {
    uint32_t vs2 = (uint32_t)input.src2;
    uint32_t vs1 = (uint16_t)input.src1;
    output.result = (uint32_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADD_WV: {
    int32_t vs2 = (int32_t)input.src2;
    int32_t vs1 = (int16_t)input.src1;
    output.result = (uint32_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUB_WV: {
    int32_t vs2 = (int32_t)input.src2;
    int32_t vs1 = (int16_t)input.src1;
    output.result = (uint32_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VZEXT_VF2: {
    output.result = (uint16_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VSEXT_VF2: {
    output.result = (uint32_t)(int16_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VZEXT_VF4: {
    output.result = (uint32_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VSEXT_VF4: {
    output.result = (uint32_t)(int8_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VADC_VVM: {
    output.result = (uint32_t)((uint32_t)input.src2 + (uint32_t)input.src1 + (uint32_t)input.src4);  
    output.vxsat = 0;
    break;
  }
  case VMADC_VVM: {
    uint64_t res = (uint32_t)input.src2 + (uint32_t)input.src1 + (uint32_t)input.src4;
    output.result = (uint32_t)((res >> 32) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VMADC_VV: {
    uint64_t res = (uint32_t)input.src2 + (uint32_t)input.src1;
    output.result = (uint32_t)((res >> 32) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VSBC_VVM: {
    output.result = (uint32_t)((uint32_t)input.src2 - (uint32_t)input.src1 - (uint32_t)input.src4);
    output.vxsat = 0;
    break;
  }
  case VMSBC_VVM: {
    uint64_t res = (int32_t)((uint32_t)input.src2 - (uint32_t)input.src1 - (uint32_t)input.src4);
    output.result = (uint32_t)((res >> 32) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VMSBC_VV: {
    uint64_t res = (int32_t)((uint32_t)input.src2 - (uint32_t)input.src1);
    output.result = (uint32_t)((res >> 32) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VAND_VV: {
    output.result = (uint32_t)((uint32_t)input.src1 & (uint32_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VOR_VV: {
    output.result = (uint32_t)((uint32_t)input.src1 | (uint32_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VXOR_VV: {
    output.result = (uint32_t)((uint32_t)input.src1 ^ (uint32_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VSLL_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 << ((uint32_t)input.src1 & 0x1f));
    output.vxsat = 0;
    break;
  }
  case VSRL_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 >> ((uint32_t)input.src1 & 0x1f));
    output.vxsat = 0;
    break;
  }
  case VSRA_VV: {
    output.result = (uint32_t)((int32_t)input.src2 >> ((uint32_t)input.src1 & 0x1f));
    output.vxsat = 0;
    break;
  }
  case VNSRL_WV: {
    uint32_t mask = 0x3f;
    uint64_t vs2 = input.src2;
    uint32_t vs1 = input.src1;
    output.result = (uint32_t)((uint64_t)(vs2 >> (vs1 & mask)));
    output.vxsat = 0;
    break;
  }
  case VNSRA_WV: {
    uint64_t mask = 0x3f;
    int64_t vs2 = input.src2;
    uint32_t vs1 = input.src1;
    output.result = (uint32_t)((int64_t)(vs2 >> (vs1 & mask)));
    output.vxsat = 0;
    break;
  }
  case VMSEQ_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 == (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSNE_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 != (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLTU_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 < (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLT_VV: {
    output.result = (uint32_t)((int32_t)input.src2 < (int32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLEU_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 <= (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLE_VV: {
    output.result = (uint32_t)((int32_t)input.src2 <= (int32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSGTU_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 > (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSGT_VV: {
    output.result = (uint32_t)((int32_t)input.src2 > (int32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMINU_VV: {
    output.result = (uint32_t)input.src2 <= (uint32_t)input.src1 ? (uint32_t)input.src2 : (uint32_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMIN_VV: {
    output.result = (int32_t)input.src2 <= (int32_t)input.src1 ? (uint32_t)input.src2 : (uint32_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMAXU_VV: {
    output.result = (uint32_t)input.src2 >= (uint32_t)input.src1 ? (uint32_t)input.src2 : (uint32_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMAX_VV: {
    output.result = (int32_t)input.src2 >= (int32_t)input.src1 ? (uint32_t)input.src2 : (uint32_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VSADDU_VV: {
    uint32_t vs2 = input.src2;
    uint32_t vs1 = input.src1;
    uint32_t vd = vs2 + vs1;
    sat = vd < vs2;
    vd |= -sat;
    output.result = vd;
    output.vxsat = sat;
    break;
  }
  case VSADD_VV: {
    output.result = (uint32_t)sat_add<int32_t, uint32_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VSSUBU_VV: {
    output.result = (uint32_t)sat_subu<uint32_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VSSUB_VV: {
    output.result = (uint32_t)sat_sub<int32_t, uint32_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VAADDU_VV: {
    uint64_t res = (uint32_t)input.src2 + (uint32_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint32_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VAADD_VV: {
    uint64_t res = (uint32_t)((uint32_t)input.src2 + (uint32_t)input.src1);
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint32_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VASUBU_VV: {
    uint64_t res = (int32_t)input.src2 - (int32_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint32_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VASUB_VV: {
    int64_t res = (int32_t)input.src2 - (int32_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint32_t)(res >> 1);
    output.vxsat = ((input.rm_s == RM_S_RNU) || (input.rm_s == RM_S_RNE)) && ((uint32_t)input.src2 == 0x7FFFFFFF) && ((uint32_t)input.src1 == 0x80000000  );
    break;
  }
  case VSSRL_VV: {
    int8_t sh = input.src1 & 0x1f;
    uint64_t val = (uint32_t)input.src2;
    INT_ROUNDING(val, input.rm_s, sh);
    output.result = (uint32_t)(val >> sh);
    output.vxsat = 0;
    break;
  }
  case VSSRA_VV: {
    int8_t sh = input.src1 & 0x1f;
    int64_t val = (int32_t)input.src2;
    INT_ROUNDING(val, input.rm_s, sh);
    output.result = (uint32_t)(val >> sh);
    output.vxsat = 0;
    break;
  }
  case VNCLIPU_WV: {
    uint32_t uint_max = 0xFFFFFFFF;
    uint64_t sign_mask = 0xFFFFFFFF00000000;

    uint64_t vs2 = (uint64_t)input.src2;
    uint16_t vs1 = (uint32_t)input.src1;
    unsigned shift = vs1 & 0x3f;
    // rounding
    INT_ROUNDING(vs2, input.rm_s, shift);

    vs2 = vs2 >> shift;

    // saturation
    if (vs2 & sign_mask) {
      vs2 = uint_max;
      output.vxsat = 1;
    } else {
      output.vxsat = 0;
    }
    output.result = (uint32_t)vs2;
    break;
  }
  case VNCLIP_WV: {
    int64_t int_max = 0x7FFFFFFF;
    int64_t int_min = 0xFFFFFFFF80000000;

    uint64_t vs2 = (uint64_t)input.src2;  
    uint32_t vs1 = (uint32_t)input.src1;
    unsigned shift = vs1 & 0x3f;

    // rounding
    INT_ROUNDING(vs2, input.rm_s, shift);

    vs2 = vs2 >> shift;

    // saturation
    if ((int64_t)vs2 < int_min) {
      vs2 = 0x80000000;
      output.vxsat = 1;
    } else if ((int64_t)vs2 > int_max) {
      vs2 = 0x7FFFFFFF;
      output.vxsat = 1;
    } else {
      output.vxsat = 0;
    }

    output.result = (uint32_t)vs2;
    break;
  }
  case VMAND_MM: {
    output.result = (uint32_t)((uint32_t)input.src2 & (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMNAND_MM: {
    output.result = (uint32_t)(~((uint32_t)input.src2 & (uint32_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMANDN_MM: {
    output.result = (uint32_t)((uint32_t)input.src2 & (~(uint32_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMXOR_MM: {
    output.result = (uint32_t)((uint32_t)input.src2 ^ (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMOR_MM: {
    output.result = (uint32_t)((uint32_t)input.src2 | (uint32_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMNOR_MM: {
    output.result = (uint32_t)(~((uint32_t)input.src2 | (uint32_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMORN_MM: {
    output.result = (uint32_t)((uint32_t)input.src2 | (~(uint32_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMXNOR_MM: {
    output.result = (uint32_t)(~((uint32_t)input.src2 ^ (uint32_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VANDN_VV: {
    output.result = (uint32_t)((uint32_t)input.src2 & (~(uint32_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VBREV_V: {
    uint32_t tmp = (uint32_t)input.src2;
    tmp = ((tmp & 0x55555555llu) << 1)  | ((tmp & 0xAAAAAAAAllu) >> 1);
    tmp = ((tmp & 0x33333333llu) << 2)  | ((tmp & 0xCCCCCCCCllu) >> 2);
    tmp = ((tmp & 0x0F0F0F0Fllu) << 4)  | ((tmp & 0xF0F0F0F0llu) >> 4);
    tmp = ((tmp & 0x00FF00FFllu) << 8)  | ((tmp & 0xFF00FF00llu) >> 8);
    tmp = ((tmp & 0x0000FFFFllu) << 16) | ((tmp & 0xFFFF0000llu) >> 16);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VBREV8_V: {
    uint32_t tmp = (uint32_t)input.src2;
    tmp = ((tmp & 0x55555555) << 1) | ((tmp & 0xAAAAAAAA) >> 1);
    tmp = ((tmp & 0x33333333) << 2) | ((tmp & 0xCCCCCCCC) >> 2);
    tmp = ((tmp & 0x0F0F0F0F) << 4) | ((tmp & 0xF0F0F0F0) >> 4);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VREV8_V: {
    uint32_t tmp = (uint32_t)input.src2;
    tmp = ((tmp & 0x00FF00FFllu) << 8)  | ((tmp & 0xFF00FF00llu) >> 8);
    tmp = ((tmp & 0x0000FFFFllu) << 16) | ((tmp & 0xFFFF0000llu) >> 16);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VCLZ_V: {
    uint8_t i = 0;
    for (; i < 32; i++) {
      if (1 & ((uint32_t)input.src2 >> (31 - i))) {
        break;
      }
    }
    output.result = i;
    output.vxsat = 0;
    break;
  }
  case VCTZ_V: {
    uint8_t i = 0;
    for (; i < 32; i++) {
      if (1 & ((uint32_t)input.src2 >> i)){
        break;
      }
    }
    output.result = i;
    output.vxsat = 0;
    break;
  }
  case VCPOP_V: {
    uint8_t cnt = 0;
    for (int i = 0; i < 32; i++) {
      if (1 & ((uint32_t)input.src2) >> i) {
        cnt++;
      }
    }
    output.result = cnt;
    output.vxsat = 0;
    break;
  }
  case VROL_VV: {
    uint8_t mask = 0x1F;
    uint32_t lshift = (uint32_t)input.src1 & mask;
    uint32_t rshift = (-lshift) & mask;
    output.result = (uint32_t)(((uint32_t)input.src2 << lshift) | ((uint32_t)input.src2 >> rshift));
    output.vxsat = 0;
    break;
  }
  case VROR_VV: {
    uint8_t mask = 0x1F;
    uint32_t rshift = (uint32_t)input.src1 & mask;
    uint32_t lshift = (-rshift) & mask;
    output.result = (uint32_t)(((uint32_t)input.src2 << lshift) | ((uint32_t)input.src2 >> rshift));
    output.vxsat = 0;
    break;
  }
  case VWSLL_VV: {
    uint8_t shift = (uint16_t)input.src1 & 0x1F;
    output.result = (uint32_t)((uint16_t)input.src2 << shift);
    output.vxsat = 0;
    break;
  }
  default:
    printf("VIntegerV2 no fuOpType\n");
    break;
  }

  output.fflags = 0;
  
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  
  return output;
}
ElementOutput VGMIntegerALUF::calculation_e64(ElementInput input) {
  ElementOutput output;
  bool sat = false;

  switch (input.fuOpType) {
  case VADD_VV: {
    output.result = (uint64_t)((uint64_t)input.src1 + (uint64_t)input.src2);
    output.vxsat = 0;
    break;
  }
  case VSUB_VV: {
    output.result = (uint64_t)((uint64_t)input.src2 - (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
case VWADDU_VV: {
    uint64_t vs2 = (uint32_t)input.src2;
    uint64_t vs1 = (uint32_t)input.src1;
    output.result = (uint64_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUBU_VV: {
    uint64_t vs2 = (uint32_t)input.src2;
    uint64_t vs1 = (uint32_t)input.src1;
    output.result = (uint64_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADD_VV: {
    uint64_t vs2 = (int32_t)input.src2;
    uint64_t vs1 = (int32_t)input.src1;
    output.result = (uint64_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUB_VV: {
    uint64_t vs2 = (int32_t)input.src2;
    uint64_t vs1 = (int32_t)input.src1;
    output.result = (uint64_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADDU_WV: {
    uint64_t vs2 = (uint64_t)input.src2;
    uint64_t vs1 = (uint32_t)input.src1;
    output.result = (uint64_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUBU_WV: {
    uint64_t vs2 = (uint64_t)input.src2;
    uint64_t vs1 = (uint32_t)input.src1;
    output.result = (uint64_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VWADD_WV: {
    int64_t vs2 = (int64_t)input.src2;
    int64_t vs1 = (int32_t)input.src1;
    output.result = (uint64_t)(vs2 + vs1);
    output.vxsat = 0;
    break;
  }
  case VWSUB_WV: {
    int64_t vs2 = (int64_t)input.src2;
    int64_t vs1 = (int32_t)input.src1;
    output.result = (uint64_t)(vs2 - vs1);
    output.vxsat = 0;
    break;
  }
  case VZEXT_VF2: {
    output.result = (uint32_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VSEXT_VF2: {
    output.result = (uint64_t)(int32_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VZEXT_VF4: {
    output.result = (uint16_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VSEXT_VF4: {
    output.result = (uint64_t)(int16_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VZEXT_VF8: {
    output.result = (uint8_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VSEXT_VF8: {
    output.result = (uint64_t)(int8_t)input.src2;
    output.vxsat = 0;
    break;
  }
  case VADC_VVM: {
    output.result = (uint64_t)((uint64_t)input.src2 + (uint64_t)input.src1 + (uint64_t)input.src4);
    output.vxsat = 0;
    break;
  }
  case VMADC_VVM: {
    uint64_t vs2_low = (uint32_t)input.src2;
    uint64_t vs1_low = (uint32_t)input.src2;
    uint64_t vs2_high = ((uint64_t)input.src2 & 0xFFFFFFFF00000000) >> 32;
    uint64_t vs1_high = ((uint64_t)input.src1 & 0xFFFFFFFF00000000) >> 32;

    uint64_t res_low = vs2_low + vs1_low + (uint64_t)input.src4;

    bool bit32 = (res_low & 0x100000000) >> 32;

    uint64_t res_high = vs2_high + vs1_high + bit32;

    output.result = (uint64_t)((res_high >> 32) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VMADC_VV: {
    uint64_t vs2_low = (uint32_t)input.src2;
    uint64_t vs1_low = (uint32_t)input.src2;
    uint64_t vs2_high = ((uint64_t)input.src2 & 0xFFFFFFFF00000000) >> 32;
    uint64_t vs1_high = ((uint64_t)input.src1 & 0xFFFFFFFF00000000) >> 32;
    
    uint64_t res_low = vs2_low + vs1_low;
    bool bit32 = (res_low & 0x100000000) >> 32;

    uint64_t res_high = vs2_high + vs1_high + bit32;


    output.result = (uint64_t)((res_high >> 32) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VSBC_VVM: {
    output.result = (uint64_t)((uint64_t)input.src2 - (uint64_t)input.src1 - (uint64_t)input.src4);
    output.vxsat = 0;
    break;
  }
  case VMSBC_VVM: {
    uint64_t vs2_low = (uint32_t)input.src2;
    uint64_t vs1_low = (uint32_t)input.src2;
    uint64_t vs2_high = ((uint64_t)input.src2 & 0xFFFFFFFF00000000) >> 32;
    uint64_t vs1_high = ((uint64_t)input.src1 & 0xFFFFFFFF00000000) >> 32;

    uint64_t res_low = vs2_low - vs1_low - (uint64_t)input.src4;
    bool bit32 = (res_low & 0x100000000) >> 32;

    uint64_t res = vs2_high - vs1_high - bit32;

    output.result = (uint64_t)((res >> 32) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VMSBC_VV: {
    uint64_t vs2_low = (uint32_t)input.src2;
    uint64_t vs1_low = (uint32_t)input.src2;
    uint64_t vs2_high = ((uint64_t)input.src2 & 0xFFFFFFFF00000000) >> 32;
    uint64_t vs1_high = ((uint64_t)input.src1 & 0xFFFFFFFF00000000) >> 32;

    uint64_t res_low = vs2_low - vs1_low;
    bool bit32 = (res_low & 0x100000000) >> 32;

    uint64_t res = vs2_high - vs1_high - bit32;

    output.result = (uint64_t)((res >> 32) & 0x1u);
    output.vxsat = 0;
    break;
  }
  case VAND_VV: {
    output.result = input.src1 & input.src2;
    output.vxsat = 0;
    break;
  }
  case VOR_VV: {
    output.result = input.src1 | input.src2;
    output.vxsat = 0;
    break;
  }
  case VXOR_VV: {
    output.result = input.src1 ^ input.src2;
    output.vxsat = 0;
    break;
  }
  case VSLL_VV: {
    output.result = input.src2 << (input.src1 & 0x3f);
    output.vxsat = 0;
    break;
  }
  case VSRL_VV: {
    output.result = input.src2 >> (input.src1 & 0x3f);
    output.vxsat = 0;
    break;
  }
  case VSRA_VV: {
    output.result = (uint64_t)((int64_t)input.src2 >> ((uint64_t)input.src1 & 0x3f));
    output.vxsat = 0;
    break;
  }
case VMSEQ_VV: {
    output.result = (uint64_t)((uint64_t)input.src2 == (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSNE_VV: {
    output.result = (uint64_t)((uint64_t)input.src2 != (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLTU_VV: {
    output.result = (uint64_t)((uint64_t)input.src2 < (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLT_VV: {
    output.result = (uint64_t)((int64_t)input.src2 < (int64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLEU_VV: {
    output.result = (uint64_t)((uint64_t)input.src2 <= (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSLE_VV: {
    output.result = (uint64_t)((int64_t)input.src2 <= (int64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSGTU_VV: {
    output.result = (uint64_t)((uint64_t)input.src2 > (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMSGT_VV: {
    output.result = (uint64_t)((int64_t)input.src2 > (int64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMINU_VV: {
    output.result = (uint64_t)input.src2 <= (uint64_t)input.src1 ? (uint64_t)input.src2 : (uint64_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMIN_VV: {
    output.result = (int64_t)input.src2 <= (int64_t)input.src1 ? (uint64_t)input.src2 : (uint64_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMAXU_VV: {
    output.result = (uint64_t)input.src2 >= (uint64_t)input.src1 ? (uint64_t)input.src2 : (uint64_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VMAX_VV: {
    output.result = (int64_t)input.src2 >= (int64_t)input.src1 ? (uint64_t)input.src2 : (uint64_t)input.src1;
    output.vxsat = 0;
    break;
  }
  case VSADDU_VV: {
    uint64_t vs2 = input.src2;
    uint64_t vs1 = input.src1;
    uint64_t vd = vs2 + vs1;
    sat = vd < vs2;
    vd |= -sat;
    output.result = vd;
    output.vxsat = sat;
    break;
  }
  case VSADD_VV: {
    output.result = (uint64_t)sat_add<int64_t, uint64_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VSSUBU_VV: {
    output.result = (uint64_t)sat_subu<uint64_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VSSUB_VV: {
    output.result = (uint64_t)sat_sub<int64_t, uint64_t>(input.src2, input.src1, sat);
    output.vxsat = sat;
    break;
  }
  case VAADDU_VV: {
    uint64_t res = (uint64_t)input.src2 + (uint64_t)input.src1;
    INT_ROUNDING(res, input.rm_s, 1);
    output.result = (uint64_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VAADD_VV: {
    uint64_t vs2_low = (uint32_t)input.src2;
    uint64_t vs1_low = (uint32_t)input.src1;
    uint64_t vs2_high = ((uint64_t)input.src2 & 0xFFFFFFFF00000000) >> 32;
    uint64_t vs1_high = ((uint64_t)input.src1 & 0xFFFFFFFF00000000) >> 32;

    uint64_t res_low = vs2_low + vs1_low;

    bool bit32 = (res_low & 0x100000000) >> 32;
    uint64_t res_high = vs2_high + vs1_high + bit32;
    INT_ROUNDING(res_low, input.rm_s, 1);

    uint64_t res = ((res_high & 0xFFFFFFFF) << 32) | (res_low & 0xFFFFFFFF);
    output.result = (uint64_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VASUBU_VV: {
    uint64_t vs2_low = (uint32_t)input.src2;
    uint64_t vs1_low = (uint32_t)input.src1;
    uint64_t vs2_high = ((uint64_t)input.src2 & 0xFFFFFFFF00000000) >> 32;
    uint64_t vs1_high = ((uint64_t)input.src1 & 0xFFFFFFFF00000000) >> 32;

    uint64_t res_low = vs2_low - vs1_low;
    bool bit32 = (res_low & 0x100000000) >> 32;

    uint64_t res_high = vs2_high - vs1_high - bit32;

    INT_ROUNDING(res_low, input.rm_s, 1);
    int64_t res = ((res_high & 0xFFFFFFFF) << 32) | (res_low & 0xFFFFFFFF);

    output.result = (uint64_t)(res >> 1);
    output.vxsat = 0;
    break;
  }
  case VASUB_VV: {
    uint64_t vs2_low = (uint32_t)input.src2;
    uint64_t vs1_low = (uint32_t)input.src1;
    uint64_t vs2_high = ((uint64_t)input.src2 & 0xFFFFFFFF00000000) >> 32;
    uint64_t vs1_high = ((uint64_t)input.src1 & 0xFFFFFFFF00000000) >> 32;

    uint64_t res_low = vs2_low - vs1_low;
    bool bit32 = (res_low & 0x100000000) >> 32;

    uint64_t res_high = vs2_high - vs1_high - bit32;

    INT_ROUNDING(res_low, input.rm_s, 1);
    int64_t res = ((res_high & 0xFFFFFFFF) << 32) | (res_low & 0xFFFFFFFF);
    output.result = (uint64_t)(res >> 1);
    output.vxsat = 0;

    break;
  }
  case VSSRL_VV: {
    int8_t sh = input.src1 & 0x3f;
    uint64_t val = (uint64_t)input.src2;
    INT_ROUNDING(val, input.rm_s, sh);
    output.result = (uint64_t)(val >> sh);
    output.vxsat = 0;
    break;
  }
  case VSSRA_VV: {
    int8_t sh = input.src1 & 0x3f;
    int64_t val = (int64_t)input.src2;
    INT_ROUNDING(val, input.rm_s, sh);
    output.result = (uint64_t)((uint64_t)val >> sh);
    output.vxsat = 0;
    break;
  }
  case VMAND_MM: {
    output.result = (uint64_t)((uint64_t)input.src2 & (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMNAND_MM: {
    output.result = (uint64_t)(~((uint64_t)input.src2 & (uint64_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMANDN_MM: {
    output.result = (uint64_t)((uint64_t)input.src2 & (~(uint64_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMXOR_MM: {
    output.result = (uint64_t)((uint64_t)input.src2 ^ (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMOR_MM: {
    output.result = (uint64_t)((uint64_t)input.src2 | (uint64_t)input.src1);
    output.vxsat = 0;
    break;
  }
  case VMNOR_MM: {
    output.result = (uint64_t)(~((uint64_t)input.src2 | (uint64_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMORN_MM: {
    output.result = (uint64_t)((uint64_t)input.src2 | (~(uint64_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VMXNOR_MM: {
    output.result = (uint64_t)(~((uint64_t)input.src2 ^ (uint64_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VANDN_VV: {
    output.result = (uint64_t)((uint64_t)input.src2 & (~(uint64_t)input.src1));
    output.vxsat = 0;
    break;
  }
  case VBREV_V: {
    uint64_t tmp = (uint64_t)input.src2;
    tmp = ((tmp & 0x5555555555555555llu) << 1)  | ((tmp & 0xAAAAAAAAAAAAAAAAllu) >> 1);
    tmp = ((tmp & 0x3333333333333333llu) << 2)  | ((tmp & 0xCCCCCCCCCCCCCCCCllu) >> 2);
    tmp = ((tmp & 0x0F0F0F0F0F0F0F0Fllu) << 4)  | ((tmp & 0xF0F0F0F0F0F0F0F0llu) >> 4);
    tmp = ((tmp & 0x00FF00FF00FF00FFllu) << 8)  | ((tmp & 0xFF00FF00FF00FF00llu) >> 8);
    tmp = ((tmp & 0x0000FFFF0000FFFFllu) << 16) | ((tmp & 0xFFFF0000FFFF0000llu) >> 16);
    tmp = ((tmp & 0x00000000FFFFFFFFllu) << 32) | ((tmp & 0xFFFFFFFF00000000llu) >> 32);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VBREV8_V: {
    uint64_t tmp = (uint64_t)input.src2;
    tmp = ((tmp & 0x5555555555555555) << 1) | ((tmp & 0xAAAAAAAAAAAAAAAA) >> 1);
    tmp = ((tmp & 0x3333333333333333) << 2) | ((tmp & 0xCCCCCCCCCCCCCCCC) >> 2);
    tmp = ((tmp & 0x0F0F0F0F0F0F0F0F) << 4) | ((tmp & 0xF0F0F0F0F0F0F0F0) >> 4);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VREV8_V: {
    uint64_t tmp = (uint64_t)input.src2;
    tmp = ((tmp & 0x00FF00FF00FF00FFllu) << 8)  | ((tmp & 0xFF00FF00FF00FF00llu) >> 8);
    tmp = ((tmp & 0x0000FFFF0000FFFFllu) << 16) | ((tmp & 0xFFFF0000FFFF0000llu) >> 16);
    tmp = ((tmp & 0x00000000FFFFFFFFllu) << 32) | ((tmp & 0xFFFFFFFF00000000llu) >> 32);
    output.result = tmp;
    output.vxsat = 0;
    break;
  }
  case VCLZ_V: {
    uint8_t i = 0;
    for (; i < 64; i++) {
      if (1 & ((uint64_t)input.src2 >> (63 - i))) {
        break;
      }
    }
    output.result = i;
    output.vxsat = 0;
    break;
  }
  case VCTZ_V: {
    uint8_t i = 0;
    for (; i < 64; i++) {
      if (1 & ((uint64_t)input.src2 >> i)){
        break;
      }
    }
    output.result = i;
    output.vxsat = 0;
    break;
  }
  case VCPOP_V: {
    uint8_t cnt = 0;
    for (int i = 0; i < 64; i++) {
      if (1 & ((uint64_t)input.src2) >> i) {
        cnt++;
      }
    }
    output.result = cnt;
    output.vxsat = 0;
    break;
  }
  case VROL_VV: {
    uint8_t mask = 0x3F;
    uint64_t lshift = (uint64_t)input.src1 & mask;
    uint64_t rshift = (-lshift) & mask;
    output.result = (uint64_t)(((uint64_t)input.src2 << lshift) | ((uint64_t)input.src2 >> rshift));
    output.vxsat = 0;
    break;
  }
  case VROR_VV: {
    uint8_t mask = 0x3F;
    uint64_t rshift = (uint64_t)input.src1 & mask;
    uint64_t lshift = (-rshift) & mask;
    output.result = (uint64_t)(((uint64_t)input.src2 << lshift) | ((uint64_t)input.src2 >> rshift));
    output.vxsat = 0;
    break;
  }
  case VWSLL_VV: {
    uint64_t vs2 = (uint32_t)input.src2;
    uint32_t vs1 = (uint32_t)input.src1;
    uint8_t shift = vs1 & 0x3F;
    output.result = (uint64_t)(vs2 << shift);
    output.vxsat = 0;
    break;
  }
  default:
    printf("VIntegerV2 no fuOpType\n");
    break;
  }
  
  output.fflags = 0;
  
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  
  return output;
}
