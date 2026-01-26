#include "../include/gm_common.h"
#include <type_traits>

// bool VGMIntegerALU::bad_fuType(VecInput input) {
//   return false;
// }

// bool VGMIntegerALU::bad_fuOpType(VecInput input) {
//   return false;
// }


namespace {
template <typename T>
ElementOutput calculation_integer_alu(ElementInput input) {
  using UT = typename std::make_unsigned<T>::type;
  using ST = typename std::make_signed<T>::type;
  constexpr int kBits = sizeof(T) * 8;
  const UT src1 = static_cast<UT>(input.src1);
  const UT src2 = static_cast<UT>(input.src2);
  const UT src4 = static_cast<UT>(input.src4);
  const int shift = static_cast<int>(src1 & static_cast<UT>(kBits - 1));

  UT result = 0;
  switch(input.fuOpType) {
    case VADD:
      result = static_cast<UT>(src1 + src2); break;
    case VADC:
      result = static_cast<UT>(src1 + src2 + src4); break;
    case VSUB:
      result = static_cast<UT>(src2 - src1); break;
    case VSBC:
      result = static_cast<UT>(src2 - src1 - src4); break;
    case VMAXU:
      result = (src1 >= src2) ? src1 : src2; break;
    case VMINU:
      result = (src1 <= src2) ? src1 : src2; break;
    case VMAX:
      result = (static_cast<ST>(src1) >= static_cast<ST>(src2)) ? src1 : src2; break;
    case VMIN:
      result = (static_cast<ST>(src1) <= static_cast<ST>(src2)) ? src1 : src2; break;
    case VAND:
      result = static_cast<UT>(src1 & src2); break;
    case VNAND:
      result = static_cast<UT>(~(src1 & src2)); break;
    case VOR:
      result = static_cast<UT>(src1 | src2); break;
    case VNOR:
      result = static_cast<UT>(~(src1 | src2)); break;
    case VXOR:
      result = static_cast<UT>(src1 ^ src2); break;
    case VXNOR:
      result = static_cast<UT>(~(src1 ^ src2)); break;
    case VSLL:
      result = static_cast<UT>(src2 << shift); break;
    case VSRL:
      result = static_cast<UT>(src2 >> shift); break;
    case VSRA:
      result = static_cast<UT>(static_cast<ST>(src2) >> shift); break;
    case VSSRL: {
      __uint128_t val = static_cast<UT>(src2);
      INT_ROUNDING(val, input.rm_s, shift);
      result = static_cast<UT>(val >> shift);
      break;
    }
    case VSSRA: {
      __int128_t val = static_cast<ST>(src2);
      INT_ROUNDING(val, input.rm_s, shift);
      result = static_cast<UT>(val >> shift);
      break;
    }
    case VRSUB:
      result = static_cast<UT>(src1 - src2); break;
    default:
      break;
  }

  ElementOutput output;
  output.result = static_cast<uint64_t>(result);
  output.fflags = 0;
  return output;
}
} // namespace

ElementOutput VGMIntegerALU::calculation_e8(ElementInput input) {
  ElementOutput output = calculation_integer_alu<uint8_t>(input);
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e16(ElementInput input) {
  ElementOutput output = calculation_integer_alu<uint16_t>(input);
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e32(ElementInput input) {
  ElementOutput output = calculation_integer_alu<uint32_t>(input);
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e64(ElementInput input) {
  ElementOutput output = calculation_integer_alu<uint64_t>(input);
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}