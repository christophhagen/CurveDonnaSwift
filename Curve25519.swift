//
//  CurveOptimised2.swift
//  CurveDonna
//
//  Created by User on 05.01.18.
//  Copyright © 2018 User. All rights reserved.
//

import Foundation

//
//  CurveOptimised.swift
//  CurveDonna
//
//  Created by User on 03.01.18.
//  Copyright © 2018 User. All rights reserved.
//

import Foundation

private typealias Limb = Int64

/** Multiply two numbers: output = in2 * in
 *
 * output must be distinct to both inputs. The inputs are reduced coefficient
 * form, the output is not.
 *
 * output[x] <= 14 * the largest product of the input limbs. */
private func product(
    output: UnsafeMutablePointer<Limb>,
    input2 in2: UnsafePointer<Limb>,
    input in1: UnsafePointer<Limb>) {

    output[0] =  in2[0] &* in1[0]
    output[1] =  in2[0] &* in1[1] &+ in2[1] &* in1[0]
    output[2] = 2 &* in2[1] &* in1[1] &+
        in2[0] &* in1[2] &+ in2[2] &* in1[0]
    output[3] =  in2[1] &* in1[2] &+ in2[2] &* in1[1] &+
        in2[0] &* in1[3] &+ in2[3] &* in1[0]
    output[4] =  in2[2] &* in1[2] &+
        2 &* (in2[1] &* in1[3] &+
            in2[3] &* in1[1]) &+
        in2[0] &* in1[4] &+ in2[4] &* in1[0]
    output[5] =
        in2[2] &* in1[3] &+ in2[3] &* in1[2] &+
        in2[1] &* in1[4] &+ in2[4] &* in1[1] &+
        in2[0] &* in1[5] &+ in2[5] &* in1[0]
    output[6] =
        2 &* (in2[3] &* in1[3] &+
            in2[1] &* in1[5] &+ in2[5] &* in1[1]) &+
        in2[2] &* in1[4] &+ in2[4] &* in1[2] &+
        in2[0] &* in1[6] &+ in2[6] &* in1[0]
    output[7] =  in2[3] &* in1[4] &+ in2[4] &* in1[3] &+
        in2[2] &* in1[5] &+ in2[5] &* in1[2] &+
        in2[1] &* in1[6] &+ in2[6] &* in1[1] &+
        in2[0] &* in1[7] &+ in2[7] &* in1[0]
    // Split expression because it's too complex for the compiler
    let temp1 =  in2[4] &* in1[4] &+
        2 &* (in2[3] &* in1[5] &+ in2[5] &* in1[3] &+
            in2[1] &* in1[7] &+ in2[7] &* in1[1])
    let temp2 =  in2[2] &* in1[6] &+ in2[6] &* in1[2] &+
        in2[0] &* in1[8] &+ in2[8] &* in1[0]
    output[8] =  temp1 &+ temp2
    output[9] =
        in2[4] &* in1[5] &+ in2[5] &* in1[4] &+
        in2[3] &* in1[6] &+ in2[6] &* in1[3] &+
        in2[2] &* in1[7] &+ in2[7] &* in1[2] &+
        in2[1] &* in1[8] &+ in2[8] &* in1[1] &+
        in2[0] &* in1[9] &+ in2[9] &* in1[0]
    let temp3 =  2 &* (
        in2[5] &* in1[5] &+ in2[3] &* in1[7] &+
            in2[7] &* in1[3] &+ in2[1] &* in1[9] &+
            in2[9] &* in1[1])
    let temp4 =  in2[4] &* in1[6] &+ in2[6] &* in1[4] &+
        in2[2] &* in1[8] &+ in2[8] &* in1[2]
    output[10] = temp3 &+ temp4
    output[11] = in2[5] &* in1[6] &+ in2[6] &* in1[5] &+
        in2[4] &* in1[7] &+ in2[7] &* in1[4] &+
        in2[3] &* in1[8] &+ in2[8] &* in1[3] &+
        in2[2] &* in1[9] &+ in2[9] &* in1[2]
    let temp5 = 2 &* (in2[5] &* in1[7] &+ in2[7] &* in1[5] &+
        in2[3] &* in1[9] &+ in2[9] &* in1[3])
    let temp6 =  in2[4] &* in1[8] &+ in2[8] &* in1[4]
    output[12] = in2[6] &* in1[6] &+ temp5 &+ temp6
    output[13] =
        in2[6] &* in1[7] &+ in2[7] &* in1[6] &+
        in2[5] &* in1[8] &+ in2[8] &* in1[5] &+
        in2[4] &* in1[9] &+ in2[9] &* in1[4]
    output[14] = 2 &* (
        in2[7] &* in1[7] &+
            in2[5] &* in1[9] &+
            in2[9] &* in1[5]) &+
        in2[6] &* in1[8] &+
        in2[8] &* in1[6]
    output[15] = in2[7] &* in1[8] &+ in2[8] &* in1[7] &+
        in2[6] &* in1[9] &+ in2[9] &* in1[6]
    output[16] = in2[8] &* in1[8] &+
        2 &* (in2[7] &* in1[9] &+ in2[9] &* in1[7])
    output[17] = in2[8] &* in1[9] &+ in2[9] &* in1[8]
    output[18] = 2 &* in2[9] &* in1[9]
}

/** Reduce a long form to a short form by taking the input mod 2^255 - 19.
 *
 * On entry: |output[i]| < 14*2^54
 * On exit: |output[0..8]| < 280*2^54 */
private func reduceDegree(_ output: UnsafeMutablePointer<Limb>) {
    /* Each of these shifts and adds ends up multiplying the value by 19.
     *
     * For output[0..8], the absolute entry value is < 14*2^54 and we add, at
     * most, 19*14*2^54 thus, on exit, |output[0..8]| < 280*2^54. */
    output[8] = output[8] &+ (output[18] << 4) &+ (output[18] << 1) &+ output[18]
    output[7] = output[7] &+ (output[17] << 4) &+ (output[17] << 1) &+ output[17]
    output[6] = output[6] &+ (output[16] << 4) &+ (output[16] << 1) &+ output[16]
    output[5] = output[5] &+ (output[15] << 4) &+ (output[15] << 1) &+ output[15]
    output[4] = output[4] &+ (output[14] << 4) &+ (output[14] << 1) &+ output[14]
    output[3] = output[3] &+ (output[13] << 4) &+ (output[13] << 1) &+ output[13]
    output[2] = output[2] &+ (output[12] << 4) &+ (output[12] << 1) &+ output[12]
    output[1] = output[1] &+ (output[11] << 4) &+ (output[11] << 1) &+ output[11]
    output[0] = output[0] &+ (output[10] << 4) &+ (output[10] << 1) &+ output[10]
}

/** return v / 2^26, using only shifts and adds.
 *
 * On entry: v can take any value. */
@inline(__always) private func divideBy2_26(_ v: Limb) -> Limb {
    /* High word of v no shift needed. */
    let highword = UInt32(UInt64(bitPattern: v) >> 32)
    /* Set to all 1s if v was negative else set to 0s. */
    let sign = Int32(bitPattern: highword) >> 31
    /* Set to 0x3ffffff if v was negative else set to 0. */
    let roundoff = UInt32(bitPattern: sign) >> 6
    /* Should return v / (1<<26) */
    return (v &+ Limb(roundoff)) >> 26
}

/* return v / (2^25), using only shifts and adds.
 *
 * On entry: v can take any value. */
@inline(__always) private func divideBy2_25(_ v: Limb) -> Limb {
    /* High word of v no shift needed */
    let highword = UInt32(UInt64(bitPattern: v) >> 32)
    /* Set to all 1s if v was negative else set to 0s. */
    let sign = Int32(bitPattern: highword) >> 31
    /* Set to 0x1ffffff if v was negative else set to 0. */
    let roundoff = UInt32(bitPattern: sign) >> 7
    /* Should return v / (1<<25) */
    return (v &+ Limb(roundoff)) >> 25
}

/** Reduce all coefficients of the short form input so that |x| < 2^26.
 *
 * On entry: |output[i]| < 280*2^54 */
private func reduceCoefficients(_ output: UnsafeMutablePointer<Limb>) {
    output[10] = 0

    for i in stride(from: 0, to: 10, by: 2) {
        let over = divideBy2_26(output[i])
        /* The entry condition (that |output[i]| < 280*2^54) means that over is, at
         * most, 280*2^28 in the first iteration of this loop. This is added to the
         * next limb and we can approximate the resulting bound of that limb by
         * 281*2^54. */
        output[i] = output[i] &- (over << 26)
        output[i+1] = output[i+1] &+ over

        /* For the first iteration, |output[i+1]| < 281*2^54, thus |over| <
         * 281*2^29. When this is added to the next limb, the resulting bound can
         * be approximated as 281*2^54.
         *
         * For subsequent iterations of the loop, 281*2^54 remains a conservative
         * bound and no overflow occurs. */
        let over2 = divideBy2_25(output[i+1])
        output[i+1] = output[i+1] &- (over2 << 25)
        output[i+2] = output[i+2] &+ over2
    }
    /* Now |output[10]| < 281*2^29 and all other coefficients are reduced. */
    output[0] = output[0] &+ (output[10] << 4) &+ (output[10] << 1) &+ output[10]
    output[10] = 0

    /* Now output[1..9] are reduced, and |output[0]| < 2^26 + 19*281*2^29
     * So |over| will be no more than 2^16. */
    let over = divideBy2_26(output[0])
    output[0] -= over << 26
    output[1] += over

    /* Now output[0,2..9] are reduced, and |output[1]| < 2^25 + 2^16 < 2^26. The
     * bound on |output[1]| is sufficient to meet our needs. */
}

/** A helpful wrapper around fproduct: output = in * in2.
 *
 * On entry: |in[i]| < 2^27 and |in2[i]| < 2^27.
 *
 * output must be distinct to both inputs. The output is reduced degree
 * (indeed, one need only provide storage for 10 limbs) and |output[i]| < 2^26.
 * output needs 19 limbs of storage for this version
 */
private func multiply(
    output: UnsafeMutablePointer<Limb>,
    input1 in1: UnsafePointer<Limb>,
    input2 in2: UnsafePointer<Limb>) {

    product(output: output, input2: in1, input: in2)
    /* |t[i]| < 14*2^54 */
    reduceDegree(output)
    reduceCoefficients(output)
    /* |t[i]| < 2^26 */
}

/** Square a number: output = in**2
 *
 * output must be distinct from the input. The inputs are reduced coefficient
 * form, the output is not.
 *
 * output[x] <= 14 * the largest product of the input limbs. */
private func squareInner(
    _ output: UnsafeMutablePointer<Limb>,
    _ input: UnsafePointer<Limb>) {
    output[0] = input[0] &* input[0]
    output[1] = 2 &*  input[0] &* input[1]
    output[2] = 2 &* (input[1] &* input[1] &+ input[0] &* input[2])
    output[3] = 2 &* (input[1] &* input[2] &+ input[0] &* input[3])
    output[4] = input[2] &* input[2] &+ 4 &* input[1] &* input[3] &+
        2 &*  input[0] &* input[4]
    output[5] =  2 &* (input[2] &* input[3] &+ input[1] &* input[4] &+
        input[0] &* input[5])
    output[6] =  2 &* (input[3] &* input[3] &+ input[2] &* input[4] &+
        input[0] &* input[6] &+ 2 &*  input[1] &* input[5])
    output[7] =  2 &* (input[3] &* input[4] &+ input[2] &* input[5] &+
        input[1] &* input[6] &+ input[0] &* input[7])
    output[8] = input[4] &* input[4] &+ 2 &* (input[2] &* input[6] &+
        input[0] &* input[8] &+ 2 &* (input[1] &* input[7] &+
            input[3] &* input[5]))
    output[9] =  2 &* (input[4] &* input[5] &+
        input[3] &* input[6] &+ input[2] &* input[7] &+
        input[1] &* input[8] &+ input[0] &* input[9])
    output[10] = 2 &* (input[5] &* input[5] &+
        input[4] &* input[6] &+ input[2] &* input[8] &+
        2 &* (input[3] &* input[7] &+ input[1] &* input[9]))
    output[11] = 2 &* (input[5] &* input[6] &+ input[4] &* input[7] &+
        input[3] &* input[8] &+ input[2] &* input[9])
    output[12] = input[6] &* input[6] &+ 2 &* (input[4] &* input[8] &+
        2 &* (input[5] &* input[7] &+ input[3] &* input[9]))
    output[13] = 2 &* (input[6] &* input[7] &+ input[5] &* input[8] &+
        input[4] &* input[9])
    output[14] = 2 &* (input[7] &* input[7] &+ input[6] &* input[8] &+
        2 &*  input[5] &* input[9])
    output[15] = 2 &* (input[7] &* input[8] &+ input[6] &* input[9])
    output[16] = input[8] &* input[8] &+ 4 &*  input[7] &* input[9]
    output[17] = 2 &*  input[8] &* input[9]
    output[18] = 2 &*  input[9] &* input[9]
}

/** fsquare sets output = in^2.
 *
 * On entry: The |in| argument is in reduced coefficients form and |in[i]| <
 * 2^27.
 *
 * On exit: The |output| argument is in reduced coefficients form (indeed, one
 * need only provide storage for 10 limbs) and |out[i]| < 2^26.
 * Ouput needs 19 limbs of storage in this implementation */
private func square(
    output: UnsafeMutablePointer<Limb>,
    _ input: UnsafePointer<Limb>) {

    squareInner(output, input)
    /* |t[i]| < 14*2^54 because the largest product of two limbs will be <
     * 2^(27+27) and fsquare_inner adds together, at most, 14 of those
     * products. */
    reduceDegree(output)
    reduceCoefficients(output)
    /* |t[i]| < 2^26 */
}

/** s32_eq returns 0xffffffff iff a == b and zero otherwise. */
private func equal(_ a: Int32, _ b: Int32) -> Int32 {
    var c = ~(a ^ b)
    c &= c << 16
    c &= c << 8
    c &= c << 4
    c &= c << 2
    c &= c << 1
    return c >> 31
}

/** s32_gte returns 0xffffffff if a >= b and zero otherwise, where a and b are
 * both non-negative. */
private func greater(_ a: Int32, _ b: Int32) -> Int32 {
    let c = a &- b
    /* a >= 0 iff a >= b. */
    return ~(c >> 31)
}

/** Take a fully reduced polynomial form number and contract it into a
 * little-endian, 32-byte array.
 *
 * On entry: |input_limbs[i]| < 2^26 */
private func contract(_ inputLimbs: [Limb]) -> [UInt8] {
    /* |input_limbs[i]| < 2^26, so it's valid to convert to an s32. */
    var input = inputLimbs.map { Int32($0) }

    for j in 0..<2 {
        for i in 0..<9 {
            let mask = input[i] >> 31
            if ((i & 1) == 1) {
                /* This calculation is a time-invariant way to make input[i]
                 * non-negative by borrowing from the next-larger limb. */
                let carry = -((input[i] & mask) >> 25)
                input[i] = input[i] &+ (carry << 25)
                input[i+1] = input[i+1] &- carry
            } else {
                let carry = -((input[i] & mask) >> 26)
                input[i] = input[i] &+ (carry << 26)
                input[i+1] = input[i+1] &- carry
            }
        }

        /* There's no greater limb for input[9] to borrow from, but we can multiply
         * by 19 and borrow from input[0], which is valid mod 2^255-19. */
        do {
            let mask = input[9] >> 31
            let carry = -((input[9] & mask) >> 25)
            input[9] = input[9] &+ (carry << 25)
            input[0] = input[0] &- (carry * 19)
        }

        /* After the first iteration, input[1..9] are non-negative and fit within
         * 25 or 26 bits, depending on position. However, input[0] may be
         * negative. */
    }

    /* The first borrow-propagation pass above ended with every limb
     except (possibly) input[0] non-negative.

     If input[0] was negative after the first pass, then it was because of a
     carry from input[9]. On entry, input[9] < 2^26 so the carry was, at most,
     one, since (2**26-1) >> 25 = 1. Thus input[0] >= -19.

     In the second pass, each limb is decreased by at most one. Thus the second
     borrow-propagation pass could only have wrapped around to decrease
     input[0] again if the first pass left input[0] negative *and* input[1]
     through input[9] were all zero.  In that case, input[1] is now 2^25 - 1,
     and this last borrow-propagation step will leave input[1] non-negative. */
    do {
        let mask = input[0] >> 31
        let carry = -((input[0] & mask) >> 26)
        input[0] = input[0] &+ (carry << 26)
        input[1] = input[1] &- carry
    }

    /* All input[i] are now non-negative. However, there might be values between
     * 2^25 and 2^26 in a limb which is, nominally, 25 bits wide. */
    for j in 0..<2 {
        for i in 0..<9 {
            if ((i & 1) == 1) {
                let carry = input[i] >> 25
                input[i] &= 0x1ffffff
                input[i+1] += carry
            } else {
                let carry = input[i] >> 26
                input[i] &= 0x3ffffff
                input[i+1] += carry
            }
        }

        do {
            let carry = input[9] >> 25
            input[9] &= 0x1ffffff
            input[0] += 19*carry
        }
    }

    /* If the first carry-chain pass, just above, ended up with a carry from
     * input[9], and that caused input[0] to be out-of-bounds, then input[0] was
     * < 2^26 + 2*19, because the carry was, at most, two.
     *
     * If the second pass carried from input[9] again then input[0] is < 2*19 and
     * the input[9] -> input[0] carry didn't push input[0] out of bounds. */

    /* It still remains the case that input might be between 2^255-19 and 2^255.
     * In this case, input[1..9] must take their maximum value and input[0] must
     * be >= (2^255-19) & 0x3ffffff, which is 0x3ffffed. */
    var mask = greater(input[0], 0x3ffffed)
    for i in 0..<10 {
        if ((i & 1) == 1) {
            mask &= greater(input[i], 0x1ffffff)
        } else {
            mask &= greater(input[i], 0x3ffffff)
        }
    }

    /* mask is either 0xffffffff (if input >= 2^255-19) and zero otherwise. Thus
     * this conditionally subtracts 2^255-19. */
    input[0] -= mask & 0x3ffffed

    for i in 0..<10 {
        if ((i & 1) == 1) {
            input[i] -= mask & 0x1ffffff
        } else {
            input[i] -= mask & 0x3ffffff
        }
    }

    input[1] <<= 2
    input[2] <<= 3
    input[3] <<= 5
    input[4] <<= 6
    input[6] <<= 1
    input[7] <<= 3
    input[8] <<= 4
    input[9] <<= 6

    var output = [UInt8](repeating: 0, count: 32)
    @inline(__always) func F(_ inIndex: Int, _ outIndex: Int) {
        output[outIndex+0] |= UInt8(input[inIndex] & 0xff)
        output[outIndex+1]  = UInt8((input[inIndex] >> 8) & 0xff)
        output[outIndex+2]  = UInt8((input[inIndex] >> 16) & 0xff)
        output[outIndex+3]  = UInt8((input[inIndex] >> 24) & 0xff)
    }
    output[0] = 0
    // TODO: Is this useless?
    output[16] = 0
    F(0,0)
    F(1,3)
    F(2,6)
    F(3,9)
    F(4,12)
    F(5,16)
    F(6,19)
    F(7,22)
    F(8,25)
    F(9,28)
    return output
}

// -----------------------------------------------------------------------------
// Shamelessly copied from djb's code
// -----------------------------------------------------------------------------
// output needs 19 limbs of storage, not 10
private func recip(output: UnsafeMutablePointer<Limb>,
                   z: UnsafeMutablePointer<Limb>) {

    let temp1 = UnsafeMutablePointer<Limb>.allocate(capacity: 19 * 5)
    let temp2 = temp1.advanced(by: 19)
    let temp3 = temp1.advanced(by: 19*2)
    let temp4 = temp1.advanced(by: 19*3)
    let temp5 = temp1.advanced(by: 19*4)

    square(output: temp2, z)  /* 2 */
    // temp2 holds z2
    square(output: temp1, temp2) /* 4 */
    square(output: output, temp1) /* 8 */
    multiply(output: temp3, input1: output, input2: z) /* 9 */
    // temp3 holds z9
    multiply(output: temp4, input1: temp3, input2: temp2) /* 11 */
    // Temp2 no longer needed here, will become z2100
    square(output: output, temp4) /* 22 */
    multiply(output: temp5, input1: output, input2: temp3) /* 2^5 - 2^0 = 31 */
    // Temp3 no longer needed here, will become z2200

    square(output: output, temp5) /* 2^6 - 2^1 */
    square(output: temp1, output) /* 2^7 - 2^2 */
    square(output: output, temp1) /* 2^8 - 2^3 */
    square(output: temp1, output) /* 2^9 - 2^4 */
    square(output: output, temp1) /* 2^10 - 2^5 */
    multiply(output: temp2, input1: output, input2: temp5) /* 2^10 - 2^0 */
    // temp5 no longer needed, will become z2500

    square(output: output, temp2) /* 2^11 - 2^1 */
    square(output: temp1, output)/* 2^12 - 2^2 */
    for _ in 0..<4 { /* from 2 to 10 by 2 */
        square(output: output, temp1)
        square(output: temp1, output)
    } /* 2^20 - 2^10 */
    multiply(output: temp3, input1: temp1, input2: temp2) /* 2^20 - 2^0 */

    square(output: output, temp3) /* 2^21 - 2^1 */
    square(output: temp1, output) /* 2^22 - 2^2 */
    for _ in 0..<9 { /* From 2 to 20 by 2 */
        square(output: output, temp1)
        square(output: temp1, output)
    } /* 2^40 - 2^20 */
    multiply(output: output, input1: temp1, input2: temp3) /* 2^40 - 2^0 */
    // temp3 no longer needed here, will become z21000

    square(output: temp1, output) /* 2^41 - 2^1 */
    square(output: output, temp1) /* 2^42 - 2^2 */
    for _ in 0..<4 { /* from 2 to 10 by 2 */
        square(output: temp1, output)
        square(output: output, temp1)
    } /* 2^50 - 2^10 */
    multiply(output: temp5, input1: output, input2: temp2) /* 2^50 - 2^0 */
    // temp2 no longer needed here

    square(output: output, temp5) /* 2^51 - 2^1 */
    square(output: temp1, output) /* 2^52 - 2^2 */
    for _ in 0..<24 { /* From 2 to 50 by 2 */
        square(output: output, temp1)
        square(output: temp1, output)
    } /* 2^100 - 2^50 */
    multiply(output: temp3, input1: temp1, input2: temp5) /* 2^100 - 2^0 */

    square(output: temp1, temp3) /* 2^101 - 2^1 */
    square(output: output, temp1) /* 2^102 - 2^2 */
    for _ in 0..<49 { /* From 2 to 100 by 2 */
        square(output: temp1, output)
        square(output: output, temp1)
    } /* 2^200 - 2^100 */
    multiply(output: temp1, input1: output, input2: temp3) /* 2^200 - 2^0 */
    // temp3 no longer needed here

    square(output: output, temp1) /* 2^201 - 2^1 */
    square(output: temp1, output) /* 2^202 - 2^2 */
    for _ in 0..<24 { /* From 2 to 50 by 2 */
        square(output: output, temp1)
        square(output: temp1, output)
    } /* 2^250 - 2^50 */
    multiply(output: output, input1: temp1, input2: temp5) /* 2^250 - 2^0 */
    // temp5 no longer needed here

    square(output: temp1, output) /* 2^251 - 2^1 */
    square(output: output, temp1) /* 2^252 - 2^2 */
    square(output: temp1, output) /* 2^253 - 2^3 */
    square(output: output, temp1) /* 2^254 - 2^4 */
    square(output: temp1, output) /* 2^255 - 2^5 */
    // output no longer needed here

    multiply(output: output, input1: temp1, input2: temp4) /* 2^255 - 21 */

    temp1.deallocate(capacity: 19 * 5)
}

/**
 Calculate the public key from a 32 byte private key and a 32 byte basepoint.
 - parameter secret: The private key (32 byte)
 - parameter basepoint: The basepoint for the elliptic curve
 - returns: The public key, or nil if the input has invalid size
 */
func curveDonna(secret: [UInt8], basepoint: [UInt8]) -> [UInt8]? {
    guard secret.count >= 32, basepoint.count >= 32 else {
        return nil
    }

    /////////////////
    //    Expand
    /////////////////
    let storage = UnsafeMutablePointer<Limb>.allocate(capacity: 200)

    let bp = storage /* 10 Limbs (10 total) */

    /* Take a little-endian, 32-byte number and expand it into polynomial form */
    @inline(__always) func F(_ index: Int, _ start: Int, _ shift: Int, _ mask: Int64) {
        var temp = Limb(basepoint[start])
        temp |= Limb(basepoint[start+1]) << 8
        temp |= Limb(basepoint[start+2]) << 16
        temp |= Limb(basepoint[start+3]) << 24
        bp[index] = (temp >> shift) & mask

    }
    F(0, 0, 0, 0x3ffffff)
    F(1, 3, 2, 0x1ffffff)
    F(2, 6, 3, 0x3ffffff)
    F(3, 9, 5, 0x1ffffff)
    F(4, 12, 6, 0x3ffffff)
    F(5, 16, 0, 0x1ffffff)
    F(6, 19, 1, 0x3ffffff)
    F(7, 22, 3, 0x1ffffff)
    F(8, 25, 4, 0x3ffffff)
    F(9, 28, 6, 0x1ffffff)

    
    /////////////////
    //    Mult
    /////////////////
    /* Calculates nQ where Q is the x-coordinate of a point on the curve
     *
     *   resultx/resultz: the x coordinate of the resulting curve point (short form)
     *   n: a little endian, 32-byte number
     *   q: a point of the curve (short form) */


    var nqpqz = storage.advanced(by: 10) /* 19 Limbs (29 total) */
    nqpqz.initialize(to: 0, count: 19*3) /* 4 long forms */
    nqpqz[0] = 1 /* init to [1, 0, 0, ...] */

    var x = nqpqz.advanced(by: 19) /* 19 Limbs (48 total) */
    x[0] = 1 /* init to [1, 0, 0, ...] */

    var z = x.advanced(by:  19) /* 19 Limbs, init to 0 (67 total) */

    var nqpqx2 = z.advanced(by: 19) /* 19 Limbs (86 total) */
    var nqpqz2 = nqpqx2.advanced(by: 19) /* 19 Limbs (105 total) */
    var nqx2   = nqpqz2.advanced(by: 19) /* 19 Limbs (124 total) */
    var nqz2   = nqx2.advanced(by: 19) /* 19 Limbs (143 total) */

    var nqpqx = nqz2.advanced(by: 19) /* 19 Limbs (162 total) */
    nqpqx.assign(from: bp, count: 10) /* Copy bp */
    nqpqx.advanced(by: 10).initialize(to: 0, count: 9) /* Rest is 0 */

    // 3 * 19 limbs storage for monty()
    let temp1 = nqpqx.advanced(by: 19) /* 19 Limbs (162 total) */
    let temp2 = temp1.advanced(by: 19) /* 19 Limbs (181 total) */
    let temp3 = temp2.advanced(by: 19) /* 19 Limbs (200 total) */

    var t: UnsafeMutablePointer<Limb>

    /* Conditionally swap two reduced-form limb arrays if 'iswap' is 1, but leave
     * them unchanged if 'iswap' is 0.  Runs in data-invariant time to avoid
     * side-channel attacks.
     *
     * NOTE that this function requires that 'iswap' be 1 or 0 other values give
     * wrong results.  Also, the two limb arrays must be in reduced-coefficient,
     * reduced-degree form: the values in a[10..19] or b[10..19] aren't swapped,
     * and all values in a[0..9],b[0..9] must have magnitude less than
     * INT32_MAX. */
    @inline(__always) func swapConditional(a: UnsafeMutablePointer<Limb>, b: UnsafeMutablePointer<Limb>, iswap: Int32) {
        let swap = -iswap

        for i in 0..<10 {
            let x = swap & (Int32(a[i]) ^ Int32(b[i]))
            a[i] = Limb(Int32(a[i]) ^ x)
            b[i] = Limb(Int32(b[i]) ^ x)
        }
    }

    for i in 0..<32 {
        var byte = secret[31 - i]
        for _  in 0..<8 {
            let bit = Int32(byte >> 7)

            swapConditional(a: x, b: nqpqx, iswap: bit)
            swapConditional(a: z, b: nqpqz, iswap: bit)

            /* Input: Q, Q', Q-Q'
             * Output: 2Q, Q+Q'
             *
             *   x2 z3: long form
             *   x3 z3: long form
             *   x z: short form, destroyed
             *   xprime zprime: short form, destroyed
             *   qmqp: short form, preserved
             *
             * On entry and exit, the absolute value of the limbs of all inputs and outputs
             * are < 2^26. */

            /* Sum two numbers: output = output + input */
            @inline(__always) func sum(
                _ output: UnsafeMutablePointer<Limb>,
                _ input: UnsafeMutablePointer<Limb>) {
                for i in 0..<10 {
                    output[i] = output[i] &+ input[i]
                }
            }

            /* Find the difference of two numbers: output = in - output
             * (note the order of the arguments!). */
            @inline(__always) func difference(
                _ output: UnsafeMutablePointer<Limb>,
                _ input: UnsafeMutablePointer<Limb>) {
                for i in 0..<10 {
                    output[i] = input[i] &- output[i]
                }
            }

            /* Multiply a number by a scalar: output = in * scalar */
            @inline(__always) func scalarProduct(
                output: UnsafeMutablePointer<Limb>,
                input: UnsafeMutablePointer<Limb>,
                scalar: Limb) {
                for i in 0..<10 {
                    output[i] = input[i] &* scalar
                }
            }

            nqpqx2.assign(from: x, count: 10)
            sum(x, z) /* |x[i]| < 2^27 */
            difference(z, nqpqx2) /* nqpqx2 == origx */ /* does x - z */ /* |dxz[i]| < 2^27 */
            // nqpqx2 no longer needed, will become origxprime

            nqpqx2.assign(from: nqpqx, count: 10) /* nqpqx == xprime */
            sum(nqpqx, nqpqz) /* nqpqz == zprime */ /* |nqpqx[i]| < 2^27 */
            difference(nqpqz, nqpqx2) /* |nqpqz[i]| < 2^27 */
            product(output: temp1, input2: nqpqx, input: z) /* temp1 == xxprime */
            /* |temp1[i]| < 14*2^54: the largest product of two limbs will be <
             * 2^(27+27) and fproduct adds together, at most, 14 of those products.
             * (Approximating that to 2^58 doesn't work out.) */
            product(output: nqpqz2, input2: x, input: nqpqz) /* nqpqz2 == zzprime */ /* |z3[i]| < 14*2^54 */
            reduceDegree(temp1)
            reduceCoefficients(temp1) /* |temp1[i]| < 2^26 */
            reduceDegree(nqpqz2)
            reduceCoefficients(nqpqz2) /* |nqpqz2[i]| < 2^26 */

            nqpqx2.assign(from: temp1, count: 10) /* nqpqx2 == origxprime */
            sum(temp1, nqpqz2) /* |temp1[i]| < 2^27 */
            difference(nqpqz2, nqpqx2) /* |nqpqz2[i]| < 2^27 */

            square(output: nqpqx2, temp1) /* nqpqx2 == xxxprime, |nqpqx2[i]| < 2^26 */
            // FINAL: nqpqx2
            square(output: temp1, nqpqz2) /* temp1 == zzzprime, |temp1[i]| < 2^26 */

            product(output: nqpqz2, input2: temp1, input: bp) /* |nqpqz2[i]| < 14*2^52 */
            // temp 1 no longer needed, will become zzz
            reduceDegree(nqpqz2)
            reduceCoefficients(nqpqz2) /* |nqpqz2[i]| < 2^26 */
            // FINAL: nqpqz2

            square(output: temp2, x) /* temp2 == xx, |temp2[i]| < 2^26 */
            // x no longer needed
            square(output: temp3, z) /* temp3 == zz, |temp3[i]| < 2^26 */
            // z no longer needed

            product(output: nqx2, input2: temp2, input: temp3) /* |x2[i]| < 14*2^52 */
            reduceDegree(nqx2)
            reduceCoefficients(nqx2) /* |x2[i]| < 2^26 */
            // FINAL: nqx2

            difference(temp3, temp2) /* temp3 == zz, |temp3[i]| < 2^27 */
            scalarProduct(output: temp1, input: temp3, scalar: 121665) /* temp1 == zzz, |temp1[i]| < 2^(27+17) */
            /* No need to call freduce_degree here:
             fscalar_product doesn't increase the degree of its input. */
            reduceCoefficients(temp1)
            /* |temp1[i]| < 2^26 */
            sum(temp1, temp2) /* |temp1[i]| < 2^27 */
            // temp2 no longer needed

            product(output: nqz2, input2: temp3, input: temp1) /* |z2[i]| < 14*2^(26+27) */
            // temp3 no longer needed
            reduceDegree(nqz2)
            reduceCoefficients(nqz2)
            /* |z2|i| < 2^26 */
            // FINAL: z2

            swapConditional(a: nqx2, b: nqpqx2, iswap: bit)
            swapConditional(a: nqz2, b: nqpqz2, iswap: bit)

            // Swap x with nqx2, z with nqz2
            t = x; x = nqx2; nqx2 = t
            t = z; z = nqz2; nqz2 = t
            // Swap nqpqx with nqpqx2, nqpqz with nqpqz2
            t = nqpqx; nqpqx = nqpqx2; nqpqx2 = t
            t = nqpqz; nqpqz = nqpqz2; nqpqz2 = t

            byte <<= 1
        }
    }

    /////////////////
    //    recip
    /////////////////
    recip(output: storage, z: z)

    multiply(output: z, input1: x, input2: storage)
    let zArray = Array(UnsafeBufferPointer(start: z, count: 10))
    let mypublic = contract(zArray)

    // Deallocate all used storage
    storage.deallocate(capacity: 200)

    return mypublic
}

