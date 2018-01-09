//
//  CurveDonnaTests.swift
//  CurveDonnaTests
//
//  Created by User on 02.01.18.
//  Copyright © 2018 User. All rights reserved.
//

import XCTest
@testable import CurveDonna

class CurveDonnaTests: XCTestCase {

   func testSignExtension() {
        guard (-32 >> 1) == -16 else {
            XCTFail("No sign extension on negative numbers for '>>'")
            return
        }
    }

    func testTwoComplement() {
        // Write it like this to silence compiler warning
        func f() -> (Int, Int) {
            return (-1, 3)
        }
        let (a,b) = f()
        // This is just (-1 & 3) == 3
        guard (a & b) == 3 else {
            XCTFail("Not a two's complement system")
            return
        }
    }

    func testEquality() {
        let base = [9] + [UInt8](repeating: 0, count: 31)

        let privateKey: [UInt8] = [
            0xb0, 0x3b, 0x34, 0xc3, 0x3a,
            0x1c, 0x44, 0xf2, 0x25, 0xb6,
            0x62, 0xd2, 0xbf, 0x48, 0x59,
            0xb8, 0x13, 0x54, 0x11, 0xfa,
            0x7b, 0x03, 0x86, 0xd4, 0x5f,
            0xb7, 0x5d, 0xc5, 0xb9, 0x1b,
            0x44, 0x66]

        let key = [UInt8](repeating: 0, count: 32)
        curve25519_donna(UnsafeMutablePointer(mutating: key), privateKey, base)

        let key2 = curveDonna(secret: privateKey, basepoint: base)!

        guard key == key2 else {
            XCTFail()
            return
        }
    }

    /// Number of runs
    private let runs = 50

    /// Expected result after 50 runs
    private let result: [UInt8] = [11, 234, 43, 109, 169, 98, 63, 11, 180, 242, 200, 144, 14, 196, 128, 116, 87, 224, 104, 88, 30, 45, 64, 108, 202, 165, 185, 165, 158, 214, 202, 45]

    let base = [9] + [UInt8](repeating: 0, count: 31)

    let startSecret: [UInt8] = [
        0xb0, 0x3b, 0x34, 0xc3, 0x3a, 0x1c, 0x44, 0xf2,
        0x25, 0xb6, 0x62, 0xd2, 0xbf, 0x48, 0x59, 0xb8,
        0x13, 0x54, 0x11, 0xfa, 0x7b, 0x03, 0x86, 0xd4,
        0x5f, 0xb7, 0x5d, 0xc5, 0xb9, 0x1b, 0x44, 0x66]

    /**
     Measure the time of 50 calls to curve25519_donna()
    */
    func testMeasureC() {
        var secret = startSecret
        let key = [UInt8](repeating: 0, count: 32)
        self.measure {
            for _ in 0..<runs {
                curve25519_donna(UnsafeMutablePointer(mutating: key), secret, base)
                secret = key
            }
        }
        XCTAssert(secret == result)
    }

    /**
     Measure the time of 50 calls to curveDonna()
     */
    func testMeasureSwift() {
        var secret = startSecret

        self.measure {
            for _ in 0..<runs {
                secret = curveDonna(secret: secret, basepoint: base)!
            }
        }
        XCTAssert(secret == result)
    }
}
