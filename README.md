# CurveDonnaSwift
A swift implementation of the curve25519-donna C algorithm

## Motivation

I simply wanted to see how performant swift is when implementing very low level operations such as public key generation. 

## Speed
As it turns out, swift is not very fast for this sort of thing. 50 iterations of the C code only take about `0.015 seconds` compared to around `1.6 seconds` for my swift implementation. I've tried to limit allocations and copies by using `UnsafeMutablePointer` a lot, which improved speed by about 40%, but it's still *more than 100 times slower* than the C version.

## Usage

Simply copy the `Curve25519.swift` file to your project.

````swift
let publicKey = curveDonna(secret: secret, basepoint: base)!
````

Note: The function returns nil if `secret` and `basepoint` are less than 32 byte.

## Optimisation

If anyone has recommendations on how to make this code faster, I'd be glad to hear about them!
