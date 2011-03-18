{-
Copyright (C) 2011  Marek Baczyñski

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
-}


module CardTests where

import Test.HUnit
import Card


test_high_card :: Test
test_high_card = TestCase (do
    h <- return (getHandForString "2c")
    assertEqual "high 2" (HighCard Two) h
    h <- return (getHandForString "2c Ac")
    assertEqual "high 2A" (HighCard Ace) h
    h <- return (getHandForString "2c 3h Ad")
    assertEqual "high 23A" (HighCard Ace) h
    h <- return (getHandForString "5c 6c 7c 8h Kd Qd Js")
    assertEqual "high 5678KQJ" (HighCard King) h
    )

test_pair ::  Test
test_pair = TestCase (do
    h <- return (getHandForString "2c 2d")
    assertEqual "pair 22" (Pair Two) h
    h <- return (getHandForString "2c 3h 2d")
    assertEqual "pair 232" (Pair Two) h
    )

test_two_pair ::  Test
test_two_pair = TestCase (do 
    h <- return (getHandForString "2c 5d 2c 5d")
    assertEqual "two pair 2525" (TwoPair Five Two) h
    h <- return (getHandForString "5c 5d 2c 2d 3h")
    assertEqual "two pair 55223" (TwoPair Five Two) h
    h <- return (getHandForString "2c 2d 5c 5d 6h 7d 8c")
    assertEqual "two pair 2255678" (TwoPair Five Two) h
    )


test_three ::  Test
test_three = TestCase (do
    h <- return (getHandForString "5d 5h 5d")
    assertEqual "three 555" (ThreeOfAKind Five) h)


test_straight :: Test
test_straight = TestCase (do
    h <- return (getHandForString "2c 3c 4c 5d 6d")
    assertEqual "straight 23456" (Straight Six) h
    h <- return (getHandForString "Ac 2c 3c 4c 5d")
    assertEqual "straight A2345" (Straight Five) h
    h <- return (getHandForString "Tc Jc Qc Kd Ad")
    assertEqual "straight 23456" (Straight Ace) h
    h <- return (getHandForString "Ac Kh 2c 3c 4c 5d")
    assertEqual "straight AK2345" (Straight Five) h
    h <- return (getHandForString "Ac Kh Jh 2c 3c 4c 5d")
    assertEqual "straight AKJ2345" (Straight Five) h
    h <- return (getHandForString "Ac Kc Qd Jd Ts 2s 3h 4h 5d")
    assertEqual "straight AKQJT2345" (Straight Ace) h
    )


test_flush = TestCase (do
    h <- return (getHandForString "6c 3c 4c 5c Ac")
    assertEqual "flush 6345A" (Flush Ace) h
    h <- return (getHandForString "2c 2c 3c 3c 4c")
    assertEqual "flush 22334" (Flush Four) h
    h <- return (getHandForString "2c 2c 2c 3c 4c Kc")
    assertEqual "flush 22234K" (Flush King) h
    h <- return (getHandForString "2c 2s 2c 3s 4c Qc Kc")
    assertEqual "flush 22-23-4QK" (Flush King) h
    )



test_full_house ::  Test
test_full_house = TestCase (do
    h <- return (getHandForString "2c 2h 2d 3c 3d")
    assertEqual "full house 22233" (FullHouse Two Three) h
    h <- return (getHandForString "2c 2h 2d 3c 3d 4s 4s")
    assertEqual "full house 2223344" (FullHouse Two Four) h
    h <- return (getHandForString "2c 2h 3d 3c 3d 4s 4s")
    assertEqual "full house 2233344" (FullHouse Three Four) h
    h <- return (getHandForString "2c 2h 4d 3c 3d 4s 4s")
    assertEqual "full house 2243344" (FullHouse Four Three) h
    h <- return (getHandForString "2c 2h 4d 3c 5d 4s 4s")
    assertEqual "full house 2243544" (FullHouse Four Two) h
    )


test_four = TestCase (do
    h <- return (getHandForString "2c 2h 2s 2d")
    assertEqual "four 2222" (FourOfAKind Two) h
    h <- return (getHandForString "2c 2h 2s 2d 3c 3d")
    assertEqual "four 222233" (FourOfAKind Two) h
    h <- return (getHandForString "2c 2h 2s 2d 3c 3d 3s")
    assertEqual "four 2222333" (FourOfAKind Two) h
    )


test_straight_flush = TestCase (do
    h <- return (getHandForString "Tc Jc Qc Kc Ac")
    assertEqual "straight flush 2345A" (StraightFlush Ace) h
    h <- return (getHandForString "2c 3c 4c 5c Ac")
    assertEqual "straight flush 2345A" (StraightFlush Five) h
    )

tests_simple = TestList [
    TestLabel "test high card" test_high_card,
    TestLabel "test pair" test_pair,
    TestLabel "test two pair" test_two_pair,
    TestLabel "test three" test_three,
    TestLabel "test straight" test_straight,
    TestLabel "test flush" test_flush,
    TestLabel "test full house" test_full_house,
    TestLabel "test four" test_four,
    TestLabel "test straight flush" test_straight_flush
    ]

tests_all = TestList [tests_simple]

main :: IO Counts
main = do runTestTT tests_all
    
