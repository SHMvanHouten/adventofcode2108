module Frequency where


removePlus input = if input !! 0 == '+'
                        then tail input
                        else input

parseInt str = read str :: Int
