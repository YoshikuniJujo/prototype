module Main where

import Android

main :: IO ()
main = runAndroid $ do
	( textView, setText, setContentView ) <- importAndroid
	myTV <- clone textView
	method myTV setText [ primStr "今日は、世界!" ]
	method myTV setContentView [ ]
	return ()
