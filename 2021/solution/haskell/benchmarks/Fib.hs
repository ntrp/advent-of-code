import Criterion.Main

slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n - 2) + slow_fib (n - 1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = memoized_fib (n - 2) + memoized_fib (n - 1)

-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bench "30" $ whnf slow_fib 30
        ],
      bgroup
        "fib_memo"
        [ bench "30" $ whnf memoized_fib 30
        ]
    ]
