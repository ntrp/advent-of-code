import Criterion.Main

distanceQuadMemo :: Int -> Int -> Int
distanceQuadMemo pos i = map dis [0 ..] !! abs (pos - i)
  where
    dis 0 = 0
    dis 1 = 1
    dis n = dis (n - 1) + n

distanceQuad :: Int -> Int -> Int
distanceQuad pos i = dis $ abs (pos - i)
  where
    dis 0 = 0
    dis 1 = 1
    dis n = dis (n - 1) + n

-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup
        "distanceQuadMemo"
        [ bench "10" $ whnf (distanceQuadMemo 0) 10,
          bench "50" $ whnf (distanceQuadMemo 0) 50,
          bench "100" $ whnf (distanceQuadMemo 0) 100,
          bench "500" $ whnf (distanceQuadMemo 0) 500
        ],
      bgroup
        "distanceQuad"
        [ bench "10" $ whnf (distanceQuad 0) 10,
          bench "50" $ whnf (distanceQuad 0) 50,
          bench "100" $ whnf (distanceQuad 0) 100,
          bench "500" $ whnf (distanceQuad 0) 500
        ]
    ]
