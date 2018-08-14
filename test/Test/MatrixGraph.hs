module Test.MatrixGraph where

import MatrixGraph

import Test.Hspec

sampleGraph :: MatrixGraph Int ()
sampleGraph =
  fromNodesAndEdges
    (1, 8)
    [ ((1, 2), ())
    , ((2, 3), ())
    , ((3, 4), ())
    , ((1, 3), ())
    , ((3, 6), ())
    , ((1, 5), ())
    , ((5, 3), ())
    , ((5, 6), ())
    , ((6, 7), ())
    , ((7, 3), ())
    , ((5, 8), ())
    ]

spec_matrixGraph :: Spec
spec_matrixGraph =
  describe "MatrixGraph" $ do
    describe "Depth-first search" $
      it "gives a known traversal order for a sample graph" $
        depthFirstSearch sampleGraph 1 `shouldBe` [1, 5, 8, 6, 7, 3, 4, 2]
    describe "Breadth-first search" $
      it "gives a known traversal order for a sample graph" $
        breadthFirstSearch sampleGraph 1 `shouldBe` [1, 2, 3, 5, 4, 6, 8, 7]
