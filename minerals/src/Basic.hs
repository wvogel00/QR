module Basic where

import Vis
import Linear.V3
import Type

testModel = VisObjects $ testText:testpolyText:testModel1:testModel2

testText = Text3d "NaCl" (V3 2 0.5 0) TimesRoman24 black
testpolyText = Text3d (show HCP) (V3 2 1.5 0) TimesRoman24 black

cubeSize = 1.0
vertex = cubeSize/2
naSize = 0.15
clSize = 0.30

testModel1 = Cube cubeSize Wireframe blue
testModel2 = [
    Trans (V3 vertex vertex 0) $ Sphere naSize Solid blue,
    Trans (V3 vertex (negate vertex) 0) $ Sphere naSize Solid blue,
    Trans (V3 (negate vertex) vertex 0) $ Sphere naSize Solid blue,
    Trans (V3 (negate vertex) (negate vertex) 0) $ Sphere naSize Solid blue,
    Trans (V3 0 0 0) $ Sphere naSize Solid blue,

    Trans (V3 vertex 0 (negate vertex)) $ Sphere naSize Solid blue,
    Trans (V3 (negate vertex) 0 (negate vertex)) $ Sphere naSize Solid blue,
    Trans (V3 0 (negate vertex) (negate vertex)) $ Sphere naSize Solid blue,
    Trans (V3 0 vertex (negate vertex)) $ Sphere naSize Solid blue,

    Trans (V3 vertex 0 vertex) $ Sphere naSize Solid blue,
    Trans (V3 (negate vertex) 0 vertex) $ Sphere naSize Solid blue,
    Trans (V3 0 (negate vertex) vertex) $ Sphere naSize Solid blue,
    Trans (V3 0 vertex vertex) $ Sphere naSize Solid blue,

    Trans (V3 vertex vertex vertex) $ Sphere clSize Solid yellow,
    Trans (V3 vertex vertex (negate vertex)) $ Sphere clSize Solid yellow,
    Trans (V3 (negate vertex) vertex vertex) $ Sphere clSize Solid yellow,
    Trans (V3 (negate vertex) vertex (negate vertex)) $ Sphere clSize Solid yellow,
    Trans (V3 vertex (negate vertex) vertex) $ Sphere clSize Solid yellow,
    Trans (V3 vertex (negate vertex) (negate vertex)) $ Sphere clSize Solid yellow,
    Trans (V3 (negate vertex) (negate vertex) vertex) $ Sphere clSize Solid yellow,
    Trans (V3 (negate vertex) (negate vertex) (negate vertex)) $ Sphere clSize Solid yellow,
    Trans (V3 vertex 0 0) $ Sphere clSize Solid yellow,
    Trans (V3 (negate vertex) 0 0) $ Sphere clSize Solid yellow,
    Trans (V3 0 (negate vertex) 0) $ Sphere clSize Solid yellow,
    Trans (V3 0 vertex 0) $ Sphere clSize Solid yellow
    ]