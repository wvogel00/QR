module Mineral where

import Vis
import Linear.V3
import Pauling
import Type
import CFParser
import XMLParser
import Basic
import Data.Maybe (Maybe(..), fromJust)
import Data.List (find)
import SpatialMath (Euler (..))

na = Ion{Type.elem = Na, eN = 1, pole = Plus, radius = 0.15}
cl = Ion{Type.elem = Cl, eN = negate 1, pole = Minus, radius = 0.3}

monoCrystal :: XML -> [VisObject Float]
monoCrystal xml = case structure xml of
    Just HCP -> hcpMonoStructure (fromJust $ _a xml) (fromJust $ _c xml)
    Just CCP -> ccpMonoStructure (fromJust $ _a xml)
    Just FCC -> ccpMonoStructure (fromJust $ _a xml)
    Nothing -> [drawText (show (name xml) ++" have no mono-crystal")]

hcpMonoStructure :: Float -> Float -> [VisObject Float]
hcpMonoStructure a c = bones ++ elems where
    vertexs = map (rot2D (V3 (-a/2) (a/2*sqrt 3) 0)) [0,60..360] :: [V3 Float]
    (top,bottom) = (map (movPoint (0,0,c)) vertexs, map (movPoint (0,0,-c)) vertexs)
    center = map (rot2D (V3 (a/sqrt 3) 0 0)) [30,150,270]
    bones = [toBone top, toBone bottom] ++ ( map (toBone.(\tuple -> [fst tuple,  snd tuple])) $ zip top bottom )
    elems = map apply $ zip (map Trans $ top++bottom++center) $ repeat (Sphere (a/3) Solid yellow)
    
ccpMonoStructure a = bones : wires ++ elems where
    bones = Cube (2 * a) Wireframe blue 
    wires = map (\ls -> Line (Just 0.1) ls blue)[ 
            [(V3 a a a), (V3 (-a) (-a) a), (V3 (-a) (-a) (-a)), (V3 a a (-a))], 
            [(V3 (-a) a a), (V3 a (-a) a), (V3 a (-a) (-a)), (V3 (-a) a (-a))],
            [(V3 (-a) (-a) a), (V3 a (-a) (-a)), (V3 a a (-a)), (V3 (-a) a a)],
            [(V3 a (-a) a), (V3 (-a) (-a) (-a)), (V3 (-a) a (-a)), (V3 a a a)],
            [(V3 (-a) (-a) a),(V3 (-a) a (-a)),(V3 a a (-a)),(V3 a (-a) a)],
            [(V3 a a a),(V3 a (-a) (-a)),(V3 (-a) (-a) (-a)),(V3 (-a) a a)]
            ]
    elems = map apply $ zip (tops++centers++bottoms) $ repeat (Sphere (a/3) Solid yellow)
    tops = (Trans (V3 0 0 a)) : [Trans (V3 x y a) | x <- [-a,a], y <- [-a,a]]
    centers = (Trans (V3 0 0 (-a))) : [((rotZ 45).(Trans (V3 x y 0))) | x <- [-a/sqrt 2,a/sqrt 2], y <- [-a/sqrt 2,a/sqrt 2]]
    bottoms = (Trans (V3 0 0 (-a))) : [Trans (V3 x y (-a)) | x <- [-a,a], y <- [-a,a]]

polyCrystal formula xmls = case getStructure xmls of
    Ionic ions -> genIonStructure CCP ions
    Covalent elems -> []
    Vanderwaals elems -> []

getStructure _ = Ionic [na,cl]

genIonStructure :: Structure -> [Ion] -> [VisObject Float]
genIonStructure HCP = hcpStructure
genIonStructure CCP = ccpStructure
genIonStructure FCC = ccpStructure -- have the same structre as ccp
genIonStructure BCC = bccStructure

hcpStructure _ = []
bccStructure _ = []

ccpStructure :: [Ion] -> [VisObject Float]
ccpStructure (a:b:_) = bones : ions where
    ions = map (moveZ (edgeLen/2)) (ionOnVertexs b a) ++ ionOnVertexs a b ++ map (moveZ (negate edgeLen/2)) (ionOnVertexs b a)
    edgeLen = 1.0
    bones = Cube edgeLen Wireframe blue
    obj c = Sphere (radius c) Solid (genColor c)
    ionOnVertexs c d = map apply.zip ionPoses $ concat $ repeat [obj c,obj d]
    ionPoses = [Trans (V3 x y 0) | x <- [-edgeLen/2,0,edgeLen/2], y <- [-edgeLen/2,0,edgeLen/2]]

apply (f,v) = f v
rot2D (V3 x y z) theta = V3 (x*cos rad - y*sin rad) (x*sin rad + y * cos rad) z where
    rad = theta/360*2*pi
movPoint (mx,my,mz) (V3 x y z) = V3 (x+mx) (y+my) (z+mz)

toBone ls = Line (Just 0.1) ls blue
rotZ z = RotEulerDeg (Euler z 0 0)
moveX x = Trans (V3 x 0 0)
moveY y = Trans (V3 0 y 0)
moveZ z = Trans (V3 0 0 z)
moveXY x y = Trans (V3 x y 0)

genColor c = if pole c == Plus then blue else yellow