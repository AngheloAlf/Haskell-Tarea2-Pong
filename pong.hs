import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

velPaleta = 5 :: Float
largoPaleta = 400 :: Float
distPaleta 200 :: Float
medLenX = 250 :: Float
medLenY = 150 :: Float

dibujarRect :: Float -> Float -> Float -> Float -> Polygon
dibujarRect x1 x2 y1 y2 = Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
 

dibujarPaleta :: Float -> Float -> Picture
dibujarPaleta x y = dibujarRect (x-10) (y-largoPaleta/2) (x+10) (y+largoPaleta/2)


data Paleta = Paleta{pal_y :: Float, pal_vy :: Float}

-- Pelota posx posy vx vy
data Pelota = Pelota Float Float Float Float


data Universo = Universo{
    pelotas :: [Pelota],
    tiempo :: Float,
    paleta1 :: Barra,
    paleta2 :: Barra,
    score1 :: Int, 
    score2 :: Int
}


procInput :: Event -> Universo -> Universo
procInput (EventKey (SpecialKey KeyUp) Down _ _) uni =
    uni {paleta1 = (paleta1 uni){pal_vy = pal_vy (paleta1 uni) + velPaleta}}
procInput (EventKey (SpecialKey KeyUp) Up _ _) uni =
    uni {paleta1 = (paleta1 uni){pal_vy = pal_vy (paleta1 uni) - velPaleta}}
procInput (EventKey (SpecialKey KeyDown) Down _ _) uni =
    uni {paleta1 = (paleta1 uni){pal_vy = pal_vy (paleta1 uni) - velPaleta}}
procInput (EventKey (SpecialKey KeyDown) Up _ _) uni =
    uni {paleta1 = (paleta1 uni){pal_vy = pal_vy (paleta1 uni) + velPaleta}}

procInput _ uni = uni


actualizarPal pa = pa{pal_y = pal_y pa + pal_vy pa}

actualizarTiempo tiempo dt = tiempo + dt


procTiempo :: Float -> Universo -> Universo
procTiempo dt uni = let
    in uni {
        paleta1 = actualizarPal (paleta1 uni),
        paleta2 = actualizarPal (paleta2 uni),
        tiempo = actualizarTiempo (tiempo uni)
}




dibujar :: Universo -> Picture
dibujar uni = Pictures [cancha,
    dibujarPaleta (-distPaleta) (pal_y ()), 
    dibujarPaleta]

mundoi = Universo{
    pelotas = ,
    tiempo = 0,
    paleta1 = ,
    paleta2 = ,
    score1 = 0, 
    score2 = 0
}

main = do 
    play display bcolor 60 mundoi dibujar procInput procTiempo






