import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Debug.Trace

velPaleta = 5 :: Float
largoPaleta = 50 :: Float
distPaleta = 450 :: Float
medLenX = 500 :: Float
medLenY = 300 :: Float

dibujarRect :: Float -> Float -> Float -> Float -> Picture
dibujarRect x1 y1 x2 y2 = Polygon [(x1,y1),(x1,y2),(x2,y2),(x2,y1)]

cancha = Color cyan (dibujarRect (-medLenX) (-medLenY) (medLenX) (medLenY))

dibujarPaleta :: Float -> Float -> Picture
dibujarPaleta x y = dibujarRect (x-10) (y - largoPaleta / 2) (x+10) (y + largoPaleta / 2)

dibujarPelota :: Float -> Float -> Picture
dibujarPelota x y = dibujarRect (x-10) (y-10) (x+10) (y+10)

data Paleta = Paleta {pal_y :: Float, pal_vy :: Float}
data Pelota = Pelota {posx :: Float, posy :: Float, velx :: Float, vely :: Float}
data Universo = Universo {
    paleta1 :: Paleta,
    paleta2 :: Paleta,
    pelota :: Pelota,
    score1 :: Int,
    score2 :: Int
}


obtenerPosPaleta :: Paleta -> Float
obtenerPosPaleta (Paleta pal_y pal_vy) = pal_y

obtenerPosXPelota :: Pelota -> Float
obtenerPosXPelota (Pelota posx posy velx vely) = posx

obtenerPosYPelota :: Pelota -> Float
obtenerPosYPelota (Pelota posx posy velx vely) = posy

procInput :: Event -> Universo -> Universo
procInput (EventKey (SpecialKey KeyUp) Down _ _) uni =
    uni {paleta2 = (paleta2 uni) {pal_vy = pal_vy (paleta2 uni) + velPaleta}}
procInput (EventKey (SpecialKey KeyUp) Up _ _) uni =
    uni {paleta2 = (paleta2 uni) {pal_vy = pal_vy (paleta2 uni) - velPaleta}}
procInput (EventKey (SpecialKey KeyDown) Down _ _) uni =
    uni {paleta2 = (paleta2 uni) {pal_vy = pal_vy (paleta2 uni) - velPaleta}}
procInput (EventKey (SpecialKey KeyDown) Up _ _) uni =
    uni {paleta2 = (paleta2 uni) {pal_vy = pal_vy (paleta2 uni) + velPaleta}}
procInput (EventKey (Char 'w') Down _ _) uni =
    uni {paleta1 = (paleta1 uni) {pal_vy = pal_vy (paleta1 uni) + velPaleta}}
procInput (EventKey (Char 'w') Up _ _) uni =
    uni {paleta1 = (paleta1 uni) {pal_vy = pal_vy (paleta1 uni) - velPaleta}}
procInput (EventKey (Char 's') Down _ _) uni =
    uni {paleta1 = (paleta1 uni) {pal_vy = pal_vy (paleta1 uni) - velPaleta}}
procInput (EventKey (Char 's') Up _ _) uni =
    uni {paleta1 = (paleta1 uni) {pal_vy = pal_vy (paleta1 uni) + velPaleta}}    
procInput _ uni = uni


procTiempo :: Float -> Universo -> Universo
procTiempo dt uni = let
    actualizarPal pa = if (obtenerPosPaleta (pa) >= 275) 
        then pa {pal_y = pal_y pa - (1)} 
        else if (obtenerPosPaleta (pa) <= -275)
            then pa {pal_y = pal_y pa + (1)}
            else pa {pal_y = pal_y pa + pal_vy pa}
        
    actualizarPelota pelota = if (obtenerPosYPelota (pelota) >= 275)
        then pelota {posy = posy pelota - vely pelota, vely = 0-vely pelota}
        else if (obtenerPosYPelota (pelota) <= -275)
            then pelota {posy = posy pelota - vely pelota, vely = 0-vely pelota}
            else pelota {posy = posy pelota + vely pelota}

    in uni {
        paleta1 = actualizarPal (paleta1 uni),
        paleta2 = actualizarPal (paleta2 uni), 
        pelota = actualizarPelota (pelota uni)
    }

dibujar :: Universo -> Picture
dibujar uni = Pictures [cancha,
    dibujarPaleta (-distPaleta) (pal_y (paleta1 uni)),
    dibujarPaleta ( distPaleta) (pal_y (paleta2 uni)), 
    dibujarPelota (posx (pelota uni)) (posy (pelota uni))]

mundoi = Universo {
    paleta1 = Paleta 0 0,
    paleta2 = Paleta 0 0,
    -- TODO: Quitar 5
    pelota = Pelota 0 0 0 5,
    score1 = 0,
    score2 = 0
}

main = do
    play (FullScreen) white 60 mundoi dibujar procInput procTiempo
