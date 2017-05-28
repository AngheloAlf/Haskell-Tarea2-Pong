-- Integrantes:
-- Anghelo Carvajal - 201473062-4
-- Andres Tapia - 201503010-3

-- Controles:
-- w: mueve j1 hacia arriba
-- s: mueve j1 hacia abajo
-- flecha arriba: mueve j2 hacia arriba
-- flecha abajo: mueve j2 hacia abajo

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture


velPaleta = 5 :: Float
largoPaleta = 50 :: Float
distPaleta = 420 :: Float
medLenX = 500 :: Float
medLenY = 250 :: Float

dibujarRect :: Float -> Float -> Float -> Float -> Picture
dibujarRect x1 y1 x2 y2 = Polygon [(x1,y1),(x1,y2),(x2,y2),(x2,y1)]

cancha = Color cyan (dibujarRect (-medLenX) (-medLenY) (medLenX) (medLenY))

dibujarPaleta :: Float -> Float -> Picture
dibujarPaleta x y = dibujarRect (x-10) (y - largoPaleta / 2) (x+10) (y + largoPaleta / 2)

dibujarPelota :: Float -> Float -> Picture
dibujarPelota x y = dibujarRect (x-10) (y-10) (x+10) (y+10)


dibujarScore :: Int -> Float -> Picture
dibujarScore score x = Translate x 260 (Text (show score))



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
    actualizarPal pa = if (obtenerPosPaleta (pa) >= (medLenY - (largoPaleta / 2))) 
        then pa {pal_y = pal_y pa - (1)} 
        else if (obtenerPosPaleta (pa) <= ((largoPaleta / 2)-medLenY))
            then pa {pal_y = pal_y pa + (1)}
            else pa {pal_y = pal_y pa + pal_vy pa}

    actualizarPelotaX pelota = if (obtenerPosXPelota (pelota) >= medLenX-10)
        then pelota {posx = 0, posy = 0}
        else if (obtenerPosXPelota (pelota) <= 10-medLenX)
            then pelota {posx = 0, posy = 0}
            else pelota {posx = posx pelota + velx pelota}

    actualizarPelotaY pelota = if (obtenerPosYPelota (pelota) >= medLenY-10)
        then pelota {posy = posy pelota - vely pelota, vely = 0-vely pelota}
        else if (obtenerPosYPelota (pelota) <= 10-medLenY)
            then pelota {posy = posy pelota - vely pelota, vely = 0-vely pelota}
            else pelota {posy = posy pelota + vely pelota}

    actualizarRebotePelota pelota paleta = if (obtenerPosXPelota (pelota) >= distPaleta - 10 && obtenerPosXPelota (pelota) <= distPaleta+10 && obtenerPosYPelota (pelota) <= (obtenerPosPaleta (paleta)) + (largoPaleta / 2) && obtenerPosYPelota (pelota) >= (obtenerPosPaleta (paleta)) - (largoPaleta / 2))
        then pelota {posx = posx pelota - velx pelota, velx = 0-velx pelota}
        else if (obtenerPosXPelota (pelota) <= 10-distPaleta && obtenerPosXPelota (pelota) >= 0-10-distPaleta && obtenerPosYPelota (pelota) <= (obtenerPosPaleta (paleta)) + (largoPaleta / 2) && obtenerPosYPelota (pelota) >= (obtenerPosPaleta (paleta)) - (largoPaleta / 2))
            then pelota {posx = posx pelota - velx pelota, velx = 0-velx pelota}
            else pelota

    actualizarPelota pelota paleta1 paleta2 = actualizarPelotaX (actualizarPelotaY (actualizarRebotePelota (actualizarRebotePelota pelota paleta1) paleta2) )

    actualizarScore1 pelota score = if (obtenerPosXPelota (pelota) >= medLenX-10)
        then score + 1
        else score

    actualizarScore2 pelota score = if (obtenerPosXPelota (pelota) <= 10-medLenX)
        then score + 1
        else score

    in uni {
        paleta1 = actualizarPal (paleta1 uni),
        paleta2 = actualizarPal (paleta2 uni), 
        score1 = actualizarScore1 (pelota uni) (score1 uni), 
        score2 = actualizarScore2 (pelota uni) (score2 uni),
        pelota = actualizarPelota (pelota uni) (paleta1 uni) (paleta2 uni)
    }

dibujar :: Universo -> Picture
dibujar uni = Pictures [cancha,
    dibujarPaleta (-distPaleta) (pal_y (paleta1 uni)),
    dibujarPaleta ( distPaleta) (pal_y (paleta2 uni)), 
    dibujarPelota (posx (pelota uni)) (posy (pelota uni)), 
    dibujarScore (score1 uni) (0-200),
    dibujarScore (score2 uni) 150]

mundoi = Universo {
    paleta1 = Paleta 0 0,
    paleta2 = Paleta 0 0,
    -- TODO: Quitar 3
    pelota = Pelota 0 0 3 3,
    score1 = 0,
    score2 = 0
}

main = do
    play (FullScreen) white 60 mundoi dibujar procInput procTiempo
