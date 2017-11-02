module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthAge :: Float
earthAge = 31557600.0

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
  case planet of
    Mercury -> seconds / (0.2408467 * earthAge)
    Venus -> seconds / (0.61519726 * earthAge)
    Earth -> seconds / earthAge
    Mars -> seconds / (1.8808158 * earthAge)
    Jupiter -> seconds / (11.862615 * earthAge)
    Saturn -> seconds / (29.447498 * earthAge)
    Neptune -> seconds / (164.79132 * earthAge)
    Uranus -> seconds / (84.016846 * earthAge)
  
