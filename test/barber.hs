module Barber where

import Probability
import Queuing
import ListUtils

-- barber shop

custServ :: Dist Time
custServ = normal [5..10]

nextCust :: Trans Time -- not dependant on serving time
nextCust _ = normal [3..6]

barbers :: Int
barbers = 1

customers :: Int
customers = 20

runs :: Int
runs = 50

barberEvent =  unit (custServ, nextCust)
barberEvents = replicate customers barberEvent 
barberSystem eval = evalSystem runs barbers barberEvents eval


-- category

data Category = ThreeOrLess | FourToTen | MoreThanTen
	deriving (Eq,Ord,Show)

cat :: Time -> Category
cat n | n <= 3 = ThreeOrLess
cat n | n <= 10 = FourToTen
cat _ = MoreThanTen

perc :: Float -> String
perc n | n <= 0.25 = "0% to 25%"
perc n | n <= 0.5 = "25% to 50%"
perc n | n <= 0.75 = "50% to 75%"
perc _ = "75% to 100%"

-- evaluation

-- avg barber idle time
barberIdle = barberSystem (perc.(idleAvgP barbers))
-- avg customer waiting time (unserved customers)
customerWait = barberSystem ( cat.(`div` customers).(waiting barbers) )
