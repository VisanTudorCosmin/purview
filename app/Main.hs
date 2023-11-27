module Main where

import Prelude hiding (div)
import Purview
import Purview.Server (serve, defaultConfiguration, devMode)
import Type.Reflection

data CounterEvent = CounterIncrement | CounterDecrement
   deriving (Eq, Show)

type CounterState = Integer

counterComponent 
   :: Typeable b
   => CounterState
   -> (CounterEvent -> Integer -> [DirectedEvent b CounterEvent]) 
   -> Purview b m
counterComponent initialState parentEventHandler = handler' [] initialState updateState viewState
   where
      updateState event state = case event of 
	  	   CounterIncrement -> (state + 1, (parentEventHandler event state) ++ [])
  		   CounterDecrement -> (state - 1, (parentEventHandler event state) ++ [])
      viewState state = div
         [ p [ text ("count: " <> show state) ]
	      , onClick CounterIncrement $ button [ text "+" ]
		   , onClick CounterDecrement $ button [ text "-" ]
		   ]
	   
data AppEvent = ResetCounters
   deriving (Show, Eq)     

appComponent :: Purview () m
appComponent = handler' [] initialState updateState viewState
   where
      initialState = ()
      updateState event state = ((), [])
      viewState state = div
	      [ counterComponent 0 toAppComponentEvent
	      , counterComponent 0 toAppComponentEvent 
   	   , counterComponent 0 toAppComponentEvent
	      ]    

toAppComponentEvent :: CounterEvent -> CounterState -> [DirectedEvent AppEvent CounterEvent] 
toAppComponentEvent CounterDecrement 0 = [Parent ResetCounters]
toAppComponentEvent _ _ = []

main = serve defaultConfiguration { devMode=True } (\_ -> appComponent)

-- sa cream o componenta care are un numar variabil de componente counter ( pentru inceput va 
-- fi trimis doar ca parametru ) si functionalitate speciala a aceste functii este ca daca un counter
-- ajunge sa aiba valoarea negativa atunci toate componentele counter vor fi restartate la 0
--
-- se pare ca nu este posibil cu acest framework acest tip de functionalitate
--
-- oare in SwiftUI sau Jetpack Compose este posibil ?
--
-- m-as fi asteptat ca o componenta sa fie un functor astfel incat
-- sa pot face usor maparea de la un event la altul, dar totusi e nevoie de o functie de tip
-- Event -> State -> ParentEvent
--
--
--data Compnent state event parentEvent = Component 
-- { initialState :: state
-- , updateState :: state -> event -> state
-- , returnParentEvent :: state -> event -> parentEvent
-- }
--
-- instance Functor (Component state event) 
--
-- cel mai probabil nu este posibil
