import Happstack.Server
import Data.Maybe (listToMaybe, fromJust)

import Yinsh
import AI
import Floyd

backendAI :: AIFunction
backendAI = aiFloyd 3 mhNumber rhControlledMarkers

-- | Get new game state after AI turn. This also resolves @WaitRemoveRun@ and
-- @WaitAddMarker@ turns for the *human* player.
aiTurn' :: AIFunction
aiTurn' gs = let gs' = backendAI gs in
                 case turnMode gs' of
                     (WaitRemoveRun _) -> backendAI $ fromJust $ newGameState gs' (0, 0)
                     WaitAddMarker     -> backendAI $ fromJust $ newGameState gs' (0, 0)
                     _                 -> gs'

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

handler :: ServerPart Response
handler = do gsString <- look "gamestate"
             let mgs = maybeRead gsString :: Maybe GameState
             case mgs of
                 Nothing ->   badRequest $ toResponse "malformed gamestate"
                 (Just gs) -> ok $ xssHeader $ toResponse $ show (aiTurn' gs)
    where xssHeader = setHeader "Access-Control-Allow-Origin" "*"

main :: IO ()
main = simpleHTTP nullConf handler
