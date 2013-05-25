import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)
import qualified Data.Vector as V
import System.Random

type Damage = Int
type Player = Int
data GameState = GameState { player :: Player,
							 healths :: V.Vector Int
						   } deriving (Show)

damageMin :: Int
damageMin = 1

damageMax :: Int
damageMax = 10

healthMax :: Int
healthMax = 100

startGameState :: Int -> GameState
startGameState players = GameState 0 (V.replicate (players -1) healthMax)

main :: IO ()
main = do
	evalStateT runTurns (startGameState 2)
	 

runTurns :: StateT GameState IO ()
runTurns = do
	winner <- runTurn

	if winner == (-1)
	    then runTurns
	    else liftIO $ putStrLn $ "Player " ++ (show winner) ++ " wins!"

runTurn :: StateT GameState IO Player
runTurn = do
	gameState <- get

	let currentPlayer = player gameState
	let playerHealths = healths gameState

	liftIO $ putStrLn $ "Player " ++ (show currentPlayer) ++ ": please enter the player you want to hit:"
	charTarget <- liftIO $ getChar

	let target = read [charTarget] :: Int

	damage <- liftIO $ genDamage
	liftIO $ putStrLn $ "Player " ++ (show currentPlayer) ++ " does " ++ (show damage) ++ " damage to player " ++ (show target) ++ "!"

	let totalPlayers = V.length playerHealths
	let nextPlayer = if currentPlayer == totalPlayers
					 then 0
					 else succ $ totalPlayers

	let newTargetHealth = (playerHealths V.! target) - damage
	let updatedHealths = playerHealths V.// [(target, newTargetHealth)]
	
	put $ GameState nextPlayer updatedHealths

	if allOthersDead gameState
	then return currentPlayer
	else return (-1)


genDamage :: IO Damage
genDamage = getStdRandom (randomR (damageMin, damageMax))

allOthersDead :: GameState -> Bool
allOthersDead gameState = V.length (V.filter (<= 0) (healths gameState)) == 1