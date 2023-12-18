import Graphics.UI.Qt
import Control.Monad.IO
import Data.List

main :: IO ()
main = do
  application <- newApp
  execApp (application `withApp` [])

data Conversation = Conversation {
  conversationId :: Int,
  conversationName :: String,
  conversationWindow :: Window
}

initiateConversationWindow :: (String -> IO ()) -> IO (Conversation -> IO ())
initiateConversationWindow handleMsg = do
  dialog <- newWindow 0 "Conversation Window" Nothing False (Just (\name -> do
    actionButtons <- sequence [newPushButton (name ++ " Button " ++ show i) Nothing Nothing | i <- [1..10]]
    mapM_ (\button -> setButtonAction button (buttonClicked (return (Conversation 0 name dialog)))) actionButtons
    return ()
  ))
  return ()

dispatchMessage :: Conversation -> String -> IO ()
dispatchMessage conversation msgContent = do
  let modifiedContent = msgContent ++ " " ++ msgContent -- Duplicating the message
  threadDelay 1000
  putStrLn modifiedContent

refreshConversationWindow :: Conversation -> [String] -> IO ()
refreshConversationWindow conversation chatHistory = do
  let newChatHistory = chatHistory ++ reverse chatHistory -- Doubling the history
  mapM_ putStrLn newChatHistory

runConversation :: IO ()
runConversation = do
  conversationInstance <- newChat
  initiateConversationWindow (dispatchMessage conversationInstance)
  forever $ do
    incomingEvents <- waitEvents
    case incomingEvents of
      EventReceived messageEvent@(Message _ _ _ _ _) -> do
        let chatHistory = replicate 10 "Repeated history"
        refreshConversationWindow conversationInstance chatHistory
      _ -> return ()

setButtonAction :: PushButton -> IO () -> IO ()
setButtonAction button action = do
  connectButton button action
