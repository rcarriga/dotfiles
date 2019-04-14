{-# LANGUAGE FlexibleContexts #-}
import           XMonad
import           XMonad.Hooks.SetWMName
import           XMonad.Actions.CycleRecentWS
import           Data.Monoid
import           System.Exit
import           XMonad.Hooks.DynamicLog
import qualified Data.Map                      as M
import           XMonad.Util.EZConfig
import           XMonad.Hooks.ManageDocks
import           XMonad.Actions.WorkspaceNames
import           XMonad.Util.Run
import           XMonad.Layout.Tabbed
import           XMonad.Layout.NoBorders
myTerminal = "kitty"
myWorkspaces = map show [1..9]
    --[ " 1 => \xf269 "
    --, " 2 -> : \xf121 "
    --, " 3 == : \xf02d "
    --, " 4 : \xf025 "
    --, " 5 : \xf03d "
    --, " 6 : \xf1e3 "
    --, " 7 : \xf07b "
    --, " 8 : \xf21b "
    --, " 9 : \xf21e "
    --]
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#888888"
myModMask = mod4Mask

startScript :: String -> X ()
startScript script_name = spawn $ "bash $HOME/.config/scripts/" ++ script_name

main :: IO ()
main = spawnPipe "xmobar" >>= xmonad . docks . myConfig

myConfig pipe =
    def { terminal           = myTerminal
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , manageHook         = manageDocks <+> def manageHook
        , layoutHook         = myLayoutHook
        , handleEventHook    = docksEventHook <+> def handleEventHook
        , startupHook        = myStartupHook
        , logHook            = myLogHook pipe
        , borderWidth        = 1
        }
        `additionalKeysP` myKeys

myLogHook pipe = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn pipe }

myLayoutHook = avoidStruts $ Tall 1 (3/100) (1/2) ||| noBorders Full

myStartupHook :: X ()
myStartupHook = do
    setWMName "XMonad"
    mapM_
        spawn
        [ "xinput set-prop \"DLL07BE:01 06CB:7A13 Touchpad\" \"libinput Tapping Enabled\" 1"
        , "xinput set-prop \"DLL07BE:01 06CB:7A13 Touchpad\" \"libinput Natural Scrolling Enabled\" 1"
        , "xset r rate 150 40"
        , "light -N 1"
        , "feh --bg-fill ~/.config/images/moon.jpg"
        , "bash ~/.config/scripts/startup"
        ]

myKeys :: [(String, X ())]
myKeys =
    [ ("<XF86MonBrightnessUp>"  , startScript "brightness UP")
    , ("<XF86MonBrightnessDown>", startScript "brightness DOWN")
    , ("<XF86AudioRaiseVolume>" , startScript "volume UP")
    , ("<XF86AudioLowerVolume>" , startScript "volume DOWN")
    , ("<XF86AudioMute>"        , startScript "volume MUTE")
    , ("M-p"                    , spawn "rofi -show run -opacity \"85\" ")
    , ("M-<Tab>", cycleRecentWS [xK_Super_L] xK_Tab xK_BackSpace)
    , ("M-t"                    , sendMessage ToggleStruts)
    ]

