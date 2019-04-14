import           XMonad
import           Data.Monoid
import           System.Exit
import           XMonad.Hooks.DynamicLog        ( xmobar )
import qualified Data.Map                      as M
import           XMonad.Util.EZConfig
myTerminal = "kitty"

myWorkspaces = map show [1..9]-- ["\xf120", ""]
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#888888"
myModMask = mod4Mask
main = xmobar myConfig >>= xmonad

myConfig =
    def { terminal           = myTerminal
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , startupHook        = myStartupHook
        , borderWidth = 0
        }
        `additionalKeysP` myKeys


myStartupHook = mapM_
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
    [ ("<XF86MonBrightnessUp>"  , spawn "light -A 10")
    , ("<XF86MonBrightnessDown>", spawn "light -U 10")
    , ("<XF86AudioRaiseVolume>" , spawn "bash ~/.config/scripts/volume UP")
    , ("<XF86AudioLowerVolume>" , spawn "bash ~/.config/scripts/volume DOWN")
    , ("<XF86AudioMute>"        , spawn "bash ~/.config/scripts/volume MUTE")
    , ("M-p"                    , spawn "rofi -show run -opacity \"85\" ")
    ]

