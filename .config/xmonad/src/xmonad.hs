{-# LANGUAGE FlexibleContexts #-}
import Data.Foldable
import Data.List
import Data.Maybe
import Graphics.X11.Xlib.Extras
import XMonad hiding (WindowClass)
import XMonad.Actions.CycleRecentWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.Environment
import XMonad.Layout.MouseResizableTile
import XMonad.Hooks.EwmhDesktops
import Data.Ratio ((%))
import XMonad.Layout.Fullscreen (FullscreenMessage(..), fullscreenSupport, fullscreenFull)

startScript :: String -> X ()
startScript script_name = spawn $ "bash $HOME/.config/scripts/" ++ script_name

setSupported :: X ()
setSupported = withDisplay $ \dpy -> do
  r    <- asks theRoot
  a    <- getAtom "_NET_SUPPORTED"
  c    <- getAtom "ATOM"
  supp <- mapM
    getAtom
    [ "_NET_WM_STATE_HIDDEN"
    , "_NET_NUMBER_OF_DESKTOPS"
    , "_NET_CLIENT_LIST"
    , "_NET_CLIENT_LIST_STACKING"
    , "_NET_CURRENT_DESKTOP"
    , "_NET_DESKTOP_NAMES"
    , "_NET_ACTIVE_WINDOW"
    , "_NET_WM_DESKTOP"
    , "_NET_WM_STRUT"
    , "_NET_WM_STATE"
    , "_NET_WM_STATE_FULLSCREEN"
    ]
  io $ changeProperty32 dpy r a c propModeReplace (fromIntegral <$> supp)



main :: IO ()
main = do
  mode <- lookupEnv "XMONAD_MODE"
  let
    gameMode = case mode of
      Just "game" -> True
      _           -> False
    workspaceNameFile = if gameMode then "/tmp/xmonadGameMode" else "/tmp/xmonad"
  safeSpawn "mkfifo" [workspaceNameFile]
  xmonad
    $                 ewmh
    $                 docks def
                        { terminal           = "kitty"
                        , modMask            = mod4Mask
                        , workspaces         = myWorkspaces
                        , normalBorderColor  = "#3E3D32"
                        , focusedBorderColor = "#bdbdbd"
                        , handleEventHook = def handleEventHook <+> fullscreenEventHook 
                        , layoutHook         = myLayoutHook
                        , startupHook        = myStartupHook gameMode
                        , logHook            = sendWorkspaceNames workspaceNameFile
                        , borderWidth        = 2
                        }
    `additionalKeysP` myKeys gameMode

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "htop"            "kitty htop"      (title =? "htop")                defaultFloating
  , NS "Blueman-manager" "blueman-manager" (className =? "Blueman-manager") defaultFloating
  ]

myWorkspaces :: [String]
myWorkspaces = map show ([1 .. 9] :: [Int]) ++ ["NSP"]

sendWorkspaceNames :: String -> X ()
sendWorkspaceNames file = do
  st             <- get
  workspaceNames <- workspaces . config <$> ask
  workspaceIcons <- foldrM storeIcon M.empty $ W.workspaces $ windowset st
  let
    curWs = W.tag $ W.workspace $ W.current $ windowset st
    tags  = sortOn (\t -> fromMaybe 99 $ t `elemIndex` myWorkspaces) workspaceNames
    outTags =
      foldr1 (\a b -> a <> "      " <> b) $ filter (/= "") $ mapMaybe (parseWorkspaceId workspaceIcons curWs) tags
  io $ appendFile file $ outTags ++ "  \n"

data NoFullscreenBorders = NoFullscreenBorders Ambiguity deriving (Read, Show)

instance SetsAmbiguous NoFullscreenBorders where
  hiddens (NoFullscreenBorders amb) ws parentRect maybeStack wrs =
    (fst <$> fullFloats) ++ hiddens amb ws parentRect maybeStack wrs
   where
    floats     = M.toList $ W.floating ws
    fullRect   = W.RationalRect (0 % 1) (0 % 1) (1 % 1) (1 % 1)
    fullFloats = filter (\(_, r) -> r == fullRect) floats

myBordersMod = lessBorders (NoFullscreenBorders Never)
myLayoutHook =
  myBordersMod
    $   avoidStruts
    $   spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True mouseResizableTile --(Tall 1 (3 / 100) (1 / 2)))
    ||| noBorders Full

myStartupHook :: Bool -> X ()
myStartupHook gamingMode = do
  setWMName "XMonad"
  setWallpaper
  startCompositor False
  setSupported
  mapM_
    spawn
    (if gamingMode
      then
        [ "pkill polybar; polybar xmonadGameMode"
        , "pgrep nm-applet || nm-applet"
        , "pgrep blueman-applet || blueman-applet"
        , "light -N 1"
        ]
      else
        [ "pkill polybar; polybar xmonad"
        , "pkill deadd-notification-center; deadd-notification-center"
        , "pkill redshift-gtk; sleep 5s && redshift-gtk -l 53:-6 -t 6500:2500"
        , "pgrep nm-applet || nm-applet"
        , "pgrep kdeconnectd || kdeconnectd"
        , "kdeconnect-indicator"
        , "pgrep blueman-applet || blueman-applet"
        , "pgrep xautolock || xautolock -locker \"sh /home/ronan/.config/scripts/lock; systemctl suspend\" -detectsleep -time 30 -notify 30 -notifier \"notify-send -u critical -t 10000 -- 'Suspending in 30 seconds'\""
        , "light -N 1"
        ]
    )

setWallpaper :: X ()
setWallpaper = spawn "feh -z --bg-fill ~/.config/images/"

compositorCommand :: String
compositorCommand = "picom-round --experimental-backends"

startCompositor :: Bool -> X ()
startCompositor force =
  spawn $ if force then "pkill picom; " <> compositorCommand else "pgrep picom || " <> compositorCommand

myKeys :: Bool -> [(String, X ())]
myKeys gameMode =
  [ ("<XF86MonBrightnessUp>"  , startScript "brightness UP")
  , ("<XF86MonBrightnessDown>", startScript "brightness DOWN")
  , ("<XF86AudioRaiseVolume>" , startScript "volume UP")
  , ("<XF86AudioLowerVolume>" , startScript "volume DOWN")
  , ("<XF86AudioMute>"        , startScript "volume MUTE")
  , ("C-S-<Space>"            , spawn "rofi -show drun")
  , ("M-b"                    , namedScratchpadAction myScratchpads "Blueman-manager")
  , ("M-<Tab>"                , cycleRecentWS [xK_Super_L] xK_Tab xK_BackSpace)
  , ("M-S-t", spawn $ "pkill polybar || polybar " ++ (if gameMode then "xmonadGameMode" else "xmonad"))
  , ("M-S-p"                  , spawn "polybar-msg cmd toggle")
  , ("M-S-n"                  , namedScratchpadAction myScratchpads "htop")
  , ("M-C-j"                  , sendMessage ShrinkSlave)
  , ("M-C-k"                  , sendMessage ExpandSlave)
  , ("M-C-S-j"                , decWindowSpacing 10)
  , ("M-C-S-k"                , incWindowSpacing 10)
  , ("M-g"                    , toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
  , ("M-f"                    , spawn "firefox")
  , ("M-S-r"                  , withFocused $ \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , ("M-s"                    , startScript "screen EDPI" >> startCompositor True >> setWallpaper)
  , ("M-S-s"                  , startScript "screen HDMI" >> startCompositor True >> setWallpaper)
  , ("M-C-s"                  , startScript "screen HDMIRIGHT" >> startCompositor True >> setWallpaper)
  , ("M-i"                    , startScript "lock")
  , ("M-S-b"                  , withFocused toggleBorder)
  , ("M-n"                    , spawn "kill -s USR1 $(pidof deadd-notification-center)")
  , ("M-u"                    , setWallpaper)
  ]

-- ######################################################################################
-- This section is used to convert a workspace ID to a string containing an icon for the focused window in that workspace
-- Can be used for XMobar (Or any status bar desired)

parseWorkspaceId :: WorkspaceIcons -> WorkspaceId -> WorkspaceId -> Maybe String
parseWorkspaceId icons cur i = case M.lookup i icons of
  Nothing | i == cur   -> Just $ "  " <> i <> "  "
  Nothing              -> Nothing
  Just _ | i == "NSP"  -> Nothing
  Just icon | i == cur -> Just $ "%{F#FFFFFF}" ++ i ++ "  " ++ [icon] ++ "%{F-}"
  Just icon            -> Just $ "%{F#777777}%{A:xdotool key Super+" ++ i ++ ":}" ++ i ++ "  " ++ [icon] ++ "%{A}%{F-}"

type WindowClass = String
type WorkspaceIcons = M.Map WorkspaceId Char

myWindowIcons :: M.Map String Char
myWindowIcons = M.fromList
  [ ("kitty"                  , '\xf120')
  , ("firefox"                , '\xf269')
  , ("Blueman-manager"        , '\xf294')
  , ("libreoffice-startcenter", '\xf15c')
  , ("libreoffice-draw"       , '\xf15c')
  , ("Steam"                  , '\xf1b6')
  , ("Zathura"                , '\xf1c1')
  , ("okular"                 , '\xf1c1')
  , ("Spotify"                , '\xf1bc')
  , ("Inkscape"               , '\xf6fc')
  , ("Kodi"                   , '\xf03d')
  , ("Transmission-gtk"       , '\xf019')
  , ("Zotero"                 , '\xf02d')
  , ("Signal"                 , '\xf0e0')
  , ("Thunderbird"            , '\xf6ed')
  , ("minecraft-launcher"     , '\xf872')
  , ("discord"                , '\xfb6e')
  ]

-- | Store the given workspace's name in given and return.
-- This is where to change if the desired format is not <Workspace tag><Window Icon>
storeIcon :: W.Workspace WorkspaceId l Window -> WorkspaceIcons -> X WorkspaceIcons
storeIcon ws aliases = do
  dis <- display <$> ask
  case W.stack ws of
    Nothing -> return aliases
    Just a  -> do
      focused <- io $ resClass <$> getClassHint dis (W.focus a)        -- Get focused window in given workspace
      let wsName = fromMaybe '\xf2d0' (M.lookup focused myWindowIcons) -- Create name to give workspace (i.e. Append icon to tag)
      return $ M.insert (W.tag ws) wsName aliases                    -- Store name in Map

-- ######################################################################################

toggleBorder :: Window -> X ()
toggleBorder w = do
  bw <- asks (borderWidth . config)
  withDisplay $ \d -> io $ do
    cw <- wa_border_width `fmap` getWindowAttributes d w
    if cw == 0 then setWindowBorderWidth d w bw else setWindowBorderWidth d w 0


