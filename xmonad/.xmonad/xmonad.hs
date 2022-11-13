import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.HintedTile
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeColMid))
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.EZConfig
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Layout.ResizableTile
import XMonad.Layout.ResizableThreeColumns (ResizableThreeCol(ResizableThreeColMid))
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.Script (execScriptHook)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Actions.UpdateFocus (adjustEventInput)
import XMonad.Util.SpawnOnce
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import System.Directory
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

main :: IO ()
main = xmonad 
  . ewmhFullscreen
  . ewmh
  . withEasySB (statusBarProp "xmobar" (pure def)) defToggleStrutsKey
  $ def
  { modMask = mod4Mask
  , layoutHook = myLayout
  , terminal = myTerminal
  , startupHook = myStartupHook
  }
  `additionalKeysP`
  myKeybinds
  where
    myTerminal = "st"

myLayout = spacingWithEdge 10 $ (tiled ||| Mirror tiled ||| Full ||| threeCols)
  where
    threeCols = ResizableThreeColMid nmaster delta ratio []
    tiled = ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = 2/3
    delta = 3/100

myKeybinds = 
  [ ("M-d", unGrab *> spawn "rofi -modi drun -show drun")
  , ("<Print>", unGrab *> spawn "maim -s | xclip -sel clip -t image/png")
  , ("M-t", withFocused toggleFloat)
  , ("M-<Up>", expandSecondary)
  , ("M-<Down>", shrinkSecondary)
  , ("M-C-h", expandSecondary)
  , ("M-C-l", shrinkSecondary)
  , ("<XF86AudioMute>", mute)
  , ("<XF86AudioRaiseVolume>", adjustVolume "+")
  , ("<XF86AudioLowerVolume>", adjustVolume "-")
  ]
  where
    toggleFloat w = windows (\s -> if M.member w (W.floating s)
        then W.sink w s
        else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s) 
    shrinkSecondary = sendMessage MirrorShrink
    expandSecondary = sendMessage MirrorExpand
    volumeCommandPrefix = "amixer set Master "
    mute = unGrab *> spawn (volumeCommandPrefix ++ "toggle")
    adjustVolume x = unGrab *> spawn (volumeCommandPrefix ++ show volumeInterval ++ "%" ++ x)
    volumeInterval = 5

myStartupHook = do
  startupHook desktopConfig
  spawn "~/.fehbg"
  spawn "~/.local/bin/set-screen.bash"
  spawnOnce "picom --no-fading-openclose"
  spawnOnce "discord"
  spawnOnce "flatpak run com.discordapp.Discord"
  spawnOnce "steam"
  adjustEventInput
