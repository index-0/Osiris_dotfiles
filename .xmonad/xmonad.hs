{-# OPTIONS_GHC -Wall -optc-march=znver1 #-}

import Data.List
import Data.Semigroup
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.Run
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

myTerminal :: String
myTerminal = "st"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Num p => p
myBorderWidth = 1

myModMask :: KeyMask
myModMask = mod4Mask

myNormalBorderColor :: [Char]
myNormalBorderColor = "#AEAEAE"

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#EFC334"

myBackgroundColor :: [Char]
myBackgroundColor = "#000000"

myForegroundColor :: [Char]
myForegroundColor = "#AEAEAE"

myWorkspaces :: [[Char]]
myWorkspaces = ["www", "dev", "sys", "eth", "mpd", "gfx", "med", "im", "p2p", "+"]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_d     ), shellPrompt myPromptConfig)
    , ((modm,               xK_m     ), manPrompt myPromptConfig)
    , ((modm,               xK_s     ), sshPrompt myPromptConfig)
    , ((modm .|. shiftMask, xK_q     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Right ), windowGo R False)
    , ((modm,               xK_Left  ), windowGo L False)
    , ((modm,               xK_Up    ), windowGo U False)
    , ((modm,               xK_Down  ), windowGo D False)
    , ((modm .|. shiftMask, xK_Right ), windowSwap R False)
    , ((modm .|. shiftMask, xK_Left  ), windowSwap L False)
    , ((modm .|. shiftMask, xK_Up    ), windowSwap U False)
    , ((modm .|. shiftMask, xK_Down  ), windowSwap D False)
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm,               xK_c     ), spawn "xmonad --recompile; killall -9 xmobar; xmonad --restart")
    ]

    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++

    [ ((modm,       xK_r), spawn "sh /home/index/.xmonad/scripts/remind.sh")
    , ((   0, 0x1008ff17), spawn "mpc next")
    , ((   0, 0x1008ff16), spawn "mpc prev")
    , ((   0, 0x1008ff14), spawn "mpc toggle")
    , ((   0, 0x1008ff15), spawn "mpc stop")
    , ((   0, 0x1008ff13), spawn "amixer -D pulse sset 'Master' 5%+")
    , ((modm, 0x1008ff13), spawn "amixer -D pulse sset 'Master' 1%+")
    , ((   0, 0x1008ff11), spawn "amixer -D pulse sset 'Master' 5%-")
    , ((modm, 0x1008ff11), spawn "amixer -D pulse sset 'Master' 1%-")
    , ((   0, 0x1008ff12), spawn "amixer -D pulse set Master 1+ toggle")
    , ((                 0, 0xff61), spawn "scrot -q 80 -b ~/pictures/screenshots/%Y-%m-%d-%T-screenshot.png && notify-send \"Fullscreen capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")
    , ((              modm, 0xff61), spawn "scrot -q 80 -u ~/pictures/screenshots/%Y-%m-%d-%T-screenshot.png && notify-send \"Window capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")
    , ((modm .|. shiftMask, 0xff61), spawn "import ~/pictures/screenshots/$(date '+%Y-%m-%d-%T')-screenshot.png && notify-send \"Rectangle area capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")
    , ((                       controlMask, 0xff61), spawn "scrot -q 80 -b -e 'xclip -selection clipboard -target image/png -i $f' && notify-send \"Fullscreen capture copied to clipboard.\"")
    , ((              modm .|. controlMask, 0xff61), spawn "scrot -q 80 -u -e 'xclip -selection clipboard -target image/png -i $f' && notify-send \"Window capture copied to clipboard.\"")
    , ((modm .|. shiftMask .|. controlMask, 0xff61), spawn "import png:- | xclip -selection clipboard -t image/png && notify-send \"Rectangle area capture copied to clipboard\"")]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

type SmartBorders a = ModifiedLayout SmartBorder a
type Layouts = (Choose Tall (Choose Grid (Choose ThreeCol (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))))
myLayout :: ModifiedLayout AvoidStruts(SmartBorders (Layouts)) Window
myLayout = avoidStruts $ smartBorders $
  Tall 1 (3/100) (1/2) |||
  Grid |||
  ThreeCol 1 (3/100) (1/2) |||
  simpleTabbed

myLayoutPrinter :: String -> String
myLayoutPrinter "Tall"            = " [ ]= "
myLayoutPrinter "Grid"            = " [G]= "
myLayoutPrinter "ThreeCol"        = " [3]= "
myLayoutPrinter "Tabbed Simplest" = " [T]= "
myLayoutPrinter x                 = x

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [  className =? "MPlayer"             --> doFloat
    ,  className =? "Gimp"                --> doShift "*"
    ,  resource  =? "desktop_window"      --> doIgnore
    ,  resource  =? "kdesktop"            --> doIgnore
    ,  resource  =? "main"                --> doCenterFloat
    ,  title     =? "main"                --> doCenterFloat]


myEventHook :: Event -> X All
myEventHook = docksEventHook

myStartupHook :: MonadIO m => m ()
myStartupHook =
        spawn "feh --bg-fill  ~/pictures/wallpapers/000000.png" >>
        spawn "xsetroot -cursor_name left_ptr"

myPromptConfig :: XPConfig
myPromptConfig = def
    { font = "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1"
    , bgColor = myBackgroundColor
    , fgColor = myForegroundColor
    , fgHLight = "#EFC334"
    , bgHLight = myBackgroundColor
    , promptBorderWidth = 0
    , position = Top
    , alwaysHighlight = True
    , height = 20
    , defaultText = ""
    , searchPredicate = isInfixOf
}

main :: IO ()
main =
    spawnPipe "xmobar .xmonad/xmobarsp" >>
    spawnPipe "xmobar .xmonad/xmobarrc" >>=
    \ xmobarrc -> xmonad $ ewmh def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP $ def {
                      ppCurrent         = xmobarColor "#EFC334" "" . pad
                    , ppHidden          = xmobarColor "#BEBEBE" "" . pad
                    , ppHiddenNoWindows = xmobarColor "#696969" "" . pad
                    , ppTitle           = xmobarColor "#5AF2EE" "" . pad
                    , ppSep             = xmobarColor "#D3D3D3" "" ""
                    , ppUrgent          = xmobarColor "#D9534F" "" . wrap "!" "!"
                    , ppWsSep           = ""
                    , ppOutput          = hPutStrLn xmobarrc
                    , ppLayout          = xmobarColor "#EEEEEE" "" . myLayoutPrinter
                      },
        startupHook        = myStartupHook
        }
