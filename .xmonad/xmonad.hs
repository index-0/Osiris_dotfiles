{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, MultiParamTypeClasses, ImplicitParams #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -optc-march=znver1 #-}

import XMonad
import XMonad.Util.Run 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks 
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.Navigation2D
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import Data.List
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "st"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myBorderWidth   = 1
myModMask       = mod4Mask
myWorkspaces    = ["www", "dev", "sys", "doc", "mpd", "gfx", "med", "im", "p2p", "+"]
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#FF9CFE"
myBackgroundColor    = "#000000"
myForegroundColor    = "#AEAEAE"

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
    , ((modm,               xK_c     ), spawn "xmonad --recompile; killall -9 conky; xmonad --restart")
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

    [ ((   0, 0x1008ff17), spawn "mpc next")
    , ((   0, 0x1008ff16), spawn "mpc prev")
    , ((   0, 0x1008ff14), spawn "mpc toggle")
    , ((   0, 0x1008ff15), spawn "mpc stop")
    , ((   0, 0x1008ff13), spawn "amixer -D pulse sset 'Master' 5%+")
    , ((modm, 0x1008ff13), spawn "amixer -D pulse sset 'Master' 1%+")
    , ((   0, 0x1008ff11), spawn "amixer -D pulse sset 'Master' 5%-")
    , ((modm, 0x1008ff11), spawn "amixer -D pulse sset 'Master' 1%-")
    , ((   0, 0x1008ff12), spawn "amixer -D pulse set Master 1+ toggle")
    , ((   0,               0xff61), spawn "scrot -q 80 -b ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png && notify-send \"Fullscreen capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")
    , ((modm,               0xff61), spawn "scrot -q 80 -u ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png && notify-send \"Window capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")
    , ((modm .|. shiftMask, 0xff61), spawn "import ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png && notify-send \"Rectangle area capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = avoidStruts $ smartBorders (tiled ||| Grid ||| threeColumns ||| simpleTabbed)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    threeColumns = ThreeCol 1 (3/100) (1/2)

myLayoutPrinter :: String -> String
myLayoutPrinter "Tall"            = " [ ]= "
myLayoutPrinter "Grid"            = " [G]= "
myLayoutPrinter "ThreeCol"        = " [3]= "
myLayoutPrinter "Tabbed Simplest" = " [T]= "
myLayoutPrinter x                 = x

myManageHook = composeAll
    [  className =? "MPlayer"             --> doFloat
    ,  className =? "Gimp"                --> doShift "*"
    ,  resource  =? "desktop_window"      --> doIgnore
    ,  resource  =? "kdesktop"            --> doIgnore
    ,  resource  =? "main"                --> doCenterFloat
    ,  title     =? "main"                --> doCenterFloat]

myEventHook = docksEventHook
 
myStartupHook = do
        spawn "feh --bg-fill /home/index/Pictures/Wallpapers/000000.png"
        spawn "conky -c /home/index/.config/conky/config"
        spawn "xsetroot -cursor_name left_ptr"

myPromptConfig = def
    { font = "-misc-fixed-medium-r-normal-*-14-130-75-75-c-70-iso10646-1" 
    , bgColor = myBackgroundColor
    , fgColor = myForegroundColor
    , fgHLight = "#FF9CFE"
    , bgHLight = myBackgroundColor
    , promptBorderWidth = 0
    , position = Top
    , alwaysHighlight = True
    , height = 20
    , defaultText = ""
    , searchPredicate = isInfixOf
}

main = 
    spawnPipe "xmobar .xmobarrc" >>= \h ->
    xmonad $ def {
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
                      ppCurrent = xmobarColor "#FF9CFE" "" . wrap "[" "]" . pad
                    , ppHidden  = xmobarColor "#FFFFCB" "" . wrap "" "" . pad
                    , ppTitle   = xmobarColor "#ABABAB" "" . shorten 65
                    , ppSep     = xmobarColor "#D3D3D3" "" ""
                    , ppUrgent  = xmobarColor "#D9534F" "" . wrap "!" "!" 
                    , ppWsSep   = ""
                    , ppOutput  = hPutStrLn h
                    , ppLayout  = xmobarColor "#EEEEEE" "" . myLayoutPrinter
                      },
        startupHook        = myStartupHook
    }
