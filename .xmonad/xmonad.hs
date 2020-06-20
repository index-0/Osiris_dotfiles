{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, MultiParamTypeClasses, ImplicitParams #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}

import XMonad
import XMonad.Util.Run 

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks 
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.Navigation2D

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh

import Data.Monoid
import Data.List
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "st"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
 
-- NOTE: from 0.9.1 on numlock mask is set automatically. The numlockMask
-- setting should be removed from configs.
--
-- You can safely remove this even on earlier xmonad versions unless you
-- need to set it to something other than the default mod2Mask, (e.g. OSX).
--
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- myNumlockMask   = mod2Mask -- deprecated in xmonad-0.9.1
------------------------------------------------------------
 
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["www", "dev", "sys", "mpd", "doc", "gfx", "media", "irc", "extra"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#101010"
myFocusedBorderColor = "#5074BE"
myBackgroundColor    = "#000000"
myForegroundColor    = "#ffffff"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_d     ), shellPrompt myPromptConfig)
    , ((modm,               xK_m     ), manPrompt myPromptConfig)
    , ((modm,               xK_s     ), sshPrompt myPromptConfig)

    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
   -- Directional navigation of windows
    , ((modm,                 xK_Right), windowGo R False)
    , ((modm,                 xK_Left ), windowGo L False)
    , ((modm,                 xK_Up   ), windowGo U False)
    , ((modm,                 xK_Down ), windowGo D False)
    
    -- Swap adjacent windows
    , ((modm .|. shiftMask, xK_Right), windowSwap R False)
    , ((modm .|. shiftMask, xK_Left ), windowSwap L False)
    , ((modm .|. shiftMask, xK_Up   ), windowSwap U False)
    , ((modm .|. shiftMask, xK_Down ), windowSwap D False)
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
 
    -- Restart xmonad
    , ((modm,               xK_c     ), spawn "xmonad --recompile; killall -9 conky; xmonad --restart")
    ]
    
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
    ++

     -- Mpc
    [ ((0, 0x1008ff17), spawn "mpc next")
    , ((0, 0x1008ff16), spawn "mpc prev")
    , ((0, 0x1008ff14), spawn "mpc toggle")
    , ((0, 0x1008ff15), spawn "mpc stop")
    
    -- Volume
    , ((0, 0x1008ff13), spawn "amixer -c 1 sset 'Master' 3%+")
    , ((0, 0x1008ff11), spawn "amixer -c 1 sset 'Master' 3%-")
    , ((0, 0x1008ff12), spawn "amixer -c 1 sset 'Master' toggle")
    
    -- Screenshot
    , ((0,                  0xff61), spawn "scrot -q 80 -b ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png && notify-send \"Fullscreen capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")
    , ((modm,               0xff61), spawn "scrot -q 80 -u ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png && notify-send \"Window capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")
    , ((modm .|. shiftMask, 0xff61), spawn "scrot -q 80 -s ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png && notify-send \"Rectangle area capture saved as:\" \"`date +%Y`-`date +%m`-`date +%d`-`date +%T`-screenshot.png\"")]



------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ smartBorders (tiled ||| Grid ||| threeColumns ||| simpleTabbed)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    threeColumns = ThreeCol 1 (3/100) (1/2)

------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [  className =? "MPlayer"         --> doFloat
    ,  className =? "Gimp"            --> doShift "*"
    ,  resource  =? "desktop_window"  --> doIgnore
    ,  resource  =? "kdesktop"        --> doIgnore
    ,  resource  =? "main"            --> doCenterFloat
    ,  title     =? "main"            --> doCenterFloat]

------------------------------------------------------------------------
-- Event handling
 
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = docksEventHook
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = do
        spawn "feh --bg-fill /home/index/Pictures/Wallpapers/Anime/Serial_Experiments_Lain/1561835350723-1.jpg"
        spawn "conky -c /home/index/.config/conky/config"
        spawn "xsetroot -cursor_name left_ptr"

myPromptConfig = def
    { font = "xft:terminus:pixelsize=12:antialias=false" 
    , bgColor = myBackgroundColor
    , fgColor = myForegroundColor
    , fgHLight = "#5074BE"
    , bgHLight = "#000032"
    , promptBorderWidth = 0
    , position = Top
    , alwaysHighlight = True
    , height = 20
    , defaultText = ""
    , searchPredicate = isInfixOf
}

main = do
    h <- spawnPipe "xmobar .xmobarrc"
    xmonad $ ewmh $ def {
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
                      ppCurrent = xmobarColor "#5074BE" "" . wrap "[" "]" . pad
                    , ppHidden  = xmobarColor "#ffffff" "" . wrap "" "" . pad
                    , ppTitle   = xmobarColor "#e2e2e2" "" . shorten 80
                    , ppSep     = xmobarColor "#5074BE" "" "; "
                    , ppUrgent  = xmobarColor "#C45500" "" . wrap "!" "!" 
                    , ppWsSep   = ""
                    , ppOutput  = hPutStrLn h 
                    , ppLayout  = const ""
                      },
        startupHook        = myStartupHook
    }