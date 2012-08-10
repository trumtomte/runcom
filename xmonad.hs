import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import System.IO
import System.Exit
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "urxvt"
myBorderWidth   = 1
myModMask       = mod1Mask
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#333333"
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myDmenu = "exe=`dmenu_path | dmenu -fn 'terminus-13' -nb '#111111' -nf '#ffffff' -sb '#2b2b2b' -sf '#ee9a00'` && exec $exe"
 

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts(
            Tall 1 (3/100) (1/2) |||
            Mirror (Tall 1 (3/100) (1/2))) |||
            noBorders (fullscreenFull Full)
        , modMask = myModMask
        , terminal = myTerminal
        , focusFollowsMouse = True
        , borderWidth = myBorderWidth
        , workspaces = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys = myKeys
        , mouseBindings = myMouseBindings
        , startupHook = myStartupHook
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "#5bc0de" "" . shorten 80
            , ppCurrent = xmobarColor "#ee9a00" "" . wrap "[" "]"
            , ppSep = xmobarColor "#555555" "" " : "
            , ppHiddenNoWindows = xmobarColor "#555555" ""
            , ppLayout = \x -> case x of
                            "Tall" -> "T"
                            "Mirror Tall" -> "M"
                            "Full" -> "F"
                            _ -> "?"
            }
        }

myStartupHook = return ()

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn myDmenu)
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), restart "xmonad" True)
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
 
