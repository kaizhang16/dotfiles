import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Hooks.DynamicLog
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Util.EZConfig

main = xmonad =<< xmobar myConfig

myConfig =
  def {terminal = myTerminal, layoutHook = myLayout, workspaces = myWorkspaces} `additionalKeys`
  myKeys

myTerminal = "urxvt"

myWorkspaces =
  [ "1 \xf0ac"
  , "2 \xf121"
  , "3 \xf120"
  , "4 \xf233"
  , "5 \xf15c"
  , "6"
  , "7 \xf1d7"
  , "8 \xf1ab"
  , "9 \xf013"
  ]

myLayout = mkToggle (single FULL) (tiled ||| Mirror tiled)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 0.618
    delta = 3 / 100

myModMask = mod1Mask

myKeys =
  [ ((myModMask, xK_o), toggleWS)
  , ((myModMask, xK_f), sendMessage $ Toggle FULL)
  , ((myModMask .|. shiftMask, xK_l), spawn "i3lock-next >/dev/null 2>&1")
  ]
