---
layout: post
title: Get Rid of XMonad Focus Thief
tags: xmonad, haskell
---

I\'ve been using xmonad for a while. One annoyance found when enabling
[EWMH](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-EwmhDesktops.html)
(see resources in the last section of my post if you don\'t know what EWMH does)
is that pidgin (and maybe some other programs as well) will try to request for window focus
whenever xmonad starts / restarts,
resulting in your focus moved to the workspace that pidgin belongs to.
Today I spent some time to investigate this issue and would like to share my workaround in this post.

# The Problem and My Workaround

The problem arises immediately after xmonad gets started / restarted. I guess there might be some interaction
to notify running X11 programs that xmonad is up. And some of these programs would respond to this nofication
by sending some requests to xmonad. And the solution is simple:
**we make xmonad ignore focus requesting messages when xmonad just get started**.

This can be done by modifying xmonad state to include the time when it got started,
and wrap the EWMH event handler to check if the message is "too early" before performing the action.
By "too early" I meant the time difference between xmonad startup and when the message get received is
below a threshold (just leave few seconds as threshold to allow your xmonad startup script gets fully executed).

# Working on Code

First we use [`XState`](http://xmonad.org/xmonad-docs/xmonad/XMonad-Core.html#t:XState) to make xmonad
record its startup time. This can be done easily using [`XMonad.Util.ExtensibleState`](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-ExtensibleState.html)
from [xmonad-contrib](http://xmonad.org/xmonad-docs/xmonad-contrib/).

What we need is a new type of data so that we can put it into `XState`. And the data we need is just the startup time:

```haskell
{-# LANGUAGE DeriveDataTypeable #-}
import XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS
import Data.Time.Clock
import Data.Typeable
import Data.Time.Calendar
newtype StartupTime =
    StartupTime UTCTime
        deriving Typeable

instance ExtensionClass StartupTime where
    initialValue = StartupTime $ UTCTime d dt
        where
            d = fromGregorian 1970 1 1
            dt = secondsToDiffTime 0
```

We don\'t have to deal with `extensionType`, as it is default to `StateExtension`.
Of course we want the startup time updated after an xmonad restart.
And actually we would never use `initialValue` since we will update it immediately after
xmonad gets started, but we need to put some value here to make compiler happy,
we could have put an `undefined` or `error "msg"` here, but I chose the safest way.

Next, we modify `startupHook` to record its startup time:

```haskell
import Control.Applicative

myStartupHook :: X ()
myStartupHook = do
    -- <your original startupHook, can leave this part blank>
    StartupTime <$> liftIO getCurrentTime >>= XS.put
```

And EWMH is enabled by passing your config into [`ewmh`](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-EwmhDesktops.html#v:ewmh) function
so that it adds few more hooks to do its job. We make our own `myEwmh` to take care of the event handler:

```haskell
import Data.Monoid
import import XMonad.Hooks.EwmhDesktops

myEwmh :: XConfig a -> XConfig a
myEwmh c = c { startupHook     = startupHook c     <> ewmhDesktopsStartup
             , handleEventHook = handleEventHook c <> myEwmhDesktopsEventHook
             , logHook         = logHook c         <> ewmhDesktopsLogHook
             }
```

And finally we wrap `ewmhDesktopsEventHook` in our `myEwmhDesktopsEventHook` to add an extra guard:
if the request is sent right after xmonad gets started (less than 5 seconds in my code),
the request gets ignored:

```haskell
myEwmhDesktopsEventHook :: Event -> X All
myEwmhDesktopsEventHook e@(ClientMessageEvent
    {ev_message_type = mt}) = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    curTime <- liftIO getCurrentTime
    StartupTime starupTime <- XS.get
    if    mt == a_aw
       && curTime `diffUTCTime` starupTime <= 5.0
       then return (All True)
       else ewmhDesktopsEventHook e
myEwmhDesktopsEventHook e = ewmhDesktopsEventHook e
```

Now you might recompile xmonad and enjoy the change.

# Resources

**About my config**

[My xmonad config](https://github.com/Javran/xmonad-javran) is on github,
and the changes related to my workaround is visualized
[here](https://github.com/Javran/xmonad-javran/compare/031b99ed70867697...ec2fdb2afe869a62).

**About EWMH**

Googling `EWMH` would just give you a "tl;dr" specification which is not useful for most of the user.

Simply put, `EWMH` allows your X11 programs to request for window focus.
[One of my old post](https://www.haskell.org/pipermail/xmonad/2013-December/013920.html) and
[the response](https://www.haskell.org/pipermail/xmonad/2013-December/013921.html) provides a little more
simple explanation.
