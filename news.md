% xmobar - Release notes

## Version 0.16

_New features_

  - New monitor `AutoMPD`, which uses asynchronous events to display
    MPD status (thanks to Ben Boeckel).
  - New monitor `BufferedPipeReader` displaying data from multiple
    pipes (thanks to Jochen Keil).
  - New monitor `DynNetwork`, which detects the active interface
    automatically, by Reto Hablützel
  - `PipeReader`'s default text is now configurable, by Reto
    Hablützel.
  - DBUS support: Jochen implemented an extension for DBUS signal
    handling.
  - Hide/Reveal: one can send signals to xmobar and make it (un)hide
    itself (Jochen again).
  - Dependencies updated to latest mtl and libmpd (thanks to Sergei
    Trofimovich).
  - Dependencies on the deprecated dbus-core removed in favour of
    dbus 0.10 (thanks to Jochen Keil)..

_Bug fixes_

  - `DiskU`, the disk usage monitor, works again correctly on Linux,
    instead of randomly crashing every now and then, and reporting
    wrong used size.

## Version 0.15 (June 4, 2012)

_Incompatible changes_

  - `Batt` monitor no longer uses `-c` to specify the charge file: it
    should figure things out by itself (cf. [issue 69]).

_New features_

  - New command line option, `-C`, that allows *adding* commands to
    those specified in the configuration file (Ben Boeckel).
  - Dependency on GHC's threaded runtime has been eliminated.
  - New MPRIS (versions 1 and 2) monitor, by Artem Tarasov.
  - New monitor option `-d` to specify the number of decimal places to
    display for float numbers.  Defaults to 0. See [issue 58].
  - New compilation option `--with_threaded`, to use GHC's threaded
    runtime to compile xmobar.  Disabled by default (cf. discussion in
    [github #36]).

_Bug fixes_

  - Stricter build dependencies versioning in cabal file.
  - [issue 56]: vertical alignment of text improved.
  - [issue 64]: display of `watts` in `Batt` monitor fixed.
  - [issue 69]: miscellaneous battery reporting issues.
  - [issue 67]: compilation in DragonFly.
  - DiskIO works also when device path in mtab are symbolic links
    to the real device file.
  - Wireless monitor honours padding settings for ESSID names.
  - CoreTemp monitor fixed for newer kernels ([github #38]).

[issue 56]: http://code.google.com/p/xmobar/issues/detail?id=56
[issue 58]: http://code.google.com/p/xmobar/issues/detail?id=58
[issue 64]: http://code.google.com/p/xmobar/issues/detail?id=64
[issue 67]: http://code.google.com/p/xmobar/issues/detail?id=67
[issue 69]: http://code.google.com/p/xmobar/issues/detail?id=69
[github #36]: https://github.com/jaor/xmobar/issues/38
[github #38]: https://github.com/jaor/xmobar/issues/38

## Version 0.14 (Dec 10, 2011)

_New features_

  - New brightness monitor, courtesy of Martin Perner.
  - New DateZone plugin, for configurable timezone and localized
    datetimes, also by Martin.
  - New keyboard layout monitor (Kbd).  Yes, by Martin.
  - Rewrite of the event handling ([issue 53], [issue 57]), you
    guessed it.
  - Cpu monitor now also reports `iowait` field ([issue 55]).
  - Battery monitor: the full charge file is now settable in the
    monitor arguments (olpc systems use `charge_full_design`; see
    [issue 62]).

_Bug fixes_

  - [issue 45]: Fix for crashes with AC status changes in battery monitor.
  - [issue 48]: The <quality> field of Wireless behaves like a percentage.
  - [issue 50]/[issue 61]: `MPD` monitor now works with libmpd 0.6.
  - [issue 60]: Fixes for crashes on power resume for battery monitor.
  - Template sections without fields are now correctly displayed.
  - Catch errors when reading battery status (Ben Boeckel).
  - Compilation issues with ghc 7.x (Sergei Trofimovich).
  - Fixes for CoreTemp monitor in new kernels (Norbert Zeh).
  - Fix for pulseaudio problems in volume monitor (Martin Perner).
  - Fix for parsing errors when a `Run` entry ended in an array
    (Martin).
  - Fixed compilation in OpenBSD (Ivo van der Sangen).

[issue 45]: http://code.google.com/p/xmobar/issues/detail?id=45
[issue 48]: http://code.google.com/p/xmobar/issues/detail?id=48
[issue 50]: http://code.google.com/p/xmobar/issues/detail?id=50
[issue 53]: http://code.google.com/p/xmobar/issues/detail?id=53
[issue 55]: http://code.google.com/p/xmobar/issues/detail?id=55
[issue 57]: http://code.google.com/p/xmobar/issues/detail?id=57
[issue 60]: http://code.google.com/p/xmobar/issues/detail?id=60
[issue 61]: http://code.google.com/p/xmobar/issues/detail?id=61
[issue 62]: http://code.google.com/p/xmobar/issues/detail?id=62

## Version 0.13 (March 28, 2011)

_New features_

  - New `Volume` monitor displaying ALSA soundcards information, by
    Thomas Tuegel.
  - New `ThermalZone` plugin substituting `Thermal` and using linux's
    *sysfs* interface (you need this one if you're using a kernel
    version equal to or above 2.6.37). See [issue 44].
  - xmobar app new has WM_CLASS, WM_NAME and _NET_WM_PID xprops
    ([issue 38]).

_Incompatible changes_

  - In the process of solving [issue 14], we've broken those
    configurations that rely on including alignment separators in the
    input fed to `StdinReader`.
  - The MPD plugin does not accept host and port options anymore: use
    the environment variables MPD_HOST and MPD_PORT instead.
  - The `Mail` plugin now takes a second parameter (a string)
    specifying its alias. As a side-effect, this solves [issue 30].

_Bug fixes_

  - [issue 14]: `StdinReader` and other plugins accepting external
    input don't get confused anymore when characters from `alignSep`
    appear in their input.
  - [issue 27]: `BottomSize` placement now respects its width argument.
  - [issue 28]: Compilation in Mac OS X fixed.
  - [issue 30]: `Mail` plugin can be specified anywhere in commands list.
  - [issue 36]: Battery monitor now supports non-standard locations of
    the `/sys/class/power_supply/AC/online` file.
  - [issue 40]: Battery monitor now supports the new power_now, that
    replaces current_now in linux kernels for v. 2.36 and above.
  - [issue 42]: More accurate net monitor rates.
  - DiskIO, Cpu and MultiCpu monitors are also more accurate now.
  - Text is now correctly centered vertically.
  - `FullBM` border spec fixed.

[issue 14]: http://code.google.com/p/xmobar/issues/detail?id=14
[issue 27]: http://code.google.com/p/xmobar/issues/detail?id=27
[issue 28]: http://code.google.com/p/xmobar/issues/detail?id=28
[issue 30]: http://code.google.com/p/xmobar/issues/detail?id=30
[issue 36]: http://code.google.com/p/xmobar/issues/detail?id=36
[issue 38]: http://code.google.com/p/xmobar/issues/detail?id=38
[issue 40]: http://code.google.com/p/xmobar/issues/detail?id=40
[issue 42]: http://code.google.com/p/xmobar/issues/detail?id=42
[issue 44]: http://code.google.com/p/xmobar/issues/detail?id=44

## Version 0.12 (Dec 24, 2010)

xmobar has a new [maintainer], a new [website], a new [mailing
list] and uses a new [source code repository].

Many thanks to Andrea Rossato, xmobar's author and maintainer so far,
for creating xmobar in the first place, and for giving me the chance
to become its maintainer. And a big thanks to Ben Boeckel, Petr Rockai
and Norbert Zeh for their patches.

[website]: http://projects.haskell.org/xmobar/
[mailing list]: http://projects.haskell.org/cgi-bin/mailman/listinfo/xmobar
[source code repository]: https://github.com/jaor/xmobar
[maintainer]: http://hacks-galore.org/jao/

_New features_

  - Window borders: configuration options `border` and `borderColor`
    allow drawing borders around xmobar's window.
  - New monitor, `Uptime`, showing the system uptime.
  - New monitor argument (`-S`) to enable displaying the `%` symbol in
    percentages or other suffixes (e.g., units in Uptime and Network);
    the symbol is now never included by default.
  - New 'run once' commands, by specifying a 0 refresh rate in `Run
    Com` ([issue 26]).
  - MPD monitor: updated to libmpd 1.5. New fields `ppos` (playlist
    position) and `remaining` (remaining time). New configuration
    options to specify MPD's host, user name and password.
  - Battery monitor: new `watts` and `timeleft` fields (Petr Rockai),
    and specific arguments to control coloring and thresholds of the
    former.
  - MultiCPU monitor: new `auto*` fields that automatically detect all
    present CPUs (Ben Boeckel).
  - CpuFreq monitor uses just one decimal digit for GHz values (Petr
    Rockai).
  - Mail plugin expands paths starting with "~/" (Ben Boeckel). Ditto
    MBox.
  - Weather monitor now skips not retrieved fields, instead of
    displaying a long error message.
  - New compilation flag, `all_extensions`.
  - Documentation and website updates.

_Bug fixes_

  - [issue 23]: Wireless monitor is now compatible with iwlib 29.
  - [issue 24]: Swap monitor's used ratio display fixed.
  - [issue 25]: Percentages only include `%` if requested using `-P`.
  - [issue 31]: MPD monitor now respects `-W` argument.
  - Fixes in CPU frequency formatting, string alignment and colour
    boxes in monitors (Norbert Zeh).
  - TopMem and TopProc now use the `-L` and `-H` options correctly for
    memory template fields.
  - MBox skips non-existent mbox paths instead of hanging.

[issue 23]: http://code.google.com/p/xmobar/issues/detail?id=23
[issue 24]: http://code.google.com/p/xmobar/issues/detail?id=24
[issue 25]: http://code.google.com/p/xmobar/issues/detail?id=25
[issue 26]: http://code.google.com/p/xmobar/issues/detail?id=26
[issue 31]: http://code.google.com/p/xmobar/issues/detail?id=31
