PLACEHOLDER
-----
* Fixed build errors.
  * Replace imports of `Foreign` with `System.IO.Unsafe` for `unsafePerformIO`.
    `Foreign` no longer provides `unsafePerformIO`.
  * Replace calls to `lens` with calls to `lensGS`, since fc-labels recently
    changed lens to take a getter and a modifier rather than a getter and a
    setter.  Eventually this library will be ported from `fc-labels` to `lens`,
    which was written after this project was first developed.

0.2.6.2
-----
* Add homepage and bug-reports to cabal file, linking to the github repository
  and its issue tracker, respectively.
* Added `CHANGELOG.md`.
