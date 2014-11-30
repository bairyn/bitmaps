PLACEHOLDER
-----
* Fixed build errors.
  * Hide import of `get` `from Control.Monad.Record`.
  * Support ``bitmap-0.0.2``s new major API changes that don't conform to the
    cabal's package versioning policy.
  * Replaced imports of `Foreign` with `System.IO.Unsafe` for `unsafePerformIO`.
    `Foreign` no longer provides `unsafePerformIO`.
  * Replaced calls to `lens` with calls to `lensGS`, since fc-labels recently
    changed lens to take a getter and a modifier rather than a getter and a
    setter.  Eventually this library will be ported from `fc-labels` to `lens`,
    which was written after this project was first developed.
* Fixed warnings.
  * Removed redundant import of `Control.Bitmap.Searchable` from
    `Data.Bitmap.String.Internal`.

0.2.6.2
-----
* Add homepage and bug-reports to cabal file, linking to the github repository
  and its issue tracker, respectively.
* Added `CHANGELOG.md`.
