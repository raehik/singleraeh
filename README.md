# singleraeh
Explicit singletons. More manual than `singletons`, but potentially easier to
use because you can give GHC more explicit information (compared to `singletons`
which often threads things through the `Sing` type family and `SingI` type
class).

Uses phadej's `defun` package for defunctionalizing type families.

Requires GHC >= 9.6 for the builtin `SNat`, `SSymbol` etc. singletons.

## License
Provided under the MIT license. See `LICENSE` for license text.
