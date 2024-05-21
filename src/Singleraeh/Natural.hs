module Singleraeh.Natural where

import GHC.TypeNats
import Unsafe.Coerce ( unsafeCoerce )

(%+) :: SNat n -> SNat m -> SNat (n + m)
n %+ m = withSomeSNat (fromSNat n + fromSNat m) unsafeCoerce

(%-) :: SNat n -> SNat -> SNat (n - m)
n %- m = withKnownNat (fromSNat n - fromSNat m) unsafeCoerce

sMod :: SNat n -> SNat m -> SNat (Mod n m)
sMod n m = withSomeSNat (mod (fromSNat n) (fromSNat m)) unsafeCoerce

sDiv :: SNat n -> SNat m -> SNat (Div n m)
sDiv n m = withSomeSNat (div (fromSNat n) (fromSNat m)) unsafeCoerce
