open BasicTests
open PermTests

open Common.Definitions

module M3Basic = MakeBasicTests(Three)
module M4Basic = MakeBasicTests(Four)
module M5Basic = MakeBasicTests(Five)

module M3Perm = MakePermTests(Three)
module M4Perm = MakePermTests(Four)
module M5Perm = MakePermTests(Five)
